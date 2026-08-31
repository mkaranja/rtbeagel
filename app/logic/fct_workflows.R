box::use(
  yaml[read_yaml],
  DBI[dbGetQuery, dbExecute],
)

# ============================================================================
# RTB-EAGEL · WORKFLOW ENGINE
# ----------------------------------------------------------------------------
# Replaces the R6 create_workflow_manager() in the old fct_workflows.R.
#
# WHAT WAS WRONG BEFORE
#
#  1. load_workflow_from_yaml() pulled only status/next_step/next_stage out of
#     the YAML. Every node using `rules:` or `parallel_options:` therefore had
#     no top-level next_step and silently became NA -> "No next step defined".
#     That killed exactly the four branch points that matter:
#     reception/approved, molecular_virus_indexing/completed,
#     subculture/completed, hardening/completed.
#
#  2. The YAML held R source strings ("if(isTRUE(sample_type == 'cutting'))...")
#     which were eval(parse())'d at runtime. evaluate_conditional_step() then
#     pattern-matched on the TEXT of those expressions to decide what to do:
#         if (grepl("services.*pathogen_detection.*service_count.*1", expr))
#     Config that is code is untestable, injectable, and breaks on whitespace.
#
#  3. Statuses were compound strings ("in_vitro_conservation_established")
#     that had to be split back apart with str_remove(). Ambiguous and fragile.
#
#  4. Nothing validated stage names, so cassava.yaml shipped with BOTH
#     `in_vitro_conservation` and `invitro_conservation` - a fan-out pointing
#     at a stage no record ever used. Failed silently, forever.
#
# WHAT THIS DOES INSTEAD
#
#  * Nodes are keyed on (stage, state) - matching tbl_stage_state - so nothing
#    needs parsing.
#  * Branches are DATA. Four block kinds, and the difference between them is
#    WHO decides and HOW MANY of the matches are taken:
#
#        then:     one destination, unconditional.          engine decides.
#        rules:    first matching branch wins; default: is   engine decides,
#                  the fallback.                             ONE taken.
#        fan_out:  every matching branch applies AT ONCE -   engine decides,
#                  the order proceeds down several paths     ALL taken.
#                  simultaneously (conservation AND
#                  distribution).
#        choice:   every matching branch is OFFERED; the     OPERATOR decides,
#                  operator takes exactly ONE.               ONE taken.
#
#    fan_out and choice look alike in the data - both filter by `when:` and
#    can return several rows - but they mean opposite things about the world.
#    fan_out is "and": the material splits and goes everywhere it matches.
#    choice is "or": the material goes to exactly one of the offered places,
#    and a human picks which. cassava at reception is the case that needs it:
#    the normal route is quarantine (glasshouse or growthroom by sample_type),
#    but cassava may instead go straight to thermotherapy - and BOTH are
#    standard, so neither can be an off-workflow move demanding a reason.
#
#    This distinction matters at record time: because next_options() returns
#    ALL offered branches of a choice, record_event() sees the operator's pick
#    inside the recommended set and does NOT flag it as an override. That is
#    the whole point - a sanctioned alternative is not a departure.
#  * Conditions are matched by set intersection. No eval, no parse, no regex.
#  * validate() checks every stage/state against the database vocabulary and
#    fails LOUDLY at startup rather than quietly at 2am.
# ============================================================================


# ---- condition matching ----------------------------------------------------
# A condition is a named list: list(service = c("a","b"), sample_type = "cutting").
# It matches when, for EVERY named variable, at least one wanted value is
# present in the context.
#
# One rule covers both cases the old code special-cased: equality is just
# membership in a one-element set. ctx$sample_type = "cutting" against
# when$sample_type = c("cutting") intersects; ctx$service = c("pathogen_detection",
# "in_vitro_distribution") against when$service = c("in_vitro_distribution")
# intersects too.
match_when <- function(when, ctx) {
  if (is.null(when) || length(when) == 0) return(TRUE)
  for (var in names(when)) {
    have <- ctx[[var]]
    if (is.null(have) || length(have) == 0) return(FALSE)
    want <- when[[var]]
    if (!any(tolower(as.character(want)) %in% tolower(as.character(have)))) return(FALSE)
  }
  TRUE
}

empty_options <- function() {
  data.frame(to_stage = character(0), step = character(0), label = character(0),
             kind = character(0), stringsAsFactors = FALSE)
}

as_option <- function(e, kind) {
  data.frame(
    to_stage = e$to_stage %||% NA_character_,
    step     = e$step     %||% NA_character_,
    label    = e$label    %||% NA_character_,
    kind     = kind,
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a


#' Load a workflow definition from YAML.
#'
#' @param path path to the workflow file
#' The unconditional transition key is `then:`, NOT `next:`. `next` is a
#' reserved word in R - a loop-control keyword - so `node$next` is a parse
#' error, not a runtime one: the whole module fails to load. Do not rename it
#' back.
#'
#' @return list(workflow_code, label, nodes = named list keyed "stage/state")
#' @export
load_workflow <- function(path) {
  if (!file.exists(path)) stop("Workflow file not found: ", path, call. = FALSE)
  raw <- read_yaml(path)
  
  if (is.null(raw$nodes)) {
    stop("Workflow '", path, "' has no `nodes:`. This engine expects the ",
         "stage/state format; the legacy `stages:` format with compound ",
         "status strings is no longer supported.", call. = FALSE)
  }
  
  nodes <- list()
  for (n in raw$nodes) {
    if (is.null(n$stage) || is.null(n$state)) {
      stop("Workflow node missing stage/state: ", paste(unlist(n), collapse = " "), call. = FALSE)
    }
    key <- paste0(n$stage, "/", n$state)
    if (!is.null(nodes[[key]])) stop("Duplicate workflow node: ", key, call. = FALSE)
    nodes[[key]] <- n
  }
  
  list(
    workflow_code = raw$workflow_code %||% tools::file_path_sans_ext(basename(path)),
    label         = raw$label,
    description   = raw$description,
    nodes         = nodes
  )
}


#' Validate a workflow against the database vocabulary.
#'
#' Every (stage, state) a node is keyed on must be a legal pair in
#' tbl_stage_state, and every stage a transition targets must exist in
#' tbl_stage. This is what would have caught `invitro_conservation` the day
#' it was typed instead of months later.
#'
#' @param wf a workflow from load_workflow()
#' @param conn a DBI connection or pool
#' @param strict if TRUE (default) raise on any problem; else return them
#' @return character vector of problems (empty if valid)
#' @export
validate_workflow <- function(wf, conn, strict = TRUE) {
  stages <- dbGetQuery(conn, "SELECT stage_code FROM tbl_stage")$stage_code
  pairs  <- dbGetQuery(conn, "SELECT stage_code, state_code FROM tbl_stage_state")
  legal  <- paste0(pairs$stage_code, "/", pairs$state_code)
  
  problems <- character(0)
  
  for (key in names(wf$nodes)) {
    if (!key %in% legal) {
      problems <- c(problems, sprintf(
        "node '%s' is not a legal (stage, state) pair in tbl_stage_state", key))
    }
    n <- wf$nodes[[key]]
    
    # Exactly one transition block. The header promises it and next_options()
    # assumes it (it checks then -> fan_out -> choice -> rules and returns at
    # the first present). A node with both `rules:` and `choice:` would
    # silently run only the rules and ignore the choice - so make that a
    # validation failure, not a surprise at 2am.
    blocks <- intersect(c("then", "rules", "fan_out", "choice"), names(n))
    if (length(blocks) > 1) {
      problems <- c(problems, sprintf(
        "node '%s' has %d transition blocks (%s); exactly one is allowed",
        key, length(blocks), paste(blocks, collapse = ", ")))
    }
    
    # An all-match block (choice/fan_out) evaluates EVERY branch, so a branch
    # with no `when:` matches unconditionally - match_when(NULL, ctx) is TRUE.
    # Under `rules:` that is exactly what a fallback should do, because the
    # first-match-wins loop stops before reaching it. Under choice/fan_out
    # there is no such loop, so the same YAML silently becomes "always offer
    # this as well".
    #
    # That is how reception/approved came to offer the glasshouse and the
    # growthroom at the same time: a `rules:` default was carried over verbatim
    # when the node was converted to `choice:`. Requiring the branch to say
    # `default: true` out loud makes the intent explicit and makes the mistake
    # impossible to repeat silently.
    for (grp in c("choice", "fan_out")) {
      br <- n[[grp]] %||% list()
      if (!length(br)) next
      for (i in seq_along(br)) {
        e <- br[[i]]
        if (is.null(e$when) && !isTRUE(e$default)) {
          problems <- c(problems, sprintf(
            paste0("node '%s' %s branch %d ('%s') has no `when:` and is not ",
                   "marked `default: true`, so it matches unconditionally and ",
                   "will be offered alongside every other branch"),
            key, grp, i, e$label %||% (e$to_stage %||% "?")))
        }
      }
      # Two branches that resolve to the same (to_stage, step) render as the
      # same option twice - visible to the operator as a duplicated line with
      # no way to tell the entries apart.
      #
      # `default: true` branches are excluded, because a default legitimately
      # restates a target it can never appear alongside: reception/approved
      # routes in_vitro to the growthroom explicitly AND falls back to the
      # growthroom for an unrecognised sample type. Those two never fire in the
      # same evaluation, so they are not a duplicate.
      guarded <- Filter(function(e) !isTRUE(e$default), br)
      sig <- vapply(guarded, function(e)
        paste0(e$to_stage %||% "", "/", e$step %||% ""), character(1))
      dup <- unique(sig[duplicated(sig)])
      for (dsig in dup) {
        problems <- c(problems, sprintf(
          "node '%s' %s has %d branches resolving to the same option '%s'",
          key, grp, sum(sig == dsig), dsig))
      }
    }
    
    targets <- character(0)
    if (!is.null(n$then$to_stage)) targets <- c(targets, n$then$to_stage)
    for (grp in c("rules", "fan_out", "choice")) {
      for (e in n[[grp]] %||% list()) targets <- c(targets, e$to_stage %||% NA_character_)
    }
    for (t in stats::na.omit(targets)) {
      if (!t %in% stages) {
        problems <- c(problems, sprintf(
          "node '%s' targets stage '%s', which is not in tbl_stage", key, t))
      }
    }
  }
  
  if (length(problems) && strict) {
    stop("Workflow '", wf$workflow_code, "' failed validation:\n  - ",
         paste(problems, collapse = "\n  - "), call. = FALSE)
  }
  problems
}


#' Resolve the next options for a given position in the workflow.
#'
#' @param wf     workflow from load_workflow()
#' @param stage  current stage_code
#' @param state  current state_code
#' @param ctx    named list of context, e.g.
#'               list(service = c("pathogen_detection","in_vitro_distribution"),
#'                    sample_type = "cutting", crop = "CASSAVA",
#'                    virus_indexing = "negative")
#' @return data.frame(to_stage, step, label, kind). Zero rows means terminal.
#'         `kind` is "single", "rule", "fan_out", or "choice". Both fan_out
#'         and choice may return SEVERAL rows, and the caller MUST read `kind`
#'         to know what several rows mean: fan_out rows all happen (render them
#'         joined by "+"); choice rows are alternatives (render them joined by
#'         "|", one to be picked). order_board() already keys its separator off
#'         `kind` this way.
#' @export
next_options <- function(wf, stage, state, ctx = list()) {
  key <- paste0(stage, "/", state)
  n <- wf$nodes[[key]]
  if (is.null(n)) return(empty_options())
  
  # plain, unconditional transition
  if (!is.null(n$then)) return(as_option(n$then, "single"))
  
  # fan-out: EVERY matching branch is taken (AND). The order splits.
  #
  # `default: true` is honoured here exactly as it is under `rules:` - the
  # branch is EXCLUDED from the normal pass and only fires if nothing else
  # matched. Without that exclusion a branch carrying no `when:` matches
  # everything (match_when(NULL, ctx) is TRUE by definition), so a catch-all
  # written for `rules:` becomes an unconditional extra option the moment the
  # node is converted to an all-match block. That is not hypothetical: it is
  # what put "Establish in Quarantine Glasshouse" and "Receive in Quarantine
  # Growthroom" on screen together.
  if (!is.null(n$fan_out)) {
    hits <- Filter(function(e) !isTRUE(e$default) && match_when(e$when, ctx), n$fan_out)
    if (!length(hits)) hits <- Filter(function(e) isTRUE(e$default), n$fan_out)
    if (!length(hits)) return(empty_options())
    return(do.call(rbind, lapply(hits, as_option, kind = "fan_out")))
  }
  
  # choice: EVERY matching branch is OFFERED (OR); the operator takes one.
  # Same filtering as fan_out, opposite meaning - see the header. A one-match
  # choice (e.g. a non-cassava cutting: only glasshouse matches) renders as a
  # single recommendation, which is correct: there was no real choice to make.
  #
  # Same `default: true` handling as fan_out above.
  if (!is.null(n$choice)) {
    hits <- Filter(function(e) !isTRUE(e$default) && match_when(e$when, ctx), n$choice)
    if (!length(hits)) hits <- Filter(function(e) isTRUE(e$default), n$choice)
    if (!length(hits)) return(empty_options())
    return(do.call(rbind, lapply(hits, as_option, kind = "choice")))
  }
  
  # rules: first match wins; `default: true` is the fallback
  if (!is.null(n$rules)) {
    for (e in n$rules) {
      if (isTRUE(e$default)) next
      if (match_when(e$when, ctx)) return(as_option(e, "rule"))
    }
    for (e in n$rules) if (isTRUE(e$default)) return(as_option(e, "rule"))
    return(empty_options())
  }
  
  empty_options()   # terminal node
}


#' Build the context for an order straight from the database.
#'
#' The old engine expected the caller to assemble `services`, `service_count`,
#' `crop`, `sample_type` and `virus_indexing` by hand, which is why the two
#' YAMLs drifted apart. One query, one shape.
#'
#' Two context variables are DERIVED and easy to get subtly wrong:
#'
#'  * virus_indexing - LATEST-ROUND, per the samples' most recent test round,
#'    NOT bool_or over all history. The old order-level bool_or was monotonic
#'    (append-only results never un-positive), which deadlocked the
#'    indexing->thermotherapy->meristem->re-index loop. Read from
#'    view_sample_virus_status (007), which is per-sample and non-monotonic.
#'    At order level here we take "any sample positive in its own latest
#'    round" - correct for the board summary. The PER-SAMPLE gate the
#'    workflow actually applies at a specific sample lives in
#'    sample_context() below.
#'
#'  * needs_cleaning - does this order request any FULFILMENT service (as
#'    opposed to diagnostic-only)? Gates indexing/completed so a
#'    pathogen-detection-only order finishes at indexing instead of being
#'    pushed down the cleaning line it never asked for.
#'
#' @export
order_context <- function(conn, order_number) {
  q <- "
    SELECT
      c.crop_name          AS crop,
      d.sample_type_code   AS sample_type,
      COALESCE((SELECT array_agg(DISTINCT os.service_code)
                FROM tbl_order_service os
                WHERE os.order_number = o.order_number
                  AND os.cancelled_on IS NULL), '{}') AS services,
      -- any FULFILMENT service requested => the order wants material back,
      -- not just a diagnostic result
      EXISTS (SELECT 1 FROM tbl_order_service os
                JOIN tbl_service_catalog sc ON sc.service_code = os.service_code
               WHERE os.order_number = o.order_number
                 AND os.cancelled_on IS NULL
                 AND sc.service_kind = 'fulfilment') AS needs_cleaning,
      -- latest-round, per sample; positive if ANY sample is currently positive
      (SELECT bool_or(v.is_positive)
         FROM view_sample_virus_status v
         JOIN tbl_sample s ON s.sample_code = v.sample_code
        WHERE s.order_number = o.order_number) AS any_positive
    FROM tbl_order o
    LEFT JOIN tbl_order_detail d ON d.order_number = o.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    WHERE o.order_number = $1"
  r <- dbGetQuery(conn, q, params = list(order_number))
  if (nrow(r) == 0) return(list())
  
  list(
    crop           = r$crop[1],
    sample_type    = r$sample_type[1],
    service        = if (is.list(r$services)) r$services[[1]] else r$services[1],
    needs_cleaning = if (isTRUE(r$needs_cleaning[1])) "yes" else "no",
    # order-level rollup, for the board. A specific sample's gate uses
    # sample_context().
    virus_indexing = if (isTRUE(r$any_positive[1])) "positive" else "negative"
  )
}


#' Context for a SPECIFIC sample: the order context, but with virus_indexing
#' resolved to THIS sample's latest round.
#'
#' This is what a stage module passes to next_options() when moving one
#' sample. Two samples of the same order can sit in indexing/completed with
#' different latest-round results - one positive (-> thermotherapy), one
#' negative (-> surface sterilization) - and only a per-sample context routes
#' them correctly. order_context()'s order-level rollup cannot.
#'
#' @export
sample_context <- function(conn, sample_code) {
  on <- dbGetQuery(conn,
                   "SELECT order_number FROM tbl_sample WHERE sample_code = $1",
                   params = list(sample_code))
  if (nrow(on) == 0) return(list())
  ctx <- order_context(conn, on$order_number[1])
  
  v <- dbGetQuery(conn,
                  "SELECT is_positive FROM view_sample_virus_status WHERE sample_code = $1",
                  params = list(sample_code))
  # No result yet => not positive (the gate only fires on a recorded positive).
  ctx$virus_indexing <- if (nrow(v) && isTRUE(v$is_positive[1])) "positive" else "negative"
  
  # Has this sample's LINEAGE cleared every test its order requires?
  #
  # A separate question from `virus_indexing`, and the workflow needs both.
  # `virus_indexing` reports the absence of a positive, so it reads "negative"
  # for material nobody has tested yet - fine for a gate that only fires on a
  # recorded positive, wrong for one that releases material as clean. Gating
  # surface sterilization on it would have recommended sterilizing untested
  # tissue.
  #
  # The strings are "clear"/"pending" rather than yes/no because YAML parses a
  # bare yes and no as booleans, and the condition matcher compares strings.
  cl <- dbGetQuery(conn,
                   "SELECT is_clear FROM view_sample_clearance WHERE sample_code = $1",
                   params = list(sample_code))
  ctx$lineage_clear <- if (nrow(cl) && isTRUE(cl$is_clear[1])) "clear" else "pending"
  ctx
}


#' Convenience: cache loaded workflows so the YAML is parsed once per process.
#' @export
workflow_cache <- local({
  store <- list()
  function(path, conn = NULL) {
    key <- normalizePath(path, mustWork = FALSE)
    if (is.null(store[[key]])) {
      wf <- load_workflow(path)
      if (!is.null(conn)) validate_workflow(wf, conn)   # fail loudly, at startup
      store[[key]] <<- wf
    }
    store[[key]]
  }
})


# ============================================================================
# ADVISORY MODEL  ·  recommended != permitted
# ----------------------------------------------------------------------------
# The old engine answered one question: "what is the next step?" - as though
# there were exactly one, always. Real labs constantly do something else:
# repeat a contaminated batch, hold material pending a customer call, take
# a clean diagnostic result and multiply it for sale.
#
# A workflow that forbids those will simply be worked around, and then the
# database records fiction. So there are two questions, and they have
# different answers:
#
#   next_options()       what the workflow RECOMMENDS. Advisory. Show it first.
#   allowed_transitions() what the vocabulary PERMITS. Everything legal.
#
# The UI should lead with the recommendation and keep the full list one click
# away. Departing from the recommendation is allowed - it just asks for a
# reason, which lands in tbl_sample_event.override_reason.
# ============================================================================

#' Every state legally reachable at a stage, per tbl_stage_state.
#'
#' This is the escape hatch: the complete set of transitions the DATABASE
#' will accept, independent of what the workflow suggests. Nothing here can
#' corrupt the record, because tbl_sample_event's composite FK still applies.
#'
#' @param conn  DBI connection or pool
#' @param stage stage_code to list states for; NULL for all stages
#' @return data.frame(stage_code, state_code, label, is_failure)
#' @export
allowed_transitions <- function(conn, stage = NULL) {
  if (is.null(stage)) {
    dbGetQuery(conn, "
      SELECT ss.stage_code, ss.state_code, st.label, st.is_failure
      FROM tbl_stage_state ss
      JOIN tbl_state st ON st.state_code = ss.state_code
      ORDER BY ss.stage_code, st.label")
  } else {
    dbGetQuery(conn, "
      SELECT ss.stage_code, ss.state_code, st.label, st.is_failure
      FROM tbl_stage_state ss
      JOIN tbl_state st ON st.state_code = ss.state_code
      WHERE ss.stage_code = $1
      ORDER BY st.label", params = list(stage))
  }
}


#' Recommended next steps, plus everything else that is permitted.
#'
#' One call for the UI: `recommended` drives the primary buttons, `other`
#' populates the "something else" menu. When `recommended` is empty the user
#' is not stuck - `other` still lists every legal move.
#'
#' @param wf    workflow from load_workflow()
#' @param conn  DBI connection or pool
#' @param stage current stage_code
#' @param state current state_code
#' @param ctx   context from order_context()
#' @return list(recommended = data.frame, other = data.frame, is_terminal = logical)
#' @export
recommend <- function(wf, conn, stage, state, ctx = list()) {
  rec <- next_options(wf, stage, state, ctx)
  
  all_moves <- allowed_transitions(conn)
  # anything the workflow did not put forward, at any stage, is still legal
  other <- all_moves[!paste0(all_moves$stage_code, "/", all_moves$state_code) %in%
                       paste0(rec$to_stage, "/", ""), , drop = FALSE]
  
  list(
    recommended = rec,
    other       = other,
    is_terminal = nrow(rec) == 0
  )
}


#' Record a sample event, flagging it when it departs from the recommendation.
#'
#' Deliberately does NOT refuse off-workflow moves. It writes them, marked,
#' with the reason. A refusal here would push the real decision onto paper
#' and out of the system - the exact failure this design exists to prevent.
#'
#' @param conn        DBI connection or pool
#' @param sample_code sample being moved
#' @param to_stage,to_state the destination
#' @param actor       username
#' @param wf,ctx      workflow + context, to judge whether this is an override
#' @param reason      required when the move is off-workflow
#' @export
record_event <- function(conn, sample_code, to_stage, to_state, actor,
                         wf = NULL, ctx = list(), reason = NULL, notes = NULL) {
  is_override <- FALSE
  
  if (!is.null(wf)) {
    cur <- dbGetQuery(conn,
                      "SELECT stage_code, state_code FROM view_sample_current WHERE sample_code = $1",
                      params = list(sample_code))
    if (nrow(cur)) {
      rec <- next_options(wf, cur$stage_code[1], cur$state_code[1], ctx)
      is_override <- !(to_stage %in% rec$to_stage)
    }
  }
  
  if (is_override && (is.null(reason) || !nzchar(reason))) {
    stop("Moving ", sample_code, " to ", to_stage, "/", to_state,
         " departs from the recommended workflow. That is allowed - but a ",
         "reason is required.", call. = FALSE)
  }
  
  # RPostgres binds must each be length 1. A bare NULL is length 0 and raises
  # "Parameter N does not have length 1" - so a NULL notes or a NULL reason
  # (the common on-workflow case, where is_override is FALSE) has to be coerced
  # to NA, which binds as SQL NULL. This is not cosmetic: without it EVERY
  # on-workflow event fails, because reason is NULL whenever is_override is FALSE.
  notes_bind  <- if (is.null(notes)  || length(notes)  == 0) NA_character_ else notes
  reason_bind <- if (is_override && !is.null(reason) && nzchar(reason)) reason else NA_character_
  
  dbExecute(conn, "
    INSERT INTO tbl_sample_event
      (sample_code, stage_code, state_code, actor, notes, is_override, override_reason)
    VALUES ($1,$2,$3,$4,$5,$6,$7)",
            params = list(sample_code, to_stage, to_state, actor, notes_bind,
                          is_override, reason_bind))
  
  invisible(is_override)
}


