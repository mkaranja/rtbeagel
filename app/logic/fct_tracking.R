box::use(
  app/logic/fct_conn[pool, load_data, shape_frame],
  app/logic/fct_workflows[next_options, order_context, workflow_cache],
)

# ============================================================================
# TRACKING · where has this order been, where is it, where might it go
# ----------------------------------------------------------------------------
# Three questions, three DIFFERENT sources. The old app answered all three
# from one mutable `status` string on tbl_project_status, which is exactly why
# it could never show history - overwriting the present destroys the past.
#
#   COMPLETED  tbl_sample_event      append-only. The past is a fact.
#   CURRENT    view_sample_current   the latest event per sample.
#   NEXT       the workflow YAML     a RECOMMENDATION, not a command.
#
# The third is why everything below says `recommended` and never `required`.
# tbl_sample_event's FK is to tbl_stage_state, not to workflow edges, so an
# operator can always do something the workflow did not suggest - they are
# just asked why. A UI that shows the recommendation as the only option is
# lying about what the system will accept.
#
# ----------------------------------------------------------------------------
# EVERY EXPORTED READER HERE RETURNS A SHAPED FRAME.
#
# load_data() returns a 0-row, 0-COLUMN frame when a query fails, and a fresh
# database legitimately returns 0 rows for nearly all of these. Both land in
# reactable(), which resolves `columns = list(...)` by name and errors on a
# frame with no columns. So each reader declares its columns as a prototype
# and passes the result through shape_frame().
#
# The prototypes are also documentation: they are the contract between these
# queries and the views in 001/003/005. If a view changes and this file does
# not, shape_frame() warns by name instead of the UI dying two layers up with
# "columns names must exist in data", which says nothing about which column or
# which view.
# ============================================================================

WF_PATH <- file.path("app", "static", "workflows", "cassava.yaml")
WF_PATH_TRACK <- WF_PATH


# ---- column contracts ------------------------------------------------
# Zero-length prototypes: names are the contract, types keep colDef() cell
# functions and format() honest when there are no rows to infer from.

proto_completed <- function() list(
  stage_code   = character(),
  label        = character(),
  sort_order   = integer(),
  first_seen   = as.POSIXct(character()),
  last_seen    = as.POSIXct(character()),
  n_events     = integer(),
  had_override = logical()
)

proto_current <- function() list(
  stage_code  = character(),
  stage_label = character(),
  sort_order  = integer(),
  state_code  = character(),
  state_label = character(),
  is_failure  = logical(),
  n_samples   = integer(),
  qty         = integer(),
  since       = as.POSIXct(character())
)

proto_summary <- function() list(
  order_number        = character(),
  approval_state      = character(),
  sample_amount       = integer(),
  payment_made        = logical(),
  amount_charged      = numeric(),
  receipt_no          = character(),
  created_by          = character(),
  created_on          = as.POSIXct(character()),
  approved_by         = character(),
  approved_on         = as.POSIXct(character()),
  order_kind          = character(),
  parent_order_number = character(),
  customer_name       = character(),
  customer_email      = character(),
  customer_phone      = character(),
  report_format       = character(),
  dispatch_method     = character(),
  ref_no              = character(),
  sampler             = character(),
  date_sampled        = as.Date(character()),
  date_received       = as.Date(character()),
  crop_name           = character(),
  variety_name        = character(),
  sample_type         = character(),
  sample_condition    = character(),
  part_name           = character(),
  bag_name            = character(),
  origin_country      = character(),
  sample_description  = character(),
  additional_info     = character(),
  derived_status      = character(),
  pct_complete        = numeric(),
  services_requested  = integer(),
  services_fulfilled  = integer()
)

proto_services <- function() list(
  order_service_id = integer(),
  service_label    = character(),
  service_kind     = character(),
  unit             = character(),
  purpose          = character(),
  origin           = character(),
  recipient        = character(),
  target_qty       = integer(),
  fulfilled_qty    = integer(),
  remaining_qty    = integer(),
  pct_complete     = numeric(),
  status           = character(),
  requested_on     = as.POSIXct(character())
)

proto_history <- function() list(
  occurred_on     = as.POSIXct(character()),
  actor           = character(),
  action          = character(),
  module          = character(),
  notes           = character(),
  sample_code     = character(),
  is_override     = logical(),
  override_reason = character()
)


#' Stages this order has actually been through, earliest first.
#' Straight from the event log - no inference.
#' @export
completed_stages <- function(order_number) {
  force(order_number)
  d <- load_data(pool, "
    SELECT e.stage_code,
           st.label,
           st.sort_order,
           min(e.occurred_on) AS first_seen,
           max(e.occurred_on) AS last_seen,
           count(*)               AS n_events,
           bool_or(e.is_override) AS had_override
    FROM tbl_sample_event e
    JOIN tbl_sample s  ON s.sample_code = e.sample_code
    JOIN tbl_stage  st ON st.stage_code = e.stage_code
    WHERE s.order_number = $1
      AND s.test_id IS NULL          -- test-samples are not pipeline stages
    GROUP BY e.stage_code, st.label, st.sort_order
    ORDER BY min(e.occurred_on)", params = list(order_number))
  shape_frame(d, proto_completed(), "completed_stages()")
}

#' Where the order's samples are RIGHT NOW, grouped by stage + state.
#' An order can legitimately sit in several stages at once - that is exactly
#' what the subculture fan-out produces.
#' @export
current_stages <- function(order_number) {
  force(order_number)
  d <- load_data(pool, "
    SELECT c.stage_code, st.label AS stage_label, st.sort_order,
           c.state_code, sv.label AS state_label, sv.is_failure,
           count(*)        AS n_samples,
           sum(s.quantity) AS qty,
           min(c.since)    AS since
    FROM view_sample_current c
    JOIN tbl_sample s  ON s.sample_code = c.sample_code
    JOIN tbl_stage  st ON st.stage_code = c.stage_code
    JOIN tbl_state  sv ON sv.state_code = c.state_code
    WHERE c.order_number = $1
      AND s.test_id IS NULL          -- test-samples are not pipeline units
    GROUP BY c.stage_code, st.label, st.sort_order, c.state_code, sv.label, sv.is_failure
    ORDER BY st.sort_order", params = list(order_number))
  shape_frame(d, proto_current(), "current_stages()")
}

empty_next <- function() {
  data.frame(from_stage = character(0), from_state = character(0),
             to_stage = character(0), step = character(0),
             label = character(0), kind = character(0),
             stringsAsFactors = FALSE)
}

#' What the workflow RECOMMENDS next, for each place the order currently sits.
#'
#' Zero rows means "the workflow has no suggestion" - NOT "nothing may be
#' done". allowed_transitions() in fct_workflows.R is the full legal set.
#' @export
next_steps <- function(order_number) {
  force(order_number)
  cur <- current_stages(order_number)
  if (nrow(cur) == 0) return(empty_next())
  
  wf <- tryCatch(workflow_cache(WF_PATH), error = function(e) NULL)
  if (is.null(wf)) return(empty_next())
  ctx <- tryCatch(order_context(pool, order_number), error = function(e) list())
  
  out <- lapply(seq_len(nrow(cur)), function(i) {
    opts <- tryCatch(next_options(wf, cur$stage_code[i], cur$state_code[i], ctx),
                     error = function(e) NULL)
    if (is.null(opts) || nrow(opts) == 0) return(NULL)
    data.frame(
      from_stage = rep(cur$stage_code[i], nrow(opts)),
      from_state = rep(cur$state_code[i], nrow(opts)),
      to_stage   = opts$to_stage,
      step       = opts$step,
      label      = opts$label,
      kind       = opts$kind,
      stringsAsFactors = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(empty_next())
  do.call(rbind, out)
}

#' The tracker in one shape: every stage marked done / current / next / future.
#' @export
tracker_steps <- function(order_number) {
  force(order_number)
  stages <- load_data(pool, "
    SELECT stage_code, label, sort_order FROM tbl_stage
    WHERE active AND stage_code <> 'archived'
    ORDER BY sort_order, label")
  stages <- shape_frame(stages, list(stage_code = character(),
                                     label      = character(),
                                     sort_order = integer()),
                        "tracker_steps()")
  # No stages at all means 002_seed did not run. Return the shaped frame with
  # the two columns the tracker adds, so the caller still gets its contract.
  if (nrow(stages) == 0) {
    stages$state  <- character(0)
    stages$detail <- character(0)
    return(stages)
  }
  
  done <- completed_stages(order_number)
  cur  <- current_stages(order_number)
  nxt  <- next_steps(order_number)
  
  # rep(), not a scalar: an order with no events yet gives zero-row frames,
  # and `stages$state <- ""` on those would be fine here (stages has rows) but
  # vapply over zero-length input returns character(0) - guard the shape.
  stages$state <- vapply(stages$stage_code, function(s) {
    if (nrow(cur)  && s %in% cur$stage_code)  return("current")
    if (nrow(done) && s %in% done$stage_code) return("done")
    if (nrow(nxt)  && s %in% nxt$to_stage)    return("next")
    "future"
  }, character(1), USE.NAMES = FALSE)
  
  stages$detail <- vapply(stages$stage_code, function(s) {
    r <- if (nrow(cur)) cur[cur$stage_code == s, , drop = FALSE] else cur[0, , drop = FALSE]
    if (nrow(r)) {
      q <- r$qty[1]
      return(paste0(r$state_label[1],
                    if (!is.na(q)) paste0(" \u00b7 ", q, " units") else ""))
    }
    d <- if (nrow(done)) done[done$stage_code == s, , drop = FALSE] else done[0, , drop = FALSE]
    if (nrow(d)) return(format(as.Date(d$last_seen[1]), "%d %b %Y"))
    ""
  }, character(1), USE.NAMES = FALSE)
  
  stages
}

#' Order header facts.
#' @export
order_summary <- function(order_number) {
  force(order_number)
  d <- load_data(pool, "
    SELECT o.order_number, o.approval_state, o.sample_amount,
           o.payment_made, o.amount_charged, o.receipt_no,
           o.created_by, o.created_on, o.approved_by, o.approved_on,
           o.order_kind, o.parent_order_number,
           cu.customer_name, cu.email AS customer_email, cu.phone AS customer_phone,
           rf.label AS report_format, dm.label AS dispatch_method,
           d.ref_no, d.sampler, d.date_sampled, d.date_received,
           c.crop_name, v.variety_name,
           stp.label AS sample_type, sc.label AS sample_condition,
           sp.part_name, sb.bag_name,
           co.country_name AS origin_country,
           d.sample_description, d.additional_info,
           p.derived_status, p.pct_complete,
           p.services_requested, p.services_fulfilled
    FROM tbl_order o
    JOIN tbl_customer cu              ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d      ON d.order_number = o.order_number
    LEFT JOIN tbl_crop c              ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v           ON v.variety_id = d.variety_id
    LEFT JOIN tbl_sample_type stp     ON stp.sample_type_code = d.sample_type_code
    LEFT JOIN tbl_sample_condition sc ON sc.condition_code = d.condition_code
    LEFT JOIN tbl_sample_part sp      ON sp.part_id = d.part_id
    LEFT JOIN tbl_sampling_bag sb     ON sb.bag_id = d.bag_id
    LEFT JOIN tbl_country co          ON co.country_code = d.origin_country_code
    LEFT JOIN tbl_report_format rf    ON rf.format_code = o.report_format_code
    LEFT JOIN tbl_dispatch_method dm  ON dm.dispatch_code = o.dispatch_code
    LEFT JOIN view_order_progress p   ON p.order_number = o.order_number
    WHERE o.order_number = $1", params = list(order_number))
  shape_frame(d, proto_summary(), "order_summary()")
}

#' Service lines with fulfilment.
#' @export
order_services <- function(order_number) {
  force(order_number)
  d <- load_data(pool, "
    SELECT order_service_id, service_label, service_kind, unit, purpose, origin,
           recipient, target_qty, fulfilled_qty, remaining_qty, pct_complete,
           status, requested_on
    FROM view_order_service_progress
    WHERE order_number = $1
    ORDER BY service_kind, service_label", params = list(order_number))
  shape_frame(d, proto_services(), "order_services()")
}

#' The append-only history, newest first. Order events and sample events in
#' one stream, because to a reader they are one story.
#' @export
order_history <- function(order_number) {
  force(order_number)
  d <- load_data(pool, "
    SELECT occurred_on, actor, action, module,
           notes, NULL::text AS sample_code,
           false AS is_override, NULL::text AS override_reason
    FROM tbl_order_event WHERE order_number = $1
    UNION ALL
    SELECT e.occurred_on, e.actor,
           st.label || ' \u2192 ' || sv.label AS action,
           'sample' AS module, e.notes, e.sample_code,
           e.is_override, e.override_reason
    FROM tbl_sample_event e
    JOIN tbl_sample s ON s.sample_code = e.sample_code
    JOIN tbl_stage st ON st.stage_code = e.stage_code
    JOIN tbl_state sv ON sv.state_code = e.state_code
    WHERE s.order_number = $1
    ORDER BY occurred_on DESC", params = list(order_number))
  shape_frame(d, proto_history(), "order_history()")
}


# ============================================================================
# THE BOARD · every order, its status, and what is recommended next
# ----------------------------------------------------------------------------
# WHY THIS IS ONE FUNCTION AND NOT next_steps() IN A LOOP
#
# next_steps() costs two queries per order (current position, then context).
# Calling it per row to render a 100-order table is 200 round trips before
# the page paints, and it gets worse as the lab gets busier - the classic
# N+1. So: THREE queries total, regardless of how many orders there are, and
# the workflow is then evaluated in memory, which is free.
# ============================================================================

#' Every order with its current position and recommended next step.
#' @export
order_board <- function() {
  base <- load_data(pool, "
    SELECT p.order_number, p.derived_status, p.pct_complete,
           p.services_requested, p.services_fulfilled,
           o.approval_state, o.sample_amount, o.created_on, o.order_kind,
           cu.customer_name,
           c.crop_name, v.variety_name,
           d.sample_type_code
    FROM view_order_progress p
    JOIN tbl_order o             ON o.order_number = p.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = o.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    ORDER BY o.created_on DESC")
  if (nrow(base) == 0) return(empty_board())
  
  # every order's current positions, in ONE query
  pos <- load_data(pool, "
    SELECT c.order_number, c.stage_code, c.state_code,
           st.label AS stage_label, sv.label AS state_label,
           count(*) AS n_samples
    FROM view_sample_current c
    JOIN tbl_sample s ON s.sample_code = c.sample_code
    JOIN tbl_stage st ON st.stage_code = c.stage_code
    JOIN tbl_state sv ON sv.state_code = c.state_code
    WHERE s.test_id IS NULL          -- exclude test-samples from pipeline counts
    GROUP BY c.order_number, c.stage_code, c.state_code, st.label, sv.label, st.sort_order
    ORDER BY st.sort_order")
  pos <- shape_frame(pos, list(order_number = character(), stage_code = character(),
                               state_code = character(), stage_label = character(),
                               state_label = character(), n_samples = integer()),
                     "order_board(): positions")
  
  # every order's workflow context, in ONE query
  ctxs <- load_data(pool, "
    SELECT o.order_number,
           c.crop_name        AS crop,
           d.sample_type_code AS sample_type,
           COALESCE((SELECT array_agg(DISTINCT os.service_code)
                     FROM tbl_order_service os
                     WHERE os.order_number = o.order_number
                       AND os.cancelled_on IS NULL), '{}') AS services,
           EXISTS (SELECT 1 FROM tbl_order_service os
                     JOIN tbl_service_catalog sc ON sc.service_code = os.service_code
                    WHERE os.order_number = o.order_number
                      AND os.cancelled_on IS NULL
                      AND sc.service_kind = 'fulfilment') AS needs_cleaning,
           -- latest-round per sample (view_sample_virus_status, 007), NOT the
           -- old monotonic bool_or over all history that deadlocked the
           -- thermotherapy loop
           (SELECT bool_or(v.is_positive)
              FROM view_sample_virus_status v
              JOIN tbl_sample s ON s.sample_code = v.sample_code
             WHERE s.order_number = o.order_number) AS any_positive
    FROM tbl_order o
    LEFT JOIN tbl_order_detail d ON d.order_number = o.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id")
  # NOT shape_frame()d: `services` is a LIST column (array_agg), which a
  # zero-length prototype cannot express. ctx_for() below already guards on
  # nrow() and is the only reader.
  
  wf <- tryCatch(workflow_cache(WF_PATH), error = function(e) NULL)
  
  ctx_for <- function(on) {
    if (nrow(ctxs) == 0) return(list())
    r <- ctxs[ctxs$order_number == on, , drop = FALSE]
    if (nrow(r) == 0) return(list())
    sv <- r$services[[1]]
    if (is.null(sv) || (length(sv) == 1 && is.na(sv[1]))) sv <- character(0)
    list(crop = r$crop[1], sample_type = r$sample_type[1],
         service = as.character(sv),
         needs_cleaning = if (isTRUE(r$needs_cleaning[1])) "yes" else "no",
         virus_indexing = if (isTRUE(r$any_positive[1])) "positive" else "negative")
  }
  
  base$current_stage <- vapply(base$order_number, function(on) {
    if (nrow(pos) == 0) return(NA_character_)
    p <- pos[pos$order_number == on, , drop = FALSE]
    if (nrow(p) == 0) return(NA_character_)
    paste(unique(paste0(p$stage_label, " \u00b7 ", p$state_label)), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
  
  # Where each order has actually been SENT, if it has been approved and
  # assigned. Once that decision is made the next step is no longer a
  # recommendation - it is a fact, and showing "Glasshouse | Thermotherapy"
  # after somebody chose one of them is worse than showing nothing.
  hv <- tryCatch(load_data(pool, "
    SELECT h.order_number, h.status, st.label AS to_label
    FROM tbl_order_handover h
    JOIN tbl_stage st ON st.stage_code = h.to_stage
    WHERE h.status IN ('assigned','pickup_requested')"),
                 error = function(e) data.frame())
  
  base$next_step <- vapply(seq_len(nrow(base)), function(i) {
    on <- base$order_number[i]
    if (is.null(wf)) return(NA_character_)
    
    # Not approved yet - the next act is a human decision, not a lab step.
    if (!identical(base$approval_state[i], "approved")) {
      return(switch(base$approval_state[i],
                    pending   = "Review & approve",
                    rejected  = "\u2014",
                    cancelled = "\u2014",
                    NA_character_))
    }
    
    ctx <- ctx_for(on)
    p <- if (nrow(pos)) pos[pos$order_number == on, , drop = FALSE] else pos
    
    # Approved but no samples yet: the order sits at reception/approved, so
    # ask the workflow what happens from there. This is what turns an empty
    # queue into "Establishment in Quarantine Glasshouse".
    if (nrow(p) == 0) {
      # ASSIGNED: the destination is settled, so say it rather than re-offering
      # the choice. This is the column changing from "what could happen" to
      # "what will happen", which is what approval decided.
      if (is.data.frame(hv) && nrow(hv) > 0) {
        h <- hv[hv$order_number == on, , drop = FALSE]
        if (nrow(h) > 0) {
          return(sprintf("%s %s", if (identical(h$status[1], "pickup_requested"))
            "Pickup requested \u2014" else "Deliver to",
            h$to_label[1]))
        }
      }
      o <- tryCatch(next_options(wf, "reception", "approved", ctx), error = function(e) NULL)
      if (is.null(o) || nrow(o) == 0) return(NA_character_)
      return(paste(unique(stats::na.omit(o$label)), collapse = "  +  "))
    }
    
    labs <- unlist(lapply(seq_len(nrow(p)), function(j) {
      o <- tryCatch(next_options(wf, p$stage_code[j], p$state_code[j], ctx),
                    error = function(e) NULL)
      if (is.null(o) || nrow(o) == 0) return(NULL)
      sep <- if (any(o$kind == "fan_out")) "  +  " else "  |  "
      paste(unique(stats::na.omit(o$label)), collapse = sep)
    }))
    if (!length(labs)) return(NA_character_)
    paste(unique(labs), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
  
  base
}

empty_board <- function() {
  data.frame(order_number = character(0), derived_status = character(0),
             pct_complete = integer(0), services_requested = integer(0),
             services_fulfilled = integer(0), approval_state = character(0),
             sample_amount = integer(0), created_on = as.POSIXct(character(0)),
             order_kind = character(0), customer_name = character(0),
             crop_name = character(0), variety_name = character(0),
             sample_type_code = character(0), current_stage = character(0),
             next_step = character(0), stringsAsFactors = FALSE)
}

#' Counts by derived status, for the KPI tiles.
#' @export
order_counts <- function() {
  d <- load_data(pool, "SELECT derived_status, count(*) AS n FROM view_order_progress GROUP BY derived_status")
  shape_frame(d, list(derived_status = character(), n = integer()), "order_counts()")
}


# ============================================================================
# VIRUS INDEXING · explants at indexing, and the test-samples under each
# ----------------------------------------------------------------------------
# One test = one sample. An explant at indexing spawns one TEST-SAMPLE per
# required test (tbl_sample.test_id set, parent_sample_code = the explant).
# The explant is routed by the ROLL-UP of its test-samples
# (view_sample_virus_status, per-explant). So the queue lists EXPLANTS (the
# pipeline units, test_id IS NULL), never the test-samples themselves.
#
# Which tests an explant needs:
#   * a fresh explant (from quarantine) -> every test its order requested
#   * a meristem tip (its parent tested positive) -> ONLY the tests that were
#     positive on the parent
# tests_for_sample() resolves this from lineage + prior results.
# ============================================================================

proto_index_queue <- function() list(
  sample_code        = character(),
  order_number       = character(),
  stage_code         = character(),
  stage_label        = character(),
  state_code         = character(),
  state_label        = character(),
  since              = as.POSIXct(character()),
  quantity           = integer(),
  parent_sample_code = character(),
  customer_name      = character(),
  crop_name          = character(),
  crop_id            = integer(),
  variety_name       = character(),
  n_tests            = integer(),
  n_resulted         = integer(),
  last_status        = character()
)

#' EXPLANTS at a virus-indexing stage (test-samples excluded), each with the
#' roll-up of its test-samples.
#'
#' `stage` selects molecular or grafting; NULL lists both. n_tests / n_resulted
#' / last_status come from view_sample_virus_status (per-explant roll-up), so
#' the row shows "2 of 3 tests in, still pending" or "all in, positive".
#' @export
index_queue <- function(stage = NULL) {
  force(stage)
  # Written out twice rather than pasted from a shared `base`. The duplication
  # is the price of every query in this file being a plain string literal that
  # the static check can read and plan against the live schema; a query
  # assembled at runtime is invisible to it, and these two were the only ones
  # never verified.
  d <- if (is.null(stage)) {
    load_data(pool, "
    SELECT q.sample_code, q.order_number, q.stage_code, q.stage_label,
           q.state_code, q.state_label, q.since, q.quantity, q.parent_sample_code,
           cu.customer_name, c.crop_name, c.crop_id, v.variety_name,
           COALESCE(vs.n_tests, 0)::int    AS n_tests,
           COALESCE(vs.n_resulted, 0)::int AS n_resulted,
           COALESCE(vs.status, 'pending')  AS last_status
    FROM view_stage_queue q
    JOIN tbl_sample s            ON s.sample_code = q.sample_code
    JOIN tbl_order o             ON o.order_number = q.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = q.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    LEFT JOIN view_sample_virus_status vs ON vs.sample_code = q.sample_code
    WHERE q.stage_code IN ('molecular_virus_indexing','grafting_virus_indexing')
      AND s.test_id IS NULL
    ORDER BY q.since")
  } else {
    load_data(pool, "
    SELECT q.sample_code, q.order_number, q.stage_code, q.stage_label,
           q.state_code, q.state_label, q.since, q.quantity, q.parent_sample_code,
           cu.customer_name, c.crop_name, c.crop_id, v.variety_name,
           COALESCE(vs.n_tests, 0)::int    AS n_tests,
           COALESCE(vs.n_resulted, 0)::int AS n_resulted,
           COALESCE(vs.status, 'pending')  AS last_status
    FROM view_stage_queue q
    JOIN tbl_sample s            ON s.sample_code = q.sample_code
    JOIN tbl_order o             ON o.order_number = q.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = q.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    LEFT JOIN view_sample_virus_status vs ON vs.sample_code = q.sample_code
    WHERE q.stage_code IN ('molecular_virus_indexing','grafting_virus_indexing')
      AND s.test_id IS NULL
      AND q.stage_code = $1
    ORDER BY q.since", params = list(stage))
  }
  shape_frame(d, proto_index_queue(), "index_queue()")
}


proto_awaited_delivery <- function() list(
  request_id      = integer(),
  order_number    = character(),
  to_stage        = character(),
  to_stage_label  = character(),
  source_sample_code = character(),
  source_stage    = character(),
  source_bench    = character(),
  status          = character(),
  reason          = character(),
  test_id         = integer(),
  test_acronym    = character(),
  requested_by    = character(),
  requested_on    = as.POSIXct(character()),
  authorized_by   = character(),
  qty_requested   = integer(),
  qty_sent        = integer(),
  qty_outstanding = integer(),
  n_deliveries    = integer(),
  last_sent_on    = as.POSIXct(character()),
  customer_name   = character(),
  crop_name       = character(),
  variety_name    = character()
)

#' What THIS bench has asked for and not yet received in full.
#'
#' The mirror of pending_requests(). That one is keyed by the bench that HOLDS
#' the material; this one by the bench WAITING for it.
#'
#' Both exist because the two benches are in different rooms with different
#' people at different screens. Once a request was raised it lived entirely in
#' the holder's queue, so a technician arriving the next morning could not tell
#' whether 12 of the 40 they asked for had arrived overnight or whether nobody
#' had started - the only way to find out was to walk down the corridor.
#' @export
awaited_deliveries <- function(to_stages = NULL) {
  d <- if (is.null(to_stages)) {
    load_data(pool, "SELECT * FROM view_awaited_delivery ORDER BY requested_on")
  } else {
    # string_to_array, NOT `= ANY($1)`: RPostgres binds a length-1 character
    # vector as plain text, and Postgres then fails on a malformed array
    # literal. One comma-joined string binds a scalar every time.
    load_data(pool, "
      SELECT * FROM view_awaited_delivery
      WHERE to_stage = ANY(string_to_array($1, ','))
      ORDER BY requested_on",
              params = list(paste(to_stages, collapse = ",")))
  }
  shape_frame(d, proto_awaited_delivery(), "awaited_deliveries()")
}


proto_meristem_held <- function() list(
  sample_code   = character(),
  order_number  = character(),
  state_code    = character(),
  state_label   = character(),
  parent_code   = character(),
  quantity      = integer(),
  customer_name = character(),
  crop_name     = character(),
  variety_name  = character(),
  since         = as.POSIXct(character()),
  draws_so_far  = integer(),
  open_requests = integer()
)

#' Completed tips standing on the meristem bench, ready to be drawn from.
#'
#' The mirror of quarantine_stock(). Culture, excision and review all end with
#' the tip STILL here - that is what a holding stage means - so the bench needs
#' the same two things quarantine has: a list of what is standing there, and
#' the ability to route it onward without waiting to be asked.
#' @export
meristem_held <- function() {
  d <- load_data(pool, "
    SELECT c.sample_code, c.order_number, c.state_code,
           sv.label AS state_label,
           s.parent_sample_code AS parent_code,
           s.quantity,
           cu.customer_name, cr.crop_name, v.variety_name,
           c.since,
           (SELECT count(*) FROM tbl_sample ch
             WHERE ch.parent_sample_code = c.sample_code)::int   AS draws_so_far,
           (SELECT count(*) FROM tbl_sample_request rq
             WHERE rq.source_sample_code = c.sample_code
               AND rq.status IN ('pending','authorized'))::int AS open_requests
    FROM view_sample_current c
    JOIN tbl_sample s            ON s.sample_code = c.sample_code
    LEFT JOIN tbl_state sv       ON sv.state_code = c.state_code
    JOIN tbl_order o             ON o.order_number = c.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = c.order_number
    LEFT JOIN tbl_crop cr        ON cr.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    WHERE c.stage_code = 'meristem_culture'
      -- COMPLETED, not approved. cassava.yaml routes
      -- meristem_culture/approved to `complete` and
      -- meristem_culture/completed out to re-indexing, so completion is the
      -- release and approval is the review that precedes it. Listing approved
      -- tips here offered material whose culture had not been finished.
      AND c.state_code = 'completed'
      AND s.test_id IS NULL
      -- a TIP: it was BORN on this bench. tbl_sample.stage_code is the stage a
      -- sample was created at and never moves, so it separates tissue excised
      -- here from an explant drawn in from thermotherapy, which was created
      -- there. The old test read the PARENT stage, which is only true from the
      -- second generation of tips onward: tips taken from an explant that
      -- arrived from thermotherapy have a parent created at thermotherapy, so
      -- the first and largest batch of every consignment was hidden.
      AND s.stage_code = 'meristem_culture'
    ORDER BY c.since")
  shape_frame(d, proto_meristem_held(), "meristem_held()")
}



proto_order_handover <- function() list(
  handover_id      = integer(),
  order_number     = character(),
  to_stage         = character(),
  to_stage_label   = character(),
  status           = character(),
  recommended      = logical(),
  assign_reason    = character(),
  assigned_by      = character(),
  assigned_on      = as.POSIXct(character()),
  requested_by     = character(),
  requested_on     = as.POSIXct(character()),
  released_by      = character(),
  collected_by     = character(),
  collected_on     = as.POSIXct(character()),
  collected_qty    = integer(),
  approval_state   = character(),
  sample_amount    = integer(),
  customer_name    = character(),
  crop_name        = character(),
  variety_name     = character(),
  sample_type_code = character()
)

#' Approved consignments assigned to a bench and not yet collected.
#'
#' @param to_stages restrict to one bench's inbound list. Every receiving module
#'   reads the same view, so unscoped each would see the others' consignments.
#' @export
order_handovers <- function(to_stages = NULL) {
  if (is.null(to_stages)) {
    d <- load_data(pool, "SELECT * FROM view_order_handover")
  } else {
    # string_to_array, not `= ANY($1)`: RPostgres binds a length-1 character
    # vector as scalar text, and Postgres then fails on "malformed array
    # literal". One comma-joined string binds a scalar every time.
    d <- load_data(pool, "
      SELECT * FROM view_order_handover
      WHERE to_stage = ANY(string_to_array($1, ','))",
                   params = list(paste(to_stages, collapse = ",")))
  }
  shape_frame(d, proto_order_handover(), "order_handovers()")
}

proto_quarantine_stock <- function() list(
  sample_code   = character(),
  order_number  = character(),
  stage_code    = character(),
  bench         = character(),
  state_code    = character(),
  quantity      = integer(),
  customer_name = character(),
  crop_name     = character(),
  variety_name  = character(),
  sample_type   = character(),
  since         = as.POSIXct(character()),
  draws_so_far  = integer(),
  open_requests = integer()
)

#' Cleared quarantine material, held and available to be routed onward.
#'
#' Reception, initiation and clearance all end with the sample STILL in
#' quarantine - that is what a holding stage means. This is the list of what is
#' standing there: material another bench can ask for, and that quarantine can
#' also route itself without waiting to be asked.
#' @export
quarantine_stock <- function() {
  d <- load_data(pool, "
    SELECT c.sample_code, c.order_number, c.stage_code,
           st.label       AS bench,
           c.state_code,
           s.quantity,
           cu.customer_name, cr.crop_name, v.variety_name,
           d.sample_type_code AS sample_type,
           c.since,
           (SELECT count(*) FROM tbl_sample ch
             WHERE ch.parent_sample_code = c.sample_code)::int      AS draws_so_far,
           (SELECT count(*) FROM tbl_sample_request rq
             WHERE rq.source_sample_code = c.sample_code
               AND rq.status IN ('pending','authorized'))::int      AS open_requests
    FROM view_sample_current c
    JOIN tbl_sample s            ON s.sample_code = c.sample_code
    JOIN tbl_stage st            ON st.stage_code = c.stage_code
    JOIN tbl_order o             ON o.order_number = c.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = c.order_number
    LEFT JOIN tbl_crop cr        ON cr.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    WHERE c.stage_code IN ('quarantine_glasshouse','quarantine_growthroom')
      AND c.state_code = 'approved'
      AND s.test_id IS NULL
    ORDER BY c.since")
  shape_frame(d, proto_quarantine_stock(), "quarantine_stock()")
}


proto_pending_request <- function() list(
  request_id         = integer(),
  order_number       = character(),
  source_sample_code = character(),
  to_stage           = character(),
  to_stage_label     = character(),
  test_id            = integer(),
  test_acronym       = character(),
  test_name          = character(),
  reason             = character(),
  requested_by       = character(),
  requested_on       = as.POSIXct(character()),
  status             = character(),
  authorized_by      = character(),
  authorized_on      = as.POSIXct(character()),
  qty_requested      = integer(),
  qty_sent           = integer(),
  qty_outstanding    = integer(),
  n_deliveries       = integer(),
  last_sent_on       = as.POSIXct(character()),
  source_stage       = character(),
  source_bench       = character(),
  source_state       = character(),
  source_units       = integer(),
  customer_name      = character(),
  crop_name          = character(),
  variety_name       = character(),
  draws_so_far       = integer()
)

#' Material other benches have asked quarantine to draw.
#'
#' Reads view_pending_request (migration 014). Quarantine queries its OWN
#' queue - the requesting module does not push anything to it and does not
#' need to know quarantine exists beyond raising the row.
#' @export
pending_requests <- function(source_stages = NULL) {
  # Scoped to the bench that HOLDS the material. Every holding stage reads this
  # view, so unscoped it showed quarantine the requests meant for meristem
  # culture and vice versa - two benches looking at each other's work.
  if (is.null(source_stages)) {
    d <- load_data(pool, "SELECT * FROM view_pending_request")
  } else {
    # string_to_array, NOT `= ANY($1)` with a vector.
    #
    # RPostgres binds a character vector of length 1 as a plain text parameter,
    # not as an array, so Postgres tried to read "meristem_culture" as an array
    # literal and failed with "malformed array literal". Passing ONE comma-
    # joined string and splitting it server-side binds a scalar every time,
    # whether the caller supplies one stage or several.
    d <- load_data(pool, "
      SELECT * FROM view_pending_request
      WHERE source_stage = ANY(string_to_array($1, ','))",
                   params = list(paste(source_stages, collapse = ",")))
  }
  shape_frame(d, proto_pending_request(), "pending_requests()")
}


proto_source_routing <- function() list(
  source_code   = character(),
  order_number  = character(),
  stage_code    = character(),
  bench         = character(),
  origin_kind   = character(),
  crop_name     = character(),
  variety_name  = character(),
  customer_name = character(),
  tests_total   = integer(),
  tests_done    = integer(),
  n_positive    = integer(),
  verdict       = character(),
  ready         = integer(),
  requested_to  = character(),
  drawn_to      = character()
  # NOTE: `delivered` is NOT here. It is derived in thermotherapy's needed()
  # from drawn_to, not returned by this query, so it must not be in the query's
  # prototype - shape_frame() checks the prototype against the SQL result and
  # would (did) report drift on every call, blanking the table.
)

#' Source material whose indexing has finished, and where it should go next.
#'
#' One row per SOURCE, not per test. The routing decision is a property of the
#' material as a whole - "were all its tests negative" - so it cannot be
#' answered from a single test row.
#'
#' `verdict` is 'positive' if ANY completed test came back positive, 'negative'
#' only when every test is done and none did. `ready` is 1 when every required
#' test has reached a terminal state; until then the material is still being
#' tested and must not be routed anywhere.
#'
#' `origin_kind` says where the material came from, because it decides what it
#' may be routed to:
#'   quarantine  -> virus indexing, surface sterilization or thermotherapy
#'   meristem    -> virus indexing only
#' It is read from the sample's CURRENT stage. tbl_sample has no material-type
#' column - sample_type_code lives on tbl_order_detail and describes what
#' ARRIVED for the order, so it can never say "this is a meristem tip", which
#' is something the lab produced rather than received.
#' @export
source_routing <- function() {
  d <- load_data(pool, "
    -- Per ORDER, not per source sample.
    --
    -- It used to look for test samples hanging directly off a quarantine
    -- sample. That held while indexing cut its tests from the quarantine
    -- source; it stopped the moment quarantine began DELIVERING material,
    -- because the test is now cut from the delivered explant and its parent is
    -- on the indexing bench, not in quarantine. Every result became invisible
    -- and this queue silently emptied.
    --
    -- The question is an order-level one anyway: were any of THIS ORDER's
    -- required tests positive. Which explant carried the test does not change
    -- the answer.
    WITH req AS (
      SELECT ot.order_number, t.test_id
      FROM tbl_order_test ot
      JOIN tbl_test_method t ON t.test_id = ot.test_id
      UNION
      SELECT od.order_number, t.test_id
      FROM tbl_order_detail od
      JOIN tbl_test_method t ON t.crop_id = od.crop_id AND t.active
      WHERE NOT EXISTS (SELECT 1 FROM tbl_order_test x
                        WHERE x.order_number = od.order_number)
    ),
    ts AS (
      -- Every test sample in the order, wherever it was cut from, rolled up
      -- per test. bool_or: one positive anywhere is a positive for that test.
      SELECT s.order_number, s.test_id,
             bool_or(cur.state_code IN ('completed','approved')) AS done,
             bool_or(lr.outcome = 'positive')                    AS positive
      FROM tbl_sample s
      LEFT JOIN view_sample_current cur ON cur.sample_code = s.sample_code
      LEFT JOIN LATERAL (
        SELECT outcome FROM tbl_test_result r
        WHERE r.sample_code = s.sample_code
        ORDER BY tested_on DESC LIMIT 1
      ) lr ON TRUE
      WHERE s.test_id IS NOT NULL
      GROUP BY s.order_number, s.test_id
    ),
    agg AS (
      SELECT r.order_number,
             count(*)::int                                                AS tests_total,
             count(*) FILTER (WHERE COALESCE(ts.done, false))::int         AS tests_done,
             count(*) FILTER (WHERE COALESCE(ts.positive, false))::int     AS n_positive
      FROM req r
      LEFT JOIN ts ON ts.order_number = r.order_number AND ts.test_id = r.test_id
      GROUP BY r.order_number
    ),
    src AS (
      -- The material a request would be raised against. One per order: the
      -- oldest still standing, which is the mother stock.
      SELECT DISTINCT ON (c.order_number)
             c.order_number, c.sample_code, c.stage_code, c.since
      FROM view_sample_current c
      JOIN tbl_sample s ON s.sample_code = c.sample_code
      WHERE s.test_id IS NULL
        AND ((c.stage_code IN ('quarantine_glasshouse','quarantine_growthroom')
              AND c.state_code = 'approved')
          OR (c.stage_code = 'meristem_culture'
              AND c.state_code IN ('completed','approved')))
      ORDER BY c.order_number, c.since
    )
    SELECT src.sample_code   AS source_code,
           src.order_number,
           src.stage_code,
           st.label          AS bench,
           CASE WHEN src.stage_code = 'meristem_culture' THEN 'meristem'
                ELSE 'quarantine' END AS origin_kind,
           c.crop_name, v.variety_name, cu.customer_name,
           agg.tests_total, agg.tests_done, agg.n_positive,
           CASE WHEN agg.tests_total = 0                    THEN 'untested'
                WHEN agg.n_positive > 0                     THEN 'positive'
                WHEN agg.tests_done = agg.tests_total       THEN 'negative'
                ELSE 'pending' END AS verdict,
           CASE WHEN agg.tests_total > 0
                 AND agg.tests_done = agg.tests_total THEN 1 ELSE 0 END AS ready,
           (SELECT string_agg(DISTINCT rq.to_stage, ',')
              FROM tbl_sample_request rq
             WHERE rq.source_sample_code = src.sample_code
               AND rq.status IN ('pending','authorized'))            AS requested_to,
           -- CURRENT stage of each child, not tbl_sample.stage_code.
           --
           -- stage_code on tbl_sample is where a sample was CREATED and does
           -- not move; a child cut to thermotherapy and since advanced to
           -- meristem still reads 'thermotherapy' there. Reading it made
           -- delivered = 1 for a source whose material had already left the
           -- bench, so Incoming offered 'Initiate' on a sample that was gone.
           -- view_sample_current is the live position.
           (SELECT string_agg(DISTINCT cc.stage_code, ',')
              FROM tbl_sample ch
              JOIN view_sample_current cc ON cc.sample_code = ch.sample_code
             WHERE ch.parent_sample_code = src.sample_code
               AND ch.test_id IS NULL)                               AS drawn_to
    FROM src
    JOIN agg                     ON agg.order_number = src.order_number
    JOIN tbl_stage st            ON st.stage_code = src.stage_code
    JOIN tbl_order o             ON o.order_number = src.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = src.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    ORDER BY src.since")
  shape_frame(d, proto_source_routing(), "source_routing()")
}


proto_indexing_material <- function() list(
  row_id           = character(),
  source_code      = character(),
  order_number     = character(),
  stage_code       = character(),
  bench            = character(),
  test_id          = integer(),
  acronym          = character(),
  test_name        = character(),
  pathogen_name    = character(),
  test_sample_code = character(),
  test_stage       = character(),
  test_state       = character(),
  state_label      = character(),
  n_tests          = integer(),
  n_resulted       = integer(),
  last_status      = character(),
  init_status      = character(),
  requested        = integer(),
  held_here        = integer(),
  origin           = character(),
  is_mer           = integer(),
  source_state     = character(),
  source_state_label = character(),
  tips_available   = integer(),
  customer_name    = character(),
  crop_name        = character(),
  variety_name     = character(),
  since            = as.POSIXct(character())
)

#' Every test that SHOULD be run, whether or not it has been started.
#'
#' One row per (approved quarantine source x required test). That shape is the
#' whole point: a queue built from test-samples alone can only show tests
#' somebody already started, so a test nobody has begun is invisible - exactly
#' the case the bench most needs to see. Cross-joining available material
#' against the order's required methods makes "not yet initiated" a ROW rather
#' than an absence.
#'
#' Material comes from (quarantine_*, approved): approved is what makes a source
#' AVAILABLE, and the source stays there as standing stock, so it appears once
#' per required test and can be cut repeatedly.
#'
#' Required methods come from tbl_order_test, falling back to every active
#' method for the order crop when none were requested at registration - the same
#' rule tests_for_sample() applies, repeated here deliberately so the queue and
#' the cut agree on what is required.
#' @export
indexing_material <- function() {
  d <- load_data(pool, "    -- Material this bench can work on, wherever it is standing.
    --
    -- THREE sources, one queue:
    --   origin 'quarantine'  approved stock, held elsewhere -> request it
    --   origin 'meristem'    cleaned tip,   held elsewhere -> request it
    --   origin 'bench'       delivered here                -> initiate it
    --
    -- LINEAGE, not parentage. Two CTEs replace an ancestor walk that could not
    -- answer the question it was asked:
    --
    --   mer       every sample BORN on the meristem bench, and everything
    --             drawn from one. tbl_sample.stage_code is the stage a sample
    --             was CREATED at and never moves, so it is the one honest
    --             record of an excised tip. The old test read the PARENT stage
    --             instead, which classed every first-generation tip as
    --             ordinary material: a tip excised from an explant that
    --             arrived from thermotherapy has a parent created AT
    --             thermotherapy, so it was excluded from this queue entirely.
    --
    --   pos_root  the tests that came back positive anywhere in a lineage,
    --             attributed to its ROOT. A test sample hangs off a sample
    --             DRAWN from the quarantine source, which is a sibling of the
    --             draw that went on to thermotherapy - never an ancestor of
    --             the tip. Walking up from a tip therefore reached the source
    --             and found nothing, so no tip was ever offered for retest.
    --
    -- Keyed per SAMPLE for anything of meristem lineage, per ORDER otherwise.
    -- Quarantine stock is interchangeable - nobody wants one test run on three
    -- explants of one consignment - but a cleaned tip is a specific piece of
    -- tissue and its retest is its own. Keying tips by order let the original
    -- completed test shadow every retest for that order.
    WITH RECURSIVE mer AS (
      SELECT s.sample_code
      FROM tbl_sample s
      WHERE s.stage_code = 'meristem_culture'
      UNION
      SELECT c.sample_code
      FROM tbl_sample c
      JOIN mer m ON m.sample_code = c.parent_sample_code
    ),
    root_of AS (
      SELECT s.sample_code, s.sample_code AS root_code, 0 AS depth
      FROM tbl_sample s
      WHERE s.parent_sample_code IS NULL
      UNION ALL
      SELECT c.sample_code, r.root_code, r.depth + 1
      FROM tbl_sample c
      JOIN root_of r ON r.sample_code = c.parent_sample_code
      WHERE r.depth < 24
    ),
    pos_root AS (
      SELECT DISTINCT r.root_code, ts.test_id
      FROM tbl_sample ts
      JOIN root_of r ON r.sample_code = ts.sample_code
      JOIN LATERAL (
        SELECT outcome FROM tbl_test_result x
        WHERE x.sample_code = ts.sample_code
        ORDER BY tested_on DESC LIMIT 1
      ) lr ON TRUE
      WHERE ts.test_id IS NOT NULL
        AND lr.outcome = 'positive'
    ),
    avail AS (
      SELECT c.sample_code, c.order_number, c.stage_code, c.state_code, c.since,
             0 AS held_here, 'quarantine'::text AS origin, 0 AS is_mer
      FROM view_sample_current c
      JOIN tbl_sample s ON s.sample_code = c.sample_code
      WHERE c.stage_code IN ('quarantine_glasshouse','quarantine_growthroom')
        AND c.state_code = 'approved'
        AND s.test_id IS NULL
      UNION ALL
      SELECT c.sample_code, c.order_number, c.stage_code, c.state_code, c.since,
             1 AS held_here, 'bench'::text AS origin,
             CASE WHEN m.sample_code IS NULL THEN 0 ELSE 1 END AS is_mer
      FROM view_sample_current c
      JOIN tbl_sample s ON s.sample_code = c.sample_code
      LEFT JOIN mer m   ON m.sample_code = c.sample_code
      WHERE c.stage_code IN ('molecular_virus_indexing','grafting_virus_indexing')
        AND c.state_code NOT IN ('completed','rejected')
        AND s.test_id IS NULL
      UNION ALL
      SELECT c.sample_code, c.order_number, c.stage_code, c.state_code, c.since,
             0 AS held_here, 'meristem'::text AS origin, 1 AS is_mer
      FROM view_sample_current c
      JOIN tbl_sample s ON s.sample_code = c.sample_code
      WHERE c.stage_code = 'meristem_culture'
        -- COMPLETED is the release gate, per cassava.yaml: approved clears a
        -- tip to be completed, and completing it is what makes the tissue
        -- available. Approval alone does not release it.
        AND c.state_code = 'completed'
        AND s.test_id IS NULL
        AND s.stage_code = 'meristem_culture'
    ),
    named AS (
      SELECT rq.fulfilled_sample_code AS sample_code, t.test_id
      FROM tbl_sample_request rq
      JOIN tbl_test_method t ON t.test_id = rq.test_id
      WHERE rq.status = 'fulfilled'
        AND rq.test_id IS NOT NULL
        AND rq.fulfilled_sample_code IS NOT NULL
    ),
    req AS (
      SELECT ot.order_number, t.test_id
      FROM tbl_order_test ot
      JOIN tbl_test_method t ON t.test_id = ot.test_id
      UNION
      SELECT od.order_number, t.test_id
      FROM tbl_order_detail od
      JOIN tbl_test_method t ON t.crop_id = od.crop_id AND t.active
      WHERE NOT EXISTS (SELECT 1 FROM tbl_order_test x
                        WHERE x.order_number = od.order_number)
    ),
    plan AS (
      SELECT a.sample_code, n.test_id
      FROM avail a JOIN named n ON n.sample_code = a.sample_code
      UNION ALL
      SELECT a.sample_code, pr.test_id
      FROM avail a
      JOIN root_of r  ON r.sample_code = a.sample_code
      JOIN pos_root pr ON pr.root_code = r.root_code
      WHERE a.is_mer = 1
        AND NOT EXISTS (SELECT 1 FROM named n2 WHERE n2.sample_code = a.sample_code)
      UNION ALL
      SELECT a.sample_code, r2.test_id
      FROM avail a JOIN req r2 ON r2.order_number = a.order_number
      WHERE NOT EXISTS (SELECT 1 FROM named n3 WHERE n3.sample_code = a.sample_code)
        AND (a.is_mer = 0
             OR NOT EXISTS (SELECT 1 FROM root_of r3
                            JOIN pos_root pr3 ON pr3.root_code = r3.root_code
                            WHERE r3.sample_code = a.sample_code))
    )
    SELECT DISTINCT ON (a.order_number, a.is_mer, pl.test_id)
           a.sample_code || ':' || pl.test_id AS row_id,
           a.held_here, a.origin, a.is_mer,
           a.sample_code AS source_code, a.order_number, a.stage_code,
           -- The MATERIAL's own state, not the test sample's. The workflow
           -- clears meristem_culture/approved to re-indexing and sends
           -- /completed back for review, so the two are not interchangeable
           -- and an operator asked to request a tip must see which it is.
           a.state_code   AS source_state,
           ssv.label      AS source_state_label,
           st.label AS bench,
           pl.test_id, tm.acronym, tm.test_name, p.pathogen_name,
           ts.sample_code AS test_sample_code,
           cur.stage_code AS test_stage, cur.state_code AS test_state,
           sv.label AS state_label,
           CASE WHEN ts.sample_code IS NULL THEN 0 ELSE 1 END::int AS n_tests,
           CASE WHEN res.outcome IS NULL THEN 0 ELSE 1 END::int    AS n_resulted,
           COALESCE(res.outcome, 'pending') AS last_status,
           -- Open requests for THIS order, test and lineage - not for this
           -- one sample. The row stands for a consignment's outstanding test,
           -- and the representative material behind it is chosen by the
           -- tie-break below, so counting per sample made the Requested chip
           -- disappear the moment a different tip won the tie-break.
           (SELECT count(*) FROM tbl_sample_request rq
             LEFT JOIN mer rm ON rm.sample_code = rq.source_sample_code
             WHERE rq.order_number = a.order_number
               AND rq.test_id = pl.test_id
               AND (CASE WHEN rm.sample_code IS NULL THEN 0 ELSE 1 END) = a.is_mer
               AND rq.status IN ('pending','authorized'))::int AS requested,
           -- How much material actually stands behind this row. Indexing asks
           -- for a consignment's retest; meristem culture then decides WHICH
           -- tip goes, so the useful number here is how many it has to choose
           -- from, not the code of whichever one the tie-break surfaced.
           (SELECT count(*) FROM view_sample_current mc
              JOIN tbl_sample ms ON ms.sample_code = mc.sample_code
             WHERE mc.order_number = a.order_number
               AND mc.stage_code = 'meristem_culture'
               AND mc.state_code = 'completed'
               AND ms.stage_code = 'meristem_culture'
               AND ms.test_id IS NULL)::int AS tips_available,
           CASE WHEN ts.sample_code IS NULL              THEN 'awaiting_initiation'
                WHEN cur.state_code = 'completed'         THEN 'completed'
                WHEN cur.state_code = 'approved'          THEN 'approved'
                WHEN cur.state_code = 'results_available' THEN 'resulted'
                WHEN cur.state_code = 'rejected'          THEN 'rejected'
                ELSE 'initiated' END AS init_status,
           cu.customer_name, c.crop_name, v.variety_name, a.since
    FROM avail a
    JOIN plan pl                 ON pl.sample_code = a.sample_code
    JOIN tbl_stage st            ON st.stage_code = a.stage_code
    LEFT JOIN tbl_state ssv      ON ssv.state_code = a.state_code
    JOIN tbl_test_method tm      ON tm.test_id = pl.test_id
    LEFT JOIN tbl_pathogen p     ON p.pathogen_id = tm.pathogen_id
    LEFT JOIN tbl_sample ts      ON ts.parent_sample_code = a.sample_code
                               AND ts.test_id = pl.test_id
    LEFT JOIN view_sample_current cur ON cur.sample_code = ts.sample_code
    LEFT JOIN tbl_state sv       ON sv.state_code = cur.state_code
    LEFT JOIN LATERAL (
      SELECT outcome FROM tbl_test_result r
      WHERE r.sample_code = ts.sample_code
      ORDER BY tested_on DESC LIMIT 1
    ) res ON TRUE
    JOIN tbl_order o             ON o.order_number = a.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = a.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    ORDER BY a.order_number, a.is_mer, pl.test_id,
             CASE WHEN ts.sample_code IS NULL           THEN 0
                  WHEN cur.state_code = 'completed'     THEN 5
                  WHEN cur.state_code = 'approved'      THEN 4
                  WHEN cur.state_code = 'results_available' THEN 3
                  WHEN cur.state_code = 'rejected'      THEN 1
                  ELSE 2 END DESC,
             a.held_here DESC, a.since, a.sample_code")
  d <- d[order(d$since, d$order_number, d$acronym), , drop = FALSE]
  shape_frame(d, proto_indexing_material(), "indexing_material()")
}

proto_test_samples <- function() list(
  sample_code   = character(),
  test_id       = integer(),
  acronym       = character(),
  test_name     = character(),
  pathogen_id   = integer(),
  pathogen_name = character(),
  outcome       = character(),
  tested_on     = as.POSIXct(character()),
  tested_by     = character()
)

#' The test-samples under one explant, each with its single latest result.
#' One row per test-sample (one test each). outcome NA = result not yet in.
#' @export
test_samples <- function(explant_code) {
  force(explant_code)
  d <- load_data(pool, "
    SELECT s.sample_code, s.test_id, t.acronym, t.test_name,
           t.pathogen_id, p.pathogen_name,
           lr.outcome, lr.tested_on, lr.tested_by
    FROM tbl_sample s
    JOIN tbl_test_method t   ON t.test_id = s.test_id
    LEFT JOIN tbl_pathogen p ON p.pathogen_id = t.pathogen_id
    LEFT JOIN LATERAL (
        SELECT outcome, tested_on, tested_by FROM tbl_test_result r
        WHERE r.sample_code = s.sample_code
        ORDER BY tested_on DESC LIMIT 1
    ) lr ON true
    WHERE s.parent_sample_code = $1 AND s.test_id IS NOT NULL
    ORDER BY t.acronym",
                 params = list(explant_code))
  shape_frame(d, proto_test_samples(), "test_samples()")
}

proto_indexing_tests <- function() list(
  explant       = character(),
  sample_code   = character(),
  test_id       = integer(),
  acronym       = character(),
  test_name     = character(),
  pathogen_name = character(),
  outcome       = character(),
  tested_on     = as.POSIXct(character()),
  tested_by     = character(),
  n_files       = integer()
)

#' ALL test-samples for every explant currently at an indexing stage, in one
#' query. The module splits this by `explant` to render each explant's tests
#' inside its expanded row - one query for the whole bench, no N+1.
#'
#' n_files = evidence files attached to the test-sample (tbl_file.sample_code).
#' tested_by is who recorded the latest result, used to enforce that a reviewer
#' is not also a recorder (segregation of duty).
#' @export
indexing_tests <- function() {
  d <- load_data(pool, "
    SELECT s.parent_sample_code AS explant, s.sample_code, s.test_id,
           t.acronym, t.test_name, p.pathogen_name,
           lr.outcome, lr.tested_on, lr.tested_by,
           COALESCE(fc.n_files, 0)::int AS n_files
    FROM tbl_sample s
    JOIN tbl_test_method t   ON t.test_id = s.test_id
    LEFT JOIN tbl_pathogen p ON p.pathogen_id = t.pathogen_id
    LEFT JOIN LATERAL (
        SELECT outcome, tested_on, tested_by FROM tbl_test_result r
        WHERE r.sample_code = s.sample_code
        ORDER BY tested_on DESC LIMIT 1
    ) lr ON true
    LEFT JOIN (
        SELECT sample_code, count(*) AS n_files FROM tbl_file GROUP BY sample_code
    ) fc ON fc.sample_code = s.sample_code
    WHERE s.test_id IS NOT NULL
      -- Filter on where the TEST SAMPLE is, not where its parent is.
      --
      -- This used to require the PARENT to be at an indexing bench, which was
      -- right when clearance MOVED the source there. Under the holding-stage
      -- model the source stays in quarantine and only the test sample is cut
      -- onto the bench - so the old condition excluded every test sample ever
      -- created, and the results-entry list came back empty for all of them.
      -- No error: just a panel with nowhere to type a result.
      AND EXISTS (
          SELECT 1 FROM view_sample_current c
          WHERE c.sample_code = s.sample_code
            AND c.stage_code IN ('molecular_virus_indexing','grafting_virus_indexing'))
    ORDER BY s.parent_sample_code, t.acronym")
  shape_frame(d, proto_indexing_tests(), "indexing_tests()")
}

#' The usernames who cultured a meristem tip (established / updated it, or
#' excised it as a child). Used to block a reviewer from approving their own
#' culture before re-indexing.
#' @export
meristem_recorders <- function(sample_code) {
  force(sample_code)
  d <- load_data(pool, "
    SELECT DISTINCT actor FROM tbl_sample_event
    WHERE sample_code = $1 AND stage_code = 'meristem_culture'
      AND state_code IN ('established','updated') AND actor IS NOT NULL",
                 params = list(sample_code))
  if (nrow(d) == 0) character(0) else d$actor
}

#' The usernames who ran a thermotherapy sample's treatment (placed / updated
#' it). Used to block a reviewer from approving their own treatment.
#' @export
thermo_recorders <- function(sample_code) {
  force(sample_code)
  d <- load_data(pool, "
    SELECT DISTINCT actor FROM tbl_sample_event
    WHERE sample_code = $1 AND stage_code = 'thermotherapy'
      AND state_code IN ('inprogress','updated') AND actor IS NOT NULL",
                 params = list(sample_code))
  if (nrow(d) == 0) character(0) else d$actor
}

#' The usernames who recorded any result for an explant's test-samples. Used to
#' block a reviewer from approving their own recordings.
#' @export
result_recorders <- function(explant_code) {
  force(explant_code)
  d <- load_data(pool, "
    SELECT DISTINCT r.tested_by
    FROM tbl_sample s
    JOIN tbl_test_result r ON r.sample_code = s.sample_code
    WHERE s.parent_sample_code = $1 AND s.test_id IS NOT NULL
      AND r.tested_by IS NOT NULL",
                 params = list(explant_code))
  if (nrow(d) == 0) character(0) else d$tested_by
}

proto_tests_for <- function() list(
  test_id       = integer(),
  acronym       = character(),
  test_name     = character(),
  pathogen_id   = integer(),
  pathogen_name = character(),
  already       = logical()
)

#' Which tests this sample should be indexed for, and whether a test-sample
#' already exists for each.
#'
#'  * fresh explant (no positive-testing ancestor): every test the order
#'    requested (tbl_order_test)
#'  * meristem tip (parent explant tested positive): ONLY the parent's
#'    positive tests - "re-test only what came back positive"
#'
#' `already` is TRUE when a test-sample for that test already hangs off this
#' explant, so the module does not double-create.
#' @export
tests_for_sample <- function(sample_code) {
  force(sample_code)
  # is this sample a meristem tip whose parent has positive test results?
  info <- load_data(pool, "
    SELECT s.order_number, s.parent_sample_code, s.stage_code
    FROM tbl_sample s WHERE s.sample_code = $1", params = list(sample_code))
  if (nrow(info) == 0) return(shape_frame(data.frame(), proto_tests_for(), "tests_for_sample()"))
  
  parent <- info$parent_sample_code[1]
  # positive tests on the parent explant's OWN test-samples (latest result each)
  pos_tests <- if (!is.na(parent)) {
    load_data(pool, "
      SELECT DISTINCT ps.test_id
      FROM tbl_sample ps
      JOIN LATERAL (
        SELECT outcome FROM tbl_test_result r
        WHERE r.sample_code = ps.sample_code
        ORDER BY tested_on DESC LIMIT 1
      ) lr ON true
      WHERE ps.parent_sample_code = $1 AND ps.test_id IS NOT NULL
        AND lr.outcome IN ('positive','inconclusive')",
              params = list(parent))
  } else data.frame(test_id = integer(0))
  
  restrict_to_positive <- !is.na(parent) && nrow(pos_tests) > 0
  
  d <- load_data(pool, "
    SELECT t.test_id, t.acronym, t.test_name, t.pathogen_id, p.pathogen_name,
           EXISTS (SELECT 1 FROM tbl_sample x
                   WHERE x.parent_sample_code = $2 AND x.test_id = t.test_id) AS already
    FROM tbl_order_test ot
    JOIN tbl_test_method t   ON t.test_id = ot.test_id
    LEFT JOIN tbl_pathogen p ON p.pathogen_id = t.pathogen_id
    WHERE ot.order_number = $1
    ORDER BY t.acronym",
                 params = list(info$order_number[1], sample_code))
  d <- shape_frame(d, proto_tests_for(), "tests_for_sample()")
  
  # FALLBACK: no test methods were requested at registration (pathogen
  # detection not selected, so tbl_order_test is empty for this order). The
  # lab's rule is then to test EVERY active method for the order's crop -
  # nothing is silently skipped. Resolve the crop via tbl_order_detail and
  # take all active tbl_test_method rows for it.
  if (nrow(d) == 0) {
    d <- load_data(pool, "
      SELECT t.test_id, t.acronym, t.test_name, t.pathogen_id, p.pathogen_name,
             EXISTS (SELECT 1 FROM tbl_sample x
                     WHERE x.parent_sample_code = $2 AND x.test_id = t.test_id) AS already
      FROM tbl_order_detail od
      JOIN tbl_test_method t   ON t.crop_id = od.crop_id AND t.active
      LEFT JOIN tbl_pathogen p ON p.pathogen_id = t.pathogen_id
      WHERE od.order_number = $1
      ORDER BY t.acronym",
                   params = list(info$order_number[1], sample_code))
    d <- shape_frame(d, proto_tests_for(), "tests_for_sample()")
  }
  
  if (restrict_to_positive && nrow(d) > 0) {
    d <- d[d$test_id %in% pos_tests$test_id, , drop = FALSE]
  }
  d
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


# ============================================================================
# STAGE QUEUES · thermotherapy and meristem culture
# ----------------------------------------------------------------------------
# Same pull-queue shape as index_queue(): read view_stage_queue for one stage,
# join the facts the module needs to render. Each stage module owns its queue
# and pulls whatever has arrived, regardless of the upstream step. A sample
# reaches thermotherapy three ways (positive index, the cassava express lane,
# or a restart) and reaches meristem one way (thermotherapy completed); the
# queue does not care which, it just lists what is currently there.
# ============================================================================

proto_thermo_queue <- function() list(
  sample_code        = character(),
  order_number       = character(),
  state_code         = character(),
  state_label        = character(),
  since              = as.POSIXct(character()),
  quantity           = integer(),
  parent_sample_code = character(),
  customer_name      = character(),
  crop_name          = character(),
  variety_name       = character(),
  conviron           = character(),
  entered_on         = as.Date(character()),
  exit_date          = as.Date(character()),
  expected_days      = integer(),
  due_out            = as.Date(character()),
  days_in            = integer(),
  overdue            = logical()
)

#' Samples at thermotherapy, with any open conviron placement.
#'
#' The LEFT JOIN to tbl_thermotherapy_detail matters: a sample that has just
#' arrived at thermotherapy/inprogress has NO chamber row yet - assigning one
#' is the module's first act - and it must still appear in the queue. A NULL
#' conviron is exactly the "needs placing" signal.
#' @export
thermo_queue <- function() {
  d <- load_data(pool, "
    SELECT q.sample_code, q.order_number, q.state_code, q.state_label,
           q.since, q.quantity, q.parent_sample_code,
           cu.customer_name, c.crop_name, v.variety_name,
           t.conviron, t.entered_on, t.exit_date,
           t.expected_days_in_conviron AS expected_days,
           (t.entered_on + t.expected_days_in_conviron)::date AS due_out,
           -- days in the chamber: to exit if exited, else to today
           (COALESCE(t.exit_date, CURRENT_DATE) - t.entered_on)::int AS days_in,
           -- overdue only matters while the sample is still IN the chamber
           (t.exit_date IS NULL
            AND t.expected_days_in_conviron IS NOT NULL
            AND CURRENT_DATE > t.entered_on + t.expected_days_in_conviron) AS overdue
    FROM view_stage_queue q
    JOIN tbl_order o             ON o.order_number = q.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = q.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    LEFT JOIN tbl_thermotherapy_detail t
           ON t.sample_code = q.sample_code
    WHERE q.stage_code = 'thermotherapy'
    ORDER BY q.since")
  shape_frame(d, proto_thermo_queue(), "thermo_queue()")
}

proto_meristem_queue <- function() list(
  sample_code        = character(),
  order_number       = character(),
  state_code         = character(),
  state_label        = character(),
  since              = as.POSIXct(character()),
  quantity           = integer(),
  parent_sample_code = character(),
  group_key          = character(),
  customer_name      = character(),
  crop_name          = character(),
  variety_name       = character(),
  n_children         = integer(),
  role               = character()
)

#' Samples at meristem culture, with a count of meristem children already
#' excised from each.
#'
#' n_children counts tbl_sample rows whose parent_sample_code is this sample
#' and whose stage is meristem_culture - the tips taken so far. Zero means the
#' source explant is present but nothing has been excised yet.
#' @export
meristem_queue <- function() {
  d <- load_data(pool, "
    SELECT q.sample_code, q.order_number, q.state_code, q.state_label,
           q.since, q.quantity, q.parent_sample_code,
           -- A TIP is a sample that was BORN on this bench: it was excised
           -- here. Everything else is source EXPLANT material, drawn in from
           -- somewhere else.
           --
           -- Read from the sample's OWN creation stage. Reading the parent's
           -- was true only from the second generation onward - a tip excised
           -- from an explant that arrived from thermotherapy has a parent
           -- created at thermotherapy, so the first batch of tips off every
           -- consignment was classified as explant material and offered
           -- excision instead of update and review.
           CASE WHEN me.stage_code = 'meristem_culture' THEN 'tip'
                ELSE 'explant' END AS role,
           -- Group a tip under its parent; group an explant under itself. The
           -- old COALESCE grouped drawn explants under their QUARANTINE parent,
           -- a code that is not on this bench at all.
           CASE WHEN me.stage_code = 'meristem_culture' THEN q.parent_sample_code
                ELSE q.sample_code END AS group_key,
           cu.customer_name, c.crop_name, v.variety_name,
           COALESCE(ch.n, 0)::int AS n_children
    FROM view_stage_queue q
    JOIN tbl_sample me           ON me.sample_code = q.sample_code
    JOIN tbl_order o             ON o.order_number = q.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = q.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    LEFT JOIN (
        SELECT parent_sample_code, count(*) AS n
        FROM tbl_sample
        WHERE stage_code = 'meristem_culture' AND parent_sample_code IS NOT NULL
        GROUP BY parent_sample_code
    ) ch ON ch.parent_sample_code = q.sample_code
    WHERE q.stage_code = 'meristem_culture'
    ORDER BY q.since")
  shape_frame(d, proto_meristem_queue(), "meristem_queue()")
}


# ============================================================================
# QUARANTINE SAMPLE CLEARANCE
# ----------------------------------------------------------------------------
# Initiation creates samples on a bench at their birth state - glasshouse
# 'established', growthroom 'received'. Only the 'approved' state exits to
# indexing (cassava.yaml). Something must move established/received -> approved,
# and that is quarantine clearance: a technician confirms the material is clean
# and cleared to proceed. This queue lists the samples awaiting that call.
# ============================================================================

proto_clearance_queue <- function() list(
  sample_code   = character(),
  order_number  = character(),
  stage_code    = character(),
  stage_label   = character(),
  state_code    = character(),
  state_label   = character(),
  since         = as.POSIXct(character()),
  quantity      = integer(),
  customer_name = character(),
  crop_name     = character(),
  variety_name  = character(),
  bench_no      = character()
)

#' Samples awaiting quarantine clearance: on a bench at their birth state
#' (glasshouse 'established' or growthroom 'received'), not yet approved.
#'
#' `order_number` lets the caller offer both per-sample clearance and a
#' whole-consignment batch (every awaiting sample of one order at once).
#' @export
clearance_queue <- function() {
  d <- load_data(pool, "
    SELECT q.sample_code, q.order_number, q.stage_code, q.stage_label,
           q.state_code, q.state_label, q.since, q.quantity,
           cu.customer_name, c.crop_name, v.variety_name,
           oq.bench_no
    FROM view_stage_queue q
    JOIN tbl_order o             ON o.order_number = q.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = q.order_number
    LEFT JOIN tbl_crop c         ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    LEFT JOIN tbl_order_quarantine oq
           ON oq.order_number = q.order_number AND oq.stage_code = q.stage_code
    WHERE (q.stage_code = 'quarantine_glasshouse' AND q.state_code = 'established')
       OR (q.stage_code = 'quarantine_growthroom' AND q.state_code = 'received')
    ORDER BY q.order_number, q.since")
  shape_frame(d, proto_clearance_queue(), "clearance_queue()")
}


# ============================================================================
# BARCODE STATION · scan any code, see history + next step
# ----------------------------------------------------------------------------
# Accepts ANY code: an explant, a meristem tip, a test-sample, or a plant. It
# resolves what the code is, where it is now, its recommended next step(s), and
# every LEGAL alternative (the PERMITTED set from tbl_stage_state, which is
# broader than the workflow recommendation). The module records the chosen step
# and routes into the owning stage module.
# ============================================================================

proto_scan_identity <- function() list(
  sample_code        = character(),
  kind               = character(),
  order_number       = character(),
  parent_sample_code = character(),
  root_sample_code   = character(),
  depth              = integer(),
  test_id            = integer(),
  test_acronym       = character(),
  stage_code         = character(),
  stage_label        = character(),
  state_code         = character(),
  state_label        = character(),
  since              = as.POSIXct(character()),
  quantity           = integer(),
  customer_name      = character(),
  crop_name          = character(),
  variety_name       = character()
)

#' Resolve a scanned code to its identity and current position.
#'
#' `kind` is one of: explant, meristem_tip, test_sample, plant. (A "plant" is a
#' sample at an in-vivo stage - hardening / in_vivo_conservation - but the code
#' space is shared, so this just labels it; the same row shape is returned.)
#' Zero rows means the code is unknown.
#' @export
scan_identity <- function(code) {
  force(code)
  if (is.null(code) || !nzchar(trimws(code))) {
    return(shape_frame(data.frame(), proto_scan_identity(), "scan_identity()"))
  }
  d <- load_data(pool, "
    SELECT s.sample_code,
           CASE
             WHEN s.test_id IS NOT NULL                       THEN 'test_sample'
             WHEN cur.stage_code IN ('hardening','in_vivo_conservation') THEN 'plant'
             WHEN s.parent_sample_code IS NOT NULL
                  AND EXISTS (SELECT 1 FROM tbl_sample p
                              WHERE p.sample_code = s.parent_sample_code) THEN 'meristem_tip'
             ELSE 'explant'
           END                                                AS kind,
           s.order_number, s.parent_sample_code,
           lin.root_sample_code, lin.depth, s.test_id, tm.acronym AS test_acronym,
           cur.stage_code, st.label AS stage_label,
           cur.state_code, sv.label AS state_label,
           cur.since, s.quantity,
           cu.customer_name, c.crop_name, v.variety_name
    FROM tbl_sample s
    LEFT JOIN view_sample_current cur ON cur.sample_code = s.sample_code
    LEFT JOIN view_sample_lineage lin ON lin.sample_code = s.sample_code
    LEFT JOIN tbl_stage st            ON st.stage_code = cur.stage_code
    LEFT JOIN tbl_state sv            ON sv.state_code = cur.state_code
    LEFT JOIN tbl_test_method tm      ON tm.test_id = s.test_id
    JOIN tbl_order o                  ON o.order_number = s.order_number
    JOIN tbl_customer cu              ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail od     ON od.order_number = s.order_number
    LEFT JOIN tbl_crop c              ON c.crop_id = od.crop_id
    LEFT JOIN tbl_variety v           ON v.variety_id = od.variety_id
    WHERE s.sample_code = $1", params = list(code))
  shape_frame(d, proto_scan_identity(), "scan_identity()")
}

proto_scan_history <- function() list(
  occurred_on = as.POSIXct(character()),
  stage_label = character(),
  state_label = character(),
  actor       = character(),
  is_override = logical(),
  notes       = character()
)

#' The event timeline for a scanned code, newest first.
#' @export
scan_history <- function(code) {
  force(code)
  d <- load_data(pool, "
    SELECT e.occurred_on, st.label AS stage_label, sv.label AS state_label,
           e.actor, e.is_override, e.notes
    FROM tbl_sample_event e
    JOIN tbl_stage st ON st.stage_code = e.stage_code
    JOIN tbl_state sv ON sv.state_code = e.state_code
    WHERE e.sample_code = $1
    ORDER BY e.occurred_on DESC", params = list(code))
  shape_frame(d, proto_scan_history(), "scan_history()")
}

proto_scan_options <- function() list(
  to_stage    = character(),
  to_state    = character(),
  state_label = character(),
  label       = character(),
  kind        = character(),
  recommended = logical()
)

#' Next-step options for a scanned sample: the workflow recommendation(s) PLUS
#' every legal alternative (PERMITTED set from tbl_stage_state).
#'
#' Recommended rows are marked recommended = TRUE and sorted first. `kind`
#' carries the workflow block type for recommended rows ("then"/"rule"/
#' "fan_out"/"choice"), or "permitted" for legal-but-not-recommended moves.
#' @export
scan_options <- function(code) {
  force(code)
  id <- scan_identity(code)
  if (nrow(id) == 0 || is.na(id$stage_code[1])) {
    return(shape_frame(data.frame(), proto_scan_options(), "scan_options()"))
  }
  stage <- id$stage_code[1]; state <- id$state_code[1]
  
  # workflow recommendations (may target other stages)
  wf  <- tryCatch(workflow_cache(WF_PATH_TRACK), error = function(e) NULL)
  ctx <- tryCatch(order_context(pool, id$order_number[1]), error = function(e) list())
  rec <- if (!is.null(wf)) {
    o <- tryCatch(next_options(wf, stage, state, ctx), error = function(e) NULL)
    if (!is.null(o) && nrow(o) > 0) o else NULL
  } else NULL
  
  # legal alternatives: every state PERMITTED within the CURRENT stage
  # (tbl_stage_state), i.e. moves the composite FK will accept
  perm <- load_data(pool, "
    SELECT ss.stage_code AS to_stage, ss.state_code AS to_state, st.label AS state_label
    FROM tbl_stage_state ss
    JOIN tbl_state st ON st.state_code = ss.state_code
    WHERE ss.stage_code = $1
    ORDER BY st.label", params = list(stage))
  
  out <- data.frame(
    to_stage = character(0), to_state = character(0), state_label = character(0),
    label = character(0), kind = character(0), recommended = logical(0),
    stringsAsFactors = FALSE)
  
  if (!is.null(rec)) {
    # recommended targets enter their stage at a conventional entry state; we
    # show the label and let the target module set the precise state on record
    out <- rbind(out, data.frame(
      to_stage = rec$to_stage,
      to_state = NA_character_,
      state_label = NA_character_,
      label = rec$label,
      kind = rec$kind,
      recommended = TRUE, stringsAsFactors = FALSE))
  }
  if (nrow(perm) > 0) {
    out <- rbind(out, data.frame(
      to_stage = perm$to_stage, to_state = perm$to_state,
      state_label = perm$state_label,
      label = paste0("Set state: ", perm$state_label),
      kind = "permitted", recommended = FALSE, stringsAsFactors = FALSE))
  }
  shape_frame(out, proto_scan_options(), "scan_options()")
}



# ============================================================================
# RECEPTION HANDOFF · consignments not yet received anywhere
# ----------------------------------------------------------------------------
# An approved order is available to be received by EITHER quarantine or
# thermotherapy, whichever bench takes it first. "Not received anywhere" means
# both of:
#   * no tbl_order_quarantine row  - quarantine has not put it on a bench
#   * no tbl_sample rows           - no bench has created samples from it
# The second guard is what makes the two benches mutually exclusive: once one
# receives the consignment and creates samples, it stops offering in the other.
# ============================================================================

proto_unreceived_orders <- function() list(
  order_number     = character(),
  customer_name    = character(),
  crop_name        = character(),
  variety_name     = character(),
  sample_type      = character(),
  sample_type_code = character(),
  sample_amount    = integer(),
  date_received    = as.Date(character()),
  approved_on      = as.POSIXct(character())
)

#' Approved consignments that no bench has received yet.
#' @export
unreceived_orders <- function() {
  d <- load_data(pool, "
    SELECT o.order_number, cu.customer_name, c.crop_name, v.variety_name,
           st.label AS sample_type, d.sample_type_code,
           o.sample_amount, d.date_received, o.approved_on
    FROM tbl_order o
    JOIN tbl_customer cu           ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d   ON d.order_number = o.order_number
    LEFT JOIN tbl_crop c           ON c.crop_id = d.crop_id
    LEFT JOIN tbl_variety v        ON v.variety_id = d.variety_id
    LEFT JOIN tbl_sample_type st   ON st.sample_type_code = d.sample_type_code
    WHERE o.approval_state = 'approved'
      AND NOT EXISTS (SELECT 1 FROM tbl_order_quarantine q
                      WHERE q.order_number = o.order_number)
      AND NOT EXISTS (SELECT 1 FROM tbl_sample s
                      WHERE s.order_number = o.order_number)
    ORDER BY o.approved_on NULLS LAST, o.created_on")
  shape_frame(d, proto_unreceived_orders(), "unreceived_orders()")
}


# ============================================================================
# APPROVAL REQUESTS · a technician asks an admin to review
# ----------------------------------------------------------------------------
# Approval is admin-only. A technician who finishes work cannot approve it, so
# instead they raise a request, recorded in tbl_order_event (the same audit
# trail every module already writes to - no new table). The module shows when a
# request was last raised so the same one is not sent repeatedly.
#
# Delivery is deliberately separate from recording: the request is a durable
# fact in the database, and whatever notifies the admins (in-app badge, email,
# or both) reads from it. That keeps the audit trail correct even if a mail
# transport is unavailable.
# ============================================================================

APPROVAL_REQUEST_ACTION <- "approval requested"

proto_approval_request <- function() list(
  requested_on = as.POSIXct(character()),
  actor        = character(),
  notes        = character()
)

#' The most recent approval request raised for a sample, if any.
#' @export
last_approval_request <- function(sample_code) {
  force(sample_code)
  d <- load_data(pool, "
    SELECT occurred_on AS requested_on, actor, notes
    FROM tbl_order_event
    WHERE module = 'approval'
      AND action = $1
      AND notes LIKE '%' || $2 || '%'
    ORDER BY occurred_on DESC
    LIMIT 1",
                 params = list(APPROVAL_REQUEST_ACTION, sample_code))
  shape_frame(d, proto_approval_request(), "last_approval_request()")
}



proto_approvers <- function() list(
  username    = character(),
  email       = character(),
  full_name   = character(),
  designation = character(),
  laboratory  = character()
)

#' Administrators who can approve, with their email addresses.
#'
#' Read live from shinymanager's `credentials` table, which owns accounts.
#' tbl_app_user deliberately does not FK to it (lab history must outlive
#' accounts), so this is a lookup for notification only - never a source of
#' historical actor identity.
#'
#' Excludes accounts with no address and accounts whose access has expired or
#' not yet started, so a reminder is never aimed at someone who cannot act.
#' Note "user" is a reserved word in SQL and must stay quoted.
#' @export
approver_emails <- function() {
  d <- load_data(pool, "
    SELECT c.\"user\" AS username,
           c.email,
           NULLIF(btrim(coalesce(c.firstname,'') || ' ' || coalesce(c.lastname,'')), '') AS full_name,
           c.designation, c.laboratory
    FROM credentials c
    WHERE c.admin IS TRUE
      AND c.email IS NOT NULL
      AND btrim(c.email) <> ''
      AND (c.start IS NULL OR c.start <= CURRENT_DATE)
      AND (c.expire IS NULL OR c.expire >= CURRENT_DATE)
    ORDER BY username")
  shape_frame(d, proto_approvers(), "approver_emails()")
}

# ============================================================================
# SURFACE STERILIZATION
# ----------------------------------------------------------------------------
# The first bench that works on material the lab has DECLARED clean rather
# than material it is still deciding about. It cuts nothing: quarantine and
# meristem culture own all cutting, so this is a pull queue like the others.
# ============================================================================

proto_sterilization_stock <- function() list(
  row_id                = character(),
  order_number          = character(),
  source_stage          = character(),
  source_bench          = character(),
  units_available       = integer(),
  suggested_sample_code = character(),
  was_cleaned           = logical(),
  available_since       = as.POSIXct(character()),
  requested             = integer(),
  in_progress           = integer(),
  customer_name         = character(),
  crop_name             = character(),
  variety_name          = character()
)

#' Clean material offered to the sterilization bench, one row per order.
#'
#' The eligibility reasoning lives in `view_lineage_clearance`, not here.
#' Surface sterilization, subculture and hardening all gate on the same fact -
#' has every test this consignment requires come back negative - and three
#' copies of that reasoning in three modules would disagree within a release.
#'
#' A row is an ORDER and a holding bench, never a specific piece of tissue.
#' Which tip actually goes is decided by the bench holding it, looking at the
#' material; naming one here would be this bench guessing about plants it
#' cannot see.
#' @export
sterilization_stock <- function() {
  d <- load_data(pool, "
    SELECT s.order_number || ':' || s.source_stage AS row_id,
           s.order_number, s.source_stage, s.source_bench,
           s.units_available, s.suggested_sample_code, s.was_cleaned,
           s.available_since,
           -- Requests already outstanding for this order, so the bench does not
           -- ask twice for the same consignment. Counted per ORDER because the
           -- row is per order: counting against the suggested sample alone
           -- would lose the request the moment a different piece was suggested.
           (SELECT count(*) FROM tbl_sample_request rq
             WHERE rq.order_number = s.order_number
               AND rq.to_stage = 'surface_sterilization'
               AND rq.status IN ('pending','authorized'))::int AS requested,
           -- Already on this bench for the same order. Material in hand is not
           -- a reason to hide the row - a consignment can need several batches
           -- - but it is a reason to say so before asking for more.
           (SELECT count(*) FROM view_sample_current vc
             WHERE vc.order_number = s.order_number
               AND vc.stage_code = 'surface_sterilization'
               AND vc.state_code NOT IN ('completed','rejected','depleted'))::int
             AS in_progress,
           cu.customer_name, cr.crop_name, v.variety_name
    FROM view_sterilization_stock s
    JOIN tbl_order o             ON o.order_number = s.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = s.order_number
    LEFT JOIN tbl_crop cr        ON cr.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    ORDER BY s.available_since, s.order_number")
  shape_frame(d, proto_sterilization_stock(), "sterilization_stock()")
}


proto_sterilization_queue <- function() list(
  sample_code       = character(),
  order_number      = character(),
  state_code        = character(),
  state_label       = character(),
  since             = as.POSIXct(character()),
  parent_sample_code = character(),
  units_held        = integer(),
  sterilant         = character(),
  concentration     = character(),
  exposure_minutes  = integer(),
  rinses            = integer(),
  sterilized_on     = as.Date(character()),
  initial_count     = integer(),
  protocol_notes    = character(),
  protocol_recorded = logical(),
  ledger_count      = integer(),
  n_contaminated    = integer(),
  n_dead            = integer(),
  n_discarded       = integer(),
  drift             = logical(),
  customer_name     = character(),
  crop_name         = character(),
  variety_name      = character()
)

#' Batches standing on the sterilization bench.
#' @export
sterilization_queue <- function() {
  d <- load_data(pool, "
    SELECT sample_code, order_number, state_code, state_label, since,
           parent_sample_code, units_held, sterilant, concentration,
           exposure_minutes, rinses, sterilized_on, initial_count,
           protocol_notes, protocol_recorded, ledger_count, n_contaminated,
           n_dead, n_discarded, drift, customer_name, crop_name, variety_name
    FROM view_sterilization_current
    ORDER BY since")
  shape_frame(d, proto_sterilization_queue(), "sterilization_queue()")
}


# ============================================================================
# SUBCULTURE / MULTIPLICATION
# ----------------------------------------------------------------------------
# The stage that turns one clean explant into the quantities the order asked
# for, and the first stage where material stops being shared stock and becomes
# committed to a named service line.
# ============================================================================

proto_subculture_stock <- function() list(
  row_id                = character(),
  order_number          = character(),
  source_stage          = character(),
  source_bench          = character(),
  batches_available     = integer(),
  units_available       = integer(),
  suggested_sample_code = character(),
  available_since       = as.POSIXct(character()),
  requested             = integer(),
  on_bench              = integer(),
  still_needed          = integer(),
  customer_name         = character(),
  crop_name             = character(),
  variety_name          = character()
)

#' Sterilized material offered to the subculture bench, one row per order.
#'
#' Subculture cuts nothing of its own: surface sterilization holds the material
#' and a technician there chooses which batch to send. This is the shop window.
#' @export
subculture_stock <- function() {
  d <- load_data(pool, "
    SELECT s.order_number || ':' || s.source_stage AS row_id,
           s.order_number, s.source_stage, s.source_bench,
           s.batches_available, s.units_available, s.suggested_sample_code,
           s.available_since,
           (SELECT count(*) FROM tbl_sample_request rq
             WHERE rq.order_number = s.order_number
               AND rq.to_stage = 'subculture'
               AND rq.status IN ('pending','authorized'))::int AS requested,
           (SELECT count(*) FROM view_sample_current vc
             WHERE vc.order_number = s.order_number
               AND vc.stage_code = 'subculture'
               AND vc.state_code NOT IN ('completed','rejected'))::int AS on_bench,
           -- What the order still owes, across every fulfilment line. The
           -- number that says whether another batch is worth starting; it is
           -- the same arithmetic view_order_service_progress does, so the
           -- bench and the order screen cannot disagree about progress.
           COALESCE((SELECT sum(dm.remaining_qty) FROM view_subculture_demand dm
                      WHERE dm.order_number = s.order_number), 0)::int AS still_needed,
           cu.customer_name, cr.crop_name, v.variety_name
    FROM view_subculture_stock s
    JOIN tbl_order o             ON o.order_number = s.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = s.order_number
    LEFT JOIN tbl_crop cr        ON cr.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    ORDER BY s.available_since, s.order_number")
  shape_frame(d, proto_subculture_stock(), "subculture_stock()")
}


proto_subculture_queue <- function() list(
  sample_code       = character(),
  order_number      = character(),
  state_code        = character(),
  state_label       = character(),
  since             = as.POSIXct(character()),
  parent_sample_code = character(),
  units_held        = integer(),
  order_service_id  = integer(),
  service_code      = character(),
  service_label     = character(),
  n_cycles          = integer(),
  last_cycle_no     = integer(),
  medium            = character(),
  vessel            = character(),
  subcultured_on    = as.Date(character()),
  last_copies_out   = integer(),
  cycle_recorded    = logical(),
  ledger_count      = integer(),
  n_contaminated    = integer(),
  n_dead            = integer(),
  n_discarded       = integer(),
  n_allocated       = integer(),
  unallocated_units = integer(),
  drift             = logical(),
  customer_name     = character(),
  crop_name         = character(),
  variety_name      = character()
)

#' Batches standing on the subculture bench.
#' @export
subculture_queue <- function() {
  d <- load_data(pool, "
    SELECT sample_code, order_number, state_code, state_label, since,
           parent_sample_code, units_held, order_service_id, service_code,
           service_label, n_cycles, last_cycle_no, medium, vessel,
           subcultured_on, last_copies_out, cycle_recorded, ledger_count,
           n_contaminated, n_dead, n_discarded, n_allocated, unallocated_units,
           drift, customer_name, crop_name, variety_name
    FROM view_subculture_current
    ORDER BY since")
  shape_frame(d, proto_subculture_queue(), "subculture_queue()")
}


proto_subculture_demand <- function() list(
  order_service_id     = integer(),
  order_number         = character(),
  service_code         = character(),
  service_label        = character(),
  unit                 = character(),
  recipient            = character(),
  target_qty           = integer(),
  fulfilled_qty        = integer(),
  remaining_qty        = integer(),
  pct_complete         = integer(),
  status               = character(),
  unallocated_on_bench = integer()
)

#' Outstanding fulfilment demand per service line.
#'
#' The keystone of the whole model shows up here: tbl_order_service has a
#' SURROGATE key, so one order can ask for 200 for sale AND 50 for
#' conservation, and each line is allocated against separately. A natural key
#' on (order, service) could not express it.
#' @export
subculture_demand <- function(order_number = NULL) {
  d <- if (is.null(order_number)) {
    load_data(pool, "SELECT * FROM view_subculture_demand ORDER BY order_number, service_label")
  } else {
    load_data(pool, "
      SELECT * FROM view_subculture_demand
      WHERE order_number = $1
      ORDER BY service_label", params = list(order_number))
  }
  shape_frame(d, proto_subculture_demand(), "subculture_demand()")
}


# ============================================================================
# HARDENING / ACCLIMATIZATION
# ----------------------------------------------------------------------------
# The one bench where attrition is the headline number. Plantlets leave sterile
# culture for substrate, and survival is what the lab reports - so it is
# computed in the view and shown, not left to be worked out on screen.
# ============================================================================

proto_hardening_stock <- function() list(
  row_id                = character(),
  order_number          = character(),
  source_stage          = character(),
  source_bench          = character(),
  batches_available     = integer(),
  units_available       = integer(),
  units_bound           = integer(),
  suggested_sample_code = character(),
  available_since       = as.POSIXct(character()),
  services_owed         = character(),
  still_needed          = integer(),
  requested             = integer(),
  on_bench              = integer(),
  customer_name         = character(),
  crop_name             = character(),
  variety_name          = character()
)

#' Subculture material bound for soil, offered to the hardening bench.
#'
#' Bound for soil means allocated to a service that needs a plant in the
#' ground. view_hardening_stock reads that from tbl_service_allocation rather
#' than from tbl_sample.order_service_id, which only records a batch's FIRST
#' commitment - so a batch allocated to conservation and then to vines is still
#' offered here for the vines.
#' @export
hardening_stock <- function() {
  d <- load_data(pool, "
    SELECT s.order_number || ':' || s.source_stage AS row_id,
           s.order_number, s.source_stage, s.source_bench,
           s.batches_available, s.units_available, s.units_bound,
           s.suggested_sample_code, s.available_since,
           s.services_owed, s.still_needed,
           (SELECT count(*) FROM tbl_sample_request rq
             WHERE rq.order_number = s.order_number
               AND rq.to_stage = 'hardening'
               AND rq.status IN ('pending','authorized'))::int AS requested,
           (SELECT count(*) FROM view_sample_current vc
             WHERE vc.order_number = s.order_number
               AND vc.stage_code = 'hardening'
               AND vc.state_code NOT IN ('completed','rejected','depleted'))::int
             AS on_bench,
           cu.customer_name, cr.crop_name, v.variety_name
    FROM view_hardening_stock s
    JOIN tbl_order o             ON o.order_number = s.order_number
    JOIN tbl_customer cu         ON cu.customer_id = o.customer_id
    LEFT JOIN tbl_order_detail d ON d.order_number = s.order_number
    LEFT JOIN tbl_crop cr        ON cr.crop_id = d.crop_id
    LEFT JOIN tbl_variety v      ON v.variety_id = d.variety_id
    ORDER BY s.available_since, s.order_number")
  shape_frame(d, proto_hardening_stock(), "hardening_stock()")
}


proto_hardening_queue <- function() list(
  sample_code       = character(),
  order_number      = character(),
  state_code        = character(),
  state_label       = character(),
  since             = as.POSIXct(character()),
  parent_sample_code = character(),
  units_held        = integer(),
  order_service_id  = integer(),
  service_code      = character(),
  service_label     = character(),
  screenhouse       = character(),
  substrate         = character(),
  potted_on         = as.Date(character()),
  initial_count     = integer(),
  weaning_days      = integer(),
  potting_notes     = character(),
  potted            = logical(),
  days_in_substrate = integer(),
  weaning_due       = logical(),
  survival_pct      = integer(),
  ledger_count      = integer(),
  n_dead            = integer(),
  n_discarded       = integer(),
  n_contaminated    = integer(),
  n_allocated       = integer(),
  unallocated_units = integer(),
  drift             = logical(),
  customer_name     = character(),
  crop_name         = character(),
  variety_name      = character()
)

#' Batches standing in the screenhouse.
#' @export
hardening_queue <- function() {
  d <- load_data(pool, "
    SELECT sample_code, order_number, state_code, state_label, since,
           parent_sample_code, units_held, order_service_id, service_code,
           service_label, screenhouse, substrate, potted_on, initial_count,
           weaning_days, potting_notes, potted, days_in_substrate, weaning_due,
           survival_pct, ledger_count, n_dead, n_discarded, n_contaminated,
           n_allocated, unallocated_units, drift, customer_name, crop_name,
           variety_name
    FROM view_hardening_current
    ORDER BY since")
  shape_frame(d, proto_hardening_queue(), "hardening_queue()")
}


#' Outstanding soil-bound demand per service line.
#' @export
hardening_demand <- function(order_number = NULL) {
  d <- if (is.null(order_number)) {
    load_data(pool, "SELECT * FROM view_hardening_demand ORDER BY order_number, service_label")
  } else {
    load_data(pool, "
      SELECT * FROM view_hardening_demand
      WHERE order_number = $1
      ORDER BY service_label", params = list(order_number))
  }
  shape_frame(d, proto_subculture_demand(), "hardening_demand()")
}


# ============================================================================
# MATERIAL CATALOGUE
# ----------------------------------------------------------------------------
# What the lab has FINISHED and not yet moved on. One view, read by every
# downstream bench, so material cannot be visible to one and invisible to
# another - which is what happened while each bench had its own stock query
# reading its own upstream.
#
# Stage-agnostic by design. Subculture and hardening publish into it today;
# conservation and distribution read from it when they are built, without a
# new view each.
# ============================================================================

proto_material_catalog <- function() list(
  sample_code        = character(),
  order_number       = character(),
  stage_code         = character(),
  bench              = character(),
  completed_on       = as.POSIXct(character()),
  parent_sample_code = character(),
  units_held         = integer(),
  n_allocated        = integer(),
  uncommitted        = integer(),
  is_committed       = logical(),
  purpose            = character(),
  destination_labels = character(),
  open_requests      = integer(),
  customer_name      = character(),
  crop_name          = character(),
  variety_name       = character()
)

#' Finished material still held, with where it may go next.
#'
#' `destinations` is deliberately NOT in the prototype: it is a text ARRAY, and
#' a zero-length prototype cannot express a list column - shape_frame() would
#' report drift on every call and blank the table. `destination_labels` carries
#' the same information as text, which is what a screen wants anyway.
#'
#' `purpose` reads from allocations when the batch is committed and from what
#' the order still owes when it is not, so an uncommitted batch still says what
#' it is FOR rather than showing a blank where the answer is knowable.
#' @export
material_catalog <- function(stages = NULL) {
  cols <- "sample_code, order_number, stage_code, bench, completed_on,
           parent_sample_code, units_held, n_allocated, uncommitted,
           is_committed, purpose, destination_labels, open_requests,
           customer_name, crop_name, variety_name"
  d <- if (is.null(stages)) {
    load_data(pool, paste("SELECT", cols,
                          "FROM view_material_catalog ORDER BY completed_on"))
  } else {
    # string_to_array, NOT `= ANY($1)`: RPostgres binds a length-1 character
    # vector as plain text and Postgres fails on a malformed array literal.
    load_data(pool, paste("SELECT", cols, "
      FROM view_material_catalog
      WHERE stage_code = ANY(string_to_array($1, ','))
      ORDER BY completed_on"),
              params = list(paste(stages, collapse = ",")))
  }
  shape_frame(d, proto_material_catalog(), "material_catalog()")
}


# ============================================================================
# SUBCULTURE CYCLE HISTORY
# ----------------------------------------------------------------------------
# view_subculture_current rolls tbl_subculture_detail up to the LATEST cycle
# only (medium, vessel, copies_out as of the most recent passage) - correct
# for a queue row, where one line per batch is the point. A lab watching for
# somaclonal drift needs the other direction too: every passage this specific
# tissue has been through, not just the last one. That is a different shape
# of question, so it is a different reader rather than another column bolted
# onto view_subculture_current.
# ============================================================================

proto_subculture_cycles <- function() list(
  cycle_no       = integer(),
  medium         = character(),
  vessel         = character(),
  subcultured_on = as.Date(character()),
  explants_in    = integer(),
  copies_out     = integer(),
  net            = integer(),
  notes          = character(),
  recorded_by    = character(),
  recorded_on    = as.POSIXct(character())
)

#' Every recorded passage of one batch, oldest first.
#' @export
subculture_cycles <- function(sample_code) {
  force(sample_code)
  d <- load_data(pool, "
    SELECT cycle_no, medium, vessel, subcultured_on, explants_in, copies_out,
           (copies_out - explants_in) AS net, notes, recorded_by, recorded_on
    FROM tbl_subculture_detail
    WHERE sample_code = $1
    ORDER BY cycle_no", params = list(sample_code))
  shape_frame(d, proto_subculture_cycles(), "subculture_cycles()")
}

