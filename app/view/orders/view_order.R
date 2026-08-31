box::use(
  shiny,
  reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
  shinyjs[useShinyjs, hide, show],
  shinytoastr[toastr_success, toastr_error],
  pool[poolWithTransaction],
  DBI[dbExecute],
  stats[setNames],
)

box::use(
  app/logic/fct_conn[pool],
  app/logic/fct_tracking[order_summary, order_services, order_history,
                         tracker_steps, next_steps, current_stages],
  app/logic/fct_workflows[workflow_cache, next_options, order_context],
  app/view/shared/order_theme,
)

# Same path every module uses. Named here rather than passed in, because the
# workflow file is a property of the deployment, not of this screen.
WF_PATH <- file.path("app", "static", "workflows", "cassava.yaml")

# Static lookup, not session state - lives at module scope so it's built
# once when the module is loaded, not once per session.
DEST_LABEL <- c(quarantine_glasshouse = "Quarantine \u00b7 glasshouse",
                quarantine_growthroom = "Quarantine \u00b7 growthroom",
                thermotherapy         = "Thermotherapy")

# ============================================================================
# VIEW ORDER · the project view
# ----------------------------------------------------------------------------
# Answers the four questions in one page:
#   what is this order   -> order_summary()
#   where has it been    -> tracker_steps(), state "done"    (event log)
#   where is it now      -> tracker_steps(), state "current" (view_sample_current)
#   what happens next    -> tracker_steps(), state "next"    (workflow: ADVISORY)
#
# The next steps are labelled "recommended", deliberately and everywhere. The
# database accepts any legal (stage, state) - see allowed_transitions() - and
# an off-workflow move only has to carry a reason. Presenting the workflow's
# suggestion as the only option would misdescribe the system and push the real
# decision onto paper, which is the failure this whole design exists to avoid.
#
# An order can be in SEVERAL stages at once. That is not an error state: it is
# what the subculture fan-out produces when one order feeds conservation and
# distribution simultaneously. The tracker shows every current position.
#
# THIS IS THE DETAIL ONLY. order_management.R owns the list and hands us an
# order_number; we render that one order. Splitting them this way means the
# detail can also be reached from anywhere else (a search hit, a deep link)
# without dragging a list along with it.
#
# ----------------------------------------------------------------------------
# RENDERING - why the shell is no longer server-rendered
# ----------------------------------------------------------------------------
# The previous version put almost the whole page behind renderUI: an outer
# `screen` output wrapped a `detail_header` output and a `detail_body` output,
# and detail_body in turn contained more uiOutput()/reactableOutput() calls
# for the tracker, next-step hint, services table and history table.
#
# Two costs came from that:
#   1. WATERFALL. A uiOutput inside a renderUI can't be requested by the
#      browser until the *outer* renderUI has already round-tripped to the
#      server and come back - the client doesn't know the inner placeholder
#      exists yet. Three nested layers (screen -> body -> tracker/services/
#      history) meant three sequential round trips before the page was
#      actually complete, instead of one.
#   2. TEARDOWN. Every time detail_body re-rendered - which happened on
#      every summ() tick, i.e. every approve/reject and every trigger_refresh
#      - the placeholder <div> for reactableOutput("services") and
#      reactableOutput("history") was destroyed and recreated. Shiny/
#      reactable then had to rebind those widgets from scratch, which is why
#      the tables would flicker and lose sort/page state on an unrelated
#      approval action.
#
# Fix: everything that is structurally the same for every order (section
# numbers, titles, subtitles, the table/tracker placeholders themselves) now
# lives directly in ui(), sent once. Only the parts that actually differ per
# order - the header, the sample/billing values, the approval button state -
# stay behind renderUI, and each is its own small output rather than being
# nested inside a bigger one. That also lets the browser request the tracker,
# services and history bindings all at once on first paint instead of in
# sequence.
#
# The "order not found" case no longer removes the section chrome; instead
# a single lightweight observer toggles visibility (see body_visibility
# below) so nothing has to re-render to hide it.
# ============================================================================

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    shiny$actionButton(ns("back"), "Back to all orders", icon = shiny$icon("arrow-left"),
                       class = "btn btn-outline-secondary btn-sm",
                       class = "wl-statusbar"),
    
    # Only the header genuinely differs per order (status, chips, approval
    # state) - it's the one part of the shell that has to stay dynamic.
    shiny$uiOutput(ns("detail_header")),
    
    shiny$div(
      id = ns("body_sections"),
      order_theme$section(
        "1", "Progress", accent = "teal",
        sub = "Where this order has been, where it is, and what is recommended next",
        shiny$uiOutput(ns("tracker")),
        shiny$uiOutput(ns("next_hint"))
      ),
      order_theme$section(
        "2", "Services requested", accent = "teal",
        sub = "One line per requested service \u00b7 an order completes when all are fulfilled",
        reactableOutput(ns("services"))
      ),
      order_theme$section(
        "3", "Sample details",
        shiny$uiOutput(ns("sample_props"))
      ),
      order_theme$section(
        "4", "Order & billing", accent = "amber",
        shiny$uiOutput(ns("billing_props"))
      ),
      order_theme$section(
        "5", "History", accent = "teal",
        sub = "Append-only \u00b7 every event, newest first",
        reactableOutput(ns("history"))
      )
    )
  )
}


#' @export
server <- function(id, res_auth, page, tab, order_number, trigger_refresh = NULL,
                   on_back = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    code <- shiny$reactive({
      x <- if (is.function(order_number)) order_number() else order_number
      shiny$req(x, nzchar(x))
      x
    })
    
    summ <- shiny$reactive({
      if (!is.null(trigger_refresh)) trigger_refresh()
      order_summary(code())
    })
    
    # The list lives in order_management; this just tells it we are done.
    shiny$observeEvent(input$back, {
      if (is.function(on_back)) on_back()
    })
    
    # Sections 1-5 are static chrome now (see header comment), so an order
    # that doesn't exist can't skip rendering them - instead we just hide
    # the whole block. A show/hide toggle is a class change on an existing
    # node, not a re-render, so this costs nothing on the common path.
    shiny$observe({
      if (nrow(summ()) == 0) hide("body_sections") else show("body_sections")
    })
    
    # ---- header ------------------------------------------------------
    output$detail_header <- shiny$renderUI({
      s <- summ()
      if (nrow(s) == 0) {
        return(shiny$div(class = "empty-state",
                         shiny$h3("Order not found"),
                         shiny$p(paste("No order with number", code()))))
      }
      s <- s[1, ]
      status <- if (is.na(s$derived_status)) "unknown" else s$derived_status
      tone <- switch(status,
                     completed = "brand", in_progress = "teal", pending_approval = "amber",
                     rejected = "red", cancelled = "ink", "ink")
      
      shiny$div(
        class = "intake-header",
        shiny$div(
          shiny$tags$h1(s$order_number),
          shiny$tags$p(paste(s$customer_name,
                             if (!is.na(s$crop_name)) paste("\u00b7", s$crop_name) else "")),
          shiny$div(
            style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-top:8px;",
            order_theme$chip(toupper(gsub("_", " ", status)), tone),
            if (!is.na(s$pct_complete)) order_theme$chip(paste0(s$pct_complete, "% complete"), "teal"),
            if (!is.na(s$order_kind) && s$order_kind != "primary") {
              order_theme$chip(toupper(s$order_kind), "amber")
            },
            if (!is.na(s$parent_order_number)) {
              shiny$span(class = "text-muted", style = "font-size:12px;",
                         paste("continues", s$parent_order_number))
            }
          )
        ),
        # Approval lives HERE, on the record being judged - not on a separate
        # screen. A reviewer decides with the tracker, the services and the
        # sample details in front of them.
        shiny$div(class = "actions", shiny$uiOutput(ns("approval_actions")))
      )
    })
    
    # ---- sample & billing detail --------------------------------------
    # These are the only genuinely data-dependent parts of what used to be
    # detail_body. Split into their own outputs so updating a prop value
    # never touches the tracker/services/history placeholders next to them.
    output$sample_props <- shiny$renderUI({
      s <- summ()
      if (nrow(s) == 0) return(NULL)
      s <- s[1, ]
      shiny$tagList(
        shiny$div(
          class = "prop-grid",
          order_theme$prop("Crop", s$crop_name),
          order_theme$prop("Variety", s$variety_name),
          order_theme$prop("Sample type", s$sample_type),
          order_theme$prop("Condition", s$sample_condition),
          order_theme$prop("Part submitted", s$part_name),
          order_theme$prop("Sampling bag", s$bag_name),
          order_theme$prop("Origin", s$origin_country),
          order_theme$prop("Samples", s$sample_amount),
          order_theme$prop("Lot / ref", s$ref_no),
          order_theme$prop("Sampled by", s$sampler),
          order_theme$prop("Date sampled", fmt_date(s$date_sampled)),
          order_theme$prop("Date received", fmt_date(s$date_received))
        ),
        if (!is.na(s$sample_description) && nzchar(s$sample_description)) {
          shiny$div(style = "margin-top:14px;",
                    order_theme$subhead("Description"),
                    shiny$p(s$sample_description))
        }
      )
    })
    
    output$billing_props <- shiny$renderUI({
      s <- summ()
      if (nrow(s) == 0) return(NULL)
      s <- s[1, ]
      shiny$div(
        class = "prop-grid",
        order_theme$prop("Customer", s$customer_name),
        order_theme$prop("Email", s$customer_email),
        order_theme$prop("Report format", s$report_format),
        order_theme$prop("Dispatch", s$dispatch_method),
        order_theme$prop("Registered by", s$created_by),
        order_theme$prop("Registered on", fmt_date(s$created_on)),
        order_theme$prop("Approved by", s$approved_by),
        order_theme$prop("Approved on", fmt_date(s$approved_on)),
        order_theme$prop("Payment", if (isTRUE(s$payment_made)) "Paid" else "Not paid"),
        order_theme$prop("Amount", if (!is.na(s$amount_charged)) format(s$amount_charged, big.mark = ",") else NA),
        order_theme$prop("Receipt", s$receipt_no)
      )
    })
    
    # ================================================================
    # APPROVAL
    # ----------------------------------------------------------------
    # approval_state is the REGISTRATION lifecycle: pending -> approved |
    # rejected | cancelled. It is NOT pipeline progress - that is derived
    # in view_order_progress from the service lines. Two different
    # questions, two different columns, and conflating them is what made
    # the old tbl_project_status unusable.
    #
    # Nothing set this column until now: every order registered since the
    # rebuild has sat at 'pending', which is why quarantine's "awaiting
    # bench" queue (approved AND no bench row) was always empty.
    #
    # One transaction writes three things:
    #   tbl_order        the decision itself
    #   tbl_review       the reviewer's record, via the exclusive arc
    #   tbl_order_event  the append-only trail
    # ================================================================
    output$approval_actions <- shiny$renderUI({
      s <- summ()
      shiny$req(nrow(s) > 0)
      st <- s$approval_state[1]
      
      if (identical(st, "pending")) {
        return(shiny$tagList(
          shiny$actionButton(ns("reject"), "Reject", icon = shiny$icon("xmark"),
                             class = "btn btn-outline-secondary"),
          shiny$actionButton(ns("approve"), "Approve", icon = shiny$icon("check"),
                             class = "btn btn-primary")
        ))
      }
      
      # Already decided: say who and when, and offer the reversal only.
      shiny$div(
        style = "text-align:right;",
        shiny$div(class = "text-muted", style = "font-size:11.5px; margin-bottom:6px;",
                  paste(tools::toTitleCase(st),
                        if (!is.na(s$approved_by[1])) paste("by", s$approved_by[1]) else "",
                        if (!is.na(s$approved_on[1])) paste("on", fmt_date(s$approved_on[1])) else "")),
        if (identical(st, "rejected")) {
          shiny$actionButton(ns("approve"), "Approve after all",
                             icon = shiny$icon("rotate-left"),
                             class = "btn btn-outline-secondary btn-sm")
        }
      )
    })
    
    # What the workflow recommends for THIS order, from reception/approved.
    # order_context() reads crop and sample_type, which is exactly what the
    # choice node branches on.
    #
    # This used to be a plain function, recomputed from scratch - workflow
    # load plus two DB reads - every time it was called: once for the modal
    # picker, once again inside the write transaction, and again for
    # dest_hint. As a reactive, all of those share one cached result for as
    # long as the order (code()) hasn't changed, so a single approve action
    # does the lookup once instead of three times.
    approval_reco <- shiny$reactive({
      out <- tryCatch({
        wf <- workflow_cache(WF_PATH, pool)
        ctx <- order_context(pool, code())
        o <- next_options(wf, "reception", "approved", ctx)
        if (is.null(o) || nrow(o) == 0) NA_character_ else o$to_stage[1]
      }, error = function(e) NA_character_)
      if (is.na(out) || !(out %in% names(DEST_LABEL))) names(DEST_LABEL)[1] else out
    })
    
    output$dest_hint <- shiny$renderUI({
      rec <- approval_reco()
      s <- summ()
      order_theme$guide(tone = "do",
                        "Determined by the workflow from ",
                        shiny$strong(if (nrow(s) && !is.na(s$crop_name[1])) s$crop_name[1] else "the crop"),
                        " and sample type ",
                        shiny$strong(if (nrow(s) && !is.na(s$sample_type[1])) s$sample_type[1] else "on file"),
                        ". If this is wrong the workflow is wrong \u2014 correct cassava.yaml ",
                        "rather than sending material somewhere the system does not expect.")
    })
    
    decide <- function(decision) {
      s <- summ(); shiny$req(nrow(s) > 0)
      user <- shiny$reactiveValuesToList(res_auth)$user
      cmt  <- input$decision_comment
      
      if (identical(decision, "rejected") && (is.null(cmt) || !nzchar(trimws(cmt)))) {
        toastr_error("A reason is required to reject an order.", title = "Missing reason")
        return(invisible(FALSE))
      }
      
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user, isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          
          dbExecute(conn, "
            UPDATE tbl_order
               SET approval_state = $1,
                   approved_by    = $2,
                   approved_on    = now(),
                   updated_on     = now()
             WHERE order_number = $3",
                    params = list(decision, user, code()))
          
          # The handover, in the SAME transaction as the approval. An order
          # approved without a destination is an order nobody is waiting for,
          # and it would sit at reception with no bench aware of it.
          if (identical(decision, "approved")) {
            # Always the workflow's route: the picker offers exactly one.
            dest <- approval_reco()
            dbExecute(conn, "
              INSERT INTO tbl_order_handover
                (order_number, to_stage, recommended, assign_reason, assigned_by)
              VALUES ($1,$2,$3,$4,$5)
              ON CONFLICT DO NOTHING",
                      params = list(code(), dest, TRUE,
                                    if (nzchar(cmt %||% "")) cmt else NA_character_, user))
          }
          
          # tbl_review's exclusive arc: exactly one of order_number /
          # sample_code / order_service_id may be set.
          dbExecute(conn, "
            INSERT INTO tbl_review (order_number, decision, comments, reviewed_by)
            VALUES ($1, $2, $3, $4)",
                    params = list(code(),
                                  if (identical(decision, "approved")) "approved" else "rejected",
                                  if (!is.null(cmt) && nzchar(trimws(cmt))) trimws(cmt) else NA,
                                  user))
          
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'review', $2, $3, $4)",
                    params = list(code(), paste("order", decision), user,
                                  if (!is.null(cmt) && nzchar(trimws(cmt))) trimws(cmt) else NA))
        })
        TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Decision failed", timeOut = 0); FALSE
      })
      
      if (ok) {
        shiny$removeModal()
        toastr_success(paste(code(), decision))
        if (!is.null(trigger_refresh)) trigger_refresh(trigger_refresh() + 1)
      }
      invisible(ok)
    }
    
    ask <- function(decision) {
      s <- summ(); shiny$req(nrow(s) > 0)
      approving <- identical(decision, "approved")
      shiny$showModal(shiny$modalDialog(
        title = paste(if (approving) "Approve" else "Reject", code()),
        shiny$div(
          class = "rtb-intake", order_theme$head_orders(),
          if (approving) {
            shiny$div(class = "update-hint",
                      "Approving releases this order to the lab: it becomes visible in ",
                      "the quarantine queue, and the workflow will recommend a bench ",
                      "based on its sample type.")
          } else {
            shiny$div(class = "update-hint",
                      "Rejecting keeps the record and its history. Nothing is deleted \u2014 ",
                      "the order simply does not enter the pipeline.")
          },
          shiny$textAreaInput(ns("decision_comment"),
                              shiny$HTML(if (approving) "Comments" else
                                "Reason <span class='mandatory_star'>*</span>"),
                              width = "100%", height = "90px")
        ),
        if (approving) local({
          # ONE destination, and it is the workflow's. The workflow branches on
          # crop and sample type, and that IS the routing rule - offering
          # alternatives invites a choice the lab has already made.
          #
          # Still a select rather than plain text, so the value is submitted
          # with the form and reads as a decision being recorded.
          rec <- approval_reco()
          shiny$div(
            order_theme$subhead("This consignment will go to"),
            shiny$selectizeInput(ns("dest_stage"), NULL,
                                 choices = setNames(rec, unname(DEST_LABEL[rec])),
                                 selected = rec, width = "100%"),
            shiny$uiOutput(ns("dest_hint")),
            shiny$div(class = "wl-meta-note",
                      "That bench sees this order in its inbound list and requests ",
                      "pickup from reception. Nothing moves until the handover is signed.")
          )
        }) else NULL,
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("confirm_decision"),
                             if (approving) "Approve" else "Reject",
                             class = if (approving) "btn btn-primary" else "btn btn-danger")
        ),
        easyClose = FALSE
      ))
      pending_decision(decision)
    }
    
    pending_decision <- shiny$reactiveVal(NULL)
    shiny$observeEvent(input$approve, { ask("approved") })
    shiny$observeEvent(input$reject,  { ask("rejected") })
    shiny$observeEvent(input$confirm_decision, {
      shiny$req(pending_decision())
      decide(pending_decision())
    })
    
    # ---- tracker -----------------------------------------------------
    output$tracker <- shiny$renderUI({
      if (!is.null(trigger_refresh)) trigger_refresh()
      st <- tracker_steps(code())
      if (nrow(st) == 0) {
        return(shiny$div(class = "update-hint", "No pipeline stages configured."))
      }
      shiny$div(
        class = "tracker",
        lapply(seq_len(nrow(st)), function(i) {
          order_theme$tracker_step(
            label  = st$label[i],
            detail = st$detail[i],
            state  = st$state[i]
          )
        })
      )
    })
    
    output$next_hint <- shiny$renderUI({
      if (!is.null(trigger_refresh)) trigger_refresh()
      nx  <- next_steps(code())
      cur <- current_stages(code())
      
      if (nrow(cur) == 0) {
        return(shiny$div(class = "update-hint",
                         "This order has no samples yet. Samples are created at initiation, ",
                         "after the consignment is received into quarantine."))
      }
      if (nrow(nx) == 0) {
        return(shiny$div(class = "update-hint",
                         "The workflow has no further recommendation from here. That does not ",
                         "mean the order is stuck \u2014 any legal transition is still available ",
                         "in the stage modules."))
      }
      
      fan <- any(nx$kind == "fan_out")
      shiny$div(
        class = "update-hint",
        shiny$strong(if (fan) "Recommended next (all apply): " else "Recommended next: "),
        paste(unique(stats::na.omit(nx$label)), collapse = if (fan) "  +  " else "  |  "),
        shiny$br(),
        shiny$span(style = "opacity:.75;",
                   "These are suggestions. Any legal transition is permitted \u2014 an ",
                   "off-workflow move simply records a reason.")
      )
    })
    
    # ---- services ----------------------------------------------------
    output$services <- renderReactable({
      if (!is.null(trigger_refresh)) trigger_refresh()
      d <- order_services(code())
      shiny$req(d)
      reactable(
        d,
        columns = order_theme$rt_cols(list(
          order_service_id = colDef(show = FALSE),
          service_kind     = colDef(show = FALSE),
          requested_on     = colDef(show = FALSE),
          remaining_qty    = colDef(show = FALSE),
          service_label = colDef(name = "SERVICE", minWidth = 160),
          purpose  = colDef(name = "PURPOSE", width = 110,
                            cell = function(v) if (is.na(v)) "\u2014" else v),
          origin   = colDef(name = "ORIGIN", width = 130, cell = function(v) {
            if (identical(v, "lab_initiated")) order_theme$chip("Lab", "amber")
            else order_theme$chip("Customer", "ink")
          }),
          recipient = colDef(name = "RECIPIENT", width = 130,
                             cell = function(v) if (is.na(v)) "\u2014" else v),
          target_qty    = colDef(name = "TARGET", width = 80),
          fulfilled_qty = colDef(name = "DONE", width = 70),
          unit          = colDef(name = "UNIT", width = 80),
          pct_complete  = colDef(name = "", width = 120, cell = function(v) {
            order_theme$mini_bar(v)
          }),
          status = colDef(name = "STATUS", width = 110, cell = function(v) {
            order_theme$chip(toupper(gsub("_", " ", v)),
                             switch(v, fulfilled = "brand", in_progress = "teal",
                                    cancelled = "ink", "amber"))
          })
        ), d),
        defaultPageSize = 8, compact = TRUE, highlight = TRUE,
        language = reactableLang(noData = "No services on this order."),
        theme = order_theme$rt_theme()
      )
    })
    
    # ---- history -----------------------------------------------------
    output$history <- renderReactable({
      if (!is.null(trigger_refresh)) trigger_refresh()
      d <- order_history(code())
      shiny$req(d)
      reactable(
        d,
        columns = order_theme$rt_cols(list(
          occurred_on = colDef(name = "WHEN", width = 150,
                               cell = function(v) format(as.POSIXct(v), "%d %b %Y %H:%M")),
          actor       = colDef(name = "WHO", width = 110),
          action      = colDef(name = "WHAT", minWidth = 190),
          module      = colDef(name = "WHERE", width = 110),
          sample_code = colDef(name = "SAMPLE", width = 110,
                               cell = function(v) if (is.na(v)) "\u2014" else v),
          notes       = colDef(name = "NOTES", minWidth = 140,
                               cell = function(v) if (is.na(v)) "" else v),
          is_override = colDef(show = FALSE),
          override_reason = colDef(name = "OVERRIDE", minWidth = 160, cell = function(v, i) {
            if (is.na(v) || !nzchar(v)) return("")
            shiny$span(order_theme$chip("OFF-WORKFLOW", "amber"), " ", v)
          })
        ), d),
        defaultPageSize = 10, compact = TRUE, highlight = TRUE,
        language = reactableLang(noData = "Nothing has happened to this order yet."),
        theme = order_theme$rt_theme()
      )
    })
    
    invisible(NULL)
  })
}

fmt_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NA_character_)
  format(as.Date(x[1]), "%d %b %Y")
}
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

#' box::use(
#'   shiny,
#'   reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
#'   shinyjs[useShinyjs],
#'   shinytoastr[toastr_success, toastr_error],
#'   pool[poolWithTransaction],
#'   DBI[dbExecute],
#'   stats[setNames],
#' )
#' 
#' box::use(
#'   app/logic/fct_conn[pool],
#'   app/logic/fct_tracking[order_summary, order_services, order_history,
#'                          tracker_steps, next_steps, current_stages],
#'   app/logic/fct_workflows[workflow_cache, next_options, order_context],
#'   app/view/shared/order_theme,
#' )
#' 
#' # Same path every module uses. Named here rather than passed in, because the
#' # workflow file is a property of the deployment, not of this screen.
#' WF_PATH <- file.path("app", "static", "workflows", "cassava.yaml")
#' 
#' # ============================================================================
#' # VIEW ORDER · the project view
#' # ----------------------------------------------------------------------------
#' # Answers the four questions in one page:
#' #   what is this order   -> order_summary()
#' #   where has it been    -> tracker_steps(), state "done"    (event log)
#' #   where is it now      -> tracker_steps(), state "current" (view_sample_current)
#' #   what happens next    -> tracker_steps(), state "next"    (workflow: ADVISORY)
#' #
#' # The next steps are labelled "recommended", deliberately and everywhere. The
#' # database accepts any legal (stage, state) - see allowed_transitions() - and
#' # an off-workflow move only has to carry a reason. Presenting the workflow's
#' # suggestion as the only option would misdescribe the system and push the real
#' # decision onto paper, which is the failure this whole design exists to avoid.
#' #
#' # An order can be in SEVERAL stages at once. That is not an error state: it is
#' # what the subculture fan-out produces when one order feeds conservation and
#' # distribution simultaneously. The tracker shows every current position.
#' #
#' # THIS IS THE DETAIL ONLY. order_management.R owns the list and hands us an
#' # order_number; we render that one order. Splitting them this way means the
#' # detail can also be reached from anywhere else (a search hit, a deep link)
#' # without dragging a list along with it.
#' # ============================================================================
#' 
#' #' @export
#' ui <- function(id) {
#'   ns <- shiny$NS(id)
#'   order_theme$page(
#'     useShinyjs(),
#'     shiny$uiOutput(ns("screen"))
#'   )
#' }
#' 
#' 
#' #' @export
#' server <- function(id, res_auth, page, tab, order_number, trigger_refresh = NULL,
#'                    on_back = NULL) {
#'   shiny$moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'     
#'     code <- shiny$reactive({
#'       x <- if (is.function(order_number)) order_number() else order_number
#'       shiny$req(x, nzchar(x))
#'       x
#'     })
#'     
#'     summ <- shiny$reactive({
#'       if (!is.null(trigger_refresh)) trigger_refresh()
#'       order_summary(code())
#'     })
#'     
#'     output$screen <- shiny$renderUI({ detail_screen(ns) })
#'     
#'     # The list lives in order_management; this just tells it we are done.
#'     shiny$observeEvent(input$back, {
#'       if (is.function(on_back)) on_back()
#'     })
#'     
#'     # ---- header ------------------------------------------------------
#'     output$detail_header <- shiny$renderUI({
#'       s <- summ()
#'       if (nrow(s) == 0) {
#'         return(shiny$div(class = "empty-state",
#'                          shiny$h3("Order not found"),
#'                          shiny$p(paste("No order with number", code()))))
#'       }
#'       s <- s[1, ]
#'       status <- if (is.na(s$derived_status)) "unknown" else s$derived_status
#'       tone <- switch(status,
#'                      completed = "brand", in_progress = "teal", pending_approval = "amber",
#'                      rejected = "red", cancelled = "ink", "ink")
#'       
#'       shiny$div(
#'         class = "intake-header",
#'         shiny$div(
#'           shiny$tags$h1(s$order_number),
#'           shiny$tags$p(paste(s$customer_name,
#'                              if (!is.na(s$crop_name)) paste("\u00b7", s$crop_name) else "")),
#'           shiny$div(
#'             style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap; margin-top:8px;",
#'             order_theme$chip(toupper(gsub("_", " ", status)), tone),
#'             if (!is.na(s$pct_complete)) order_theme$chip(paste0(s$pct_complete, "% complete"), "teal"),
#'             if (!is.na(s$order_kind) && s$order_kind != "primary") {
#'               order_theme$chip(toupper(s$order_kind), "amber")
#'             },
#'             if (!is.na(s$parent_order_number)) {
#'               shiny$span(class = "text-muted", style = "font-size:12px;",
#'                          paste("continues", s$parent_order_number))
#'             }
#'           )
#'         ),
#'         # Approval lives HERE, on the record being judged - not on a separate
#'         # screen. A reviewer decides with the tracker, the services and the
#'         # sample details in front of them.
#'         shiny$div(class = "actions", shiny$uiOutput(ns("approval_actions")))
#'       )
#'     })
#'     
#'     # ---- body --------------------------------------------------------
#'     output$detail_body <- shiny$renderUI({
#'       s <- summ()
#'       shiny$req(nrow(s) > 0)
#'       s <- s[1, ]
#'       
#'       shiny$tagList(
#'         order_theme$section(
#'           "1", "Progress", accent = "teal",
#'           sub = "Where this order has been, where it is, and what is recommended next",
#'           shiny$uiOutput(ns("tracker")),
#'           shiny$uiOutput(ns("next_hint"))
#'         ),
#'         order_theme$section(
#'           "2", "Services requested", accent = "teal",
#'           sub = "One line per requested service \u00b7 an order completes when all are fulfilled",
#'           reactableOutput(ns("services"))
#'         ),
#'         order_theme$section(
#'           "3", "Sample details",
#'           shiny$div(
#'             class = "prop-grid",
#'             order_theme$prop("Crop", s$crop_name),
#'             order_theme$prop("Variety", s$variety_name),
#'             order_theme$prop("Sample type", s$sample_type),
#'             order_theme$prop("Condition", s$sample_condition),
#'             order_theme$prop("Part submitted", s$part_name),
#'             order_theme$prop("Sampling bag", s$bag_name),
#'             order_theme$prop("Origin", s$origin_country),
#'             order_theme$prop("Samples", s$sample_amount),
#'             order_theme$prop("Lot / ref", s$ref_no),
#'             order_theme$prop("Sampled by", s$sampler),
#'             order_theme$prop("Date sampled", fmt_date(s$date_sampled)),
#'             order_theme$prop("Date received", fmt_date(s$date_received))
#'           ),
#'           if (!is.na(s$sample_description) && nzchar(s$sample_description)) {
#'             shiny$div(style = "margin-top:14px;",
#'                       order_theme$subhead("Description"),
#'                       shiny$p(s$sample_description))
#'           }
#'         ),
#'         order_theme$section(
#'           "4", "Order & billing", accent = "amber",
#'           shiny$div(
#'             class = "prop-grid",
#'             order_theme$prop("Customer", s$customer_name),
#'             order_theme$prop("Email", s$customer_email),
#'             order_theme$prop("Report format", s$report_format),
#'             order_theme$prop("Dispatch", s$dispatch_method),
#'             order_theme$prop("Registered by", s$created_by),
#'             order_theme$prop("Registered on", fmt_date(s$created_on)),
#'             order_theme$prop("Approved by", s$approved_by),
#'             order_theme$prop("Approved on", fmt_date(s$approved_on)),
#'             order_theme$prop("Payment", if (isTRUE(s$payment_made)) "Paid" else "Not paid"),
#'             order_theme$prop("Amount", if (!is.na(s$amount_charged)) format(s$amount_charged, big.mark = ",") else NA),
#'             order_theme$prop("Receipt", s$receipt_no)
#'           )
#'         ),
#'         order_theme$section(
#'           "5", "History", accent = "teal",
#'           sub = "Append-only \u00b7 every event, newest first",
#'           reactableOutput(ns("history"))
#'         )
#'       )
#'     })
#'     
#'     # ================================================================
#'     # APPROVAL
#'     # ----------------------------------------------------------------
#'     # approval_state is the REGISTRATION lifecycle: pending -> approved |
#'     # rejected | cancelled. It is NOT pipeline progress - that is derived
#'     # in view_order_progress from the service lines. Two different
#'     # questions, two different columns, and conflating them is what made
#'     # the old tbl_project_status unusable.
#'     #
#'     # Nothing set this column until now: every order registered since the
#'     # rebuild has sat at 'pending', which is why quarantine's "awaiting
#'     # bench" queue (approved AND no bench row) was always empty.
#'     #
#'     # One transaction writes three things:
#'     #   tbl_order        the decision itself
#'     #   tbl_review       the reviewer's record, via the exclusive arc
#'     #   tbl_order_event  the append-only trail
#'     # ================================================================
#'     output$approval_actions <- shiny$renderUI({
#'       s <- summ()
#'       shiny$req(nrow(s) > 0)
#'       st <- s$approval_state[1]
#'       
#'       if (identical(st, "pending")) {
#'         return(shiny$tagList(
#'           shiny$actionButton(ns("reject"), "Reject", icon = shiny$icon("xmark"),
#'                              class = "btn btn-outline-secondary"),
#'           shiny$actionButton(ns("approve"), "Approve", icon = shiny$icon("check"),
#'                              class = "btn btn-primary")
#'         ))
#'       }
#'       
#'       # Already decided: say who and when, and offer the reversal only.
#'       shiny$div(
#'         style = "text-align:right;",
#'         shiny$div(class = "text-muted", style = "font-size:11.5px; margin-bottom:6px;",
#'                   paste(tools::toTitleCase(st),
#'                         if (!is.na(s$approved_by[1])) paste("by", s$approved_by[1]) else "",
#'                         if (!is.na(s$approved_on[1])) paste("on", fmt_date(s$approved_on[1])) else "")),
#'         if (identical(st, "rejected")) {
#'           shiny$actionButton(ns("approve"), "Approve after all",
#'                              icon = shiny$icon("rotate-left"),
#'                              class = "btn btn-outline-secondary btn-sm")
#'         }
#'       )
#'     })
#'     
#'     DEST_LABEL <- c(quarantine_glasshouse = "Quarantine \u00b7 glasshouse",
#'                     quarantine_growthroom = "Quarantine \u00b7 growthroom",
#'                     thermotherapy         = "Thermotherapy")
#'     
#'     # What the workflow recommends for THIS order, from reception/approved.
#'     # order_context() reads crop and sample_type, which is exactly what the
#'     # choice node branches on.
#'     approval_reco <- function() {
#'       out <- tryCatch({
#'         wf <- workflow_cache(WF_PATH, pool)
#'         ctx <- order_context(pool, code())
#'         o <- next_options(wf, "reception", "approved", ctx)
#'         if (is.null(o) || nrow(o) == 0) NA_character_ else o$to_stage[1]
#'       }, error = function(e) NA_character_)
#'       if (is.na(out) || !(out %in% names(DEST_LABEL))) names(DEST_LABEL)[1] else out
#'     }
#'     
#'     output$dest_hint <- shiny$renderUI({
#'       rec <- approval_reco()
#'       s <- summ()
#'       order_theme$guide(tone = "do",
#'                         "Determined by the workflow from ",
#'                         shiny$strong(if (nrow(s) && !is.na(s$crop_name[1])) s$crop_name[1] else "the crop"),
#'                         " and sample type ",
#'                         shiny$strong(if (nrow(s) && !is.na(s$sample_type[1])) s$sample_type[1] else "on file"),
#'                         ". If this is wrong the workflow is wrong \u2014 correct cassava.yaml ",
#'                         "rather than sending material somewhere the system does not expect.")
#'     })
#'     
#'     decide <- function(decision) {
#'       s <- summ(); shiny$req(nrow(s) > 0)
#'       user <- shiny$reactiveValuesToList(res_auth)$user
#'       cmt  <- input$decision_comment
#'       
#'       if (identical(decision, "rejected") && (is.null(cmt) || !nzchar(trimws(cmt)))) {
#'         toastr_error("A reason is required to reject an order.", title = "Missing reason")
#'         return(invisible(FALSE))
#'       }
#'       
#'       ok <- tryCatch({
#'         poolWithTransaction(pool, function(conn) {
#'           dbExecute(conn, "SELECT ensure_app_user($1, $2)",
#'                     params = list(user, isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
#'           
#'           dbExecute(conn, "
#'             UPDATE tbl_order
#'                SET approval_state = $1,
#'                    approved_by    = $2,
#'                    approved_on    = now(),
#'                    updated_on     = now()
#'              WHERE order_number = $3",
#'                     params = list(decision, user, code()))
#'           
#'           # The handover, in the SAME transaction as the approval. An order
#'           # approved without a destination is an order nobody is waiting for,
#'           # and it would sit at reception with no bench aware of it.
#'           if (identical(decision, "approved")) {
#'             # Always the workflow's route: the picker offers exactly one.
#'             dest <- approval_reco()
#'             dbExecute(conn, "
#'               INSERT INTO tbl_order_handover
#'                 (order_number, to_stage, recommended, assign_reason, assigned_by)
#'               VALUES ($1,$2,$3,$4,$5)
#'               ON CONFLICT DO NOTHING",
#'                       params = list(code(), dest, TRUE,
#'                                     if (nzchar(cmt %||% "")) cmt else NA_character_, user))
#'           }
#'           
#'           # tbl_review's exclusive arc: exactly one of order_number /
#'           # sample_code / order_service_id may be set.
#'           dbExecute(conn, "
#'             INSERT INTO tbl_review (order_number, decision, comments, reviewed_by)
#'             VALUES ($1, $2, $3, $4)",
#'                     params = list(code(),
#'                                   if (identical(decision, "approved")) "approved" else "rejected",
#'                                   if (!is.null(cmt) && nzchar(trimws(cmt))) trimws(cmt) else NA,
#'                                   user))
#'           
#'           dbExecute(conn, "
#'             INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
#'             VALUES ($1, 'review', $2, $3, $4)",
#'                     params = list(code(), paste("order", decision), user,
#'                                   if (!is.null(cmt) && nzchar(trimws(cmt))) trimws(cmt) else NA))
#'         })
#'         TRUE
#'       }, error = function(e) {
#'         toastr_error(conditionMessage(e), title = "Decision failed", timeOut = 0); FALSE
#'       })
#'       
#'       if (ok) {
#'         shiny$removeModal()
#'         toastr_success(paste(code(), decision))
#'         if (!is.null(trigger_refresh)) trigger_refresh(trigger_refresh() + 1)
#'       }
#'       invisible(ok)
#'     }
#'     
#'     ask <- function(decision) {
#'       s <- summ(); shiny$req(nrow(s) > 0)
#'       approving <- identical(decision, "approved")
#'       shiny$showModal(shiny$modalDialog(
#'         title = paste(if (approving) "Approve" else "Reject", code()),
#'         shiny$div(
#'           class = "rtb-intake", order_theme$head_orders(),
#'           if (approving) {
#'             shiny$div(class = "update-hint",
#'                       "Approving releases this order to the lab: it becomes visible in ",
#'                       "the quarantine queue, and the workflow will recommend a bench ",
#'                       "based on its sample type.")
#'           } else {
#'             shiny$div(class = "update-hint",
#'                       "Rejecting keeps the record and its history. Nothing is deleted \u2014 ",
#'                       "the order simply does not enter the pipeline.")
#'           },
#'           shiny$textAreaInput(ns("decision_comment"),
#'                               shiny$HTML(if (approving) "Comments" else
#'                                 "Reason <span class='mandatory_star'>*</span>"),
#'                               width = "100%", height = "90px")
#'         ),
#'         if (approving) local({
#'           # ONE destination, and it is the workflow's. The workflow branches on
#'           # crop and sample type, and that IS the routing rule - offering
#'           # alternatives invites a choice the lab has already made.
#'           #
#'           # Still a select rather than plain text, so the value is submitted
#'           # with the form and reads as a decision being recorded.
#'           rec <- approval_reco()
#'           shiny$div(
#'             order_theme$subhead("This consignment will go to"),
#'             shiny$selectizeInput(ns("dest_stage"), NULL,
#'                                  choices = setNames(rec, unname(DEST_LABEL[rec])),
#'                                  selected = rec, width = "100%"),
#'             shiny$uiOutput(ns("dest_hint")),
#'             shiny$div(class = "wl-meta-note",
#'                       "That bench sees this order in its inbound list and requests ",
#'                       "pickup from reception. Nothing moves until the handover is signed.")
#'           )
#'         }) else NULL,
#'         footer = shiny$tagList(
#'           shiny$modalButton("Cancel"),
#'           shiny$actionButton(ns("confirm_decision"),
#'                              if (approving) "Approve" else "Reject",
#'                              class = if (approving) "btn btn-primary" else "btn btn-danger")
#'         ),
#'         easyClose = FALSE
#'       ))
#'       pending_decision(decision)
#'     }
#'     
#'     pending_decision <- shiny$reactiveVal(NULL)
#'     shiny$observeEvent(input$approve, { ask("approved") })
#'     shiny$observeEvent(input$reject,  { ask("rejected") })
#'     shiny$observeEvent(input$confirm_decision, {
#'       shiny$req(pending_decision())
#'       decide(pending_decision())
#'     })
#'     
#'     # ---- tracker -----------------------------------------------------
#'     output$tracker <- shiny$renderUI({
#'       if (!is.null(trigger_refresh)) trigger_refresh()
#'       st <- tracker_steps(code())
#'       if (nrow(st) == 0) {
#'         return(shiny$div(class = "update-hint", "No pipeline stages configured."))
#'       }
#'       shiny$div(
#'         class = "tracker",
#'         lapply(seq_len(nrow(st)), function(i) {
#'           order_theme$tracker_step(
#'             label  = st$label[i],
#'             detail = st$detail[i],
#'             state  = st$state[i]
#'           )
#'         })
#'       )
#'     })
#'     
#'     output$next_hint <- shiny$renderUI({
#'       if (!is.null(trigger_refresh)) trigger_refresh()
#'       nx  <- next_steps(code())
#'       cur <- current_stages(code())
#'       
#'       if (nrow(cur) == 0) {
#'         return(shiny$div(class = "update-hint",
#'                          "This order has no samples yet. Samples are created at initiation, ",
#'                          "after the consignment is received into quarantine."))
#'       }
#'       if (nrow(nx) == 0) {
#'         return(shiny$div(class = "update-hint",
#'                          "The workflow has no further recommendation from here. That does not ",
#'                          "mean the order is stuck \u2014 any legal transition is still available ",
#'                          "in the stage modules."))
#'       }
#'       
#'       fan <- any(nx$kind == "fan_out")
#'       shiny$div(
#'         class = "update-hint",
#'         shiny$strong(if (fan) "Recommended next (all apply): " else "Recommended next: "),
#'         paste(unique(stats::na.omit(nx$label)), collapse = if (fan) "  +  " else "  |  "),
#'         shiny$br(),
#'         shiny$span(style = "opacity:.75;",
#'                    "These are suggestions. Any legal transition is permitted \u2014 an ",
#'                    "off-workflow move simply records a reason.")
#'       )
#'     })
#'     
#'     # ---- services ----------------------------------------------------
#'     output$services <- renderReactable({
#'       if (!is.null(trigger_refresh)) trigger_refresh()
#'       d <- order_services(code())
#'       d$`__bar` <- rep(NA, nrow(d))
#'       shiny$req(d)
#'       reactable(
#'         d,
#'         columns = order_theme$rt_cols(list(
#'           order_service_id = colDef(show = FALSE),
#'           service_kind     = colDef(show = FALSE),
#'           requested_on     = colDef(show = FALSE),
#'           remaining_qty    = colDef(show = FALSE),
#'           service_label = colDef(name = "SERVICE", minWidth = 160),
#'           purpose  = colDef(name = "PURPOSE", width = 110,
#'                             cell = function(v) if (is.na(v)) "\u2014" else v),
#'           origin   = colDef(name = "ORIGIN", width = 130, cell = function(v) {
#'             if (identical(v, "lab_initiated")) order_theme$chip("Lab", "amber")
#'             else order_theme$chip("Customer", "ink")
#'           }),
#'           recipient = colDef(name = "RECIPIENT", width = 130,
#'                              cell = function(v) if (is.na(v)) "\u2014" else v),
#'           target_qty    = colDef(name = "TARGET", width = 80),
#'           fulfilled_qty = colDef(name = "DONE", width = 70),
#'           unit          = colDef(name = "UNIT", width = 80),
#'           pct_complete  = colDef(name = "", width = 120, cell = function(v) {
#'             order_theme$mini_bar(v)
#'           }),
#'           status = colDef(name = "STATUS", width = 110, cell = function(v) {
#'             order_theme$chip(toupper(gsub("_", " ", v)),
#'                              switch(v, fulfilled = "brand", in_progress = "teal",
#'                                     cancelled = "ink", "amber"))
#'           }),
#'           `__bar` = colDef(show = FALSE)
#'         ), d),
#'         defaultPageSize = 8, compact = TRUE, highlight = TRUE,
#'         language = reactableLang(noData = "No services on this order."),
#'         theme = order_theme$rt_theme()
#'       )
#'     })
#'     
#'     # ---- history -----------------------------------------------------
#'     output$history <- renderReactable({
#'       if (!is.null(trigger_refresh)) trigger_refresh()
#'       d <- order_history(code())
#'       shiny$req(d)
#'       reactable(
#'         d,
#'         columns = order_theme$rt_cols(list(
#'           occurred_on = colDef(name = "WHEN", width = 150,
#'                                cell = function(v) format(as.POSIXct(v), "%d %b %Y %H:%M")),
#'           actor       = colDef(name = "WHO", width = 110),
#'           action      = colDef(name = "WHAT", minWidth = 190),
#'           module      = colDef(name = "WHERE", width = 110),
#'           sample_code = colDef(name = "SAMPLE", width = 110,
#'                                cell = function(v) if (is.na(v)) "\u2014" else v),
#'           notes       = colDef(name = "NOTES", minWidth = 140,
#'                                cell = function(v) if (is.na(v)) "" else v),
#'           is_override = colDef(show = FALSE),
#'           override_reason = colDef(name = "OVERRIDE", minWidth = 160, cell = function(v, i) {
#'             if (is.na(v) || !nzchar(v)) return("")
#'             shiny$span(order_theme$chip("OFF-WORKFLOW", "amber"), " ", v)
#'           })
#'         ), d),
#'         defaultPageSize = 10, compact = TRUE, highlight = TRUE,
#'         language = reactableLang(noData = "Nothing has happened to this order yet."),
#'         theme = order_theme$rt_theme()
#'       )
#'     })
#'     
#'     invisible(NULL)
#'   })
#' }
#' 
#' # ---------------------------------------------------------------------------
#' # SCREENS
#' # ---------------------------------------------------------------------------
#' 
#' detail_screen <- function(ns) {
#'   shiny$tagList(
#'     shiny$actionButton(ns("back"), "Back to all orders", icon = shiny$icon("arrow-left"),
#'                        class = "btn btn-outline-secondary btn-sm",
#'                        class = "wl-statusbar"),
#'     shiny$uiOutput(ns("detail_header")),
#'     shiny$uiOutput(ns("detail_body"))
#'   )
#' }
#' 
#' fmt_date <- function(x) {
#'   if (is.null(x) || length(x) == 0 || is.na(x[1])) return(NA_character_)
#'   format(as.Date(x[1]), "%d %b %Y")
#' }
#' `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a