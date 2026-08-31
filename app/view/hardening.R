box::use(
  shiny,
  reactable[reactable, reactableOutput, renderReactable, colDef],
  shinyjs[useShinyjs, runjs],
  shinytoastr[toastr_success, toastr_error, toastr_warning],
  htmlwidgets[JS],
  pool[poolWithTransaction],
  DBI[dbExecute, dbGetQuery],
  stats[setNames],
)

box::use(
  app/logic/fct_conn[pool],
  app/logic/fct_tracking[hardening_stock, hardening_queue, hardening_demand,
                         pending_requests, awaited_deliveries],
  app/logic/fct_workflows[workflow_cache, next_options, sample_context, record_event],
  app/view/shared/label_print,
  app/view/shared/order_theme,
)

# ============================================================================
# HARDENING / ACCLIMATIZATION · clinical worklist (worklist + detail panel)
# ----------------------------------------------------------------------------
# The bench where plantlets leave sterile culture for substrate, and the lab
# finds out how many survive contact with the real world.
#
# WHAT MAKES THIS BENCH DIFFERENT
#   Attrition is the headline number, not a footnote. Every other stage loses
#   the occasional explant to contamination; here a predictable fraction of
#   every batch dies, and the survival rate is what the lab reports and what
#   the next season's planning is built on. So survival is computed in the view
#   and shown on every row - against what was actually PLANTED, which is stored
#   rather than derived, because the ledger can be corrected and the number a
#   customer was told cannot.
#
# WHAT ARRIVES HERE
#   Completed subculture material ALLOCATED TO A SOIL-BOUND SERVICE - in vivo
#   conservation, mini-tubers, vines. Material grown for in vitro distribution
#   never appears, because it is never going into soil. That list comes from
#   the service catalogue, the same place the workflow's fan_out reads it.
#
# WHAT LEAVES
#   hardening/completed fans out to in vivo conservation and mini-tuber
#   distribution, whichever the order's services call for.
#
# TIME MATTERS HERE, uniquely
#   Every other bench is event-driven: something happens, somebody records it.
#   A batch in substrate is on a clock - it is weaned after so many days
#   whether or not anyone looked - so the queue carries days elapsed and flags
#   a batch whose weaning period has run out.
# ============================================================================

MY_TAB       <- "hardening"
WF_PATH      <- file.path("app", "static", "workflows", "cassava.yaml")
CHILD_PREFIX <- "HD"

ENTRY_STATE <- c(in_vivo_conservation     = "established",
                 mini_tubers_distribution = "established")
DEST_TAB <- c(in_vivo_conservation     = "conservation",
              mini_tubers_distribution = "distribution")

# Why plants were lost. `dead` is the expected one here and is listed first;
# on other benches contamination leads.
LOSS_REASONS <- c("Died"        = "dead",
                  "Discarded"   = "discarded",
                  "Contaminated" = "contaminated")

state_tone <- function(s) switch(s %||% "",
                                 established = "ink", updated = "amber",
                                 dead = "amber", depleted = "ink",
                                 completed = "brand", rejected = "amber", "ink")

# Survival bands. A number alone does not say whether it is good, and this is
# the one figure on this screen somebody will act on at a glance.
survival_tone <- function(p) {
  if (is.na(p)) "ink" else if (p >= 80) "brand" else if (p >= 50) "teal" else "amber"
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    order_theme$page_header(
      title = "Hardening / Acclimatization",
      sub   = "Move cleared plantlets into substrate and record what survives."
    ),
    
    shiny$uiOutput(ns("kpis")),
    shiny$uiOutput(ns("awaiting")),
    shiny$uiOutput(ns("guide")),
    
    order_theme$toolbar(
      order_theme$search_box(ns("q"), "Search batch, order, screenhouse..."),
      order_theme$scan_box(ns("scan"), ns("scan_go"),
                           placeholder = "Scan a batch id...")
    ),
    
    order_theme$workbench(
      list_ui   = shiny$uiOutput(ns("list_pane")),
      detail_ui = shiny$uiOutput(ns("detail"))
    )
  )
}


#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    printer <- label_print$server("print", module_name = "hardening", user = user)
    
    fld <- function(r, name, default = NA) {
      if (is.null(r) || is.null(r[[name]]) || length(r[[name]]) == 0) return(default)
      v <- r[[name]][1]
      if (length(v) == 0) default else v
    }
    
    user     <- shiny$reactive(shiny$reactiveValuesToList(res_auth)$user)
    is_admin <- shiny$reactive(isTRUE(shiny$reactiveValuesToList(res_auth)$admin))
    
    # ---- scoped refresh ----------------------------------------------
    refresh <- shiny$reactiveVal(0)
    self_refresh <- function() refresh(shiny$isolate(refresh()) + 1)
    if (!is.null(tab) && is.function(tab)) {
      shiny$observeEvent(tab(), {
        if (identical(tab(), MY_TAB)) self_refresh()
      }, ignoreInit = TRUE)
    }
    signal_others <- function() {
      if (!is.null(trigger_refresh)) trigger_refresh(shiny$isolate(trigger_refresh()) + 1)
    }
    
    # ---- data ---------------------------------------------------------
    stock <- shiny$reactive({ refresh(); hardening_stock() })
    queue <- shiny$reactive({ refresh(); hardening_queue() })
    reqs  <- shiny$reactive({ refresh(); pending_requests("hardening") })
    # The mirror of reqs(): what THIS bench asked for and has not received in
    # full. Both read the same balances, so the bench that owes and the bench
    # that waits can never be shown different numbers about one request.
    awaited <- shiny$reactive({ refresh(); awaited_deliveries("hardening") })
    
    vstate <- shiny$reactive(input$vstate %||% "available")
    
    search_rows <- function(d, cols) {
      q <- input$q
      if (is.null(q) || !nzchar(q) || nrow(d) == 0) return(d)
      cols <- intersect(cols, names(d))
      if (length(cols) == 0) return(d)
      hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
      d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
    }
    
    stock_rows <- shiny$reactive({
      d <- stock()
      # The action column must EXIST before a colDef can name it; a zero-row
      # frame is the normal state on a fresh database, so rep() and not NA.
      if (nrow(d) > 0 || !is.null(d$row_id)) d$act <- rep(NA_character_, nrow(d))
      search_rows(d, c("order_number", "customer_name", "crop_name",
                       "variety_name", "services_owed"))
    })
    
    # These arms MUST match the counts in output$kpis, or a tab reads 7 and the
    # table shows something else.
    bench_rows <- shiny$reactive({
      d <- queue()
      if (nrow(d) > 0 || !is.null(d$sample_code)) d$act <- rep(NA_character_, nrow(d))
      if (nrow(d) > 0) {
        live <- d$state_code %in% c("established", "updated", "dead")
        d <- switch(vstate(),
                    substrate = d[live, , drop = FALSE],
                    weaning   = d[live & !is.na(d$weaning_due) & d$weaning_due, , drop = FALSE],
                    finished  = d[d$state_code %in% c("completed", "rejected", "depleted"), , drop = FALSE],
                    d)
      }
      search_rows(d, c("sample_code", "order_number", "customer_name",
                       "crop_name", "variety_name", "screenhouse", "substrate",
                       "service_label"))
    })
    
    n_available <- shiny$reactive({ nrow(stock()) })
    n_requests  <- shiny$reactive({ nrow(reqs()) })
    n_substrate <- shiny$reactive({
      d <- queue(); if (nrow(d) == 0) 0L else
        sum(d$state_code %in% c("established", "updated", "dead"))
    })
    n_weaning <- shiny$reactive({
      d <- queue()
      if (nrow(d) == 0) return(0L)
      sum(d$state_code %in% c("established", "updated", "dead") &
            !is.na(d$weaning_due) & d$weaning_due)
    })
    n_finished <- shiny$reactive({
      d <- queue(); if (nrow(d) == 0) 0L else
        sum(d$state_code %in% c("completed", "rejected", "depleted"))
    })
    
    # Survival across everything currently in substrate. One number, and the
    # one a supervisor walking past the screen actually wants.
    survival_now <- shiny$reactive({
      d <- queue()
      if (nrow(d) == 0) return(NA_integer_)
      live <- d$state_code %in% c("established", "updated", "dead") & !is.na(d$initial_count)
      if (!any(live)) return(NA_integer_)
      planted <- sum(as.integer(d$initial_count[live]), na.rm = TRUE)
      alive   <- sum(as.integer(d$units_held[live]), na.rm = TRUE)
      if (planted == 0) NA_integer_ else as.integer(floor(100 * alive / planted))
    })
    
    output$kpis <- shiny$renderUI({
      order_theme$flow_stepper(list(
        list(title = "Available", sub = "soil-bound material",
             count = n_available(), unit = "orders", value = "available",
             active = identical(vstate(), "available"),
             waiting = n_available() > 0),
        list(title = "Requests", sub = "other benches want plants",
             count = n_requests(), unit = "to handle", value = "requests",
             active = identical(vstate(), "requests"),
             waiting = n_requests() > 0),
        list(title = "In substrate", sub = if (is.na(survival_now())) "in the screenhouse"
             else sprintf("%d%% surviving", survival_now()),
             count = n_substrate(), unit = "batches", value = "substrate",
             active = identical(vstate(), "substrate")),
        list(title = "Weaning due", sub = "past their weaning period",
             count = n_weaning(), unit = "batches", value = "weaning",
             active = identical(vstate(), "weaning"),
             waiting = n_weaning() > 0),
        list(title = "Finished", sub = "completed or ended",
             count = n_finished(), unit = "batches", value = "finished",
             active = identical(vstate(), "finished"))
      ), input_id = ns("vstate"))
    })
    
    # A standing reminder of material this bench is still waiting for. It sits
    # above every tab rather than on one, because the question it answers -
    # has what I asked for arrived yet - is the reason somebody walks to
    # another room, and it should be answered before they get up.
    output$awaiting <- shiny$renderUI({
      d <- awaited()
      if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
      owed <- sum(as.integer(d$qty_outstanding), na.rm = TRUE)
      part <- sum(as.integer(d$qty_sent) > 0, na.rm = TRUE)
      shiny$div(
        class = "flow-cta warn",
        shiny$span(class = "fc-ico", shiny$icon("truck-ramp-box")),
        shiny$span(
          shiny$strong(sprintf("%d plants", owed)),
          if (nrow(d) == 1) " still to come on 1 request" else sprintf(" still to come on %d requests", nrow(d)),
          if (part > 0) sprintf(" (%d part-delivered)", part) else "",
          ". ",
          paste(sprintf("%s: %d of %d from %s",
                        d$order_number, as.integer(d$qty_sent),
                        as.integer(d$qty_requested),
                        ifelse(is.na(d$source_bench), "\u2014", d$source_bench)),
                collapse = " \u00b7 ")))
    })
    
    output$guide <- shiny$renderUI({
      switch(vstate(),
             available = if (n_available() > 0)
               order_theme$guide(tone = "do",
                                 shiny$strong(n_available()),
                                 if (n_available() == 1) " consignment has material promised to soil. "
                                 else " consignments have material promised to soil. ",
                                 "Only subculture material allocated to in vivo conservation, mini-tubers ",
                                 "or vines appears here \u2014 plantlets grown for in vitro distribution ",
                                 "are never going into substrate.")
             else
               order_theme$guide(
                 "Nothing is ready. Material appears here once subculture completes a ",
                 "batch and commits some of it to a service that needs a plant in soil."),
             requests = if (n_requests() > 0)
               order_theme$guide(tone = "do",
                                 "Authorise a request, then enter the batch you are actually sending.")
             else order_theme$guide("No bench has asked for plants from here."),
             substrate = if (n_substrate() > 0)
               order_theme$guide(tone = "do",
                                 "Record potting on a new batch, then log losses as they happen. ",
                                 if (!is.na(survival_now())) shiny$tagList(
                                   shiny$strong(sprintf("%d%%", survival_now())),
                                   " of everything planted is still alive.") else NULL)
             else order_theme$guide("Nothing is in substrate. Request material from the Available tab."),
             weaning = if (n_weaning() > 0)
               order_theme$guide(tone = "do",
                                 shiny$strong(n_weaning()),
                                 if (n_weaning() == 1) " batch has been in substrate longer than its weaning "
                                 else " batches have been in substrate longer than their weaning ",
                                 "period. Nothing happens to them automatically \u2014 check them, commit ",
                                 "what survived, and close them out.")
             else order_theme$guide(
               "No batch is past its weaning period. Batches with no weaning target set ",
               "are not counted here either way."),
             finished = order_theme$guide(
               "Batches that have left the screenhouse, and batches that died in it."))
    })
    
    # ---- the three lists ----------------------------------------------
    output$list_pane <- shiny$renderUI({
      switch(vstate(),
             available = order_theme$table_card(reactableOutput(ns("stock_tbl"))),
             requests  = order_theme$table_card(reactableOutput(ns("req_tbl"))),
             order_theme$table_card(reactableOutput(ns("bench_tbl"))))
    })
    
    output$stock_tbl <- renderReactable({
      d <- stock_rows()
      keep <- c("order_number", "customer_name", "crop_name", "services_owed",
                "units_bound", "still_needed", "act")
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick_stock"), "row_id")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(sel_stock(), "row_id")),
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          row_id = colDef(show = FALSE), source_stage = colDef(show = FALSE),
          source_bench = colDef(show = FALSE),
          suggested_sample_code = colDef(show = FALSE),
          available_since = colDef(show = FALSE),
          batches_available = colDef(show = FALSE),
          units_available = colDef(show = FALSE),
          requested = colDef(show = FALSE), on_bench = colDef(show = FALSE),
          variety_name = colDef(show = FALSE),
          
          order_number = colDef(name = "ORDER", minWidth = 160,
                                cell = function(v) shiny$tags$strong(v)),
          customer_name = colDef(name = "CUSTOMER", minWidth = 140),
          crop_name = colDef(name = "CROP", width = 95),
          # WHAT the plants are for. A bench about to pot 200 plants should be
          # able to see it is 200 vines and not 200 of something else.
          services_owed = colDef(name = "PROMISED AS", minWidth = 175,
                                 cell = function(v) {
                                   if (is.na(v)) order_theme$chip("all met", "brand") else v
                                 }),
          # What was actually promised to soil, not everything the batch holds.
          # Asking for all of it would strand material another bench is
          # counting on for in vitro work.
          units_bound = colDef(name = "SOIL-BOUND", width = 120,
                               cell = function(v, i) {
                                 n <- if (is.na(v)) 0L else as.integer(v)
                                 shiny$tagList(
                                   order_theme$chip(sprintf("%d units", n), if (n > 0) "teal" else "ink"),
                                   shiny$tags$small(class = "wl-meta-note",
                                                    sprintf("of %d held", as.integer(d$units_available[i]))))
                               }),
          still_needed = colDef(name = "STILL OWED", width = 120, cell = function(v) {
            n <- if (is.na(v)) 0L else as.integer(v)
            if (n == 0) order_theme$chip("met", "brand")
            else order_theme$chip(sprintf("%d plants", n), "amber")
          }),
          act = colDef(name = "", width = 175, sortable = FALSE, cell = function(v, i) {
            if (as.integer(d$requested[i]) > 0) order_theme$chip("Requested", "amber")
            else if (is.na(d$units_bound[i]) || as.integer(d$units_bound[i]) == 0)
              order_theme$chip("Nothing bound", "ink")
            else
              shiny$tags$button(class = "btn btn-outline-success btn-sm", type = "button",
                                onclick = sprintf(
                                  "Shiny.setInputValue('%s', {code: '%s', n: Math.random()})",
                                  ns("act_stock"), d$row_id[i]),
                                "Request plants")
          })
        ), keep), d),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "Nothing is ready. Subculture releases material once it is committed to a soil-bound service."),
        theme = order_theme$rt_theme())
    })
    
    output$bench_tbl <- renderReactable({
      d <- bench_rows()
      keep <- c("sample_code", "order_number", "screenhouse", "survival_pct",
                "days_in_substrate", "unallocated_units", "state_label", "act")
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick_bench"), "sample_code")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(sel_bench(), "sample_code")),
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          state_code = colDef(show = FALSE), since = colDef(show = FALSE),
          parent_sample_code = colDef(show = FALSE),
          order_service_id = colDef(show = FALSE), service_code = colDef(show = FALSE),
          service_label = colDef(show = FALSE), substrate = colDef(show = FALSE),
          potted_on = colDef(show = FALSE), weaning_days = colDef(show = FALSE),
          potting_notes = colDef(show = FALSE), potted = colDef(show = FALSE),
          weaning_due = colDef(show = FALSE), units_held = colDef(show = FALSE),
          initial_count = colDef(show = FALSE), ledger_count = colDef(show = FALSE),
          n_dead = colDef(show = FALSE), n_discarded = colDef(show = FALSE),
          n_contaminated = colDef(show = FALSE), n_allocated = colDef(show = FALSE),
          drift = colDef(show = FALSE), customer_name = colDef(show = FALSE),
          crop_name = colDef(show = FALSE), variety_name = colDef(show = FALSE),
          
          sample_code = colDef(name = "BATCH", minWidth = 120,
                               cell = function(v) shiny$tags$strong(v)),
          order_number = colDef(name = "ORDER", minWidth = 150),
          screenhouse = colDef(name = "SCREENHOUSE", minWidth = 130,
                               cell = function(v) if (is.na(v)) "\u2014" else v),
          # Alive over planted, and the percentage. The count alone does not say
          # whether a batch is doing well; the percentage alone hides how many
          # plants are actually at stake.
          survival_pct = colDef(name = "SURVIVING", width = 145,
                                cell = function(v, i) {
                                  if (is.na(v)) return(order_theme$chip("not potted", "ink"))
                                  p <- as.integer(v)
                                  shiny$tagList(
                                    order_theme$chip(sprintf("%d%%", p), survival_tone(p)),
                                    shiny$tags$small(class = "wl-meta-note",
                                                     sprintf("%d of %d", as.integer(d$units_held[i]),
                                                             as.integer(d$initial_count[i]))))
                                }),
          days_in_substrate = colDef(name = "IN SUBSTRATE", width = 130,
                                     cell = function(v, i) {
                                       if (is.na(v)) return("\u2014")
                                       n <- as.integer(v)
                                       shiny$tagList(
                                         shiny$span(sprintf("%d day%s", n, if (n == 1) "" else "s")),
                                         if (isTRUE(d$weaning_due[i]))
                                           order_theme$chip("weaning due", "amber") else NULL)
                                     }),
          unallocated_units = colDef(name = "UNCOMMITTED", width = 125,
                                     cell = function(v) {
                                       n <- if (is.na(v)) 0L else as.integer(v)
                                       if (n == 0) order_theme$chip("all committed", "brand")
                                       else order_theme$chip(sprintf("%d plants", n), "amber")
                                     }),
          state_label = colDef(name = "STATE", width = 125, cell = function(v, i) {
            shiny$tagList(
              order_theme$chip(v %||% "", state_tone(d$state_code[i])),
              if (isTRUE(d$drift[i])) order_theme$chip("count drift", "amber") else NULL)
          }),
          act = colDef(name = "", width = 150, sortable = FALSE, cell = function(v, i) {
            st <- d$state_code[i]
            if (st %in% c("completed", "rejected", "depleted")) ""
            else if (!isTRUE(d$potted[i])) order_theme$chip("Needs potting", "amber")
            else if (as.integer(d$unallocated_units[i]) > 0)
              order_theme$chip("To commit", "amber")
            else order_theme$chip("Ready to close", "teal")
          })
        ), keep), d),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "No batch here. Request soil-bound material from the Available tab."),
        theme = order_theme$rt_theme())
    })
    
    output$req_tbl <- renderReactable({
      d <- reqs()
      keep <- c("order_number", "to_stage_label", "reason", "requested_by",
                "qty_outstanding", "status", "customer_name", "crop_name")
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick_req"), "request_id")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(sel_req(), "request_id")),
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          request_id = colDef(show = FALSE), to_stage = colDef(show = FALSE),
          source_sample_code = colDef(show = FALSE),
          source_stage = colDef(show = FALSE), source_bench = colDef(show = FALSE),
          source_state = colDef(show = FALSE), source_units = colDef(show = FALSE),
          test_id = colDef(show = FALSE), test_acronym = colDef(show = FALSE),
          test_name = colDef(show = FALSE), requested_on = colDef(show = FALSE),
          authorized_on = colDef(show = FALSE), authorized_by = colDef(show = FALSE),
          variety_name = colDef(show = FALSE), draws_so_far = colDef(show = FALSE),
          # A column with NO colDef renders with defaults - rt_only() can only
          # hide one that exists. These arrived with partial fulfilment and
          # would otherwise have appeared raw on every request table.
          qty_requested = colDef(show = FALSE), qty_sent = colDef(show = FALSE),
          n_deliveries = colDef(show = FALSE), last_sent_on = colDef(show = FALSE),
          
          order_number = colDef(name = "ORDER", minWidth = 155,
                                cell = function(v) shiny$tags$strong(v)),
          to_stage_label = colDef(name = "NEEDED AT", minWidth = 165,
                                  cell = function(v) order_theme$chip(v, "amber")),
          reason = colDef(name = "WHY", minWidth = 185),
          requested_by = colDef(name = "ASKED BY", width = 110),
          customer_name = colDef(name = "CUSTOMER", minWidth = 130),
          crop_name = colDef(name = "CROP", width = 90),
          # OWED, not just "to enter". A request answered once and still short is
          # the case this column exists for: the number is what tells the
          # technician whether to walk to the bench at all.
          qty_outstanding = colDef(name = "STILL OWED", width = 120,
                                   cell = function(v, i) {
                                     n <- if (is.na(v)) 0L else as.integer(v)
                                     sent <- as.integer(d$qty_sent[i] %||% 0L)
                                     shiny$tagList(
                                       order_theme$chip(sprintf("%d plants", n), if (n > 0) "amber" else "brand"),
                                       if (sent > 0)
                                         shiny$tags$small(class = "wl-meta-note",
                                                          sprintf("%d of %d sent", sent,
                                                                  as.integer(d$qty_requested[i] %||% 0L)))
                                       else NULL)
                                   }),
          status = colDef(name = "", width = 140, cell = function(v)
            if (identical(v, "pending")) order_theme$chip("To authorise", "amber")
            else if (identical(v, "partial")) order_theme$chip("Part sent", "amber")
            else order_theme$chip("To enter", "teal"))
        ), keep), d),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang("No bench has asked for plants from here."),
        theme = order_theme$rt_theme())
    })
    
    # ---- selection ----------------------------------------------------
    sel_stock <- shiny$reactiveVal(NULL)
    sel_bench <- shiny$reactiveVal(NULL)
    sel_req   <- shiny$reactiveVal(NULL)
    
    clear_sel <- function() { sel_stock(NULL); sel_bench(NULL); sel_req(NULL) }
    shiny$observeEvent(input$pick_stock, { clear_sel(); sel_stock(input$pick_stock$code) })
    shiny$observeEvent(input$pick_bench, { clear_sel(); sel_bench(input$pick_bench$code) })
    shiny$observeEvent(input$pick_req,   { clear_sel(); sel_req(input$pick_req$code) })
    shiny$observeEvent(input$detail_close, { clear_sel() })
    shiny$observeEvent(input$vstate, { clear_sel() })
    
    stock_row <- shiny$reactive({
      id <- sel_stock(); if (is.null(id)) return(NULL)
      d <- stock(); if (nrow(d) == 0) return(NULL)
      x <- d[d$row_id == id, , drop = FALSE]
      if (nrow(x) == 0) NULL else x
    })
    bench_row <- shiny$reactive({
      sc <- sel_bench(); if (is.null(sc)) return(NULL)
      d <- queue(); if (nrow(d) == 0) return(NULL)
      x <- d[d$sample_code == sc, , drop = FALSE]
      if (nrow(x) == 0) NULL else x
    })
    req_row <- shiny$reactive({
      id <- sel_req(); if (is.null(id)) return(NULL)
      d <- reqs(); if (nrow(d) == 0) return(NULL)
      x <- d[as.character(d$request_id) == as.character(id), , drop = FALSE]
      if (nrow(x) == 0) NULL else x
    })
    
    shiny$observeEvent(input$scan_go, {
      code <- shiny$isolate(input$scan)
      if (is.null(code) || !nzchar(code)) return()
      d <- queue()
      if (nrow(d) > 0 && code %in% d$sample_code) { clear_sel(); sel_bench(code) }
      else toastr_warning(sprintf("%s is not a batch in this screenhouse.", code),
                          title = "Not found")
    })
    
    output$detail <- shiny$renderUI({
      if (!is.null(stock_row())) return(shiny$uiOutput(ns("stock_detail")))
      if (!is.null(req_row()))   return(shiny$uiOutput(ns("req_detail")))
      if (!is.null(bench_row())) return(shiny$uiOutput(ns("bench_detail")))
      shiny$div(class = "wl-detail-inner",
                shiny$div(class = "wl-empty",
                          shiny$div(class = "wl-empty-ico", shiny$icon("hand-pointer")),
                          shiny$div(class = "wl-empty-title", "Select a row to begin"),
                          shiny$div(class = "wl-empty-body",
                                    "Choose a consignment to request plants, a batch in ",
                                    "substrate to record potting or losses, or a request ",
                                    "from another bench to fill.")))
    })
    
    # ---- detail: available material -----------------------------------
    output$stock_detail <- shiny$renderUI({
      x <- stock_row(); if (is.null(x)) return(NULL)
      dm <- hardening_demand(x$order_number[1])
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = x$order_number[1],
                  sub = sprintf("%s \u00b7 %s", x$customer_name[1] %||% "", x$source_bench[1]),
                  close_input = ns("detail_close")),
                order_theme$section(
                  "\u2192", "Request plants for hardening",
                  sub = x$source_bench[1],
                  order_theme$guide(
                    "Asks ", shiny$strong(x$source_bench[1]), " for material from this ",
                    "consignment. A technician there chooses which batch to send."),
                  order_theme$prop_grid(
                    order_theme$prop("Customer", x$customer_name[1]),
                    order_theme$prop("Crop", x$crop_name[1] %||% "\u2014"),
                    order_theme$prop("Variety", x$variety_name[1] %||% "\u2014"),
                    order_theme$prop("Batches ready", as.character(x$batches_available[1])),
                    order_theme$prop("Promised to soil", as.character(x$units_bound[1])),
                    order_theme$prop("Already here", as.character(x$on_bench[1]))),
                  
                  order_theme$subhead("Soil-bound lines on this order"),
                  if (nrow(dm) == 0)
                    order_theme$guide("This order has no services that need a plant in soil.")
                  else
                    shiny$div(class = "svc-lines", lapply(seq_len(nrow(dm)), function(i)
                      order_theme$service_line(
                        label     = dm$service_label[i],
                        sub       = if (is.na(dm$recipient[i])) "" else dm$recipient[i],
                        fulfilled = as.integer(dm$fulfilled_qty[i]),
                        target    = as.integer(dm$target_qty[i]),
                        pct       = as.integer(dm$pct_complete[i]),
                        status    = dm$status[i]))),
                  
                  shiny$numericInput(ns("req_qty_wanted"), "Plants needed",
                                     value = 1, min = 1, width = "240px"),
                  order_theme$guide(
                    "The holding bench sends what is ready and the request stays open for the ",
                    "rest, so ask for what you actually need rather than what you think they ",
                    "can spare today."),
                  shiny$textAreaInput(ns("req_reason"), "Why this is needed", width = "100%",
                                      value = "plants needed for hardening"),
                  order_theme$detail_actions(
                    shiny$actionButton(ns("do_request"), "Request plants",
                                       class = "btn btn-success"),
                    shiny$actionButton(ns("detail_close"), "Cancel",
                                       class = "btn btn-sm btn-outline-secondary")))
      )
    })
    
    # ---- detail: a batch in substrate ---------------------------------
    output$bench_detail <- shiny$renderUI({
      x <- bench_row(); if (is.null(x)) return(NULL)
      sc <- x$sample_code[1]
      st <- x$state_code[1]
      held <- as.integer(x$units_held[1] %||% 0L)
      free <- as.integer(x$unallocated_units[1] %||% 0L)
      potted <- isTRUE(x$potted[1])
      dm <- hardening_demand(x$order_number[1])
      open_lines <- if (nrow(dm) == 0) dm else dm[as.integer(dm$remaining_qty) > 0, , drop = FALSE]
      
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = sc,
                  sub = sprintf("%s \u00b7 %s", x$order_number[1], x$customer_name[1] %||% ""),
                  close_input = ns("detail_close")),
                
                shiny$div(class = "wl-statusbar",
                          order_theme$chip(x$state_label[1] %||% "", state_tone(st)),
                          order_theme$chip(sprintf("from %s", x$parent_sample_code[1]), "ink"),
                          if (!is.na(x$survival_pct[1]))
                            order_theme$chip(sprintf("%d%% surviving", as.integer(x$survival_pct[1])),
                                             survival_tone(as.integer(x$survival_pct[1]))) else NULL,
                          if (isTRUE(x$weaning_due[1])) order_theme$chip("weaning due", "amber") else NULL,
                          if (isTRUE(x$drift[1])) order_theme$chip("count drift", "amber") else NULL),
                
                if (!(st %in% c("completed", "rejected", "depleted"))) shiny$tagList(
                  
                  # ---- 1. potting ----
                  if (!potted) shiny$tagList(
                    order_theme$subhead("1. Record potting"),
                    shiny$div(class = "update-hint",
                              "What went into substrate, where, and in what. The count entered here ",
                              "is the denominator of this batch's survival rate for the rest of its ",
                              "life, so it is what was actually planted \u2014 not what was sent."),
                    shiny$textInput(ns("screenhouse"),
                                    shiny$HTML("Screenhouse <span class='mandatory_star'>*</span>"),
                                    width = "100%", placeholder = "e.g. SH-2, bay 4"),
                    shiny$textInput(ns("substrate"),
                                    shiny$HTML("Substrate <span class='mandatory_star'>*</span>"),
                                    width = "100%", placeholder = "e.g. sterilized forest soil + sand 2:1"),
                    shiny$dateInput(ns("potted_on"), "Date potted", value = Sys.Date()),
                    shiny$numericInput(ns("initial_count"),
                                       sprintf("Plants potted (%d received)", held),
                                       value = held, min = 1),
                    shiny$numericInput(ns("weaning_days"), "Weaning period (days)",
                                       value = 21, min = 1),
                    shiny$textAreaInput(ns("potting_notes"), "Notes", width = "100%"),
                    order_theme$detail_actions(
                      shiny$actionButton(ns("save_potting"), "Save potting",
                                         class = "btn btn-success"))
                  ) else shiny$tagList(
                    order_theme$subhead("Potting"),
                    order_theme$prop_grid(
                      order_theme$prop("Screenhouse", x$screenhouse[1]),
                      order_theme$prop("Substrate", x$substrate[1]),
                      order_theme$prop("Potted on", as.character(x$potted_on[1])),
                      order_theme$prop("In substrate",
                                       if (is.na(x$days_in_substrate[1])) "\u2014"
                                       else sprintf("%d days", as.integer(x$days_in_substrate[1]))),
                      order_theme$prop("Weaning period",
                                       if (is.na(x$weaning_days[1])) "not set"
                                       else sprintf("%d days", as.integer(x$weaning_days[1]))),
                      order_theme$prop("Planted", as.character(x$initial_count[1])),
                      order_theme$prop("Surviving", as.character(held)),
                      order_theme$prop("Lost",
                                       sprintf("%d died \u00b7 %d discarded \u00b7 %d contaminated",
                                               as.integer(x$n_dead[1]),
                                               as.integer(x$n_discarded[1]),
                                               as.integer(x$n_contaminated[1]))))),
                  
                  # ---- 2. losses ----
                  if (potted) shiny$tagList(
                    order_theme$subhead("2. Log a loss"),
                    shiny$div(class = "update-hint",
                              "Losing plants to acclimatization is expected, not an incident. Each ",
                              "entry is kept with its reason, and the survival rate moves with it."),
                    shiny$selectInput(ns("loss_reason"), "What happened",
                                      choices = LOSS_REASONS, width = "240px"),
                    shiny$numericInput(ns("loss_n"), sprintf("How many (%d alive)", held),
                                       value = 1, min = 1, max = max(1L, held)),
                    shiny$textAreaInput(ns("loss_notes"), "Notes", width = "100%"),
                    order_theme$detail_actions(
                      shiny$actionButton(ns("log_loss"), "Log loss",
                                         class = "btn btn-outline-secondary"))) else NULL,
                  
                  # ---- 3. allocation ----
                  if (potted) shiny$tagList(
                    order_theme$subhead("3. Commit plants to a service"),
                    shiny$div(class = "update-hint",
                              "Committing promises plants to one line of this order. Only lines that ",
                              "need a plant in soil are offered here."),
                    if (nrow(open_lines) == 0)
                      order_theme$guide("Every soil-bound line on this order is met.")
                    else if (free <= 0)
                      order_theme$guide("Every plant in this batch is already committed.")
                    else shiny$tagList(
                      shiny$div(class = "svc-lines", lapply(seq_len(nrow(open_lines)), function(i)
                        order_theme$service_line(
                          label     = open_lines$service_label[i],
                          sub       = if (is.na(open_lines$recipient[i])) "" else open_lines$recipient[i],
                          fulfilled = as.integer(open_lines$fulfilled_qty[i]),
                          target    = as.integer(open_lines$target_qty[i]),
                          pct       = as.integer(open_lines$pct_complete[i]),
                          status    = open_lines$status[i]))),
                      shiny$selectInput(ns("alloc_service"), "Service line",
                                        choices = setNames(
                                          as.character(open_lines$order_service_id),
                                          sprintf("%s \u00b7 %d of %d still owed",
                                                  open_lines$service_label,
                                                  as.integer(open_lines$remaining_qty),
                                                  as.integer(open_lines$target_qty))),
                                        width = "100%"),
                      shiny$numericInput(ns("alloc_qty"),
                                         sprintf("Plants to commit (%d uncommitted)", free),
                                         value = 1, min = 1, max = free),
                      order_theme$detail_actions(
                        shiny$actionButton(ns("do_allocate"), "Commit to this service",
                                           class = "btn btn-success")))) else NULL,
                  
                  # ---- 4. close ----
                  if (potted) shiny$tagList(
                    order_theme$subhead("4. Close the batch"),
                    if (held == 0)
                      shiny$tagList(
                        order_theme$guide(tone = "do",
                                          "Nothing survived. Closing records the batch as depleted \u2014 its ",
                                          "survival rate stays on record, and whatever it was promised to has ",
                                          "to be met from another batch."),
                        order_theme$detail_actions(
                          shiny$actionButton(ns("deplete"), "Close as depleted",
                                             class = "btn btn-outline-secondary")))
                    else if (as.integer(x$n_allocated[1]) == 0)
                      order_theme$guide(
                        "Nothing from this batch has been committed yet. Completing it now would ",
                        "send plants onward with no service line to answer, so commit first.")
                    else shiny$tagList(
                      shiny$div(class = "update-hint",
                                "Completing releases the batch onward \u2014 to in vivo conservation ",
                                "or mini-tuber distribution, whichever the committed services call for."),
                      shiny$textAreaInput(ns("close_notes"), "Notes", width = "100%"),
                      order_theme$detail_actions(
                        shiny$actionButton(ns("reject"), "Reject",
                                           class = "btn btn-outline-secondary"),
                        shiny$actionButton(ns("complete"), "Complete batch",
                                           class = "btn btn-success")))) else NULL
                  
                ) else shiny$tagList(
                  order_theme$subhead("History"),
                  order_theme$prop_grid(
                    order_theme$prop("Screenhouse", x$screenhouse[1] %||% "\u2014"),
                    order_theme$prop("Substrate", x$substrate[1] %||% "\u2014"),
                    order_theme$prop("Planted", as.character(x$initial_count[1])),
                    order_theme$prop("Surviving", as.character(held)),
                    order_theme$prop("Survival",
                                     if (is.na(x$survival_pct[1])) "\u2014"
                                     else sprintf("%d%%", as.integer(x$survival_pct[1]))),
                    order_theme$prop("Committed", as.character(as.integer(x$n_allocated[1]))),
                    order_theme$prop("Lost",
                                     sprintf("%d died \u00b7 %d discarded \u00b7 %d contaminated",
                                             as.integer(x$n_dead[1]),
                                             as.integer(x$n_discarded[1]),
                                             as.integer(x$n_contaminated[1])))),
                  if (identical(st, "completed")) shiny$div(class = "flow-cta ok",
                                                            shiny$span(class = "fc-ico", shiny$icon("circle-check")),
                                                            shiny$span("Complete. This batch has been released onward."))
                  else if (identical(st, "depleted")) shiny$div(class = "update-hint",
                                                                "Nothing survived in this batch. Its survival rate stays on record.")
                  else NULL)
      )
    })
    
    # ---- detail: a request from another bench --------------------------
    output$req_detail <- shiny$renderUI({
      x <- req_row(); if (is.null(x)) return(NULL)
      pending <- identical(x$status[1], "pending")
      el <- eligible_batches()
      ch <- batch_choices(el)
      ok_codes <- if (length(ch)) el$sample_code[as.integer(el$units_held) > 1L] else character(0)
      sug <- x$source_sample_code[1]
      sel <- if (sug %in% ok_codes) sug else if (length(ok_codes)) ok_codes[1] else NULL
      
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = x$order_number[1],
                  sub = sprintf("%s \u00b7 %s", x$to_stage_label[1], x$customer_name[1] %||% ""),
                  close_input = ns("detail_close")),
                order_theme$section(
                  if (pending) "\u21a9" else "\u2713",
                  if (pending) "Authorise this request" else "Enter the batch you are sending",
                  sub = x$to_stage_label[1],
                  order_theme$guide(
                    if (pending)
                      "Authorising allows plants from this consignment to be taken. It moves nothing."
                    else shiny$tagList(
                      "Choose the batch to draw from. ", shiny$strong(sug),
                      " is what was suggested; send whichever batch is strongest.")),
                  order_theme$prop_grid(
                    order_theme$prop("Customer", x$customer_name[1]),
                    order_theme$prop("Asked by", x$requested_by[1]),
                    order_theme$prop("Reason", x$reason[1]),
                    if (!pending) order_theme$prop("Authorised by", x$authorized_by[1]) else NULL),
                  # What is still owed on this request, and what has already gone.
                  # Both benches read the same numbers from view_request_progress, so
                  # they cannot be told different things about the same request.
                  order_theme$prop_grid(
                    order_theme$prop("Asked for", sprintf("%d %s",
                                                          as.integer(x$qty_requested[1] %||% 1L), "plants")),
                    order_theme$prop("Already sent", sprintf("%d in %d deliver%s",
                                                             as.integer(x$qty_sent[1] %||% 0L),
                                                             as.integer(x$n_deliveries[1] %||% 0L),
                                                             if (identical(as.integer(x$n_deliveries[1] %||% 0L), 1L)) "y" else "ies")),
                    order_theme$prop("Still owed", sprintf("%d %s",
                                                           as.integer(x$qty_outstanding[1] %||% 0L), "plants"))),
                  if (!pending) {
                    if (length(ch) == 0)
                      order_theme$guide(tone = "do",
                                        "No completed batch of this order is standing here with plants to spare.")
                    else shiny$tagList(
                      shiny$selectInput(ns("req_batch"), "Batch to draw from",
                                        choices = ch, selected = sel, width = "100%"),
                      shiny$numericInput(ns("req_qty"),
                                         sprintf("Plants to send now (%d still owed)",
                                                 as.integer(x$qty_outstanding[1] %||% 1L)),
                                         value = max(1L, min(
                                           as.integer(x$qty_outstanding[1] %||% 1L),
                                           spare_in(el, sel))),
                                         min = 1,
                                         max = as.integer(x$qty_outstanding[1] %||% 1L),
                                         step = 1, width = "240px"),
                      order_theme$guide(
                        "Send what is ready. Anything short of the balance leaves the request ",
                        "open here and on ", x$to_stage_label[1], "'s screen, with the ",
                        "remainder still owed."))
                  } else NULL,
                  order_theme$detail_actions(
                    if (pending)
                      shiny$actionButton(ns("req_authorize"), "Authorise this request",
                                         class = "btn btn-success")
                    else
                      shiny$actionButton(ns("req_enter"), "Add batch and send",
                                         class = "btn btn-success"),
                    shiny$actionButton(ns("req_cancel"), "Cancel request",
                                       class = "btn btn-sm btn-outline-secondary")))
      )
    })
    
    eligible_batches <- shiny$reactive({
      x <- req_row(); if (is.null(x)) return(NULL)
      d <- queue(); if (!is.data.frame(d) || nrow(d) == 0) return(d)
      d[d$order_number == x$order_number[1] & d$state_code == "completed", , drop = FALSE]
    })
    
    # How many a chosen batch can spare, keeping one back. Returns 0 rather
    # than integer(0) when nothing is selected, because integer(0) propagates
    # silently through min() and lands a numericInput below its own minimum.
    spare_in <- function(d, code) {
      if (!is.data.frame(d) || nrow(d) == 0 || is.null(code) || !nzchar(code %||% ""))
        return(0L)
      q <- as.integer(d$units_held[d$sample_code == code])
      if (length(q) == 0 || is.na(q[1])) 0L else max(q[1] - 1L, 0L)
    }
    
    batch_choices <- function(d) {
      if (!is.data.frame(d) || nrow(d) == 0) return(character(0))
      q <- as.integer(d$units_held); q[is.na(q)] <- 0L
      lab <- sprintf("%s  \u00b7  %d plant%s%s", d$sample_code, q,
                     ifelse(q == 1, "", "s"),
                     ifelse(q <= 1, "  \u2014 cannot be drawn", ""))
      setNames(d$sample_code, lab)
    }
    
    # ---- request plants -------------------------------------------------
    do_request <- function(x) {
      if (is.null(x) || nrow(x) == 0) return(invisible(FALSE))
      if (as.integer(fld(x, "requested", 0L)) > 0) {
        toastr_warning("This consignment has already been requested.",
                       title = "Already requested"); return()
      }
      order <- fld(x, "order_number", "")
      why <- shiny$isolate(input$req_reason)
      # HOW MUCH, not just what. Without it the holding bench has no total to
      # deliver against, and "partly sent" is not a thing anyone can express.
      want <- suppressWarnings(as.integer(shiny$isolate(input$req_qty_wanted)))
      if (length(want) == 0 || is.na(want) || want < 1) want <- 1L
      if (is.null(why) || !nzchar(trimws(why))) why <- "plants needed for hardening"
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # Re-read inside the transaction: the row was rendered from a
          # snapshot, and another bench may have taken the last batch.
          still <- dbGetQuery(conn, "
            SELECT units_bound, suggested_sample_code
            FROM view_hardening_stock WHERE order_number = $1",
                              params = list(order))
          if (nrow(still) == 0 || as.integer(still$units_bound[1]) < 1)
            stop("No soil-bound material from ", order, " is available any more.",
                 call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_sample_request
              (order_number, source_sample_code, to_stage, reason, requested_by,
               status, qty_requested)
            VALUES ($1, $2, 'hardening', $3, $4, 'pending', $5)",
                    params = list(order, still$suggested_sample_code[1], why, user(),
                                  want))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'hardening', 'plants requested', $2, $3)",
                    params = list(order, user(), "requested from subculture"))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Request failed", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success("Requested \u2014 subculture will send the plants.", title = "Requested")
        clear_sel(); self_refresh(); signal_others()
      }
    }
    
    shiny$observeEvent(input$do_request, { do_request(stock_row()) })
    shiny$observeEvent(input$act_stock, {
      d <- stock(); if (nrow(d) == 0) return()
      x <- d[d$row_id == input$act_stock$code, , drop = FALSE]
      if (nrow(x) == 0) return()
      clear_sel(); sel_stock(input$act_stock$code)
      do_request(x)
    })
    
    # ---- record potting -------------------------------------------------
    shiny$observeEvent(input$save_potting, {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      held <- as.integer(x$units_held[1] %||% 0L)
      sh <- trimws(input$screenhouse %||% "")
      sub <- trimws(input$substrate %||% "")
      n0 <- suppressWarnings(as.integer(input$initial_count))
      if (!nzchar(sh) || !nzchar(sub)) {
        toastr_error("Name the screenhouse and the substrate.", title = "Missing"); return()
      }
      if (is.na(n0) || n0 < 1) {
        toastr_error("At least one plant must go into substrate.",
                     title = "Invalid count"); return()
      }
      if (n0 > held) {
        toastr_error(sprintf("%s received %d plant%s \u2014 you cannot pot %d.",
                             sc, held, if (held == 1) "" else "s", n0),
                     title = "More than arrived"); return()
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          dbExecute(conn, "
            INSERT INTO tbl_hardening_detail
              (sample_code, screenhouse, substrate, potted_on, initial_count,
               weaning_days, notes, recorded_by)
            VALUES ($1,$2,$3,$4,$5,$6,$7,$8)",
                    params = list(sc, sh, sub, as.character(input$potted_on), n0,
                                  int_or_na(input$weaning_days),
                                  nz(input$potting_notes), user()))
          # The ledger opens with what ARRIVED, not with what was potted, and
          # the plants that never made it into substrate are then discarded
          # from it explicitly.
          #
          # Opening at the potted figure and discarding the difference counted
          # those plants twice - they were already excluded from the opening
          # balance - so every potted batch read `count drift` from the moment
          # it was potted. The ledger has to account for everything that
          # arrived, or it is not a ledger.
          #
          # survival_pct is separate and unaffected: it is against
          # tbl_hardening_detail.initial_count, what was actually planted,
          # because that is the number a customer was told about.
          dbExecute(conn, "
            INSERT INTO tbl_culture_count
              (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'hardening','initial',$2,$3,$4)",
                    params = list(sc, held, "received on this bench", user()))
          if (n0 < held) {
            dbExecute(conn, "
              INSERT INTO tbl_culture_count
                (sample_code, stage_code, reason, delta, notes, recorded_by)
              VALUES ($1,'hardening','discarded',$2,$3,$4)",
                      params = list(sc, -(held - n0),
                                    "not potted; unusable on handling", user()))
          }
          dbExecute(conn, "UPDATE tbl_sample SET quantity = $2 WHERE sample_code = $1",
                    params = list(sc, n0))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "hardening", "updated", user(),
                       wf = wf, ctx = ctx,
                       notes = sprintf("potted %d plant%s in %s, %s", n0,
                                       if (n0 == 1) "" else "s", sub, sh))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not save", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(code = sc, title = "HARDENING BATCH",
                                 line1 = sprintf("%s \u00b7 %d plants", sh, n0),
                                 line2 = format(Sys.Date(), "%d %b %Y"),
                                 stringsAsFactors = FALSE))
        toastr_success(sprintf("%s potted \u2014 %d plant%s in substrate.", sc, n0,
                               if (n0 == 1) "" else "s"))
        self_refresh()
      }
    })
    
    # ---- log a loss -----------------------------------------------------
    shiny$observeEvent(input$log_loss, {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      reason <- input$loss_reason %||% "dead"
      n <- suppressWarnings(as.integer(input$loss_n))
      held <- as.integer(x$units_held[1] %||% 0L)
      free <- as.integer(x$unallocated_units[1] %||% 0L)
      if (is.na(n) || n < 1) {
        toastr_error("How many were lost?", title = "Invalid"); return()
      }
      if (n > held) {
        toastr_error(sprintf("%s holds %d plant%s \u2014 you cannot lose %d.",
                             sc, held, if (held == 1) "" else "s", n),
                     title = "More than the batch holds"); return()
      }
      if (n > free) {
        toastr_warning(sprintf(
          "%d of these were already committed to a service. That promise now has to be met from another batch.",
          n - free), title = "Committed plants lost", timeOut = 0)
      }
      left <- held - n
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # Guarded, not read-then-write: two people walking the screenhouse
          # and logging losses on one batch would otherwise each read the old
          # count and the second write would undo the first.
          moved <- dbExecute(conn, "
            UPDATE tbl_sample SET quantity = quantity - $2
             WHERE sample_code = $1 AND quantity - $2 >= 0",
                             params = list(sc, n))
          if (moved == 0)
            stop(sc, " no longer holds ", n, " plant(s). Somebody else may have logged ",
                 "a loss. Refresh and try again.", call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_culture_count
              (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'hardening',$2,$3,$4,$5)",
                    params = list(sc, reason, -n, nz(input$loss_notes), user()))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          # A batch with nothing left is DEPLETED, whatever it died of. Leaving
          # it in a working state left an empty batch in the queue with actions
          # that could not do anything.
          to_state <- if (left == 0) "depleted"
          else if (identical(reason, "dead")) "dead" else "updated"
          record_event(conn, sc, "hardening", to_state, user(),
                       wf = wf, ctx = ctx,
                       notes = sprintf("%d %s; %d remaining", n, reason, left))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not log", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success(sprintf("%d %s logged for %s \u2014 %d remaining.",
                               n, reason, sc, left))
        self_refresh()
      }
    })
    
    # ---- allocate to a service line -------------------------------------
    shiny$observeEvent(input$do_allocate, {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      osid <- suppressWarnings(as.integer(input$alloc_service))
      qty  <- suppressWarnings(as.integer(input$alloc_qty))
      free <- as.integer(x$unallocated_units[1] %||% 0L)
      if (is.na(osid)) { toastr_error("Choose a service line.", title = "Missing"); return() }
      if (is.na(qty) || qty < 1) {
        toastr_error("How many plants are you committing?", title = "Invalid"); return()
      }
      if (qty > free) {
        toastr_error(sprintf("%s has %d uncommitted plant%s \u2014 you cannot commit %d.",
                             sc, free, if (free == 1) "" else "s", qty),
                     title = "More than is free"); return()
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # Everything below is re-checked here rather than trusted from the
          # form. Allocation is the act that turns plants into a promise to a
          # customer - the one place in this pipeline where being wrong becomes
          # visible outside the lab.
          line <- dbGetQuery(conn, "
            SELECT os.order_service_id, os.order_number, p.remaining_qty, p.service_label
            FROM tbl_order_service os
            JOIN view_order_service_progress p ON p.order_service_id = os.order_service_id
            WHERE os.order_service_id = $1 AND os.cancelled_on IS NULL",
                             params = list(osid))
          if (nrow(line) == 0)
            stop("That service line no longer exists or has been cancelled.", call. = FALSE)
          if (!identical(line$order_number[1], x$order_number[1]))
            stop("That service line belongs to a different order.", call. = FALSE)
          if (as.integer(line$remaining_qty[1]) < qty)
            stop(sprintf("%s needs only %d more plant(s).", line$service_label[1],
                         as.integer(line$remaining_qty[1])), call. = FALSE)
          got <- dbGetQuery(conn, "
            SELECT unallocated_units FROM view_hardening_current WHERE sample_code = $1",
                            params = list(sc))
          if (nrow(got) == 0 || as.integer(got$unallocated_units[1]) < qty)
            stop(sc, " no longer has ", qty, " uncommitted plant(s). Refresh and try again.",
                 call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_service_allocation
              (order_service_id, sample_code, qty, allocated_by, notes)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(osid, sc, qty, user(),
                                  sprintf("allocated from hardening batch %s", sc)))
          # Set once, on first commitment, and left alone. A batch split across
          # two lines is described by its allocations, not by overwriting this.
          dbExecute(conn, "
            UPDATE tbl_sample SET order_service_id = $2
             WHERE sample_code = $1 AND order_service_id IS NULL",
                    params = list(sc, osid))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'hardening', 'plants allocated', $2, $3)",
                    params = list(x$order_number[1], user(),
                                  sprintf("%d plant(s) from %s to %s", qty, sc,
                                          line$service_label[1])))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not allocate", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success(sprintf("%d plant(s) committed from %s.", qty, sc),
                       title = "Allocated")
        self_refresh(); signal_others()
      }
    })
    
    # ---- close the batch -------------------------------------------------
    close_batch <- function(decision, to_state, msg, need_admin = TRUE) {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      if (need_admin && !is_admin()) {
        toastr_error("Only an administrator can close a batch.",
                     title = "Not permitted"); return()
      }
      note <- nz(input$close_notes)
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          if (!is.na(decision))
            dbExecute(conn, "
              INSERT INTO tbl_review (sample_code, stage_code, decision, comments, reviewed_by)
              VALUES ($1, 'hardening', $2, $3, $4)",
                      params = list(sc, decision, note, user()))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "hardening", to_state, user(),
                       wf = wf, ctx = ctx,
                       notes = if (is.na(decision)) "no plants survived"
                       else sprintf("review: %s", decision))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'hardening', $2, $3, $4)",
                    params = list(x$order_number[1], sprintf("batch %s", to_state),
                                  user(),
                                  if (is.na(x$survival_pct[1])) sc
                                  else sprintf("%s at %d%% survival", sc,
                                               as.integer(x$survival_pct[1]))))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Failed", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success(sprintf(msg, sc))
        if (identical(to_state, "completed")) {
          opts <- tryCatch({
            wf <- workflow_cache(WF_PATH, pool)
            o <- next_options(wf, "hardening", "completed", sample_context(pool, sc))
            unique(o$to_stage)
          }, error = function(e) character(0))
          if (length(opts))
            toastr_success(paste("Onward to:", paste(gsub("_", " ", opts), collapse = ", ")),
                           title = "Released")
        }
        clear_sel(); self_refresh(); signal_others()
      }
    }
    
    shiny$observeEvent(input$complete, {
      close_batch("approved", "completed", "%s complete \u2014 released onward.")
    })
    shiny$observeEvent(input$reject, {
      close_batch("rejected", "rejected", "%s rejected \u2014 repeat the hardening.")
    })
    # A batch that died needs no reviewer. Requiring one would leave dead trays
    # sitting in the queue waiting on a signature that decides nothing.
    shiny$observeEvent(input$deplete, {
      close_batch(NA_character_, "depleted", "%s closed \u2014 nothing survived.",
                  need_admin = FALSE)
    })
    
    # ---- requests from other benches -------------------------------------
    shiny$observeEvent(input$req_authorize, {
      x <- req_row(); if (is.null(x)) return()
      if (!is_admin()) {
        toastr_error("Only an administrator can authorise a request.",
                     title = "Not permitted"); return()
      }
      ok <- tryCatch({
        dbExecute(pool, "
          UPDATE tbl_sample_request
             SET status = 'authorized', authorized_by = $1, authorized_on = now()
           WHERE request_id = $2 AND status = 'pending'",
                  params = list(user(), as.integer(x$request_id[1]))); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e)); FALSE })
      if (ok) {
        toastr_success("Authorised \u2014 now waiting for a batch to be entered.")
        clear_sel(); self_refresh()
      }
    })
    
    shiny$observeEvent(input$req_enter, {
      x <- req_row(); if (is.null(x)) return()
      to <- x$to_stage[1]
      state <- unname(ENTRY_STATE[to])
      if (is.na(state)) {
        toastr_error(sprintf("No entry state is defined for %s.", to),
                     title = "Cannot enter"); return()
      }
      qty <- max(1L, as.integer(input$req_qty %||% 1L))
      # Never send more than is still owed. The form caps this too, but the
      # form is client state and this is the write; over-delivering would leave
      # the request settled with material the asking bench never accounted for.
      owed <- as.integer(x$qty_outstanding[1] %||% qty)
      if (!is.na(owed) && owed > 0 && qty > owed) {
        toastr_error(sprintf("Only %d plant(s) are still owed on this request.", owed),
                     title = "More than is owed"); return()
      }
      src <- input$req_batch %||% x$source_sample_code[1]
      el <- eligible_batches()
      if (!is.data.frame(el) || !(src %in% el$sample_code)) {
        toastr_error(sprintf("%s is not a completed batch standing here for %s.",
                             src, x$order_number[1]),
                     title = "Cannot draw", timeOut = 0); return()
      }
      drawn <- NA_character_
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          moved <- dbExecute(conn, "
            UPDATE tbl_sample SET quantity = quantity - $2
             WHERE sample_code = $1 AND quantity - $2 >= 1",
                             params = list(src, qty))
          if (moved == 0)
            stop(src, " cannot spare ", qty, " plant(s) - one must stay so it can answer ",
                 "a later request. Refresh and try again.", call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_culture_count
              (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'hardening','discarded',$2,$3,$4)",
                    params = list(src, -qty,
                                  sprintf("drawn to %s", gsub("_", " ", to)), user()))
          child <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                              params = list(CHILD_PREFIX))$code[1]
          # The service line travels WITH the plants. Material committed here is
          # committed downstream too, and re-deciding it at the next bench would
          # let one promise be counted twice.
          dbExecute(conn, "
            INSERT INTO tbl_sample (sample_code, order_number, parent_sample_code,
                                    stage_code, order_service_id, quantity, created_by, created_on)
            SELECT $1, $2, $3, $4, p.order_service_id, $5, $6, now()
            FROM tbl_sample p WHERE p.sample_code = $3",
                    params = list(child, x$order_number[1], src, to, qty, user()))
          dbExecute(conn, "
            INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, notes)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(child, to, state, user(),
                                  sprintf("drawn from %s for %s", src, gsub("_", " ", to))))
          # PARTIAL FULFILMENT. A delivery is a row in its own right, not an
          # overwrite of the request, because a request for 40 is routinely
          # answered with the 12 that are ready today and the rest when they
          # are - and both benches have to be able to see the balance.
          #
          # source_sample_code is recorded per delivery rather than taken from
          # the request: which batch is drawn is decided here, at the bench,
          # and two deliveries against one request often come from two batches.
          dbExecute(conn, "
            INSERT INTO tbl_request_fulfilment
              (request_id, sample_code, source_sample_code, qty, fulfilled_by)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(as.integer(x$request_id[1]), child, src,
                                  qty, user()))
          # Settled or still owing, decided from the LEDGER and not from what
          # this delivery happened to carry. Marking it fulfilled because a
          # delivery was made is how a request for 40 answered with 12 would
          # have disappeared from the holding bench's queue with 28 still owed.
          dbExecute(conn, "
            UPDATE tbl_sample_request r
               SET status = CASE WHEN p.qty_sent >= r.qty_requested
                                 THEN 'fulfilled' ELSE 'partial' END,
                   fulfilled_sample_code = $2, fulfilled_qty = $3,
                   fulfilled_by = $4, fulfilled_on = now()
              FROM view_request_progress p
             WHERE r.request_id = $1 AND p.request_id = r.request_id",
                    params = list(as.integer(x$request_id[1]), child, qty, user()))
          drawn <<- child
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not enter", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(code = drawn, title = "HARDENED PLANTS",
                                 line1 = sprintf("from %s", src),
                                 line2 = format(Sys.Date(), "%d %b %Y"),
                                 stringsAsFactors = FALSE))
        toastr_success(sprintf("%s entered from %s.", drawn, src), title = "Sent")
        clear_sel(); self_refresh(); signal_others()
        dest <- unname(DEST_TAB[to])
        if (!is.na(dest)) runjs(sprintf(
          "Shiny.setInputValue('rtb_goto', {tab: '%s', n: Math.random()}, {priority: 'event'})",
          dest))
      }
    })
    
    shiny$observeEvent(input$req_cancel, {
      x <- req_row(); if (is.null(x)) return()
      ok <- tryCatch({
        dbExecute(pool, "
          UPDATE tbl_sample_request
             SET status = 'cancelled', cancelled_on = now(),
                 cancel_reason = 'cancelled in hardening'
           WHERE request_id = $1", params = list(as.integer(x$request_id[1]))); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e)); FALSE })
      if (ok) { toastr_success("Request cancelled."); clear_sel(); self_refresh() }
    })
    
    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Empty text is NA, so an untouched optional field binds as SQL NULL rather
# than an empty string that later reads as "recorded, but blank".
nz <- function(v) if (is.null(v) || !nzchar(trimws(v %||% ""))) NA_character_ else trimws(v)

# RPostgres binds must each be length 1, so a NULL numeric has to become NA
# before it reaches params.
int_or_na <- function(v) {
  n <- suppressWarnings(as.integer(v))
  if (length(n) == 0 || is.na(n)) NA_integer_ else n
}