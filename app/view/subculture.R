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
  app/logic/fct_tracking[subculture_stock, subculture_queue, subculture_demand,
                         pending_requests, awaited_deliveries,
                         material_catalog, subculture_cycles],
  app/logic/fct_workflows[workflow_cache, next_options, sample_context, record_event],
  app/view/shared/label_print,
  app/view/shared/order_theme,
)

# ============================================================================
# SUBCULTURE / MULTIPLICATION · clinical worklist (worklist + detail panel)
# ----------------------------------------------------------------------------
# Two things happen on this bench, and they are different acts:
#
#   MULTIPLY   a batch is subcultured, repeatedly. Each passage is a CYCLE,
#              recorded in its own right, because the number of passages is
#              what a tissue-culture lab watches for somaclonal drift. Cycles
#              raise the count; contamination and death lower it.
#
#   ALLOCATE   material stops being shared stock and is committed to a named
#              service line. This is where tbl_order_service earns its
#              surrogate key: one order can ask for 200 vines for sale AND 50
#              plantlets for conservation, and each line is allocated against
#              separately. A natural key on (order, service) could not express
#              that, and neither could this screen.
#
# Allocation is embedded here rather than being its own module. The test is
# whether a thing has its own queue and its own user; allocation has neither -
# it is the last act of the technician who grew the material.
#
# WHAT ARRIVES HERE
#   Completed batches from surface sterilization, pulled. This bench cuts
#   nothing of its own: it asks, and a technician there chooses which batch to
#   send, looking at the plants.
#
# WHAT LEAVES
#   subculture/completed is the workflow's one fan_out - conservation,
#   distribution and hardening all at once, whichever the order's services
#   call for. Allocation is what decides which, so a batch is not complete
#   until its material is committed.
# ============================================================================

MY_TAB       <- "subculture"
WF_PATH      <- file.path("app", "static", "workflows", "cassava.yaml")
CHILD_PREFIX <- "SC"

# The lab keeps a working stock of five in culture at all times. It is the
# seed corn: a line drawn down to nothing cannot be recovered without going
# back to quarantine, which for a cleaned consignment means repeating
# thermotherapy and meristem culture - months of work to undo one afternoon's
# over-delivery.
#
# So five is the floor a draw may not cross, not a target. Other benches keep
# one back for the same reason at a smaller scale; here the number is five
# because a subculture line has to be multipliable, and one jar is not.
RESERVE <- 5L

# Every bench that draws FROM this one, and the state material arrives in.
ENTRY_STATE <- c(in_vitro_conservation = "established",
                 in_vitro_distribution = "established",
                 hardening             = "established")
DEST_TAB <- c(in_vitro_conservation = "conservation",
              in_vitro_distribution = "distribution",
              hardening             = "hardening")

LOSS_REASONS <- c("Contaminated" = "contaminated",
                  "Dead"         = "dead",
                  "Discarded"    = "discarded")
# 010 seeds `contaminated` and `dead` for this bench. They used to land on
# `updated`, which reads as routine progress - and contamination is the single
# commonest reason a subculture line is lost, so it is the one thing the state
# most needed to be able to say.
LOSS_STATE <- c(contaminated = "contaminated", dead = "dead", discarded = "updated")

state_tone <- function(s) switch(s %||% "",
                                 established = "ink", updated = "amber",
                                 contaminated = "amber", dead = "amber",
                                 depleted = "ink",
                                 completed = "brand", rejected = "amber", "ink")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    order_theme$page_header(
      title = "Subculture / Multiplication",
      sub   = "Multiply cleared material and commit it to the services the order asked for."
    ),
    
    shiny$uiOutput(ns("kpis")),
    shiny$uiOutput(ns("awaiting")),
    shiny$uiOutput(ns("guide")),
    
    order_theme$toolbar(
      order_theme$search_box(ns("q"), "Search batch, order, customer..."),
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
    
    printer <- label_print$server("print", module_name = "subculture", user = user)
    
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
    stock <- shiny$reactive({ refresh(); subculture_stock() })
    queue <- shiny$reactive({ refresh(); subculture_queue() })
    reqs  <- shiny$reactive({ refresh(); pending_requests("subculture") })
    # The mirror of reqs(): what THIS bench asked for and has not received in
    # full. Both read the same balances, so the bench that owes and the bench
    # that waits can never be shown different numbers about one request.
    awaited <- shiny$reactive({ refresh(); awaited_deliveries("subculture") })
    # Finished material and what it is for. A completed batch leaves the
    # working queue but does not leave the bench - it stands here until a
    # downstream bench asks for it, and until then it is stock this bench can
    # grow more from.
    catalog <- shiny$reactive({ refresh(); material_catalog("subculture") })
    
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
      search_rows(d, c("order_number", "customer_name", "crop_name", "variety_name"))
    })
    
    # These arms MUST match the counts in output$kpis, or a tab reads 7 and the
    # table shows something else.
    bench_rows <- shiny$reactive({
      d <- queue()
      if (nrow(d) > 0 || !is.null(d$sample_code)) d$act <- rep(NA_character_, nrow(d))
      if (nrow(d) > 0) {
        d <- switch(vstate(),
                    growing   = d[d$state_code %in% c("established", "updated"), , drop = FALSE],
                    allocate  = d[d$state_code %in% c("established", "updated") &
                                    as.integer(d$unallocated_units) > 0 &
                                    isTRUE(d$cycle_recorded) | FALSE, , drop = FALSE],
                    finished  = d[d$state_code %in% c("completed", "rejected", "depleted"), , drop = FALSE],
                    d)
      }
      search_rows(d, c("sample_code", "order_number", "customer_name",
                       "crop_name", "variety_name", "medium", "service_label"))
    })
    
    n_available <- shiny$reactive({ nrow(stock()) })
    n_requests  <- shiny$reactive({ nrow(reqs()) })
    n_growing   <- shiny$reactive({
      d <- queue(); if (nrow(d) == 0) 0L else
        sum(d$state_code %in% c("established", "updated"))
    })
    n_catalog   <- shiny$reactive({ nrow(catalog()) })
    # Material sitting finished with nothing asking for it. Not an error - a
    # consignment can be complete - but it is the number that says whether this
    # bench should be growing more or whether the next bench has stopped
    # collecting.
    n_uncollected <- shiny$reactive({
      d <- catalog()
      if (nrow(d) == 0) return(0L)
      sum(as.integer(d$open_requests) == 0, na.rm = TRUE)
    })
    n_catalog <- shiny$reactive({ nrow(catalog()) })
    # Catalogued AND with somewhere to go. A batch nobody can collect is not
    # waiting on this bench, so it must not pulse the tab as if it were.
    n_ready_to_move <- shiny$reactive({
      d <- catalog()
      if (nrow(d) == 0) return(0L)
      sum(!is.na(d$destination_labels) & nzchar(d$destination_labels %||% ""))
    })
    n_finished  <- shiny$reactive({
      d <- queue(); if (nrow(d) == 0) 0L else
        sum(d$state_code %in% c("completed", "rejected", "depleted"))
    })
    # Material grown but not yet promised to anything. The number that says
    # whether this bench has actually delivered, as opposed to merely produced.
    n_uncommitted <- shiny$reactive({
      d <- queue()
      if (nrow(d) == 0) return(0L)
      sum(as.integer(d$unallocated_units)[d$state_code %in% c("established", "updated")],
          na.rm = TRUE)
    })
    
    output$kpis <- shiny$renderUI({
      order_theme$flow_stepper(list(
        list(title = "Available", sub = "cleared material",
             count = n_available(), unit = "orders", value = "available",
             active = identical(vstate(), "available"),
             waiting = n_available() > 0),
        list(title = "Requests", sub = "other benches want material",
             count = n_requests(), unit = "to handle", value = "requests",
             active = identical(vstate(), "requests"),
             waiting = n_requests() > 0),
        list(title = "Growing", sub = "on this bench",
             count = n_growing(), unit = "batches", value = "growing",
             active = identical(vstate(), "growing")),
        list(title = "Uncommitted", sub = "grown, not yet allocated",
             count = n_uncommitted(), unit = "units", value = "allocate",
             active = identical(vstate(), "allocate"),
             waiting = n_uncommitted() > 0),
        list(title = "Catalog", sub = "finished, ready to move on",
             count = n_catalog(), unit = "batches", value = "catalog",
             active = identical(vstate(), "catalog")),
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
          shiny$strong(sprintf("%d units", owed)),
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
                                 if (n_available() == 1) " consignment has sterilized material ready. "
                                 else " consignments have sterilized material ready. ",
                                 "Requesting asks surface sterilization, and a technician there chooses ",
                                 "which batch to send.")
             else
               order_theme$guide(
                 "Nothing is ready. Material appears here once surface sterilization ",
                 "completes a batch that has explants to spare."),
             requests = if (n_requests() > 0)
               order_theme$guide(tone = "do",
                                 "Authorise a request, then enter the batch you are actually sending.")
             else order_theme$guide("No bench has asked for material from here."),
             growing = if (n_growing() > 0)
               order_theme$guide(tone = "do",
                                 "Record each multiplication cycle as you do it, and log losses as they ",
                                 "happen. A batch is completed once its material has been committed.")
             else order_theme$guide("No batch is growing. Request material from the Available tab."),
             allocate = if (n_uncommitted() > 0)
               order_theme$guide(tone = "do",
                                 shiny$strong(n_uncommitted()),
                                 if (n_uncommitted() == 1) " unit has been grown but not promised to anything. "
                                 else " units have been grown but not promised to anything. ",
                                 "Allocating commits material to a service line the order asked for \u2014 ",
                                 "an order wanting vines for sale and plantlets for conservation is two ",
                                 "separate promises, and they are allocated separately.")
             else order_theme$guide("Everything grown has been committed."),
             catalog = if (n_catalog() > 0)
               order_theme$guide(tone = "do",
                                 shiny$strong(n_catalog()),
                                 if (n_catalog() == 1) " finished batch is standing here. "
                                 else " finished batches are standing here. ",
                                 "Each row says what its material is promised to and which bench may ",
                                 "draw it. A batch can also be grown further \u2014 starting a new culture ",
                                 "from one splits off fresh stock without touching what is committed.",
                                 if (n_uncollected() > 0) shiny$tagList(" ",
                                                                        shiny$strong(n_uncollected()),
                                                                        if (n_uncollected() == 1) " has nothing asking for it yet."
                                                                        else " have nothing asking for them yet.") else NULL)
             else
               order_theme$guide(
                 "Nothing finished yet. A batch joins the catalog once it is completed ",
                 "and still holds material."),
             finished = order_theme$guide(
               "Batches that have left this bench, and batches that ended here."))
    })
    
    # ---- the three lists ----------------------------------------------
    output$list_pane <- shiny$renderUI({
      switch(vstate(),
             available = order_theme$table_card(reactableOutput(ns("stock_tbl"))),
             requests  = order_theme$table_card(reactableOutput(ns("req_tbl"))),
             catalog   = order_theme$table_card(reactableOutput(ns("cat_tbl"))),
             order_theme$table_card(reactableOutput(ns("bench_tbl"))))
    })
    
    output$stock_tbl <- renderReactable({
      d <- stock_rows()
      keep <- c("order_number", "customer_name", "crop_name", "variety_name",
                "units_available", "still_needed", "act")
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
          requested = colDef(show = FALSE), on_bench = colDef(show = FALSE),
          
          order_number = colDef(name = "ORDER", minWidth = 160,
                                cell = function(v) shiny$tags$strong(v)),
          customer_name = colDef(name = "CUSTOMER", minWidth = 140),
          crop_name = colDef(name = "CROP", width = 95),
          variety_name = colDef(name = "VARIETY", minWidth = 110,
                                cell = function(v) if (is.na(v)) "\u2014" else v),
          units_available = colDef(name = "READY", width = 110, cell = function(v, i) {
            n <- if (is.na(v)) 0L else as.integer(v)
            shiny$tagList(
              order_theme$chip(sprintf("%d units", n), if (n > 0) "teal" else "ink"),
              shiny$tags$small(class = "wl-meta-note",
                               sprintf("%d batch%s", as.integer(d$batches_available[i]),
                                       if (identical(as.integer(d$batches_available[i]), 1L)) "" else "es")))
          }),
          # What the ORDER still owes, not what this bench holds. It is the
          # reason to start another batch, and it comes from the same
          # arithmetic the order screen uses, so the two cannot disagree.
          still_needed = colDef(name = "STILL OWED", width = 120, cell = function(v) {
            n <- if (is.na(v)) 0L else as.integer(v)
            if (n == 0) order_theme$chip("met", "brand")
            else order_theme$chip(sprintf("%d units", n), "amber")
          }),
          act = colDef(name = "", width = 175, sortable = FALSE, cell = function(v, i) {
            if (as.integer(d$requested[i]) > 0) order_theme$chip("Requested", "amber")
            else if (is.na(d$units_available[i]) || as.integer(d$units_available[i]) == 0)
              order_theme$chip("Nothing ready", "ink")
            else
              shiny$tags$button(class = "btn btn-outline-success btn-sm", type = "button",
                                onclick = sprintf(
                                  "Shiny.setInputValue('%s', {code: '%s', n: Math.random()})",
                                  ns("act_stock"), d$row_id[i]),
                                "Request material")
          })
        ), keep), d),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "Nothing is ready. Surface sterilization releases material when a batch completes."),
        theme = order_theme$rt_theme())
    })
    
    output$bench_tbl <- renderReactable({
      d <- bench_rows()
      keep <- c("sample_code", "order_number", "last_cycle_no", "units_held",
                "unallocated_units", "service_label", "state_label", "act")
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick_bench"), "sample_code")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(sel_bench(), "sample_code")),
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          state_code = colDef(show = FALSE), since = colDef(show = FALSE),
          parent_sample_code = colDef(show = FALSE),
          order_service_id = colDef(show = FALSE), service_code = colDef(show = FALSE),
          n_cycles = colDef(show = FALSE), medium = colDef(show = FALSE),
          vessel = colDef(show = FALSE), subcultured_on = colDef(show = FALSE),
          last_copies_out = colDef(show = FALSE), cycle_recorded = colDef(show = FALSE),
          ledger_count = colDef(show = FALSE), n_contaminated = colDef(show = FALSE),
          n_dead = colDef(show = FALSE), n_discarded = colDef(show = FALSE),
          n_allocated = colDef(show = FALSE), drift = colDef(show = FALSE),
          customer_name = colDef(show = FALSE), crop_name = colDef(show = FALSE),
          variety_name = colDef(show = FALSE),
          
          sample_code = colDef(name = "BATCH", minWidth = 120,
                               cell = function(v) shiny$tags$strong(v)),
          order_number = colDef(name = "ORDER", minWidth = 155),
          # Passages, not cycles-completed. A lab watching for somaclonal drift
          # cares how many times this tissue has been cut, and that number only
          # ever goes up.
          last_cycle_no = colDef(name = "PASSAGE", width = 95, cell = function(v) {
            if (is.na(v)) order_theme$chip("none yet", "ink")
            else order_theme$chip(sprintf("#%d", as.integer(v)), "ink")
          }),
          units_held = colDef(name = "HOLDING", width = 130, cell = function(v, i) {
            n <- if (is.na(v)) 0L else as.integer(v)
            lost <- as.integer(d$n_contaminated[i]) + as.integer(d$n_dead[i]) +
              as.integer(d$n_discarded[i])
            shiny$tagList(
              order_theme$chip(as.character(n), if (n > 0) "brand" else "amber"),
              if (!is.na(lost) && lost > 0)
                shiny$tags$small(class = "wl-meta-note", sprintf("%d lost", lost)) else NULL)
          }),
          unallocated_units = colDef(name = "UNCOMMITTED", width = 130,
                                     cell = function(v) {
                                       n <- if (is.na(v)) 0L else as.integer(v)
                                       if (n == 0) order_theme$chip("all committed", "brand")
                                       else order_theme$chip(sprintf("%d units", n), "amber")
                                     }),
          # What this batch has been promised to. NULL is not a gap in the
          # data: it means shared stock nobody has committed yet, which is the
          # normal state of material until allocation.
          service_label = colDef(name = "PROMISED TO", minWidth = 160,
                                 cell = function(v, i) {
                                   if (is.na(v)) order_theme$chip("shared stock", "ink")
                                   else order_theme$chip(v, "teal")
                                 }),
          state_label = colDef(name = "STATE", width = 125, cell = function(v, i) {
            shiny$tagList(
              order_theme$chip(v %||% "", state_tone(d$state_code[i])),
              if (isTRUE(d$drift[i]) && !(d$state_code[i] %in% c("completed", "rejected", "depleted")))
                order_theme$chip("count drift", "amber") else NULL)
          }),
          act = colDef(name = "", width = 150, sortable = FALSE, cell = function(v, i) {
            st <- d$state_code[i]
            if (st %in% c("completed", "rejected", "depleted")) ""
            else if (!isTRUE(d$cycle_recorded[i])) order_theme$chip("Needs a cycle", "amber")
            else if (as.integer(d$unallocated_units[i]) > 0)
              order_theme$chip("To allocate", "amber")
            else order_theme$chip("Ready to close", "teal")
          })
        ), keep), d),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "No batch here. Request cleared material from the Available tab."),
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
          to_stage_label = colDef(name = "NEEDED AT", minWidth = 155,
                                  cell = function(v) order_theme$chip(v, "amber")),
          reason = colDef(name = "WHY", minWidth = 190),
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
                                       order_theme$chip(sprintf("%d units", n), if (n > 0) "amber" else "brand"),
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
        language = order_theme$rt_lang("No bench has asked for material from here."),
        theme = order_theme$rt_theme())
    })
    
    # ---- the catalog ---------------------------------------------------
    output$cat_tbl <- renderReactable({
      d <- catalog()
      if (nrow(d) > 0 || !is.null(d$sample_code)) d$act <- rep(NA_character_, nrow(d))
      keep <- c("sample_code", "order_number", "units_held", "uncommitted",
                "purpose", "destination_labels", "open_requests", "act")
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick_cat"), "sample_code")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(sel_cat(), "sample_code")),
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          stage_code = colDef(show = FALSE), bench = colDef(show = FALSE),
          completed_on = colDef(show = FALSE),
          parent_sample_code = colDef(show = FALSE),
          n_allocated = colDef(show = FALSE), is_committed = colDef(show = FALSE),
          customer_name = colDef(show = FALSE), crop_name = colDef(show = FALSE),
          variety_name = colDef(show = FALSE),
          
          sample_code = colDef(name = "BATCH", minWidth = 120,
                               cell = function(v) shiny$tags$strong(v)),
          order_number = colDef(name = "ORDER", minWidth = 150),
          units_held = colDef(name = "HOLDING", width = 100),
          # Committed material is spoken for. What is left is what this bench
          # may still promise, or grow from - and it is the number that decides
          # whether starting a new culture is possible at all.
          uncommitted = colDef(name = "FREE", width = 110, cell = function(v, i) {
            n <- if (is.na(v)) 0L else as.integer(v)
            shiny$tagList(
              order_theme$chip(sprintf("%d", n), if (n > 0) "amber" else "brand"),
              if (isTRUE(d$is_committed[i]))
                shiny$tags$small(class = "wl-meta-note",
                                 sprintf("%d promised", as.integer(d$n_allocated[i])))
              else shiny$tags$small(class = "wl-meta-note", "shared stock"))
          }),
          # WHAT the material is for. For an uncommitted batch this is what the
          # order still owes, so the catalog is useful before allocation as
          # well as after it.
          purpose = colDef(name = "FOR", minWidth = 200,
                           cell = function(v) if (is.na(v)) "\u2014" else v),
          # WHICH bench may take it, from tbl_service_route - so this bench,
          # hardening, conservation and distribution all read one answer.
          destination_labels = colDef(name = "GOES TO", minWidth = 175,
                                      cell = function(v) {
                                        if (is.na(v)) return(order_theme$chip("no route set", "ink"))
                                        shiny$div(lapply(strsplit(v, ", ")[[1]],
                                                         function(x) order_theme$chip(x, "teal")))
                                      }),
          open_requests = colDef(name = "ASKED FOR", width = 115,
                                 cell = function(v) {
                                   n <- if (is.na(v)) 0L else as.integer(v)
                                   if (n == 0) order_theme$chip("nobody yet", "ink")
                                   else order_theme$chip(sprintf("%d open", n), "amber")
                                 }),
          act = colDef(name = "", width = 165, sortable = FALSE,
                       cell = function(v, i) {
                         if (as.integer(d$uncommitted[i]) > RESERVE)
                           shiny$tags$button(class = "btn btn-outline-success btn-sm", type = "button",
                                             onclick = sprintf(
                                               "Shiny.setInputValue('%s', {code: '%s', n: Math.random()})",
                                               ns("act_grow"), d$sample_code[i]),
                                             "Grow more")
                         else order_theme$chip("at the reserve", "ink")
                       })
        ), keep), d),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "Nothing finished yet. A batch joins the catalog when it is completed and still holds material."),
        theme = order_theme$rt_theme())
    })
    
    # Shared by both detail panels (bench and catalog) - whichever selection
    # is live at the moment decides which batch's passages are shown. sel_bench
    # and sel_cat are mutually exclusive (clear_sel() enforces it), so at most
    # one of these ever supplies a sample_code at a time.
    output$cycle_hist_tbl <- renderReactable({
      sc <- if (!is.null(bench_row())) bench_row()$sample_code[1]
      else if (!is.null(cat_row())) cat_row()$sample_code[1]
      else return(NULL)
      d <- subculture_cycles(sc)
      keep <- c("cycle_no", "subcultured_on", "medium", "vessel",
                "explants_in", "copies_out", "net")
      reactable(
        d,
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          notes = colDef(show = FALSE), recorded_by = colDef(show = FALSE),
          recorded_on = colDef(show = FALSE),
          
          cycle_no = colDef(name = "PASSAGE", width = 90,
                            cell = function(v) sprintf("#%d", as.integer(v))),
          subcultured_on = colDef(name = "DATE", width = 110,
                                  cell = function(v) if (is.na(v)) "\u2014" else format(v, "%d %b %Y")),
          medium = colDef(name = "MEDIUM", minWidth = 150,
                          cell = function(v) if (is.na(v) || !nzchar(v)) "\u2014" else v),
          vessel = colDef(name = "VESSEL", width = 100,
                          cell = function(v) if (is.na(v) || !nzchar(v)) "\u2014" else v),
          explants_in = colDef(name = "IN", width = 70),
          copies_out = colDef(name = "OUT", width = 70),
          # The multiplication factor can be derived from in/out; whether this
          # passage grew or shrank the line is the one fact worth a glance
          # without doing that arithmetic.
          net = colDef(name = "NET", width = 90, cell = function(v) {
            n <- as.integer(v)
            order_theme$chip(sprintf("%+d", n), if (n > 0) "brand" else if (n < 0) "amber" else "ink")
          })
        ), keep), d),
        defaultPageSize = 6, compact = TRUE,
        language = order_theme$rt_lang("No cycle recorded yet."),
        theme = order_theme$rt_theme())
    })
    
    output$cat_detail <- shiny$renderUI({
      x <- cat_row(); if (is.null(x)) return(NULL)
      sc <- x$sample_code[1]
      free <- as.integer(x$uncommitted[1] %||% 0L)
      spare <- max(free - RESERVE, 0L)
      dm <- subculture_demand(x$order_number[1])
      open_lines <- if (nrow(dm) == 0) dm else dm[as.integer(dm$remaining_qty) > 0, , drop = FALSE]
      owed <- if (nrow(open_lines) == 0) 0L else sum(as.integer(open_lines$remaining_qty))
      
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = sc,
                  sub = sprintf("%s \u00b7 %s", x$order_number[1], x$customer_name[1] %||% ""),
                  close_input = ns("detail_close")),
                
                shiny$div(class = "wl-statusbar",
                          order_theme$chip("Finished", "brand"),
                          order_theme$chip(sprintf("%d holding", as.integer(x$units_held[1])), "ink"),
                          if (isTRUE(x$is_committed[1]))
                            order_theme$chip(sprintf("%d promised", as.integer(x$n_allocated[1])), "teal")
                          else order_theme$chip("shared stock", "ink")),
                
                order_theme$prop_grid(
                  order_theme$prop("For", x$purpose[1] %||% "\u2014"),
                  order_theme$prop("Goes to", x$destination_labels[1] %||% "no route set"),
                  order_theme$prop("Free to commit", as.character(free)),
                  order_theme$prop("Asked for by",
                                   if (as.integer(x$open_requests[1]) == 0) "nobody yet"
                                   else sprintf("%d open request(s)", as.integer(x$open_requests[1])))),
                
                order_theme$subhead("Cycle history"),
                order_theme$table_card(reactableOutput(ns("cycle_hist_tbl"))),
                
                # ---- grow more, on demand ----
                order_theme$subhead("Start a new culture from this batch"),
                shiny$div(class = "update-hint",
                          "Splits fresh stock off this batch as a new line of its own, which can ",
                          "then be cycled and committed independently. Committed material is never ",
                          "touched, and the working reserve of ", shiny$strong(RESERVE),
                          " stays behind."),
                if (owed > 0)
                  order_theme$guide(tone = "do",
                                    "This order still owes ", shiny$strong(owed),
                                    " unit(s) across ", nrow(open_lines),
                                    if (nrow(open_lines) == 1) " service line." else " service lines.")
                else
                  order_theme$guide(
                    "Every fulfilment line on this order is met. Growing more is still allowed \u2014 ",
                    "it becomes stock for a later order \u2014 but nothing is waiting on it."),
                if (spare <= 0)
                  order_theme$guide(tone = "do",
                                    "Nothing can be split off: ", shiny$strong(free),
                                    " free and ", shiny$strong(RESERVE), " must stay as working stock. ",
                                    "Cycle this batch on the Growing tab first.")
                else shiny$tagList(
                  shiny$numericInput(ns("grow_n"),
                                     sprintf("Units for the new culture (%d can be split)", spare),
                                     value = min(spare, max(1L, owed)), min = 1, max = spare),
                  shiny$textInput(ns("grow_medium"), "Medium for the new line", width = "100%",
                                  placeholder = "e.g. MS + 0.5 mg/L BAP"),
                  shiny$textAreaInput(ns("grow_notes"), "Notes", width = "100%"),
                  order_theme$detail_actions(
                    shiny$actionButton(ns("do_grow"), "Start new culture",
                                       class = "btn btn-success"),
                    shiny$actionButton(ns("detail_close"), "Cancel",
                                       class = "btn btn-sm btn-outline-secondary")))
      )
    })
    
    # Splitting a batch, not multiplying one. A cycle raises the count of an
    # existing line; this creates a NEW line with its own code, its own cycles
    # and its own allocations - which is what lets one finished batch answer
    # demand that arrives later without disturbing what it already owes.
    do_grow <- function(x) {
      if (is.null(x) || nrow(x) == 0) return(invisible(FALSE))
      sc <- fld(x, "sample_code", "")
      free <- as.integer(fld(x, "uncommitted", 0L))
      n <- suppressWarnings(as.integer(shiny$isolate(input$grow_n)))
      if (length(n) == 0 || is.na(n) || n < 1) n <- 1L
      if (n > max(free - RESERVE, 0L)) {
        toastr_error(sprintf("%s can spare %d unit(s): %d are free and %d must stay.",
                             sc, max(free - RESERVE, 0L), free, RESERVE),
                     title = "More than can be split"); return()
      }
      child <- NA_character_
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # Guarded on UNCOMMITTED units, not on quantity. Splitting on the raw
          # count would let a batch give away material it has already promised
          # to a service line, and the shortfall would only surface at dispatch.
          # The reserve sits ON TOP of what is already promised, not alongside
          # it. Two separate minimums - remainder >= reserve AND remainder >=
          # committed - reduce to the larger of the two, so a batch holding 40
          # with 30 committed would let 10 go and leave exactly the 30 that are
          # spoken for. The moment those 30 ship, the line is empty and there
          # is no working stock to grow from, which is the one thing the
          # reserve exists to prevent.
          moved <- dbExecute(conn, "
            UPDATE tbl_sample s SET quantity = s.quantity - $2
             WHERE s.sample_code = $1
               AND s.quantity - $2 >= $3 + COALESCE(
                     (SELECT sum(a.qty) FROM tbl_service_allocation a
                       WHERE a.sample_code = s.sample_code), 0)",
                             params = list(sc, n, RESERVE))
          if (moved == 0)
            stop(sc, " cannot spare ", n, " unit(s) once its commitments and the ",
                 "reserve of ", RESERVE, " are accounted for. Refresh and try again.",
                 call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_culture_count
              (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'subculture','discarded',$2,$3,$4)",
                    params = list(sc, -n, "split into a new culture", user()))
          nc <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                           params = list(CHILD_PREFIX))$code[1]
          # order_service_id stays NULL. The new line is fresh shared stock and
          # can answer any line the order owes; inheriting the parent's
          # commitment would pre-decide that and let one promise be counted on
          # two batches.
          dbExecute(conn, "
            INSERT INTO tbl_sample (sample_code, order_number, parent_sample_code,
                                    stage_code, quantity, created_by, created_on)
            VALUES ($1,$2,$3,'subculture',$4,$5,now())",
                    params = list(nc, fld(x, "order_number", ""), sc, n, user()))
          dbExecute(conn, "
            INSERT INTO tbl_culture_count
              (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'subculture','initial',$2,$3,$4)",
                    params = list(nc, n, sprintf("split from %s", sc), user()))
          med <- shiny$isolate(input$grow_medium)
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, nc)
          record_event(conn, nc, "subculture", "established", user(),
                       wf = wf, ctx = ctx,
                       notes = sprintf("split from %s: %d unit(s)%s", sc, n,
                                       if (nzchar(trimws(med %||% "")))
                                         sprintf(" on %s", trimws(med)) else ""))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'subculture', 'new culture started', $2, $3)",
                    params = list(fld(x, "order_number", ""), user(),
                                  sprintf("%s split from %s, %d unit(s)", nc, sc, n)))
          child <<- nc
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not start", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(code = child, title = "SUBCULTURE LINE",
                                 line1 = sprintf("split from %s", sc),
                                 line2 = format(Sys.Date(), "%d %b %Y"),
                                 stringsAsFactors = FALSE))
        toastr_success(sprintf("%s started from %s with %d unit(s).", child, sc, n),
                       title = "New culture")
        clear_sel(); self_refresh(); signal_others()
      }
    }
    
    shiny$observeEvent(input$do_grow, { do_grow(cat_row()) })
    shiny$observeEvent(input$act_grow, {
      d <- catalog(); if (nrow(d) == 0) return()
      x <- d[d$sample_code == input$act_grow$code, , drop = FALSE]
      if (nrow(x) == 0) return()
      clear_sel(); sel_cat(input$act_grow$code)
    })
    
    # ---- selection ----------------------------------------------------
    sel_stock <- shiny$reactiveVal(NULL)
    sel_bench <- shiny$reactiveVal(NULL)
    sel_req   <- shiny$reactiveVal(NULL)
    sel_cat   <- shiny$reactiveVal(NULL)
    
    clear_sel <- function() {
      sel_stock(NULL); sel_bench(NULL); sel_req(NULL); sel_cat(NULL)
    }
    shiny$observeEvent(input$pick_stock, { clear_sel(); sel_stock(input$pick_stock$code) })
    shiny$observeEvent(input$pick_bench, { clear_sel(); sel_bench(input$pick_bench$code) })
    shiny$observeEvent(input$pick_req,   { clear_sel(); sel_req(input$pick_req$code) })
    shiny$observeEvent(input$pick_cat,   { clear_sel(); sel_cat(input$pick_cat$code) })
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
    cat_row <- shiny$reactive({
      sc <- sel_cat(); if (is.null(sc)) return(NULL)
      d <- catalog(); if (nrow(d) == 0) return(NULL)
      x <- d[d$sample_code == sc, , drop = FALSE]
      if (nrow(x) == 0) NULL else x
    })
    
    shiny$observeEvent(input$scan_go, {
      code <- shiny$isolate(input$scan)
      if (is.null(code) || !nzchar(code)) return()
      d <- queue()
      if (nrow(d) > 0 && code %in% d$sample_code) { clear_sel(); sel_bench(code) }
      else toastr_warning(sprintf("%s is not a batch on this bench.", code),
                          title = "Not found")
    })
    
    output$detail <- shiny$renderUI({
      if (!is.null(stock_row())) return(shiny$uiOutput(ns("stock_detail")))
      if (!is.null(req_row()))   return(shiny$uiOutput(ns("req_detail")))
      if (!is.null(cat_row()))   return(shiny$uiOutput(ns("cat_detail")))
      if (!is.null(bench_row())) return(shiny$uiOutput(ns("bench_detail")))
      shiny$div(class = "wl-detail-inner",
                shiny$div(class = "wl-empty",
                          shiny$div(class = "wl-empty-ico", shiny$icon("hand-pointer")),
                          shiny$div(class = "wl-empty-title", "Select a row to begin"),
                          shiny$div(class = "wl-empty-body",
                                    "Choose a consignment to request material, a batch to ",
                                    "record a cycle or commit its material, or a request ",
                                    "from another bench to fill.")))
    })
    
    # ---- detail: available material -----------------------------------
    output$stock_detail <- shiny$renderUI({
      x <- stock_row(); if (is.null(x)) return(NULL)
      dm <- subculture_demand(x$order_number[1])
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = x$order_number[1],
                  sub = sprintf("%s \u00b7 %s", x$customer_name[1] %||% "", x$source_bench[1]),
                  close_input = ns("detail_close")),
                order_theme$section(
                  "\u2192", "Request material for multiplication",
                  sub = x$source_bench[1],
                  order_theme$guide(
                    "Asks ", shiny$strong(x$source_bench[1]), " for material from this ",
                    "consignment. A technician there chooses which batch to send \u2014 this ",
                    "bench does not cut its own material."),
                  order_theme$prop_grid(
                    order_theme$prop("Customer", x$customer_name[1]),
                    order_theme$prop("Crop", x$crop_name[1] %||% "\u2014"),
                    order_theme$prop("Variety", x$variety_name[1] %||% "\u2014"),
                    order_theme$prop("Batches ready", as.character(x$batches_available[1])),
                    order_theme$prop("Units ready", as.character(x$units_available[1])),
                    order_theme$prop("Already here", as.character(x$on_bench[1]))),
                  
                  # What the order actually asked for, line by line. Requesting more
                  # material without seeing this is how a bench multiplies past the
                  # target for one service while another goes unfilled.
                  order_theme$subhead("What this order still owes"),
                  if (nrow(dm) == 0)
                    order_theme$guide("This order has no fulfilment services on it.")
                  else
                    shiny$div(class = "svc-lines", lapply(seq_len(nrow(dm)), function(i)
                      order_theme$service_line(
                        label     = dm$service_label[i],
                        sub       = if (is.na(dm$recipient[i])) "" else dm$recipient[i],
                        fulfilled = as.integer(dm$fulfilled_qty[i]),
                        target    = as.integer(dm$target_qty[i]),
                        pct       = as.integer(dm$pct_complete[i]),
                        status    = dm$status[i]))),
                  
                  shiny$numericInput(ns("req_qty_wanted"), "Units needed",
                                     value = 1, min = 1, width = "240px"),
                  order_theme$guide(
                    "The holding bench sends what is ready and the request stays open for the ",
                    "rest, so ask for what you actually need rather than what you think they ",
                    "can spare today."),
                  shiny$textAreaInput(ns("req_reason"), "Why this is needed", width = "100%",
                                      value = "material needed for multiplication"),
                  order_theme$detail_actions(
                    shiny$actionButton(ns("do_request"), "Request material",
                                       class = "btn btn-success"),
                    shiny$actionButton(ns("detail_close"), "Cancel",
                                       class = "btn btn-sm btn-outline-secondary")))
      )
    })
    
    # ---- detail: a batch on the bench ---------------------------------
    output$bench_detail <- shiny$renderUI({
      x <- bench_row(); if (is.null(x)) return(NULL)
      sc <- x$sample_code[1]
      st <- x$state_code[1]
      held <- as.integer(x$units_held[1] %||% 0L)
      free <- as.integer(x$unallocated_units[1] %||% 0L)
      next_cycle <- if (is.na(x$last_cycle_no[1])) 1L else as.integer(x$last_cycle_no[1]) + 1L
      dm <- subculture_demand(x$order_number[1])
      open_lines <- if (nrow(dm) == 0) dm else dm[as.integer(dm$remaining_qty) > 0, , drop = FALSE]
      
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = sc,
                  sub = sprintf("%s \u00b7 %s", x$order_number[1], x$customer_name[1] %||% ""),
                  close_input = ns("detail_close")),
                
                shiny$div(class = "wl-statusbar",
                          order_theme$chip(x$state_label[1] %||% "", state_tone(st)),
                          order_theme$chip(sprintf("from %s", x$parent_sample_code[1]), "ink"),
                          if (!is.na(x$last_cycle_no[1]))
                            order_theme$chip(sprintf("passage %d", as.integer(x$last_cycle_no[1])), "ink")
                          else NULL,
                          if (isTRUE(x$drift[1]) && !(st %in% c("completed", "rejected", "depleted")))
                            order_theme$chip("count drift", "amber") else NULL),
                
                # Every passage this line has been through, not just the latest -
                # the number a lab watching for somaclonal drift actually needs.
                order_theme$subhead("Cycle history"),
                order_theme$table_card(reactableOutput(ns("cycle_hist_tbl"))),
                
                if (!(st %in% c("completed", "rejected", "depleted"))) shiny$tagList(
                  # ---- 1. a multiplication cycle ----
                  order_theme$subhead(sprintf("1. Record multiplication cycle #%d", next_cycle)),
                  shiny$div(class = "update-hint",
                            "Each passage is recorded in its own right, so the number of times this ",
                            "tissue has been cut is never lost. Explants in and copies out are both ",
                            "kept \u2014 the multiplication factor can be derived from them, but they ",
                            "cannot be recovered from it."),
                  shiny$textInput(ns("medium"),
                                  shiny$HTML("Medium <span class='mandatory_star'>*</span>"),
                                  width = "100%", value = x$medium[1] %||% "",
                                  placeholder = "e.g. MS + 0.5 mg/L BAP"),
                  shiny$textInput(ns("vessel"), "Vessel", width = "240px",
                                  value = x$vessel[1] %||% ""),
                  shiny$dateInput(ns("cycled_on"), "Date subcultured", value = Sys.Date()),
                  shiny$numericInput(ns("explants_in"),
                                     sprintf("Explants used (%d holding)", held),
                                     value = min(held, max(1L, held)), min = 1),
                  shiny$numericInput(ns("copies_out"), "Copies obtained", value = 1, min = 1),
                  shiny$textAreaInput(ns("cycle_notes"), "Notes", width = "100%"),
                  order_theme$detail_actions(
                    shiny$actionButton(ns("save_cycle"), "Record cycle",
                                       class = "btn btn-success")),
                  
                  # ---- 2. losses ----
                  order_theme$subhead("2. Log a loss"),
                  shiny$div(class = "update-hint",
                            "Each entry is kept, with its reason, and the batch count moves with it."),
                  shiny$selectInput(ns("loss_reason"), "What happened",
                                    choices = LOSS_REASONS, width = "240px"),
                  shiny$numericInput(ns("loss_n"), sprintf("How many (%d holding)", held),
                                     value = 1, min = 1, max = max(1L, held)),
                  shiny$uiOutput(ns("loss_warn")),
                  order_theme$detail_actions(
                    shiny$actionButton(ns("log_loss"), "Log loss",
                                       class = "btn btn-outline-secondary")),
                  
                  # ---- 3. allocation ----
                  order_theme$subhead("3. Commit material to a service"),
                  shiny$div(class = "update-hint",
                            "Allocating promises material to one line of this order. An order ",
                            "wanting vines for sale and plantlets for conservation is two separate ",
                            "promises, and each is allocated against on its own."),
                  if (nrow(open_lines) == 0)
                    order_theme$guide(
                      "Every fulfilment line on this order is met. Nothing further to commit.")
                  else if (free <= 0)
                    order_theme$guide(
                      "Every unit in this batch is already committed. Record another cycle to ",
                      "grow more.")
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
                                       sprintf("Units to commit (%d uncommitted)", free),
                                       value = 1, min = 1, max = free),
                    order_theme$detail_actions(
                      shiny$actionButton(ns("do_allocate"), "Commit to this service",
                                         class = "btn btn-success"))),
                  
                  # ---- 4. close the batch ----
                  order_theme$subhead("4. Close the batch"),
                  if (!isTRUE(x$cycle_recorded[1]))
                    order_theme$guide("Record at least one cycle before closing.")
                  else if (as.integer(x$n_allocated[1]) == 0)
                    order_theme$guide(
                      "Nothing from this batch has been committed yet. Completing it now would ",
                      "send material onward with no service line to answer, so allocate first.")
                  else shiny$tagList(
                    shiny$div(class = "update-hint",
                              "Completing releases the batch onward \u2014 to conservation, ",
                              "distribution or hardening, whichever the committed services call ",
                              "for."),
                    shiny$textAreaInput(ns("close_notes"), "Notes", width = "100%"),
                    order_theme$detail_actions(
                      shiny$actionButton(ns("reject"), "Reject",
                                         class = "btn btn-outline-secondary"),
                      shiny$actionButton(ns("complete"), "Complete batch",
                                         class = "btn btn-success")))
                ) else shiny$tagList(
                  order_theme$subhead("History"),
                  order_theme$prop_grid(
                    order_theme$prop("Passages", as.character(x$n_cycles[1])),
                    order_theme$prop("Medium", x$medium[1] %||% "\u2014"),
                    order_theme$prop("Holding",
                                     if (identical(st, "depleted"))
                                       sprintf("%d (before depletion)", held)
                                     else as.character(held)),
                    order_theme$prop("Committed", as.character(as.integer(x$n_allocated[1]))),
                    order_theme$prop("Promised to", x$service_label[1] %||% "shared stock"),
                    order_theme$prop("Lost",
                                     sprintf("%d contaminated \u00b7 %d dead \u00b7 %d discarded",
                                             as.integer(x$n_contaminated[1]),
                                             as.integer(x$n_dead[1]),
                                             as.integer(x$n_discarded[1])))),
                  if (identical(st, "completed")) shiny$div(class = "flow-cta ok",
                                                            shiny$span(class = "fc-ico", shiny$icon("circle-check")),
                                                            shiny$span("Complete. This batch has been released onward."))
                  else if (identical(st, "depleted")) shiny$div(class = "update-hint",
                                                                "No material remains in this batch. Its cycle history and ",
                                                                "the reason stay on record, and anything it was promised to ",
                                                                "has to be met from another batch.")
                  else NULL)
      )
    })
    
    # ---- detail: a request from another bench --------------------------
    output$req_detail <- shiny$renderUI({
      x <- req_row(); if (is.null(x)) return(NULL)
      pending <- identical(x$status[1], "pending")
      el <- eligible_batches()
      ch <- batch_choices(el)
      ok_codes <- if (length(ch)) el$sample_code[as.integer(el$units_held) > RESERVE] else character(0)
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
                      "Authorising allows material from this consignment to be taken. It cuts nothing."
                    else shiny$tagList(
                      "Choose the batch to draw from. ", shiny$strong(sug),
                      " is what was suggested; send whichever batch is healthiest.")),
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
                                                          as.integer(x$qty_requested[1] %||% 1L), "units")),
                    order_theme$prop("Already sent", sprintf("%d in %d deliver%s",
                                                             as.integer(x$qty_sent[1] %||% 0L),
                                                             as.integer(x$n_deliveries[1] %||% 0L),
                                                             if (identical(as.integer(x$n_deliveries[1] %||% 0L), 1L)) "y" else "ies")),
                    order_theme$prop("Still owed", sprintf("%d %s",
                                                           as.integer(x$qty_outstanding[1] %||% 0L), "units"))),
                  if (!pending) {
                    if (length(ch) == 0)
                      order_theme$guide(tone = "do",
                                        "No completed batch of this order is standing here with material to spare.")
                    # AT THE RESERVE AND SHORT. Sending is not the answer here and
                    # neither is refusing: the line has to be built back up first, and
                    # the bench that asked has to be able to see that is what is
                    # happening rather than watching a request sit untouched.
                    else if (length(ok_codes) == 0 &&
                             as.integer(x$qty_outstanding[1] %||% 0L) > 0)
                      shiny$tagList(
                        order_theme$guide(tone = "do",
                                          "Every batch of this order is at or below the working reserve of ",
                                          shiny$strong(RESERVE), ", and ",
                                          shiny$strong(as.integer(x$qty_outstanding[1] %||% 0L)),
                                          " are still owed. Start a culture cycle to build the line back up \u2014 ",
                                          "the request stays open, and ", x$to_stage_label[1],
                                          " can see that it is being grown rather than ignored."),
                        shiny$div(class = "svc-lines", lapply(seq_len(nrow(el)), function(i)
                          shiny$div(class = "wl-meta-note",
                                    sprintf("%s holds %d, %d spare", el$sample_code[i],
                                            as.integer(el$units_held[i]),
                                            max(as.integer(el$units_held[i]) - RESERVE, 0L))))),
                        order_theme$detail_actions(
                          shiny$actionButton(ns("go_multiply"), "Go to this batch and multiply",
                                             class = "btn btn-success")))
                    else shiny$tagList(
                      shiny$selectInput(ns("req_batch"), "Batch to draw from",
                                        choices = ch, selected = sel, width = "100%"),
                      shiny$numericInput(ns("req_qty"),
                                         sprintf("Units to send now (%d still owed)",
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
      if (length(q) == 0 || is.na(q[1])) 0L else max(q[1] - RESERVE, 0L)
    }
    
    batch_choices <- function(d) {
      if (!is.data.frame(d) || nrow(d) == 0) return(character(0))
      q <- as.integer(d$units_held); q[is.na(q)] <- 0L
      lab <- sprintf("%s  \u00b7  %d unit%s  \u00b7  %d spare%s", d$sample_code, q,
                     ifelse(q == 1, "", "s"), pmax(q - RESERVE, 0L),
                     ifelse(q <= RESERVE, "  \u2014 at the reserve, initiate a cycle", ""))
      setNames(d$sample_code, lab)
    }
    
    # ---- request material ----------------------------------------------
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
      if (is.null(why) || !nzchar(trimws(why))) why <- "material needed for multiplication"
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # Re-read availability inside the transaction: the row was rendered
          # from a snapshot, and another bench may have taken the last batch.
          still <- dbGetQuery(conn, "
            SELECT units_available, suggested_sample_code
            FROM view_subculture_stock WHERE order_number = $1",
                              params = list(order))
          if (nrow(still) == 0 || as.integer(still$units_available[1]) < 1)
            stop("No sterilized material from ", order, " is available any more.",
                 call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_sample_request
              (order_number, source_sample_code, to_stage, reason, requested_by,
               status, qty_requested)
            VALUES ($1, $2, 'subculture', $3, $4, 'pending', $5)",
                    params = list(order, still$suggested_sample_code[1], why, user(),
                                  want))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'subculture', 'material requested', $2, $3)",
                    params = list(order, user(), "requested from surface sterilization"))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Request failed", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success("Requested \u2014 surface sterilization will send the material.",
                       title = "Requested")
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
    
    # ---- record a multiplication cycle ---------------------------------
    shiny$observeEvent(input$save_cycle, {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      held <- as.integer(x$units_held[1] %||% 0L)
      medium <- trimws(input$medium %||% "")
      nin  <- suppressWarnings(as.integer(input$explants_in))
      nout <- suppressWarnings(as.integer(input$copies_out))
      cyc  <- if (is.na(x$last_cycle_no[1])) 1L else as.integer(x$last_cycle_no[1]) + 1L
      if (!nzchar(medium)) {
        toastr_error("Name the medium used.", title = "Missing"); return()
      }
      if (is.na(nin) || nin < 1 || is.na(nout) || nout < 1) {
        toastr_error("Explants used and copies obtained must both be at least one.",
                     title = "Invalid"); return()
      }
      if (nin > held) {
        toastr_error(sprintf("%s holds %d unit%s \u2014 you cannot use %d.",
                             sc, held, if (held == 1) "" else "s", nin),
                     title = "More than the batch holds"); return()
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          dbExecute(conn, "
            INSERT INTO tbl_subculture_detail
              (sample_code, cycle_no, medium, vessel, subcultured_on,
               explants_in, copies_out, notes, recorded_by)
            VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9)",
                    params = list(sc, cyc, medium, nz(input$vessel),
                                  as.character(input$cycled_on), nin, nout,
                                  nz(input$cycle_notes), user()))
          # OPEN the ledger on the first cycle, with what the batch actually
          # arrived holding.
          #
          # A batch is minted by the bench that draws it, and that bench writes
          # the draw against ITS OWN stage - so nothing had ever recorded the
          # arriving units on this one. The ledger then held only what cycles
          # added, and view_subculture_current compared it against a quantity
          # that included the arrival: every batch read `count drift` from its
          # first cycle onward, which is exactly how a real drift warning gets
          # learned as noise and ignored.
          #
          # Surface sterilization does not need this because its protocol form
          # counts the batch explicitly. Subculture has no such step - material
          # arrives and starts multiplying.
          if (cyc == 1L) {
            dbExecute(conn, "
              INSERT INTO tbl_culture_count (sample_code, stage_code, reason, delta, notes, recorded_by)
              SELECT $1,'subculture','initial', s.quantity, 'arrived on this bench', $2
              FROM tbl_sample s
              WHERE s.sample_code = $1 AND s.quantity > 0
                AND NOT EXISTS (SELECT 1 FROM tbl_culture_count k
                                 WHERE k.sample_code = $1
                                   AND k.stage_code = 'subculture'
                                   AND k.reason = 'initial')",
                      params = list(sc, user()))
          }
          # The NET movement, not the gross. A cycle consumes the explants it
          # cuts and yields the copies it produces; recording only the copies
          # would inflate the batch by the material that no longer exists.
          net <- nout - nin
          if (net != 0) {
            dbExecute(conn, "
              INSERT INTO tbl_culture_count (sample_code, stage_code, reason, delta, notes, recorded_by)
              VALUES ($1,'subculture',$2,$3,$4,$5)",
                      params = list(sc,
                                    if (net > 0) "multiplied" else "correction",
                                    net,
                                    sprintf("cycle %d: %d in, %d out", cyc, nin, nout),
                                    user()))
          }
          # Guarded, not read-then-write: two technicians recording cycles on
          # one batch would otherwise each read the old count and the second
          # write would undo the first.
          moved <- dbExecute(conn, "
            UPDATE tbl_sample SET quantity = quantity + $2
             WHERE sample_code = $1 AND quantity + $2 >= 0",
                             params = list(sc, net))
          if (moved == 0)
            stop(sc, " no longer holds ", nin, " unit(s). Refresh and try again.",
                 call. = FALSE)
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "subculture", "updated", user(),
                       wf = wf, ctx = ctx,
                       notes = sprintf("cycle %d on %s: %d in, %d out",
                                       cyc, medium, nin, nout))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not record", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success(sprintf("Cycle %d recorded for %s.", cyc, sc))
        self_refresh()
      }
    })
    
    # ---- log a loss -----------------------------------------------------
    output$loss_warn <- shiny$renderUI({
      x <- bench_row(); if (is.null(x)) return(NULL)
      held <- as.integer(x$units_held[1] %||% 0L)
      n <- suppressWarnings(as.integer(input$loss_n))
      if (is.na(n) || n < 1 || n != held) return(NULL)
      shiny$div(class = "update-hint",
                "Logging all ", held, " will close this batch as depleted. ",
                "Its cycle history and the reason stay on record.")
    })
    
    shiny$observeEvent(input$log_loss, {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      reason <- input$loss_reason %||% "contaminated"
      n <- suppressWarnings(as.integer(input$loss_n))
      held <- as.integer(x$units_held[1] %||% 0L)
      free <- as.integer(x$unallocated_units[1] %||% 0L)
      if (is.na(n) || n < 1) {
        toastr_error("How many were lost?", title = "Invalid"); return()
      }
      if (n > held) {
        toastr_error(sprintf("%s holds %d unit%s \u2014 you cannot lose %d.",
                             sc, held, if (held == 1) "" else "s", n),
                     title = "More than the batch holds"); return()
      }
      # A loss that eats into COMMITTED material is a promise the lab can no
      # longer keep from this batch, and saying so at the moment it happens is
      # the difference between a shortfall found here and one found at dispatch.
      if (n > free) {
        toastr_warning(sprintf(
          "%d of these were already committed to a service. That promise now has to be met from elsewhere.",
          n - free), title = "Committed material lost", timeOut = 0)
      }
      total_loss <- (n == held)
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # tbl_sample_quantity_check forbids zero, so a total loss must not
          # try to write it. The column stops moving here and keeps its last
          # true value - a historical fact, not a live count - exactly as it
          # already does once a batch is completed or rejected. This UPDATE
          # still guards against a race (someone else changing the count
          # between the read that produced `held` and this write); it just
          # matches on the value instead of changing it.
          moved <- if (total_loss) {
            dbExecute(conn, "
              UPDATE tbl_sample SET quantity = quantity
               WHERE sample_code = $1 AND quantity = $2",
                      params = list(sc, held))
          } else {
            dbExecute(conn, "
              UPDATE tbl_sample SET quantity = quantity - $2
               WHERE sample_code = $1 AND quantity - $2 >= 0",
                      params = list(sc, n))
          }
          if (moved == 0)
            stop(sc, " no longer holds ", n, " unit(s). Somebody else may have logged ",
                 "a loss. Refresh and try again.", call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_culture_count (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'subculture',$2,$3,$4,$5)",
                    params = list(sc, reason, -n, NA_character_, user()))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          # A batch with nothing left is DEPLETED, whatever it died of. Leaving
          # it in a working state left an empty batch in the queue with actions
          # that could not do anything.
          to_state <- if (total_loss) "depleted" else unname(LOSS_STATE[reason])
          if (is.null(to_state) || is.na(to_state)) to_state <- "updated"
          record_event(conn, sc, "subculture", to_state, user(),
                       wf = wf, ctx = ctx,
                       notes = sprintf("%d %s; %d remaining", n, reason, held - n))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not log", timeOut = 0); FALSE
      })
      if (ok) {
        if (total_loss) {
          toastr_warning(sprintf("%s closed as depleted \u2014 nothing survived.", sc),
                         title = "Batch depleted", timeOut = 0)
        } else {
          toastr_success(sprintf("%d %s logged for %s.", n, reason, sc))
        }
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
        toastr_error("How many units are you committing?", title = "Invalid"); return()
      }
      if (qty > free) {
        toastr_error(sprintf("%s has %d uncommitted unit%s \u2014 you cannot commit %d.",
                             sc, free, if (free == 1) "" else "s", qty),
                     title = "More than is free"); return()
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # Everything below is re-checked here rather than trusted from the
          # form. The screen was rendered from a snapshot and allocation is the
          # act that turns material into a promise to a customer - the one
          # place in this pipeline where being wrong is visible outside the lab.
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
            stop(sprintf("%s needs only %d more unit(s).", line$service_label[1],
                         as.integer(line$remaining_qty[1])), call. = FALSE)
          # Re-derive what is free from the ledger, not from the form.
          got <- dbGetQuery(conn, "
            SELECT unallocated_units FROM view_subculture_current WHERE sample_code = $1",
                            params = list(sc))
          if (nrow(got) == 0 || as.integer(got$unallocated_units[1]) < qty)
            stop(sc, " no longer has ", qty, " uncommitted unit(s). Refresh and try again.",
                 call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_service_allocation
              (order_service_id, sample_code, qty, allocated_by, notes)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(osid, sc, qty, user(),
                                  sprintf("allocated from subculture batch %s", sc)))
          # order_service_id on the SAMPLE answers a different question from the
          # allocation row: what is this material FOR. It is set once, on first
          # commitment, and left alone afterwards - a batch split across two
          # lines is described by its allocations, not by overwriting this.
          dbExecute(conn, "
            UPDATE tbl_sample SET order_service_id = $2
             WHERE sample_code = $1 AND order_service_id IS NULL",
                    params = list(sc, osid))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'subculture', 'material allocated', $2, $3)",
                    params = list(x$order_number[1], user(),
                                  sprintf("%d unit(s) from %s to %s", qty, sc,
                                          line$service_label[1])))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not allocate", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success(sprintf("%d unit(s) committed from %s.", qty, sc),
                       title = "Allocated")
        self_refresh(); signal_others()
      }
    })
    
    # ---- close the batch -------------------------------------------------
    close_batch <- function(decision, to_state, msg) {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      if (!is_admin()) {
        toastr_error("Only an administrator can close a batch.",
                     title = "Not permitted"); return()
      }
      note <- nz(input$close_notes)
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          dbExecute(conn, "
            INSERT INTO tbl_review (sample_code, stage_code, decision, comments, reviewed_by)
            VALUES ($1, 'subculture', $2, $3, $4)",
                    params = list(sc, decision, note, user()))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "subculture", to_state, user(),
                       wf = wf, ctx = ctx, notes = sprintf("review: %s", decision))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'subculture', $2, $3, $4)",
                    params = list(x$order_number[1], sprintf("batch %s", decision),
                                  user(), sc))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Failed", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success(sprintf(msg, sc))
        # subculture/completed is the workflow's one fan_out: several benches at
        # once, whichever the order's services call for. Naming them is more
        # use than a bare confirmation, because nobody has to go looking.
        if (identical(to_state, "completed")) {
          opts <- tryCatch({
            wf <- workflow_cache(WF_PATH, pool)
            o <- next_options(wf, "subculture", "completed",
                              sample_context(pool, sc))
            unique(o$to_stage)
          }, error = function(e) character(0))
          if (length(opts))
            toastr_success(paste("Onward to:", paste(gsub("_", " ", opts), collapse = ", ")),
                           title = "Released")
        }
        self_refresh(); signal_others()
      }
    }
    
    shiny$observeEvent(input$complete, {
      close_batch("approved", "completed", "%s complete \u2014 released onward.")
    })
    shiny$observeEvent(input$reject, {
      close_batch("rejected", "rejected", "%s rejected \u2014 repeat the subculture.")
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
        toastr_error(sprintf("Only %d unit(s) are still owed on this request.", owed),
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
          # RESERVE, not one. A single guarded UPDATE rather than a
          # read-then-write, so two technicians drawing from one line at the
          # same moment cannot both pass the check and take it below the floor.
          moved <- dbExecute(conn, "
            UPDATE tbl_sample SET quantity = quantity - $2
             WHERE sample_code = $1 AND quantity - $2 >= $3",
                             params = list(src, qty, RESERVE))
          if (moved == 0)
            stop(src, " cannot spare ", qty, " unit(s): ", RESERVE, " must stay in ",
                 "culture as working stock. Start a culture cycle to build the line ",
                 "back up, then send the rest.", call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_culture_count (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'subculture','discarded',$2,$3,$4)",
                    params = list(src, -qty,
                                  sprintf("drawn to %s", gsub("_", " ", to)), user()))
          child <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                              params = list(CHILD_PREFIX))$code[1]
          # The service line travels WITH the material. Material committed here
          # is committed downstream too, and re-deciding it at the next bench
          # would let one promise be counted twice.
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
        printer$queue(data.frame(code = drawn, title = "SUBCULTURE MATERIAL",
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
    
    # Jump to the batch that needs building up, with the request left open.
    # Refusing the request outright would lose the record of what is owed; this
    # keeps it and puts the operator where the work is.
    shiny$observeEvent(input$go_multiply, {
      el <- eligible_batches()
      if (!is.data.frame(el) || nrow(el) == 0) {
        toastr_warning("No batch of this order is standing on this bench.",
                       title = "Nothing to multiply"); return()
      }
      pick <- el$sample_code[which.max(as.integer(el$units_held))]
      clear_sel()
      # flow_stepper is not a selectInput - it sets its value with
      # Shiny.setInputValue from an onclick, so updateSelectInput would have
      # done nothing at all and the operator would have been left on the
      # Requests tab wondering what the button did. Set the same input the
      # stepper sets, and move the highlight with it.
      runjs(sprintf(
        paste0("Shiny.setInputValue('%s', 'growing');",
               "var s=document.querySelectorAll(\"[data-value]\");",
               "Array.prototype.forEach.call(s, function(k){",
               "if(k.getAttribute('data-value')==='growing'){",
               "Array.prototype.forEach.call(k.parentNode.children, function(j){",
               "j.classList.remove('on'); j.setAttribute('aria-selected','false');});",
               "k.classList.add('on'); k.setAttribute('aria-selected','true');}});"),
        ns("vstate")))
      sel_bench(pick)
      toastr_success(sprintf("%s is the strongest line \u2014 record a cycle to build it up.",
                             pick), title = "Multiply first")
    })
    
    shiny$observeEvent(input$req_cancel, {
      x <- req_row(); if (is.null(x)) return()
      ok <- tryCatch({
        dbExecute(pool, "
          UPDATE tbl_sample_request
             SET status = 'cancelled', cancelled_on = now(),
                 cancel_reason = 'cancelled in subculture'
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

