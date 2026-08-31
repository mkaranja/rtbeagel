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
  app/logic/fct_tracking[sterilization_stock, sterilization_queue,
                         pending_requests, awaited_deliveries],
  app/logic/fct_workflows[workflow_cache, sample_context, record_event],
  app/view/shared/label_print,
  app/view/shared/order_theme,
)

# ============================================================================
# SURFACE STERILIZATION · clinical worklist (worklist + right detail panel)
# ----------------------------------------------------------------------------
# The first bench that works on material the lab has DECLARED clean. Everything
# upstream is diagnosis; from here on the question is yield.
#
# WHAT MAKES MATERIAL AVAILABLE
#   Every test the consignment requires has come back negative. Two routes
#   reach that state and this bench does not distinguish between them:
#
#     never positive   the quarantine stock is itself clean
#     cleaned          positive -> thermotherapy -> meristem culture ->
#                      retest negative. Only the meristem tissue is clean; the
#                      quarantine stock it came from is still infected and is
#                      never offered, however the retest came out.
#
#   That reasoning is in view_lineage_clearance, not here. Subculture and
#   hardening gate on the same fact, and three copies of it would disagree.
#
# THIS BENCH CUTS NOTHING
#   Quarantine and meristem culture own all cutting. The Available tab is a
#   shop window: it names the ORDER and how much material stands behind it,
#   and a request goes to whichever bench is holding it. A technician there
#   chooses which piece to send, looking at the plants.
# ============================================================================

MY_TAB  <- "surface"
CHILD_PREFIX <- "SS"       # batches drawn from this bench get their own series

# Every bench that draws FROM this one, and the state material arrives in
# there. Subculture is the workflow's own next step from
# surface_sterilization/completed.
ENTRY_STATE <- c(subculture            = "established",
                 in_vitro_conservation = "established",
                 in_vitro_distribution = "established",
                 hardening             = "established")
DEST_TAB <- c(subculture = "subculture")
WF_PATH <- file.path("app", "static", "workflows", "cassava.yaml")

# Why a batch lost explants. `initial` is written once, by the protocol form;
# the rest are the operator's during culture.
LOSS_REASONS <- c("Contaminated" = "contaminated",
                  "Dead"         = "dead",
                  "Discarded"    = "discarded")

# The state a loss puts the batch into. Recording contamination and leaving the
# batch reading `established` was how a contaminated batch stayed invisible to
# the reviewer.
LOSS_STATE <- c(contaminated = "contaminated", dead = "dead", discarded = "updated")

state_tone <- function(s) switch(s %||% "",
                                 established = "ink", updated = "amber",
                                 healthy = "brand", contaminated = "amber",
                                 dead = "amber", depleted = "ink",
                                 completed = "brand", rejected = "amber", "ink")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    order_theme$page_header(
      title = "Surface Sterilization",
      sub   = "Sterilize cleared material and record what survives."
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
    
    printer <- label_print$server("print", module_name = "surface_sterilization",
                                  user = user)
    
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
    stock <- shiny$reactive({ refresh(); sterilization_stock() })
    # Requests FROM other benches for material standing here. Subculture pulls
    # from this bench the way this bench pulls from meristem culture, so the
    # same two acts apply: a supervisor authorises that material may be taken,
    # a technician enters what was actually cut.
    reqs  <- shiny$reactive({ refresh(); pending_requests("surface_sterilization") })
    # The mirror of reqs(): what THIS bench asked for and has not received in
    # full. Both read the same balances, so the bench that owes and the bench
    # that waits can never be shown different numbers about one request.
    awaited <- shiny$reactive({ refresh(); awaited_deliveries("surface_sterilization") })
    queue <- shiny$reactive({ refresh(); sterilization_queue() })
    
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
      # The action column must EXIST before a colDef can name it; reactable
      # rejects the whole table otherwise. rep(), not NA, because a zero-row
      # frame is the normal state here on a fresh database.
      if (nrow(d) > 0 || !is.null(d$row_id)) d$act <- rep(NA_character_, nrow(d))
      search_rows(d, c("order_number", "customer_name", "crop_name",
                       "variety_name", "source_bench"))
    })
    
    # These arms MUST stay identical to the counts in output$kpis, or a tab
    # reads 7 and the table shows something else.
    bench_rows <- shiny$reactive({
      d <- queue()
      if (nrow(d) > 0 || !is.null(d$sample_code)) d$act <- rep(NA_character_, nrow(d))
      if (nrow(d) > 0) {
        d <- switch(vstate(),
                    working  = d[d$state_code %in% c("established", "updated",
                                                     "contaminated", "dead"), , drop = FALSE],
                    review   = d[d$state_code == "healthy", , drop = FALSE],
                    finished = d[d$state_code %in% c("completed", "rejected",
                                                     "depleted"), , drop = FALSE],
                    d)
      }
      search_rows(d, c("sample_code", "order_number", "customer_name",
                       "crop_name", "variety_name", "sterilant"))
    })
    
    n_available <- shiny$reactive({
      d <- stock(); if (nrow(d) == 0) 0L else nrow(d)
    })
    n_working <- shiny$reactive({
      d <- queue(); if (nrow(d) == 0) 0L else
        sum(d$state_code %in% c("established", "updated", "contaminated", "dead"))
    })
    n_review <- shiny$reactive({
      d <- queue(); if (nrow(d) == 0) 0L else sum(d$state_code == "healthy")
    })
    n_requests <- shiny$reactive({ d <- reqs(); if (nrow(d) == 0) 0L else nrow(d) })
    n_finished <- shiny$reactive({
      d <- queue(); if (nrow(d) == 0) 0L else
        sum(d$state_code %in% c("completed", "rejected", "depleted"))
    })
    
    output$kpis <- shiny$renderUI({
      order_theme$flow_stepper(list(
        list(title = "Available", sub = "cleared material",
             count = n_available(), unit = "orders", value = "available",
             active = identical(vstate(), "available"),
             waiting = n_available() > 0),
        list(title = "In sterilization", sub = "on this bench",
             count = n_working(), unit = "batches", value = "working",
             active = identical(vstate(), "working")),
        list(title = "Review", sub = "healthy, awaiting sign-off",
             count = n_review(), unit = "batches", value = "review",
             active = identical(vstate(), "review"),
             waiting = n_review() > 0),
        list(title = "Requests", sub = "other benches want material",
             count = n_requests(), unit = "to handle", value = "requests",
             active = identical(vstate(), "requests"),
             waiting = n_requests() > 0),
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
          shiny$strong(sprintf("%d explants", owed)),
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
                                 if (n_available() == 1) " consignment has cleared every test it requires. "
                                 else " consignments have cleared every test they require. ",
                                 "Requesting asks the bench holding the material \u2014 quarantine, or ",
                                 "meristem culture where the material had to be cleaned first \u2014 and a ",
                                 "technician there chooses which piece to send.")
             else
               order_theme$guide(
                 "Nothing has cleared yet. A consignment appears here once every test it ",
                 "requires has come back negative, whether first time or on retest after ",
                 "meristem culture."),
             working = if (n_working() > 0)
               order_theme$guide(tone = "do",
                                 "Record the protocol on a new batch, then log contamination and losses ",
                                 "as they happen. Mark a batch healthy when it is ready for review.")
             else
               order_theme$guide("No batch is in sterilization. Request material from the Available tab."),
             review = if (n_review() > 0)
               order_theme$guide(tone = "do",
                                 shiny$strong(n_review()),
                                 if (n_review() == 1) " batch is healthy and awaiting sign-off. "
                                 else " batches are healthy and awaiting sign-off. ",
                                 "Completing a batch releases it to subculture.")
             else order_theme$guide("Nothing is waiting for review."),
             requests = if (n_requests() > 0)
               order_theme$guide(tone = "do",
                                 shiny$strong(n_requests()),
                                 if (n_requests() == 1) " bench has asked for material standing here. "
                                 else " requests for material standing here. ",
                                 "Authorise one, then enter the batch that was actually sent \u2014 you ",
                                 "choose which, looking at the plants.")
             else order_theme$guide("No bench has asked for material from here."),
             finished = order_theme$guide(
               "Batches that have left this bench, and batches that ended here."))
    })
    
    # ---- the two lists ------------------------------------------------
    # Available material and batches on the bench are different KINDS of thing:
    # one is a consignment offering material, the other a physical batch with a
    # code. They do not share columns, so they do not share a table.
    output$list_pane <- shiny$renderUI({
      switch(vstate(),
             available = order_theme$table_card(reactableOutput(ns("stock_tbl"))),
             requests  = order_theme$table_card(reactableOutput(ns("req_tbl"))),
             order_theme$table_card(reactableOutput(ns("bench_tbl"))))
    })
    
    output$stock_tbl <- renderReactable({
      d <- stock_rows()
      keep <- c("order_number", "customer_name", "crop_name", "variety_name",
                "source_bench", "units_available", "act")
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick_stock"), "row_id")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(sel_stock(), "row_id")),
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          row_id = colDef(show = FALSE), source_stage = colDef(show = FALSE),
          suggested_sample_code = colDef(show = FALSE),
          available_since = colDef(show = FALSE),
          requested = colDef(show = FALSE), in_progress = colDef(show = FALSE),
          was_cleaned = colDef(show = FALSE),
          
          order_number = colDef(name = "ORDER", minWidth = 160,
                                cell = function(v) shiny$tags$strong(v)),
          customer_name = colDef(name = "CUSTOMER", minWidth = 140),
          crop_name = colDef(name = "CROP", width = 95),
          variety_name = colDef(name = "VARIETY", minWidth = 110,
                                cell = function(v) if (is.na(v)) "\u2014" else v),
          # WHERE the material is, and whether it had to be cleaned to get
          # here. A cleaned consignment is the interesting case and should not
          # look identical to one that was never infected.
          source_bench = colDef(name = "HELD IN", minWidth = 165,
                                cell = function(v, i) {
                                  shiny$tagList(
                                    order_theme$chip(v, if (isTRUE(d$was_cleaned[i])) "teal" else "brand"),
                                    if (isTRUE(d$was_cleaned[i]))
                                      shiny$tags$small(class = "wl-meta-note", "cleaned") else NULL)
                                }),
          units_available = colDef(name = "AVAILABLE", width = 115,
                                   cell = function(v) {
                                     n <- if (is.na(v)) 0L else as.integer(v)
                                     order_theme$chip(sprintf("%d ready", n), if (n > 0) "teal" else "ink")
                                   }),
          act = colDef(name = "", width = 175, sortable = FALSE,
                       cell = function(v, i) {
                         if (as.integer(d$requested[i]) > 0)
                           order_theme$chip("Requested", "amber")
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
          "Nothing has cleared yet. A consignment appears here once every test it requires is negative."),
        theme = order_theme$rt_theme())
    })
    
    output$bench_tbl <- renderReactable({
      d <- bench_rows()
      keep <- c("sample_code", "order_number", "sterilant", "units_held",
                "initial_count", "state_label", "act")
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick_bench"), "sample_code")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(sel_bench(), "sample_code")),
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          state_code = colDef(show = FALSE), since = colDef(show = FALSE),
          parent_sample_code = colDef(show = FALSE),
          concentration = colDef(show = FALSE), exposure_minutes = colDef(show = FALSE),
          rinses = colDef(show = FALSE), sterilized_on = colDef(show = FALSE),
          protocol_notes = colDef(show = FALSE), protocol_recorded = colDef(show = FALSE),
          ledger_count = colDef(show = FALSE), n_contaminated = colDef(show = FALSE),
          n_dead = colDef(show = FALSE), n_discarded = colDef(show = FALSE),
          drift = colDef(show = FALSE), customer_name = colDef(show = FALSE),
          crop_name = colDef(show = FALSE), variety_name = colDef(show = FALSE),
          
          sample_code = colDef(name = "BATCH", minWidth = 120,
                               cell = function(v) shiny$tags$strong(v)),
          order_number = colDef(name = "ORDER", minWidth = 155),
          sterilant = colDef(name = "STERILANT", minWidth = 130,
                             cell = function(v) if (is.na(v)) "\u2014" else v),
          initial_count = colDef(name = "STARTED", width = 95,
                                 cell = function(v) if (is.na(v)) "\u2014" else as.character(v)),
          # Surviving over started, with the loss beside it. A batch that began
          # with 40 and holds 12 is not the same situation as one that began
          # with 12, and one number cannot say which happened.
          units_held = colDef(name = "SURVIVING", width = 135,
                              cell = function(v, i) {
                                n <- if (is.na(v)) 0L else as.integer(v)
                                lost <- as.integer(d$n_contaminated[i]) + as.integer(d$n_dead[i]) +
                                  as.integer(d$n_discarded[i])
                                shiny$tagList(
                                  order_theme$chip(as.character(n), if (n > 0) "brand" else "amber"),
                                  if (!is.na(lost) && lost > 0)
                                    shiny$tags$small(class = "wl-meta-note", sprintf("%d lost", lost)) else NULL)
                              }),
          state_label = colDef(name = "STATE", width = 130, cell = function(v, i) {
            shiny$tagList(
              order_theme$chip(v %||% "", state_tone(d$state_code[i])),
              # The ledger and tbl_sample.quantity disagreeing means something
              # wrote one without the other. Better seen here than found in a
              # report months later.
              if (isTRUE(d$drift[i])) order_theme$chip("count drift", "amber") else NULL)
          }),
          act = colDef(name = "", width = 165, sortable = FALSE,
                       cell = function(v, i) {
                         st <- d$state_code[i]
                         if (!isTRUE(d$protocol_recorded[i]))
                           order_theme$chip("Needs protocol", "amber")
                         else if (identical(st, "healthy")) order_theme$chip("For review", "teal")
                         else if (st %in% c("completed", "rejected", "depleted")) ""
                         else order_theme$chip("In culture", "ink")
                       })
        ), keep), d),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "No batch here. Request cleared material from the Available tab."),
        theme = order_theme$rt_theme())
    })
    
    # ---- requests from other benches ----------------------------------
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
          to_stage_label = colDef(name = "NEEDED AT", minWidth = 150,
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
                                       order_theme$chip(sprintf("%d explants", n), if (n > 0) "amber" else "brand"),
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
                    if (pending) shiny$tagList(
                      "Authorising allows material from this consignment to be taken. It cuts ",
                      "nothing \u2014 the request then waits here for a technician to enter what ",
                      "was actually sent.")
                    else shiny$tagList(
                      "Choose the batch to draw from. ", shiny$strong(sug),
                      " is what ", x$to_stage_label[1], " suggested; send whichever batch is ",
                      "healthiest. The batch stays here and can be drawn again.")),
                  order_theme$prop_grid(
                    order_theme$prop("Customer", x$customer_name[1]),
                    order_theme$prop("Crop", x$crop_name[1] %||% "\u2014"),
                    order_theme$prop("Asked by", x$requested_by[1]),
                    order_theme$prop("Reason", x$reason[1]),
                    if (!pending) order_theme$prop("Authorised by", x$authorized_by[1]) else NULL),
                  # What is still owed on this request, and what has already gone.
                  # Both benches read the same numbers from view_request_progress, so
                  # they cannot be told different things about the same request.
                  order_theme$prop_grid(
                    order_theme$prop("Asked for", sprintf("%d %s",
                                                          as.integer(x$qty_requested[1] %||% 1L), "explants")),
                    order_theme$prop("Already sent", sprintf("%d in %d deliver%s",
                                                             as.integer(x$qty_sent[1] %||% 0L),
                                                             as.integer(x$n_deliveries[1] %||% 0L),
                                                             if (identical(as.integer(x$n_deliveries[1] %||% 0L), 1L)) "y" else "ies")),
                    order_theme$prop("Still owed", sprintf("%d %s",
                                                           as.integer(x$qty_outstanding[1] %||% 0L), "explants"))),
                  if (!pending) {
                    if (length(ch) == 0)
                      order_theme$guide(tone = "do",
                                        "No completed batch of this order is standing here with material to ",
                                        "spare. Complete a batch first, then enter this request.")
                    else shiny$tagList(
                      shiny$selectInput(ns("req_batch"), "Batch to draw from",
                                        choices = ch, selected = sel, width = "100%"),
                      if (length(ok_codes) == 0)
                        order_theme$guide(tone = "do",
                                          "Every batch of this order is down to a single explant. One must stay ",
                                          "so the material can answer a later request.") else NULL,
                      shiny$numericInput(ns("req_qty"),
                                         sprintf("Explants to send now (%d still owed)",
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
    
    # Completed batches of this order that still hold material. The dropdown
    # only offers these; the write re-checks, because the dropdown is client
    # state and the write is the thing that has to be right.
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
      lab <- sprintf("%s  \u00b7  %d explant%s%s", d$sample_code, q,
                     ifelse(q == 1, "", "s"),
                     ifelse(q <= 1, "  \u2014 cannot be drawn", ""))
      setNames(d$sample_code, lab)
    }
    
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
        sel_req(NULL); self_refresh()
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
        toastr_error(sprintf("Only %d explant(s) are still owed on this request.", owed),
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
          # Guarded spend, leaving one explant behind: the same rule every
          # holding bench applies, and it has to be a single guarded UPDATE
          # rather than a read-then-write, or two technicians drawing at once
          # would both pass the check.
          moved <- dbExecute(conn, "
            UPDATE tbl_sample SET quantity = quantity - $2
             WHERE sample_code = $1 AND quantity - $2 >= 1",
                             params = list(src, qty))
          if (moved == 0)
            stop(src, " cannot spare ", qty, " explant(s) - one must stay so it can ",
                 "answer a later request. Refresh and try again.", call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_culture_count (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'surface_sterilization','discarded',$2,$3,$4)",
                    params = list(src, -qty,
                                  sprintf("drawn to %s", gsub("_", " ", to)), user()))
          child <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                              params = list(CHILD_PREFIX))$code[1]
          # order_service_id stays NULL. This is still shared stock: which
          # service line it will answer is decided at allocation, downstream,
          # and guessing here would commit material before anyone has.
          dbExecute(conn, "
            INSERT INTO tbl_sample (sample_code, order_number, parent_sample_code,
                                    stage_code, quantity, created_by, created_on)
            VALUES ($1,$2,$3,$4,$5,$6,now())",
                    params = list(child, x$order_number[1], src, to, qty, user()))
          dbExecute(conn, "
            INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, notes)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(child, to, state, user(),
                                  sprintf("drawn from %s for %s%s", src,
                                          gsub("_", " ", to),
                                          if (identical(src, x$source_sample_code[1])) ""
                                          else sprintf(" (requested against %s)",
                                                       x$source_sample_code[1]))))
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
        printer$queue(data.frame(code = drawn, title = "STERILIZED MATERIAL",
                                 line1 = sprintf("from %s", src),
                                 line2 = format(Sys.Date(), "%d %b %Y"),
                                 stringsAsFactors = FALSE))
        toastr_success(sprintf("%s entered from %s.", drawn, src),
                       title = "Sent")
        sel_req(NULL); self_refresh(); signal_others()
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
                 cancel_reason = 'cancelled in surface sterilization'
           WHERE request_id = $1", params = list(as.integer(x$request_id[1]))); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e)); FALSE })
      if (ok) { toastr_success("Request cancelled."); sel_req(NULL); self_refresh() }
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
      if (nrow(d) > 0 && code %in% d$sample_code) {
        sel_stock(NULL); sel_bench(code)
      } else {
        toastr_warning(sprintf("%s is not a batch on this bench.", code),
                       title = "Not found")
      }
    })
    
    # ---- detail: available material -----------------------------------
    output$detail <- shiny$renderUI({
      if (!is.null(stock_row())) return(shiny$uiOutput(ns("stock_detail")))
      if (!is.null(req_row()))   return(shiny$uiOutput(ns("req_detail")))
      if (!is.null(bench_row())) return(shiny$uiOutput(ns("bench_detail")))
      shiny$div(class = "wl-detail-inner",
                shiny$div(class = "wl-empty",
                          shiny$div(class = "wl-empty-ico", shiny$icon("hand-pointer")),
                          shiny$div(class = "wl-empty-title", "Select a row to begin"),
                          shiny$div(class = "wl-empty-body",
                                    "Choose a cleared consignment to request material, a batch ",
                                    "on the bench to record its protocol and losses, or a request ",
                                    "from another bench to authorise and fill.")))
    })
    
    output$stock_detail <- shiny$renderUI({
      x <- stock_row(); if (is.null(x)) return(NULL)
      cleaned <- isTRUE(x$was_cleaned[1])
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = x$order_number[1],
                  sub = sprintf("%s \u00b7 %s", x$customer_name[1] %||% "",
                                x$source_bench[1]),
                  close_input = ns("detail_close")),
                shiny$div(class = "wl-statusbar",
                          order_theme$chip("Cleared", "brand"),
                          order_theme$chip(if (cleaned) "Cleaned via meristem culture"
                                           else "Clean on first testing",
                                           if (cleaned) "teal" else "ink")),
                order_theme$section(
                  "\u2192", "Request material for sterilization",
                  sub = x$source_bench[1],
                  order_theme$guide(
                    "Asks ", shiny$strong(x$source_bench[1]), " for material from this ",
                    "consignment. A technician there chooses which piece to send and enters ",
                    "what was cut \u2014 this bench does not cut its own material.",
                    if (cleaned) shiny$tagList(
                      " This consignment tested positive and was cleaned: only the meristem ",
                      "tissue is offered, never the quarantine stock it came from.") else NULL),
                  order_theme$prop_grid(
                    order_theme$prop("Customer", x$customer_name[1]),
                    order_theme$prop("Crop", x$crop_name[1] %||% "\u2014"),
                    order_theme$prop("Variety", x$variety_name[1] %||% "\u2014"),
                    order_theme$prop("Held in", x$source_bench[1]),
                    order_theme$prop("Units available", as.character(x$units_available[1]))
                  ),
                  if (as.integer(x$in_progress[1]) > 0)
                    order_theme$guide(
                      shiny$strong(as.integer(x$in_progress[1])),
                      if (as.integer(x$in_progress[1]) == 1) " batch from this consignment is already "
                      else " batches from this consignment are already ",
                      "in sterilization. Asking again is allowed \u2014 a consignment can need ",
                      "several batches \u2014 but check before you do.") else NULL,
                  shiny$numericInput(ns("req_qty_wanted"), "Explants needed",
                                     value = 1, min = 1, width = "240px"),
                  order_theme$guide(
                    "The holding bench sends what is ready and the request stays open for the ",
                    "rest, so ask for what you actually need rather than what you think they ",
                    "can spare today."),
                  shiny$textAreaInput(ns("req_reason"), "Why this is needed", width = "100%",
                                      value = "material cleared for surface sterilization"),
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
      recorded <- isTRUE(x$protocol_recorded[1])
      held <- as.integer(x$units_held[1] %||% 0L)
      
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = sc,
                  sub = sprintf("%s \u00b7 %s", x$order_number[1], x$customer_name[1] %||% ""),
                  close_input = ns("detail_close")),
                
                shiny$div(class = "wl-statusbar",
                          order_theme$chip(x$state_label[1] %||% "", state_tone(st)),
                          order_theme$chip(sprintf("from %s", x$parent_sample_code[1]), "ink"),
                          if (isTRUE(x$drift[1])) order_theme$chip("count drift", "amber") else NULL),
                
                # ---- 1. the protocol, recorded once ----
                if (!recorded) shiny$tagList(
                  order_theme$subhead("1. Record the sterilization protocol"),
                  shiny$div(class = "update-hint",
                            "What this batch was treated with. Recorded once, at the start; ",
                            "losses are logged separately as they happen."),
                  shiny$textInput(ns("sterilant"),
                                  shiny$HTML("Sterilant <span class='mandatory_star'>*</span>"),
                                  width = "100%", placeholder = "e.g. sodium hypochlorite"),
                  shiny$textInput(ns("concentration"), "Concentration", width = "240px",
                                  placeholder = "e.g. 1.5%"),
                  shiny$numericInput(ns("exposure"), "Exposure (minutes)", value = 15, min = 1),
                  shiny$numericInput(ns("rinses"), "Sterile rinses", value = 3, min = 0),
                  shiny$dateInput(ns("sterilized_on"), "Date sterilized", value = Sys.Date()),
                  shiny$numericInput(ns("initial_count"),
                                     sprintf("Explants in this batch (%d received)", held),
                                     value = held, min = 1),
                  shiny$textAreaInput(ns("protocol_notes"), "Notes", width = "100%"),
                  order_theme$detail_actions(
                    shiny$actionButton(ns("save_protocol"), "Save protocol",
                                       class = "btn btn-success"))
                ) else shiny$tagList(
                  order_theme$subhead("Protocol"),
                  order_theme$prop_grid(
                    order_theme$prop("Sterilant", x$sterilant[1]),
                    order_theme$prop("Concentration", x$concentration[1] %||% "\u2014"),
                    order_theme$prop("Exposure",
                                     if (is.na(x$exposure_minutes[1])) "\u2014"
                                     else sprintf("%d min", as.integer(x$exposure_minutes[1]))),
                    order_theme$prop("Rinses",
                                     if (is.na(x$rinses[1])) "\u2014"
                                     else as.character(as.integer(x$rinses[1]))),
                    order_theme$prop("Sterilized on", as.character(x$sterilized_on[1])),
                    order_theme$prop("Started with", as.character(x$initial_count[1])),
                    order_theme$prop("Surviving", as.character(held)),
                    order_theme$prop("Lost",
                                     sprintf("%d contaminated \u00b7 %d dead \u00b7 %d discarded",
                                             as.integer(x$n_contaminated[1]),
                                             as.integer(x$n_dead[1]),
                                             as.integer(x$n_discarded[1])))
                  )),
                
                # ---- 2. losses, while the batch is live ----
                if (recorded && !(st %in% c("completed", "rejected", "depleted")))
                  shiny$tagList(
                    order_theme$subhead("2. Log a loss"),
                    shiny$div(class = "update-hint",
                              "Each entry is kept, with its reason. The batch count moves with it, ",
                              "and a batch that loses everything is marked depleted."),
                    shiny$selectInput(ns("loss_reason"), "What happened",
                                      choices = LOSS_REASONS, width = "240px"),
                    shiny$numericInput(ns("loss_n"), sprintf("How many (%d surviving)", held),
                                       value = 1, min = 1, max = max(1L, held)),
                    shiny$textAreaInput(ns("loss_notes"), "Notes", width = "100%"),
                    order_theme$detail_actions(
                      shiny$actionButton(ns("log_loss"), "Log loss",
                                         class = "btn btn-outline-secondary"),
                      if (held > 0 && !identical(st, "healthy"))
                        shiny$actionButton(ns("mark_healthy"), "Mark batch healthy",
                                           class = "btn btn-success") else NULL))
                else NULL,
                
                # ---- 3. review ----
                if (identical(st, "healthy")) shiny$tagList(
                  order_theme$subhead("3. Review"),
                  if (!is_admin()) shiny$div(class = "flow-cta warn",
                                             shiny$span(class = "fc-ico", shiny$icon("user-shield")),
                                             shiny$span("Only an administrator can complete a batch."))
                  else shiny$tagList(
                    shiny$div(class = "update-hint",
                              "Completing releases the batch to subculture. Rejecting returns it here ",
                              "to be repeated."),
                    shiny$textAreaInput(ns("review_notes"), "Review comments", width = "100%"),
                    order_theme$detail_actions(
                      shiny$actionButton(ns("reject"), "Reject",
                                         class = "btn btn-outline-secondary"),
                      shiny$actionButton(ns("complete"), "Complete batch",
                                         class = "btn btn-success")))
                ) else NULL,
                
                if (identical(st, "completed")) shiny$div(class = "flow-cta ok",
                                                          shiny$span(class = "fc-ico", shiny$icon("circle-check")),
                                                          shiny$span("Complete. This batch is available to subculture.")) else NULL,
                if (identical(st, "depleted")) shiny$div(class = "update-hint",
                                                         "Every explant in this batch was lost. Request more material to repeat it.") else NULL
      )
    })
    
    # ---- request material ---------------------------------------------
    do_request <- function(x) {
      if (is.null(x) || nrow(x) == 0) return(invisible(FALSE))
      if (as.integer(fld(x, "requested", 0L)) > 0) {
        toastr_warning("This consignment has already been requested.",
                       title = "Already requested"); return()
      }
      src   <- fld(x, "suggested_sample_code", "")
      order <- fld(x, "order_number", "")
      why   <- shiny$isolate(input$req_reason)
      # HOW MUCH, not just what. Without it the holding bench has no total to
      # deliver against, and "partly sent" is not a thing anyone can express.
      want <- suppressWarnings(as.integer(shiny$isolate(input$req_qty_wanted)))
      if (length(want) == 0 || is.na(want) || want < 1) want <- 1L
      if (is.null(why) || !nzchar(trimws(why)))
        why <- "material cleared for surface sterilization"
      if (!nzchar(src)) {
        toastr_error("No material stands behind this consignment any more.",
                     title = "Nothing to request"); return()
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # Re-read availability inside the transaction. The row was rendered
          # from a snapshot, and another bench may have drawn the last unit
          # since; asking for material that is gone would leave a request
          # nobody can fill.
          still <- dbGetQuery(conn, "
            SELECT units_available, suggested_sample_code
            FROM view_sterilization_stock
            WHERE order_number = $1 AND source_stage = $2",
                              params = list(order, fld(x, "source_stage", "")))
          if (nrow(still) == 0 || as.integer(still$units_available[1]) < 1)
            stop("No material from ", order, " is available any more.", call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_sample_request
              (order_number, source_sample_code, to_stage, reason, requested_by,
               status, qty_requested)
            VALUES ($1, $2, 'surface_sterilization', $3, $4, 'pending', $5)",
                    params = list(order, still$suggested_sample_code[1], why, user(),
                                  want))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'surface_sterilization', 'material requested', $2, $3)",
                    params = list(order, user(),
                                  sprintf("requested from %s", fld(x, "source_bench", ""))))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Request failed", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success(sprintf("Requested from %s \u2014 they will send the material.",
                               fld(x, "source_bench", "the holding bench")),
                       title = "Requested")
        sel_stock(NULL); self_refresh(); signal_others()
      }
    }
    
    shiny$observeEvent(input$do_request, { do_request(stock_row()) })
    shiny$observeEvent(input$act_stock, {
      d <- stock(); if (nrow(d) == 0) return()
      x <- d[d$row_id == input$act_stock$code, , drop = FALSE]
      if (nrow(x) == 0) return()
      sel_stock(input$act_stock$code); sel_bench(NULL)
      do_request(x)
    })
    
    # ---- save the protocol --------------------------------------------
    shiny$observeEvent(input$save_protocol, {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      sterilant <- trimws(input$sterilant %||% "")
      n0 <- suppressWarnings(as.integer(input$initial_count))
      if (!nzchar(sterilant)) {
        toastr_error("Name the sterilant used.", title = "Missing"); return()
      }
      if (is.na(n0) || n0 < 1) {
        toastr_error("The batch must start with at least one explant.",
                     title = "Invalid count"); return()
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          dbExecute(conn, "
            INSERT INTO tbl_sterilization_detail
              (sample_code, sterilant, concentration, exposure_minutes, rinses,
               sterilized_on, initial_count, notes, recorded_by)
            VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9)",
                    params = list(sc, sterilant,
                                  nz(input$concentration), int_or_na(input$exposure),
                                  int_or_na(input$rinses),
                                  as.character(input$sterilized_on), n0,
                                  nz(input$protocol_notes), user()))
          # The ledger opens with what the batch started with, so the surviving
          # count is always derivable from entries rather than from a number
          # somebody typed and later edited.
          dbExecute(conn, "
            INSERT INTO tbl_culture_count
              (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'surface_sterilization','initial',$2,$3,$4)",
                    params = list(sc, n0, "batch established", user()))
          # tbl_sample.quantity is the live count the draw rules spend, so it
          # moves with the ledger in the SAME transaction. Written apart, the
          # two drift and view_sterilization_current flags it.
          dbExecute(conn, "UPDATE tbl_sample SET quantity = $2 WHERE sample_code = $1",
                    params = list(sc, n0))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "surface_sterilization", "updated", user(),
                       wf = wf, ctx = ctx,
                       notes = sprintf("sterilized with %s; %d explant%s", sterilant,
                                       n0, if (n0 == 1) "" else "s"))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not save", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(code = sc, title = "STERILIZED BATCH",
                                 line1 = sprintf("%s \u00b7 %d explants", sterilant, n0),
                                 line2 = format(Sys.Date(), "%d %b %Y"),
                                 stringsAsFactors = FALSE))
        toastr_success(sprintf("Protocol saved for %s.", sc))
        self_refresh()
      }
    })
    
    # ---- log a loss ----------------------------------------------------
    shiny$observeEvent(input$log_loss, {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      reason <- input$loss_reason %||% "contaminated"
      n <- suppressWarnings(as.integer(input$loss_n))
      held <- as.integer(x$units_held[1] %||% 0L)
      if (is.na(n) || n < 1) {
        toastr_error("How many were lost?", title = "Invalid"); return()
      }
      if (n > held) {
        toastr_error(sprintf("%s holds %d explant%s \u2014 you cannot lose %d.",
                             sc, held, if (held == 1) "" else "s", n),
                     title = "More than the batch holds"); return()
      }
      left <- held - n
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          # Guarded UPDATE, not a read-then-write. Two people logging losses on
          # the same batch at once would each read the old count and the second
          # write would undo the first; the WHERE clause makes the loser fail
          # loudly instead.
          moved <- dbExecute(conn, "
            UPDATE tbl_sample SET quantity = quantity - $2
             WHERE sample_code = $1 AND quantity - $2 >= 0",
                             params = list(sc, n))
          if (moved == 0)
            stop(sc, " no longer holds ", n, " explant(s). Somebody else may have ",
                 "logged a loss. Refresh and try again.", call. = FALSE)
          dbExecute(conn, "
            INSERT INTO tbl_culture_count
              (sample_code, stage_code, reason, delta, notes, recorded_by)
            VALUES ($1,'surface_sterilization',$2,$3,$4,$5)",
                    params = list(sc, reason, -n, nz(input$loss_notes), user()))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          # A batch with nothing left is DEPLETED, whatever it died of. Leaving
          # it in a working state left an empty batch sitting in the queue with
          # actions that could not do anything.
          to_state <- if (left == 0) "depleted" else unname(LOSS_STATE[reason])
          if (is.null(to_state) || is.na(to_state)) to_state <- "updated"
          record_event(conn, sc, "surface_sterilization", to_state, user(),
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
    
    shiny$observeEvent(input$mark_healthy, {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      if (as.integer(x$units_held[1] %||% 0L) < 1) {
        toastr_error("Nothing survives in this batch.", title = "Empty batch"); return()
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "surface_sterilization", "healthy", user(),
                       wf = wf, ctx = ctx, notes = "batch healthy; awaiting review")
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Failed", timeOut = 0); FALSE
      })
      if (ok) { toastr_success(sprintf("%s marked healthy.", sc)); self_refresh() }
    })
    
    # ---- review --------------------------------------------------------
    review <- function(decision, to_state, msg) {
      x <- bench_row(); if (is.null(x)) return()
      sc <- x$sample_code[1]
      if (!is_admin()) {
        toastr_error("Only an administrator can review a batch.",
                     title = "Not permitted"); return()
      }
      note <- nz(input$review_notes)
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), is_admin()))
          dbExecute(conn, "
            INSERT INTO tbl_review (sample_code, stage_code, decision, comments, reviewed_by)
            VALUES ($1, 'surface_sterilization', $2, $3, $4)",
                    params = list(sc, decision, note, user()))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "surface_sterilization", to_state, user(),
                       wf = wf, ctx = ctx, notes = sprintf("review: %s", decision))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'surface_sterilization', $2, $3, $4)",
                    params = list(x$order_number[1], sprintf("batch %s", decision),
                                  user(), sc))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Review failed", timeOut = 0); FALSE
      })
      if (ok) { toastr_success(sprintf(msg, sc)); self_refresh(); signal_others() }
    }
    
    shiny$observeEvent(input$complete, {
      review("approved", "completed", "%s complete \u2014 available to subculture.")
    })
    shiny$observeEvent(input$reject, {
      review("rejected", "rejected", "%s rejected \u2014 repeat the sterilization.")
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