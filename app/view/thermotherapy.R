box::use(
  shiny,
  bs4Dash[bs4Card],
  reactable[reactable, reactableOutput, renderReactable, colDef,
            reactableTheme, reactableLang],
  shinyjs[useShinyjs, runjs],
  shinytoastr[toastr_success, toastr_error, toastr_warning],
  htmlwidgets[JS],
  pool[poolWithTransaction],
  DBI[dbExecute, dbGetQuery],
)

box::use(
  app/logic/fct_conn[pool, load_data],
  app/logic/fct_tracking[thermo_queue, scan_identity, unreceived_orders, source_routing,
                         last_approval_request],
  app/logic/fct_workflows[workflow_cache, next_options, order_context, sample_context, record_event],
  app/logic/fct_notify[notify_approvers],
  app/view/shared/order_theme,
)

# ============================================================================
# THERMOTHERAPY · clinical worklist (worklist + right detail panel)
# ----------------------------------------------------------------------------
# Heat treatment in a conviron chamber. A sample reaches this bench three ways
# - a positive index result, the cassava express lane from reception, or a
# restart after a rejected round - all landing at thermotherapy/inprogress.
# The pull queue lists them without distinguishing.
#
# Two-pane worklist, matching virus indexing: the bench on the left, a detail
# panel on the right that opens on row click. Everything happens inline in the
# panel - no modals. Three acts, mapped to the legal state machine
# (inprogress / updated / completed / rejected):
#
#   Place     write tbl_thermotherapy_detail (one OPEN row per sample). State
#             stays inprogress - the sample is in the chamber, treatment running.
#   Update    inprogress -> updated. A checkpoint.
#   Review    updated -> completed (close the chamber, set exit_date; recommends
#             meristem culture) OR updated -> rejected (restart).
#
# The chamber row and the state event are written in ONE transaction so
# view_conviron_occupancy and the queue never disagree. State moves through
# record_event(), so an off-workflow move records a reason rather than being
# refused.
#
# PERFORMANCE: data reactives depend on a LOCAL refresh + tab-activation, never
# the global trigger_refresh - no app-wide re-query storm.
# ============================================================================

MY_TAB  <- "thermotherapy"
CODE_PREFIX <- "IN"   # first-generation samples share the intake series
WF_PATH <- file.path("app", "static", "workflows", "cassava.yaml")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    order_theme$page_header(
      title = "Thermotherapy",
      sub   = "Place samples in a conviron and track heat treatment."
    ),
    
    shiny$uiOutput(ns("kpis")),
    shiny$uiOutput(ns("guide")),
    
    order_theme$toolbar(
      order_theme$search_box(ns("q"), "Search sample, order, customer, conviron..."),
      order_theme$scan_box(ns("scan"), ns("scan_go"))
    ),
    
    order_theme$workbench(
      # Incoming means EVERYTHING not yet on this bench that could come to it,
      # and there are two ways material arrives - by request from quarantine,
      # or as a consignment received straight onto this bench. They were split
      # across two tabs, which made "Incoming" look like it meant only the
      # second. One tab, two clearly headed lists.
      list_ui   = shiny$tagList(
        shiny$conditionalPanel(
          condition = sprintf("input['%s'] == 'incoming'", ns("filter")),
          order_theme$table_card(
            order_theme$table_note(
              title = "Request from quarantine. ",
              "Material that tested positive and needs heat treatment. ",
              "Requesting asks quarantine to cut a sample from it \u2014 this ",
              "bench does not cut material itself."),
            reactableOutput(ns("need_tbl")))),
        order_theme$table_card(
          shiny$conditionalPanel(
            condition = sprintf("input['%s'] == 'incoming'", ns("filter")),
            order_theme$table_note(
              title = "Receive a consignment directly. ",
              "Approved consignments that can come straight onto this bench ",
              "without passing through quarantine.")),
          reactableOutput(ns("tbl")))
      ),
      detail_ui = shiny$uiOutput(ns("detail"))
    )
  )
}



#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    user <- shiny$reactive(shiny$reactiveValuesToList(res_auth)$user)
    # approval is admin-only; shinymanager supplies the flag
    is_admin <- shiny$reactive(isTRUE(shiny$reactiveValuesToList(res_auth)$admin))
    
    # ---- scoped refresh ----------------------------------------------
    refresh <- shiny$reactiveVal(0)
    self_refresh <- function() refresh(shiny$isolate(refresh()) + 1)
    if (!is.null(tab) && is.function(tab)) {
      shiny$observeEvent(tab(), { if (identical(tab(), MY_TAB)) self_refresh() }, ignoreInit = TRUE)
    }
    signal_others <- function() {
      if (!is.null(trigger_refresh)) trigger_refresh(shiny$isolate(trigger_refresh()) + 1)
    }
    
    # A failing query must take down ONE table, not the module.
    #
    # queue(), incoming() and needed() feed the KPI stepper, the guide AND
    # cur_tab(). An error in any of them therefore blanked the tabs, the
    # guidance and both tables at once - the whole screen went empty with the
    # cause buried in the console. Caught here, the rest of the page still
    # renders and the message is shown where the operator is looking.
    q_err <- shiny$reactiveVal(NULL)
    
    safe <- function(expr, what) {
      tryCatch(expr, error = function(e) {
        q_err(sprintf("%s: %s", what, conditionMessage(e)))
        NULL
      })
    }
    
    # nrow() that tolerates a failed query
    n_rows <- function(d) if (is.null(d) || !is.data.frame(d)) 0L else nrow(d)
    
    # ---- RENDER DIAGNOSTICS ---------------------------------------------
    # Each render is wrapped so a failure names ITSELF - in the console and on
    # screen - instead of leaving a blank space with no clue which piece died.
    #
    #   console:  [thermotherapy] render 'kpis' FAILED: <message>
    #   screen:   a red labelled banner where that piece should have been
    #
    # A render that returns NULL (nothing to show) is reported too, quietly, so
    # "the KPIs are blank" can be told apart from "the KPIs errored": the first
    # logs 'empty', the second logs 'FAILED'.
    trace_on <- TRUE   # flip to FALSE to silence the console notes
    
    trace <- function(where, expr) {
      out <- tryCatch(force(expr), error = function(e) {
        msg <- conditionMessage(e)
        if (trace_on) message(sprintf("[thermotherapy] render '%s' FAILED: %s",
                                      where, msg))
        # A visible marker, not an empty div - this is the whole point.
        structure(
          shiny$div(class = "q-banner error",
                    shiny$strong(sprintf("[%s] did not render. ", where)), msg),
          failed = TRUE)
      })
      if (is.null(out) && trace_on)
        message(sprintf("[thermotherapy] render '%s' returned NULL (empty)", where))
      out
    }
    
    # Report each data reactive's state ONCE per refresh, so the console shows a
    # clean row-count census rather than nothing. Fires off the refresh token,
    # not off any render, so it cannot itself break a render.
    shiny$observe({
      refresh()
      if (!trace_on) return()
      message(sprintf(
        "[thermotherapy] census - queue:%s incoming:%s needed:%s  q_err:%s",
        n_rows(shiny$isolate(queue())),
        n_rows(shiny$isolate(incoming())),
        n_rows(shiny$isolate(needed())),
        shiny$isolate(q_err()) %||% "none"))
    })
    
    queue <- shiny$reactive({ refresh(); q_err(NULL); safe(thermo_queue(), "bench queue") })
    

    # consignments no bench has received yet - cassava may come straight here
    # instead of through quarantine (the workflow offers both at reception)
    incoming <- shiny$reactive({ refresh(); safe(unreceived_orders(), "incoming consignments") })
    
    # ---- MATERIAL THIS BENCH CAN PULL -----------------------------------
    # Thermotherapy asks for its OWN material. Virus indexing finishes testing
    # and stops - it does not send anything anywhere and does not need to know
    # this module exists. This bench watches for material whose indexing came
    # back positive and asks quarantine to draw it, which is the pull-queue
    # rule the rest of the pipeline already follows.
    MY_STAGE <- "thermotherapy"
    
    needed <- shiny$reactive({
      refresh()
      d <- safe(source_routing(), "material to request")
      # `delivered` is added below on the populated path, but the table has a
      # colDef for it, and reactable errors on a colDef with no column. So an
      # empty frame must carry the column too - shape it here, on the one path
      # that skips the code that would otherwise create it.
      if (is.null(d) || n_rows(d) == 0) {
        if (is.data.frame(d)) {
          d$delivered <- integer(0)
          d$request   <- character(0)
        }
        return(d)
      }
      # NA-safe: string_agg returns NULL when there are no matching rows, and
      # grepl(NA) is NA, which would drop the row from a logical subset.
      # Same shape as virus indexing's awaiting-initiation. A positive order
      # appears here whether or not its material has reached this bench yet:
      #
      #   delivered = 0  the material is still in quarantine or meristem  ->
      #                  request a draw
      #   delivered = 1  a sample has already been drawn TO thermotherapy  ->
      #                  initiate it directly, no round trip
      #
      # The old filter EXCLUDED drawn material, so a delivered sample vanished
      # from Incoming and the operator had nothing telling them to place it.
      # That is the disconnect: the request was granted and then invisible.
      req <- ifelse(is.na(d$requested_to), "", d$requested_to)
      drw <- ifelse(is.na(d$drawn_to),     "", d$drawn_to)
      d$delivered <- as.integer(grepl(MY_STAGE, drw, fixed = TRUE))
      keep <- as.integer(d$ready) == 1L &
        d$verdict == "positive" &
        # Hide only a source that has an OPEN request and nothing drawn
        # yet - it is already asked for and waiting on quarantine. Once
        # something is drawn (delivered = 1) it stays, to be initiated.
        (d$delivered == 1L | !grepl(MY_STAGE, req, fixed = TRUE))
      d <- d[keep, , drop = FALSE]
      d$request <- rep(NA_character_, n_rows(d))
      d
    })

    
    # Ask quarantine for it. This module never cuts material itself.
    shiny$observeEvent(input$need_go, {
      code <- input$need_go$code
      d <- needed()
      x <- d[d$source_code == code, , drop = FALSE]
      if (nrow(x) == 0) return()
      reason <- sprintf("thermotherapy: indexing positive (%d of %d tests, %d positive)",
                        x$tests_done[1], x$tests_total[1], x$n_positive[1])
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_sample_request
              (order_number, source_sample_code, to_stage, reason, requested_by)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(x$order_number[1], x$source_code[1], MY_STAGE, reason, user()))
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not request", timeOut = 0); FALSE
      })
      if (ok) {
        toastr_success(sprintf("%s requested. Quarantine will draw the sample.",
                               x$source_code[1]), title = "Requested")
        self_refresh()
        # The material is at the quarantine bench, so that is where the next
        # physical act happens.
        runjs("Shiny.setInputValue('rtb_goto', {tab: 'quarantine', n: Math.random()}, {priority: 'event'})")
      }
    })
    
    # "Initiate" on a DELIVERED row. The sample was already drawn to this bench
    # by quarantine fulfilling the request, so it is sitting in thermo_queue()
    # at inprogress with no conviron yet - the "needs placing" state. This does
    # not create anything; it jumps the operator to that sample on the bench,
    # where placing it is the existing first act.
    shiny$observeEvent(input$init_go, {
      src_code <- input$init_go$code
      d <- queue()
      # the child drawn from this source that is here and not yet a test sample
      child <- d[!is.na(d$parent_sample_code) &
                   d$parent_sample_code == src_code, , drop = FALSE]
      if (nrow(child) == 0) {
        toastr_warning(paste("That material has not arrived on the bench yet.",
                             "Quarantine may not have drawn it. Refresh and check."),
                       title = "Not here yet"); return()
      }
      # Move to the placing tab, then select the sample on the next flush.
      # input$filter's observer clears selected(), so the order matters:
      # change the tab, let that observer run, THEN set the selection.
      sc <- child$sample_code[1]
      runjs(sprintf(
        "Shiny.setInputValue('%s', 'unplaced', {priority: 'event'});", ns("filter")))
      session$onFlushed(function() selected(sc), once = TRUE)
      toastr_success(sprintf("%s is on the bench - place it in a conviron.", sc),
                     title = "Ready to place")
    })
    
    output$need_tbl <- renderReactable({ trace("need_tbl", {
      d <- needed()
      # reactable errors on a frame with no columns, which is what a failed
      # query leaves behind. A one-column placeholder renders the empty state
      # instead of throwing a second error on top of the first.
      if (is.null(d) || !is.data.frame(d) || ncol(d) == 0) {
        return(reactable(data.frame(` ` = character()),
                         language = order_theme$rt_lang(
                           "Could not load material to request."),
                         theme = order_theme$rt_theme()))
      }
      reactable(
        d,
        columns = order_theme$rt_cols(list(
          stage_code = colDef(show = FALSE), origin_kind = colDef(show = FALSE),
          ready = colDef(show = FALSE), requested_to = colDef(show = FALSE),
          drawn_to = colDef(show = FALSE), variety_name = colDef(show = FALSE),
          tests_done = colDef(show = FALSE),
          source_code = colDef(name = "MATERIAL", minWidth = 120,
                               cell = function(v) shiny$tags$strong(v)),
          verdict = colDef(name = "INDEXING", width = 120,
                           cell = function(v) order_theme$chip("Positive", "amber")),
          n_positive = colDef(name = "POSITIVE", width = 95),
          tests_total = colDef(name = "TESTS", width = 80),
          delivered = colDef(show = FALSE),
          bench = colDef(name = "HELD IN", minWidth = 130, cell = function(v, i) {
            if (identical(as.integer(d$delivered[i]), 1L))
              order_theme$chip("On this bench", "brand")
            else v
          }),
          order_number = colDef(name = "ORDER", minWidth = 150),
          customer_name = colDef(name = "CUSTOMER", minWidth = 130),
          crop_name = colDef(name = "CROP", width = 95),
          # One column, two acts, decided by whether material is here yet - the
          # same rule virus indexing uses. Delivered: initiate. Not yet: request.
          request = colDef(name = "", width = 165, sortable = FALSE,
                           cell = JS(sprintf(
                             "function(ci){ var d = ci.row['delivered'] == 1;
                 var id = d ? '%s' : '%s';
                 var lbl = d ? 'Initiate' : 'Request material';
                 return '<button class=\"btn btn-success btn-sm\" onclick=\"Shiny.setInputValue(\\'' + id + '\\', {code: \\'' + ci.row['source_code'] + '\\', n: Math.random()})\">' + lbl + '</button>'; }",
                             ns("init_go"), ns("need_go"))), html = TRUE)
        ), d),
        defaultPageSize = 10, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "No material is waiting. Indexing must finish and come back positive first."),
        theme = order_theme$rt_theme())
    }) })
    # The tab to land on. "On the bench" is the natural default, but on a bench
    # with nothing on it that is an empty table and no clue - the one thing to
    # act on sits under Incoming, and the operator has to already know that.
    #
    # So: if the bench is empty and there IS something to bring in, open there.
    # Once the operator picks a tab, their choice wins.
    cur_tab <- shiny$reactive({
      f <- input$filter
      if (!is.null(f) && length(f) == 1 && nzchar(f)) return(f)
      if (n_rows(queue()) == 0 && (n_rows(incoming()) + n_rows(needed())) > 0) "incoming" else "all"
    })
    
    incoming_mode <- shiny$reactive(identical(cur_tab(), "incoming"))
    
    rows <- shiny$reactive({
      d <- queue(); if (is.null(d)) return(NULL)
      f <- cur_tab()
      if (n_rows(d) > 0) {
        d <- switch(f,
                    unplaced = d[is.na(d$conviron), , drop = FALSE],
                    placed   = d[!is.na(d$conviron), , drop = FALSE],
                    overdue  = d[!is.na(d$overdue) & d$overdue, , drop = FALSE],
                    d)
      }
      q <- input$q
      if (!is.null(q) && nzchar(q) && n_rows(d) > 0) {
        cols <- intersect(c("sample_code","order_number","customer_name","crop_name","conviron"), names(d))
        hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    incoming_rows <- shiny$reactive({
      d <- incoming(); if (is.null(d)) return(NULL)
      q <- input$q
      if (!is.null(q) && nzchar(q) && n_rows(d) > 0) {
        cols <- intersect(c("order_number","customer_name","crop_name","variety_name"), names(d))
        hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    # The KPI row IS the tab bar. Each tile's count is exactly the number of
    # rows clicking it produces - see rows() below, whose switch() arms these
    # mirror one for one. `filter` is the same input the dropdown used to set,
    # so rows(), incoming_mode() and the selection-reset observer are unchanged.
    #
    # isolate() on the active tab is deliberate: flow_pick_js moves the `on`
    # class in the browser, so this row must NOT re-render on a tab change.
    # Taking a reactive dependency here would re-render it on every click,
    # flashing the whole row and undoing that.
    # The tab bar, in quarantine's flow-stepper language. Incoming -> needs
    # placing -> in chamber is the real order of the work, so those three carry
    # ordinals. "On the bench" is a view across them and "Overdue" is an
    # exception within "In chamber", not a step after it - both are marked.
    #
    # `filter` is the same input the dropdown used to set, so rows(),
    # incoming_mode() and the selection-reset observer are unchanged.
    #
    # isolate() on the active tab: the highlight moves client-side, so this
    # must not re-render when a tab is clicked.
    output$kpis <- shiny$renderUI({ trace("kpis", {
      d <- queue()
      unplaced <- if (n_rows(d)) sum(is.na(d$conviron)) else 0L
      placed   <- if (n_rows(d)) sum(!is.na(d$conviron)) else 0L
      overdue  <- if (n_rows(d)) sum(!is.na(d$overdue) & d$overdue) else 0L
      inc <- n_rows(incoming())
      # NOT isolate(cur_tab()) here. cur_tab() calls queue()/incoming()/needed()
      # again; reading it inside the KPI render coupled the tab bar to all three
      # queries, so any one of them failing took the whole stepper down. The
      # active tab is a plain read of the input instead.
      cur <- input$filter %||% "all"
      order_theme$flow_stepper(
        list(
          list(title = "On the bench", sub = "every sample here", value = "all",
               num = "\u2211", count = n_rows(d), unit = "samples",
               active = identical(cur, "all")),
          # Counts BOTH routes in. A count that showed only consignments made
          # the tab look empty while material sat waiting to be requested.
          list(title = "Incoming", sub = "request or receive material",
               value = "incoming", count = inc + n_rows(needed()), unit = "to bring in",
               active = identical(cur, "incoming"),
               waiting = (inc + n_rows(needed())) > 0),
          list(title = "Needs placing", sub = "assign to a conviron", value = "unplaced",
               count = unplaced, unit = "to place",
               active = identical(cur, "unplaced"), waiting = unplaced > 0),
          list(title = "In chamber", sub = "under heat treatment", value = "placed",
               count = placed, unit = "treating",
               active = identical(cur, "placed")),
          list(title = "Overdue", sub = "past expected exit", value = "overdue",
               num = "!", count = overdue, unit = "overdue",
               active = identical(cur, "overdue"), waiting = overdue > 0)
        ),
        input_id = ns("filter")
      )
    }) })
    
    
    # ---- worklist ----------------------------------------------------
    # THE instruction: one sentence, specific to the tab, ending in the handoff.
    output$guide <- shiny$renderUI({ trace("guide", {
      # A failed query is the most important thing on the screen. Silent empty
      # tables are what made this look like "the module shows nothing".
      if (!is.null(q_err())) {
        return(order_theme$guide(tone = "do",
                                 shiny$strong("A query failed and part of this page is empty. "),
                                 q_err(),
                                 shiny$br(),
                                 shiny$tags$small("The rest of the bench still works. Report this message ",
                                                  "- it names the query.")))
      }
      d <- queue()
      unplaced <- if (n_rows(d)) sum(is.na(d$conviron)) else 0L
      overdue  <- if (n_rows(d)) sum(!is.na(d$overdue) & d$overdue) else 0L
      inc <- n_rows(incoming())
      switch(
        cur_tab(),
        # Names BOTH routes and what to do about each. The old text mentioned
        # only consignments, so an operator with material waiting to be
        # requested was told nothing was waiting.
        incoming = local({
          nd <- n_rows(needed())
          if (nd == 0 && inc == 0)
            order_theme$guide(
              "Nothing to bring in. Material appears here once its indexing is ",
              "complete and a test came back positive, or when a consignment is ",
              "approved for this bench.")
          else
            order_theme$guide(tone = "do",
                              if (nd > 0) shiny$tagList(
                                shiny$strong(nd),
                                if (nd == 1) " source tested positive and needs heat treatment \u2014 "
                                else " sources tested positive and need heat treatment \u2014 ",
                                "request it from quarantine below. ") else NULL,
                              if (inc > 0) shiny$tagList(
                                shiny$strong(inc),
                                if (inc == 1) " approved consignment can be received"
                                else " approved consignments can be received",
                                " straight onto this bench.") else NULL)
        }),
        unplaced = if (unplaced > 0)
          order_theme$guide(tone = "do",
                            "Assign each sample to a conviron and set its expected exit date.")
        else order_theme$guide("Every sample on this bench has been placed."),
        placed = order_theme$guide(
          "Under treatment. Record checkpoints as you go; finish a sample when its ",
          "schedule completes, then approve it to hand it on.",
          action = order_theme$goto("Meristem Culture", "meristem")),
        overdue = if (overdue > 0)
          order_theme$guide(tone = "do",
                            shiny$strong(overdue),
                            if (overdue == 1) " sample is past its expected exit date."
                            else " samples are past their expected exit date.",
                            " Finish treatment or extend the schedule.")
        else order_theme$guide(tone = "done", "Nothing is overdue."),
        local({
          nd <- n_rows(needed()); nb <- n_rows(queue())
          if (nb == 0 && nd > 0)
            order_theme$guide(tone = "do",
                              "Nothing is on this bench yet. ", shiny$strong(nd),
                              if (nd == 1) " source has" else " sources have",
                              " tested positive and can be requested under ",
                              shiny$strong("Incoming"), ".")
          else if (nb == 0)
            order_theme$guide(
              "Nothing is on this bench, and nothing is waiting to come in. ",
              "Material appears once virus indexing completes a test and it ",
              "comes back positive.")
          else
            order_theme$guide(
              "Every sample on the thermotherapy bench. Pick a step above to ",
              "work on one task at a time.")
        })
      )
    }) })
    
    output$tbl <- renderReactable({ trace("tbl", {
      if (incoming_mode()) {
        di <- incoming_rows()
        return(reactable(
          di,
          onClick = JS(sprintf("function(rowInfo){ Shiny.setInputValue('%s', {code: rowInfo.row['order_number'], n: Math.random()}); }", ns("pick_order"))),
          rowStyle = JS("function(rowInfo){ return {cursor:'pointer'}; }"),
          rowClass = JS(sprintf("function(rowInfo){ return rowInfo.row['order_number'] === '%s' ? 'wl-selected' : null; }",
                                sel_order() %||% "")),
          columns = order_theme$rt_cols(list(
            sample_type_code = colDef(show = FALSE),
            order_number = colDef(name = "ORDER", minWidth = 150, cell = function(v) shiny$tags$strong(v)),
            customer_name = colDef(name = "CUSTOMER", minWidth = 130),
            crop_name = colDef(name = "CROP", width = 95, cell = function(v) if (is.na(v)) "\u2014" else v),
            variety_name = colDef(name = "VARIETY", width = 105, cell = function(v) if (is.na(v)) "\u2014" else v),
            sample_type = colDef(name = "TYPE", width = 105, cell = function(v) if (is.na(v)) "\u2014" else v),
            sample_amount = colDef(name = "AMOUNT", width = 90,
                                   cell = function(v) if (is.na(v)) "\u2014" else as.character(v)),
            date_received = colDef(name = "ARRIVED", width = 100,
                                   cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %y")),
            approved_on = colDef(name = "APPROVED", width = 100,
                                 cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %y"))
          ), di),
          defaultPageSize = 14, compact = TRUE, highlight = TRUE,
          language = reactableLang(
            noData = "No consignments awaiting receipt. Approved orders appear here until a bench receives them."),
          theme = order_theme$rt_theme()))
      }
      d <- rows()
      if (is.null(d) || !is.data.frame(d) || ncol(d) == 0) {
        return(reactable(data.frame(` ` = character()),
                         language = order_theme$rt_lang("Could not load the bench."),
                         theme = order_theme$rt_theme()))
      }
      reactable(
        d,
        onClick = JS(sprintf("function(rowInfo){ Shiny.setInputValue('%s', {code: rowInfo.row['sample_code'], n: Math.random()}); }", ns("pick"))),
        rowStyle = JS("function(rowInfo){ return {cursor:'pointer'}; }"),
        rowClass = JS(sprintf("function(rowInfo){ return rowInfo.row['sample_code'] === '%s' ? 'wl-selected' : null; }",
                              selected() %||% "")),
        columns = order_theme$rt_cols(list(
          quantity = colDef(show = FALSE), parent_sample_code = colDef(show = FALSE),
          due_out = colDef(show = FALSE), expected_days = colDef(show = FALSE),
          state_code = colDef(show = FALSE),
          sample_code = colDef(name = "SAMPLE", width = 110, cell = function(v) shiny$tags$strong(v)),
          order_number = colDef(name = "ORDER", width = 140),
          customer_name = colDef(name = "CUSTOMER", minWidth = 120),
          crop_name = colDef(name = "CROP", width = 85, cell = function(v) if (is.na(v)) "\u2014" else v),
          variety_name = colDef(name = "VARIETY", width = 95, cell = function(v) if (is.na(v)) "\u2014" else v),
          state_label = colDef(name = "STATUS", minWidth = 150, cell = function(v, i) {
            st <- d$state_code[i]; placed <- !is.na(d$conviron[i]); exited <- !is.na(d$exit_date[i])
            over <- !is.na(d$overdue[i]) && d$overdue[i]
            info <- switch(st,
                           inprogress = if (!placed) list("Needs placing", "amber")
                           else if (over) list("In chamber \u00b7 overdue", "amber")
                           else list("In chamber", "brand"),
                           updated    = list("In chamber \u00b7 checkpoint", "brand"),
                           completed  = list("Awaiting review", "amber"),
                           approved   = list("Approved \u00b7 ready to push", "teal"),
                           rejected   = list("Rejected \u00b7 re-place", "amber"),
                           list(d$state_label[i], "ink"))
            order_theme$chip(info[[1]], info[[2]])
          }),
          conviron = colDef(name = "CONVIRON", width = 115, cell = function(v, i) {
            if (is.na(v)) return(order_theme$chip("Unplaced", "amber"))
            exited <- !is.na(d$exit_date[i])
            over <- !is.na(d$overdue[i]) && d$overdue[i]
            order_theme$chip(v, if (exited) "ink" else if (over) "amber" else "brand")
          }),
          exit_date = colDef(show = FALSE),
          entered_on = colDef(name = "IN", width = 90,
                              cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %y")),
          days_in = colDef(name = "DAYS", width = 70, cell = function(v, i) {
            if (is.na(v)) return("\u2014")
            od <- d$overdue[i]
            order_theme$chip(as.character(v), if (!is.na(od) && od) "amber" else "ink")
          }),
          overdue = colDef(show = FALSE),
          since = colDef(name = "SINCE", width = 95,
                         cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %y"))
        ), d),
        defaultPageSize = 14, compact = TRUE, highlight = TRUE,
        # Says where to go, not just that there is nothing here. An empty bench
        # is the normal state before any material has been requested.
        language = order_theme$rt_lang(
          "Nothing on this bench yet. Material arrives once you request it under Incoming."),
        theme = order_theme$rt_theme())
    }) })
    
    # ---- selection + scan --------------------------------------------
    selected  <- shiny$reactiveVal(NULL)   # a sample on the bench
    sel_order <- shiny$reactiveVal(NULL)   # an incoming consignment
    
    shiny$observeEvent(input$pick_order, { sel_order(input$pick_order$code) })
    shiny$observeEvent(input$filter, { sel_order(NULL); selected(NULL) })
    
    shiny$observeEvent(input$pick, { selected(input$pick$code) })
    shiny$observeEvent(input$detail_close, { selected(NULL) })
    
    shiny$observeEvent(input$scan_go, {
      code <- trimws(input$scan %||% "")
      if (!nzchar(code)) return()
      id <- scan_identity(code)
      if (nrow(id) == 0) { toastr_warning(sprintf("No sample found for %s.", code), title = "Not found"); return() }
      if (!(code %in% queue()$sample_code)) {
        toastr_warning(sprintf("%s is not on the thermotherapy bench (currently %s).",
                               code, id$stage_label[1] %||% id$kind[1]), title = "Not on bench"); return()
      }
      selected(code)
      toastr_success(sprintf("Showing %s.", code))
      runjs(sprintf("var el=document.getElementById('%s'); if(el) el.value='';", ns("scan")))
    })
    
    sel_row <- shiny$reactive({
      sc <- selected(); if (is.null(sc)) return(NULL)
      d <- queue(); r <- d[d$sample_code == sc, , drop = FALSE]
      if (nrow(r) == 1) r else NULL
    })
    
    # ---- detail panel ------------------------------------------------
    output$detail <- shiny$renderUI({ trace("detail", {
      # ---- incoming consignments: receive into thermotherapy ----------
      if (incoming_mode()) {
        on <- sel_order()
        di <- incoming()
        row <- if (!is.null(on) && nrow(di)) di[di$order_number == on, , drop = FALSE] else NULL
        if (is.null(row) || nrow(row) != 1) {
          return(shiny$div(class = "wl-detail-inner",
                           shiny$div(class = "wl-empty",
                                     shiny$div(class = "wl-empty-ico", shiny$icon("inbox")),
                                     shiny$div(class = "wl-empty-title",
                                               "Select a consignment to receive"),
                                     shiny$div(class = "wl-empty-body",
                                               "These approved consignments have not been received by any bench. ",
                                               "Receiving one here creates its samples and starts heat treatment.", shiny$br(),
                                               "A consignment received in quarantine stops appearing here."))))
        }
        rec <- thermo_recommended()
        return(shiny$div(class = "wl-detail-inner",
                         order_theme$detail_head(
                           title = on,
                           sub = sprintf("%s \u00b7 %s", row$customer_name[1] %||% "",
                                         row$crop_name[1] %||% "\u2014"),
                           close_input = ns("detail_close")),
                         order_theme$subhead("Receive into thermotherapy"),
                         if (isTRUE(rec)) shiny$div(class = "flow-cta ok",
                                                    shiny$span(class = "fc-ico", shiny$icon("circle-check")),
                                                    shiny$span("The workflow offers thermotherapy directly for this consignment."))
                         else shiny$div(class = "flow-cta warn",
                                        shiny$span(class = "fc-ico", shiny$icon("triangle-exclamation")),
                                        shiny$span("The workflow routes this consignment through quarantine. ",
                                                   "Receiving it here is off-workflow and needs a reason.")),
                         shiny$div(class = "update-hint",
                                   "Order requests ", shiny$strong(row$sample_amount[1] %||% "\u2014"),
                                   " unit(s). Create the samples that will be treated."),
                         shiny$numericInput(ns("recv_n"),
                                            shiny$HTML("Number of samples <span class='mandatory_star'>*</span>"),
                                            value = 1, min = 1, max = 500),
                         shiny$numericInput(ns("recv_qty"), "Units per sample", value = 1, min = 1),
                         shiny$dateInput(ns("recv_on"), "Received on", value = Sys.Date()),
                         if (!isTRUE(rec)) shiny$textAreaInput(ns("recv_reason"),
                                                               shiny$HTML("Reason <span class='mandatory_star'>*</span>"), width = "100%"),
                         shiny$textAreaInput(ns("recv_notes"), "Notes", width = "100%"),
                         shiny$div(class = "wl-actions",
                                   shiny$actionButton(ns("receive"), "Receive consignment",
                                                      class = "btn btn-success", icon = shiny$icon("inbox")))))
      }
      
      r <- sel_row()
      if (is.null(r)) {
        return(shiny$div(class = "wl-detail-inner",
                         shiny$div(class = "wl-empty",
                                   shiny$div(class = "wl-empty-ico", shiny$icon("hand-pointer")),
                                   shiny$div(class = "wl-empty-title",
                                             "Select a sample to begin"),
                                   shiny$div(class = "wl-empty-body",
                                             "Click any row to place it in a conviron, record a checkpoint, or ",
                                             "complete treatment.", shiny$br(),
                                             "You can also scan a barcode to jump straight to a sample."))))
      }
      sc <- r$sample_code[1]; st <- r$state_code[1]
      placed <- !is.na(r$conviron[1])          # has a chamber record
      exited <- !is.na(r$exit_date[1])          # has left the chamber
      # placement status the operator actually sees
      place_chip <- if (!placed) order_theme$chip("Unplaced", "amber")
      else if (exited) order_theme$chip(paste(r$conviron[1], "\u00b7 exited"), "ink")
      else if (!is.na(r$overdue[1]) && r$overdue[1]) order_theme$chip(paste(r$conviron[1], "\u00b7 overdue"), "amber")
      else order_theme$chip(paste(r$conviron[1], "\u00b7 in chamber"), "brand")
      
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = sc,
                  sub = sprintf("%s \u00b7 %s", r$order_number[1], r$customer_name[1] %||% ""),
                  close_input = ns("detail_close")),
                
                shiny$div(class = "wl-statusbar",
                          order_theme$chip(r$state_label[1], switch(st,
                                                                    updated = "amber", completed = "brand", rejected = "amber", "ink")),
                          place_chip),
                
                # chamber summary whenever there is a chamber record
                if (placed) shiny$div(class = "wl-statusbar",
                                      order_theme$prop("Conviron", r$conviron[1]),
                                      order_theme$prop("Entered", if (is.na(r$entered_on[1])) "\u2014" else format(as.Date(r$entered_on[1]), "%d %b %Y")),
                                      if (exited)
                                        order_theme$prop("Exited", format(as.Date(r$exit_date[1]), "%d %b %Y"))
                                      else
                                        order_theme$prop("Due out", if (is.na(r$due_out[1])) "\u2014" else format(as.Date(r$due_out[1]), "%d %b %Y")),
                                      order_theme$prop(if (exited) "Days in chamber" else "Days in", if (is.na(r$days_in[1])) "\u2014" else
                                        paste0(r$days_in[1], if (!exited && !is.na(r$overdue[1]) && r$overdue[1]) " (overdue)" else ""))),
                
                # ---- act by state (state drives the mode, not placement) ----
                if (identical(st, "completed")) {
                  # REVIEW / APPROVE gate. Approval is ADMIN-ONLY: a technician who
                  # finishes treatment raises a request instead, which an administrator
                  # picks up.
                  admin <- is_admin()
                  req <- if (!admin) last_approval_request(sc) else NULL
                  shiny$tagList(
                    order_theme$subhead("Review & approve"),
                    shiny$div(class = "update-hint",
                              "Treatment is complete and the chamber is closed. An administrator ",
                              "approves the material to release it for meristem culture."),
                    if (!admin) shiny$tagList(
                      shiny$div(class = "flow-cta warn",
                                shiny$span(class = "fc-ico", shiny$icon("user-shield")),
                                shiny$span("Only an administrator can approve. Send a reminder so one reviews it.")),
                      if (!is.null(req) && nrow(req) > 0)
                        shiny$div(class = "wl-meta-note",
                                  sprintf("Last requested by %s on %s.", req$actor[1],
                                          format(as.POSIXct(req$requested_on[1]), "%d %b %Y %H:%M")))
                      else NULL,
                      shiny$div(class = "wl-actions",
                                shiny$actionButton(ns("request_approval"), "Send approval reminder",
                                                   class = "btn btn-primary", icon = shiny$icon("envelope"))))
                    else shiny$tagList(
                      shiny$textAreaInput(ns("approve_notes"), "Review comments", width = "100%"),
                      shiny$div(class = "wl-actions",
                                shiny$actionButton(ns("reject_treatment"), "Reject", class = "btn btn-outline-secondary"),
                                shiny$actionButton(ns("approve"), "Approve", class = "btn btn-success"))))
                }
                else if (identical(st, "approved")) shiny$tagList(
                  order_theme$subhead("Push to meristem culture"),
                  shiny$div(class = "flow-cta ok",
                            shiny$span(class = "fc-ico", shiny$icon("circle-check")),
                            shiny$span("Approved for meristem culture. Push it to hand the sample ",
                                       "to the meristem bench, where it is received and cultured.")),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("push"), "Push to meristem culture",
                                               class = "btn btn-success", icon = shiny$icon("arrow-right"))))
                else if (identical(st, "rejected")) shiny$tagList(
                  shiny$div(class = "update-hint",
                            "Treatment was rejected. Re-place the sample to run another round."),
                  order_theme$subhead("Re-place in conviron"),
                  shiny$textInput(ns("conviron"), shiny$HTML("Conviron <span class='mandatory_star'>*</span>"),
                                  placeholder = "e.g. C3"),
                  shiny$numericInput(ns("expected_days"), "Expected days in chamber", value = 30, min = 1, max = 365),
                  shiny$dateInput(ns("entered_on"), "Entered on", value = Sys.Date()),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("place"), "Re-place in conviron", class = "btn btn-success")))
                else if (identical(st, "inprogress") && !placed) shiny$tagList(
                  order_theme$subhead("Place in conviron"),
                  shiny$div(class = "update-hint",
                            "Assign a chamber. The sample stays in progress while treatment runs; ",
                            "expected days drives the overdue flag."),
                  shiny$textInput(ns("conviron"), shiny$HTML("Conviron <span class='mandatory_star'>*</span>"),
                                  placeholder = "e.g. C3"),
                  shiny$numericInput(ns("expected_days"), "Expected days in chamber", value = 30, min = 1, max = 365),
                  shiny$dateInput(ns("entered_on"), "Entered on", value = Sys.Date()),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("place"), "Place in conviron", class = "btn btn-success")))
                else if (identical(st, "inprogress")) shiny$tagList(
                  order_theme$subhead("Update"),
                  shiny$div(class = "update-hint",
                            "The sample is in the chamber. Record a checkpoint \u2014 moves it to ",
                            "\u2018updated\u2019, ready to finish."),
                  shiny$textAreaInput(ns("update_notes"), "Notes", width = "100%"),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("update"), "Mark updated", class = "btn btn-success")))
                else if (identical(st, "updated")) shiny$tagList(
                  order_theme$subhead("Finish treatment"),
                  shiny$div(class = "update-hint",
                            "Close the chamber. The sample moves to \u2018completed\u2019 and awaits ",
                            "review before it can go to meristem culture."),
                  shiny$dateInput(ns("exit_date"), "Exit date", value = Sys.Date()),
                  shiny$textAreaInput(ns("review_notes"), "Notes", width = "100%"),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("finish"), "Finish treatment", class = "btn btn-success"),
                            shiny$actionButton(ns("reject_treatment"), "Reject", class = "btn btn-outline-secondary")))
                else shiny$div(class = "update-hint",
                               "This sample has left thermotherapy.")
      )
    }) })
    
    active <- function() { r <- sel_row(); if (is.null(r)) NULL else r }
    
    # Does the workflow offer thermotherapy directly for the selected
    # consignment? The reception node offers quarantine and (for cassava)
    # thermotherapy as a choice, so this asks the YAML rather than hardcoding
    # the crop here.
    thermo_recommended <- shiny$reactive({
      on <- sel_order(); if (is.null(on)) return(FALSE)
      wf <- tryCatch(workflow_cache(WF_PATH), error = function(e) NULL)
      if (is.null(wf)) return(FALSE)
      ctx <- tryCatch(order_context(pool, on), error = function(e) list())
      opts <- tryCatch(next_options(wf, "reception", "approved", ctx), error = function(e) NULL)
      !is.null(opts) && nrow(opts) > 0 && "thermotherapy" %in% opts$to_stage
    })
    
    # ---- receive a consignment into thermotherapy --------------------
    shiny$observeEvent(input$receive, {
      on <- sel_order(); shiny$req(on)
      n <- input$recv_n; q <- input$recv_qty
      if (is.null(n) || is.na(n) || n < 1) { toastr_error("Number of samples must be at least 1.", title = "Invalid"); return() }
      if (n > 500) { toastr_error("500 samples is the per-batch limit. Split it.", title = "Too many"); return() }
      if (is.null(q) || is.na(q) || q < 1) { toastr_error("Units per sample must be at least 1.", title = "Invalid"); return() }
      off_workflow <- !isTRUE(thermo_recommended())
      if (off_workflow) {
        rsn <- input$recv_reason
        if (is.null(rsn) || !nzchar(trimws(rsn))) {
          toastr_error("A reason is required to receive this consignment here.", title = "Reason needed"); return()
        }
      }
      created <- character(0)
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          # guard inside the transaction: another bench may have received this
          # consignment between the page rendering and this click
          taken <- dbGetQuery(conn, "
            SELECT (EXISTS (SELECT 1 FROM tbl_sample WHERE order_number = $1)
                 OR EXISTS (SELECT 1 FROM tbl_order_quarantine WHERE order_number = $1)) AS taken",
                              params = list(on))$taken[1]
          if (isTRUE(taken)) stop("This consignment has already been received by another bench.")
          
          note <- if (nzchar(input$recv_notes %||% "")) input$recv_notes else NA_character_
          for (i in seq_len(as.integer(n))) {
            sc <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                             params = list(CODE_PREFIX))$code[1]
            dbExecute(conn, "
              INSERT INTO tbl_sample (sample_code, order_number, stage_code, quantity, created_by, created_on)
              VALUES ($1, $2, 'thermotherapy', $3, $4, $5)",
                      params = list(sc, on, as.integer(q), user(), as.character(input$recv_on)))
            # a birth at thermotherapy, written directly; the composite FK still
            # enforces that (thermotherapy, inprogress) is a legal pair
            dbExecute(conn, "
              INSERT INTO tbl_sample_event
                (sample_code, stage_code, state_code, actor, occurred_on, notes, is_override, override_reason)
              VALUES ($1, 'thermotherapy', 'inprogress', $2, $3, $4, $5, $6)",
                      params = list(sc, user(), as.character(input$recv_on),
                                    note, off_workflow,
                                    if (off_workflow) trimws(input$recv_reason) else NA_character_))
            # `<<-`, not `<-`. poolWithTransaction() runs this body in its own
            # function, so a plain assignment writes to a LOCAL copy and the
            # outer `created` stays empty. Reads INSIDE the transaction saw the
            # local one and looked right; every read AFTER it got character(0).
            created <<- c(created, sc)
          }
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'thermotherapy', $2, $3, $4)",
                    params = list(on, sprintf("received %d sample%s into thermotherapy",
                                              length(created), if (length(created) == 1) "" else "s"),
                                  user(), sprintf("%s to %s", created[1], created[length(created)])))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Receive failed", timeOut = 0); FALSE })
      if (ok) {
        toastr_success(sprintf("%d sample%s received \u2014 place them in a conviron.",
                               length(created), if (length(created) == 1) "" else "s"))
        sel_order(NULL); self_refresh(); signal_others()
      }
    })
    
    # ---- place -------------------------------------------------------
    shiny$observeEvent(input$place, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      conv <- input$conviron
      if (is.null(conv) || !nzchar(trimws(conv))) { toastr_error("A conviron identifier is required.", title = "Missing"); return() }
      ed <- input$expected_days
      if (is.null(ed) || is.na(ed) || ed < 1) { toastr_error("Expected days must be at least 1.", title = "Invalid"); return() }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_thermotherapy_detail
              (sample_code, conviron, expected_days_in_conviron, entered_on)
            VALUES ($1, $2, $3, $4)
            ON CONFLICT (sample_code) DO UPDATE
              SET conviron = EXCLUDED.conviron,
                  expected_days_in_conviron = EXCLUDED.expected_days_in_conviron,
                  entered_on = EXCLUDED.entered_on, exit_date = NULL, exit_notes = NULL",
                    params = list(sc, trimws(input$conviron), as.integer(input$expected_days),
                                  as.character(input$entered_on)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "thermotherapy", "inprogress", user(),
                       wf = wf, ctx = ctx, notes = sprintf("placed in conviron %s", trimws(input$conviron)))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Place failed", timeOut = 0); FALSE })
      if (ok) { toastr_success(sprintf("%s placed in %s.", sc, trimws(input$conviron))); self_refresh() }
    })
    
    # ---- update ------------------------------------------------------
    shiny$observeEvent(input$update, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "thermotherapy", "updated", user(),
                       wf = wf, ctx = ctx,
                       notes = if (nzchar(input$update_notes %||% "")) input$update_notes else NA)
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Update failed", timeOut = 0); FALSE })
      if (ok) { toastr_success(sprintf("%s marked updated.", sc)); self_refresh() }
    })
    
    # ---- finish treatment: updated -> completed (close the chamber) --
    shiny$observeEvent(input$finish, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            UPDATE tbl_thermotherapy_detail SET exit_date = $2, exit_notes = $3
            WHERE sample_code = $1 AND exit_date IS NULL",
                    params = list(sc, as.character(input$exit_date),
                                  if (nzchar(input$review_notes %||% "")) input$review_notes else NA))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "thermotherapy", "completed", user(),
                       wf = wf, ctx = ctx,
                       notes = if (nzchar(input$review_notes %||% "")) input$review_notes else "chamber closed")
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Finish failed", timeOut = 0); FALSE })
      if (ok) { toastr_success(sprintf("%s treatment finished \u2014 ready for review.", sc)); self_refresh() }
    })
    
    # ---- review/approve gate: completed -> approved (+ tbl_review) ----
    do_approval <- function(decision) {  # "approved" | "rejected"
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      if (identical(decision, "approved") && !is_admin()) {
        toastr_error("Only an administrator can approve.", title = "Not permitted"); return()
      }
      note <- if (nzchar(input$approve_notes %||% "")) input$approve_notes else NA
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_review (sample_code, stage_code, decision, comments, reviewed_by)
            VALUES ($1, 'thermotherapy', $2, $3, $4)",
                    params = list(sc, decision, note, user()))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "thermotherapy", decision, user(),
                       wf = wf, ctx = ctx, notes = sprintf("review: %s", decision))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Review failed", timeOut = 0); FALSE })
      if (ok) { toastr_success(sprintf("%s %s.", sc, decision)); self_refresh() }
    }
    shiny$observeEvent(input$approve, { do_approval("approved") })
    
    # ---- request approval: a technician asks an admin to review -------
    # Recorded in tbl_order_event so the request is a durable, auditable fact.
    # notify_approvers() is the single place delivery is wired in.
    shiny$observeEvent(input$request_approval, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'approval', 'approval requested', $2, $3)",
                    params = list(r$order_number[1], user(),
                                  sprintf("%s awaiting thermotherapy approval", sc)))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Request failed", timeOut = 0); FALSE })
      if (ok) {
        res <- notify_approvers(sample_code = sc, stage = "thermotherapy",
                                order_number = r$order_number[1], requested_by = user())
        plural <- if (identical(res$n, 1L)) "" else "s"
        if (isTRUE(res$delivered)) {
          toastr_success(sprintf("Approval reminder emailed to %d administrator%s.", res$n, plural))
        } else if (nzchar(res$mailto %||% "")) {
          # no server transport: open the technician's own mail client, already
          # addressed and filled in. The request is recorded either way.
          runjs(sprintf("window.location.href = '%s';", res$mailto))
          toastr_success(sprintf("Approval requested \u2014 opening an email to %d administrator%s.",
                                 res$n, plural))
        } else {
          toastr_warning(paste("Approval requested, but no administrator has an email",
                               "address on file."), title = "Recorded, not sent")
        }
        self_refresh()
      }
    })
    # reject fires from both the review gate (completed) and finish (updated)
    shiny$observeEvent(input$reject_treatment, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      note <- if (nzchar(input$approve_notes %||% "")) input$approve_notes
      else if (nzchar(input$review_notes %||% "")) input$review_notes else NA
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "thermotherapy", "rejected", user(),
                       wf = wf, ctx = ctx, notes = if (!is.na(note)) note else "treatment rejected")
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Reject failed", timeOut = 0); FALSE })
      if (ok) { toastr_success(sprintf("%s rejected.", sc)); self_refresh() }
    })
    
    # ---- push: approved -> meristem_culture/received (the handoff) ---
    shiny$observeEvent(input$push, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          # move the sample onto the meristem bench in 'received' - it leaves
          # thermotherapy (view_sample_current follows the latest event) and
          # shows up in meristem's Incoming queue to be received.
          record_event(conn, sc, "meristem_culture", "received", user(),
                       wf = wf, ctx = ctx, notes = "pushed from thermotherapy")
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'thermotherapy', $2, $3, $4)",
                    params = list(r$order_number[1], sprintf("pushed %s to meristem", sc), user(),
                                  "awaiting receipt at meristem culture"))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Push failed", timeOut = 0); FALSE })
      if (ok) {
        toastr_success(sprintf("%s pushed to meristem culture.", sc))
        selected(NULL); self_refresh(); signal_others()
      }
    })
    
    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

