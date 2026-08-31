box::use(
  shiny,
  reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
  shinyjs[useShinyjs, hide, show],
  htmlwidgets[JS],
)

box::use(
  app/logic/fct_tracking[order_board, order_counts],
  app/view/orders/view_order,
  app/view/shared/order_theme,
)

# ============================================================================
# ORDER MANAGEMENT · every order, its status, and what happens next
# ----------------------------------------------------------------------------
# The question this page answers is "what needs doing?", which the old command
# centre could not answer at all: its percentage column read from
# view_project_completion, which INNER JOINed a table the app never wrote, so
# every order showed 0% and the column was hidden.
#
# THREE COLUMNS DO THE WORK, and they come from three different places:
#
#   STATUS   view_order_progress.derived_status - COMPUTED from the service
#            lines. Not stored, so it cannot drift. An order is complete when
#            its services are, by construction.
#   CURRENT  view_sample_current - where the samples actually are. An order
#            may appear in several stages at once; that is the fan-out
#            working, not a bug.
#   NEXT     the workflow - a RECOMMENDATION. The column is headed "NEXT
#            (RECOMMENDED)" for that reason. An unapproved order shows
#            "Review & approve" instead, because the next act there is a
#            human decision rather than a lab step.
#
# LIST AND DETAIL: this module holds `selected` and swaps between its own
# table and view_order's detail. Clicking a row just works, and view_order
# stays reusable from anywhere else.
#
# ----------------------------------------------------------------------------
# RENDERING - list/detail is a visibility toggle, not a re-render
# ----------------------------------------------------------------------------
# The previous version put the whole page behind one uiOutput("screen"),
# whose renderUI picked between list_screen(ns) and view_order$ui() depending
# on `selected()`. That cost more than a slow first paint:
#
#   - Nothing painted until the server round-tripped once just to say "here's
#     the list screen" - and list_screen's own kpis/guide/tbl placeholders
#     then needed a SECOND round trip before they could be requested, because
#     the browser didn't know they existed until the first one came back.
#   - Because input$q and input$status_filter live inside list_screen(), and
#     list_screen() was rebuilt from scratch by renderUI every time selected()
#     changed, opening an order and clicking "back" silently reset the search
#     box and the active tab. Not a slowdown - a bug.
#   - Returning to the list also tore down and rebuilt the `tbl` reactable,
#     losing its sort/page state, for the same reason view_order's tables
#     used to.
#
# Fix: both the list screen and view_order's UI are mounted once, in ui(),
# and a plain observer toggles which one is visible via shinyjs. Shiny
# suspends computation for outputs that are hidden (suspendWhenHidden,
# on by default), so the inactive side isn't doing wasted work - and
# view_order's own reactives additionally short-circuit via req(selected())
# regardless, so the detail side does nothing at all until an order_number
# is actually selected. Nothing is torn down on the way back, so search text,
# the active tab and table sort order all survive.
# ============================================================================

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    shiny$div(id = ns("list_pane"), list_screen(ns)),
    shiny$div(id = ns("detail_pane"), view_order$ui(ns("detail_mod")))
  )
}


#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected <- shiny$reactiveVal(NULL)   # NULL = list, else that order
    
    shiny$observe({
      if (is.null(selected())) {
        show("list_pane");  hide("detail_pane")
      } else {
        hide("list_pane");  show("detail_pane")
      }
    })
    
    # view_order's server is started ONCE, and reads `selected` reactively -
    # instantiating it per click would stack observers.
    view_order$server(
      "detail_mod",
      res_auth = res_auth,
      page = page,
      tab = tab,
      order_number = selected,
      trigger_refresh = trigger_refresh,
      on_back = function() selected(NULL)
    )
    
    shiny$observeEvent(input$open_row, {
      shiny$req(input$open_row$id)
      selected(input$open_row$id)
    })
    
    # ---- data --------------------------------------------------------
    # Fetching and filtering used to happen in the same reactive, which meant
    # every keystroke in the search box - input$q updates on every keystroke
    # by default - re-ran order_board() against the database, then threw most
    # of the result away client-side. raw_board() now depends on nothing but
    # trigger_refresh; status_filter and the (debounced) search text only
    # re-filter the data frame already sitting in memory.
    raw_board <- shiny$reactive({
      if (!is.null(trigger_refresh)) trigger_refresh()
      order_board()
    })
    
    q_debounced <- shiny$debounce(shiny$reactive(input$q), 300)
    
    board <- shiny$reactive({
      d <- raw_board()
      
      # Default is needs_action, matching the tab that renders active at
      # startup. The dropdown used to carry selected = "needs_action", so the
      # input existed from the first render; the tab bar sets nothing until it
      # is clicked, so the default has to live here instead or the page would
      # open showing ALL orders with "Needs action" highlighted.
      f <- input$status_filter %||% "needs_action"
      if (!identical(f, "all") && nrow(d) > 0) {
        d <- if (identical(f, "needs_action")) {
          d[d$derived_status %in% c("pending_approval", "approved", "in_progress"), , drop = FALSE]
        } else {
          d[d$derived_status == f, , drop = FALSE]
        }
      }
      
      q <- q_debounced()
      if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
        cols <- intersect(c("order_number", "customer_name", "crop_name",
                            "variety_name", "current_stage"), names(d))
        # Vectorised across columns instead of apply(..., 1, ...), which
        # coerces the whole slice to a character matrix before it can even
        # start iterating row by row.
        hay <- do.call(paste, c(as.list(d[cols]), sep = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    # kpis and guide both used to call order_counts() independently - same
    # data, two DB round trips per refresh. One cached reactive, two readers.
    counts <- shiny$reactive({
      if (!is.null(trigger_refresh)) trigger_refresh()
      order_counts()
    })
    count_of <- function(a, s) {
      r <- a[a$derived_status == s, , drop = FALSE]
      if (nrow(r)) as.integer(r$n[1]) else 0L
    }
    
    
    shiny$observe({
      print("order_board")
      print(system.time(order_board()))
      print("order_counts")
      print(system.time(order_counts()))
    })
    # The KPI row IS the tab bar, on `status_filter` - the same input the
    # dropdown used to set, so board() is unchanged.
    #
    # "Needs action" leads because it is the question the page exists to answer,
    # and it is the default. Its count is the sum of the three statuses board()
    # includes under that filter; the arithmetic is repeated here rather than
    # shared because board() filters rows and this counts them, but the two
    # definitions must be kept in step.
    #
    # "Rejected" earns a tab only when there is something in it. A permanent
    # tab reading 0 for a rare terminal state costs attention on every visit;
    # when it is non-zero it matters enough to surface. If it is hidden the
    # orders are still reachable under "All orders".
    #
    # isolate() on the active tab: the highlight moves client-side.
    # The tab bar, in quarantine's flow-stepper language. Awaiting approval ->
    # in progress -> completed is the order an order actually travels in, so
    # those three carry ordinals. "Needs action", "Rejected" and "All orders"
    # are views ACROSS that sequence rather than positions in it, so they are
    # marked instead of numbered - otherwise "Awaiting approval" would be
    # step 2 and the numbers would stop meaning anything.
    output$kpis <- shiny$renderUI({
      a <- counts()
      
      needs  <- count_of(a, "pending_approval") + count_of(a, "approved") + count_of(a, "in_progress")
      reject <- count_of(a, "rejected")
      total  <- if (nrow(a)) sum(as.integer(a$n)) else 0L
      cur    <- shiny$isolate(input$status_filter) %||% "needs_action"
      
      steps <- list(
        list(title = "Awaiting approval", sub = "registration to review",
             value = "pending_approval", count = count_of(a, "pending_approval"), unit = "to approve",
             active = identical(cur, "pending_approval"), waiting = count_of(a, "pending_approval") > 0),
        list(title = "In progress", sub = "services part delivered",
             value = "in_progress", count = count_of(a, "in_progress"), unit = "running",
             active = identical(cur, "in_progress")),
        list(title = "Completed", sub = "all services fulfilled",
             value = "completed", count = count_of(a, "completed"), unit = "done",
             active = identical(cur, "completed"))
      )
      if (reject > 0) {
        steps <- c(steps, list(
          list(title = "Rejected", sub = "registration declined",
               value = "rejected", num = "!", count = reject, unit = "rejected",
               active = identical(cur, "rejected"))))
      }
      # steps <- c(steps, list(
      #   list(title = "All orders", sub = "every status", value = "all",
      #        num = "\u2211", count = total, unit = "orders",
      #        active = identical(cur, "all"))))
      
      order_theme$flow_stepper(steps, input_id = ns("status_filter"))
    })
    
    
    # THE instruction, per tab.
    output$guide <- shiny$renderUI({
      a <- counts()
      pend <- count_of(a, "pending_approval")
      switch(
        input$status_filter %||% "needs_action",
        pending_approval = if (pend > 0)
          order_theme$guide(tone = "do",
                            shiny$strong(pend),
                            if (pend == 1) " order is waiting for approval."
                            else " orders are waiting for approval.",
                            " Open one to review its details, then approve or reject it.")
        else order_theme$guide("Nothing is waiting for approval."),
        in_progress = order_theme$guide(
          "Work has begun on these orders. Progress is measured by service ",
          "fulfilment, so an order reads 0% until material is allocated against ",
          "its service lines.",
          action = order_theme$goto("Quarantine", "quarantine")),
        completed = order_theme$guide(tone = "done",
                                      "Every service on these orders has been fulfilled."),
        rejected  = order_theme$guide("Registration was declined for these orders."),
        all       = order_theme$guide("Every order, whatever its status."),
        order_theme$guide(
          "Orders with work still outstanding. Select one to see its services, ",
          "progress and full history.")
      )
    })
    
    output$tbl <- renderReactable({
      d <- board()
      d$`__open` <- rep(NA, nrow(d))   # rep(): an empty board is the fresh-DB state
      
      reactable(
        d,
        columns = list(
          approval_state     = colDef(show = FALSE),
          sample_type_code   = colDef(show = FALSE),
          services_requested = colDef(show = FALSE),
          services_fulfilled = colDef(show = FALSE),
          order_kind         = colDef(show = FALSE),
          variety_name       = colDef(show = FALSE),
          sample_amount      = colDef(show = FALSE),
          
          order_number = colDef(name = "ORDER", width = 165, cell = function(v) {
            shiny$tags$strong(v)
          }),
          customer_name = colDef(name = "CUSTOMER", minWidth = 140),
          crop_name = colDef(name = "CROP", width = 100,
                             cell = function(v) if (is.na(v)) "\u2014" else v),
          
          derived_status = colDef(name = "STATUS", width = 140, cell = function(v) {
            order_theme$chip(toupper(gsub("_", " ", v)),
                             switch(v, completed = "brand", in_progress = "teal",
                                    pending_approval = "amber", rejected = "red",
                                    cancelled = "ink", "ink"))
          }),
          
          pct_complete = colDef(name = "PROGRESS", width = 115,
                                cell = function(v) order_theme$mini_bar(v)),
          
          current_stage = colDef(name = "CURRENT", minWidth = 165, cell = function(v) {
            if (is.na(v)) {
              return(shiny$span(style = "color:var(--ink-faint); font-size:12px;",
                                "No samples yet"))
            }
            shiny$span(style = "font-size:12px;", v)
          }),
          
          # RECOMMENDED, and the header says so. The database will accept any
          # legal transition; this column is the workflow's opinion.
          next_step = colDef(name = "NEXT (RECOMMENDED)", minWidth = 195, cell = function(v) {
            if (is.na(v)) {
              return(shiny$span(style = "color:var(--ink-faint); font-size:12px;", "\u2014"))
            }
            shiny$span(style = "font-size:12px; color:var(--teal); font-weight:500;", v)
          }),
          
          created_on = colDef(name = "REGISTERED", width = 105,
                              cell = function(v) format(as.Date(v), "%d %b %y")),
          
          `__open` = colDef(name = "", sortable = FALSE, width = 85, html = TRUE,
                            cell = JS(sprintf("function(ci){
              return '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + ci.row['order_number'] + '\\', n: Math.random()})\">Open</button>';
            }", ns("open_row"))))
        ),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = reactableLang(
          noData = "No orders yet \u2014 register one under NEW ORDER REGISTRATION."
        ),
        theme = order_theme$rt_theme()
      )
    })
    
    invisible(NULL)
  })
}

list_screen <- function(ns) {
  shiny$tagList(
    order_theme$page_header(
      title = "Order management",
      sub   = "Every order, its live status, and what happens next."
    ),
    shiny$uiOutput(ns("kpis")),
    shiny$uiOutput(ns("guide")),
    order_theme$toolbar(
      order_theme$search_box(ns("q"), "Search order, customer, crop, stage...")
    ),
    order_theme$table_card(reactableOutput(ns("tbl")))
  )
}



`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' box::use(
#'   shiny,
#'   reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
#'   shinyjs[useShinyjs],
#'   htmlwidgets[JS],
#' )
#' 
#' box::use(
#'   app/logic/fct_tracking[order_board, order_counts],
#'   app/view/orders/view_order,
#'   app/view/shared/order_theme,
#' )
#' 
#' # ============================================================================
#' # ORDER MANAGEMENT · every order, its status, and what happens next
#' # ----------------------------------------------------------------------------
#' # The question this page answers is "what needs doing?", which the old command
#' # centre could not answer at all: its percentage column read from
#' # view_project_completion, which INNER JOINed a table the app never wrote, so
#' # every order showed 0% and the column was hidden.
#' #
#' # THREE COLUMNS DO THE WORK, and they come from three different places:
#' #
#' #   STATUS   view_order_progress.derived_status - COMPUTED from the service
#' #            lines. Not stored, so it cannot drift. An order is complete when
#' #            its services are, by construction.
#' #   CURRENT  view_sample_current - where the samples actually are. An order
#' #            may appear in several stages at once; that is the fan-out
#' #            working, not a bug.
#' #   NEXT     the workflow - a RECOMMENDATION. The column is headed "NEXT
#' #            (RECOMMENDED)" for that reason. An unapproved order shows
#' #            "Review & approve" instead, because the next act there is a
#' #            human decision rather than a lab step.
#' #
#' # LIST AND DETAIL: this module holds `selected` and swaps between its own
#' # table and view_order's detail. Clicking a row just works, and view_order
#' # stays reusable from anywhere else.
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
#' server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
#'   shiny$moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'     
#'     selected <- shiny$reactiveVal(NULL)   # NULL = list, else that order
#'     
#'     output$screen <- shiny$renderUI({
#'       if (is.null(selected())) list_screen(ns) else shiny$uiOutput(ns("detail"))
#'     })
#'     
#'     # view_order's server is started ONCE, and reads `selected` reactively -
#'     # instantiating it per click would stack observers.
#'     view_order$server(
#'       "detail_mod",
#'       res_auth = res_auth,
#'       page = page,
#'       tab = tab,
#'       order_number = selected,
#'       trigger_refresh = trigger_refresh,
#'       on_back = function() selected(NULL)
#'     )
#'     
#'     output$detail <- shiny$renderUI({
#'       shiny$req(selected())
#'       view_order$ui(ns("detail_mod"))
#'     })
#'     
#'     shiny$observeEvent(input$open_row, {
#'       shiny$req(input$open_row$id)
#'       selected(input$open_row$id)
#'     })
#'     
#'     # ---- data --------------------------------------------------------
#'     board <- shiny$reactive({
#'       if (!is.null(trigger_refresh)) trigger_refresh()
#'       d <- order_board()
#'       
#'       # Default is needs_action, matching the tab that renders active at
#'       # startup. The dropdown used to carry selected = "needs_action", so the
#'       # input existed from the first render; the tab bar sets nothing until it
#'       # is clicked, so the default has to live here instead or the page would
#'       # open showing ALL orders with "Needs action" highlighted.
#'       f <- input$status_filter %||% "needs_action"
#'       if (!identical(f, "all") && nrow(d) > 0) {
#'         d <- if (identical(f, "needs_action")) {
#'           d[d$derived_status %in% c("pending_approval", "approved", "in_progress"), , drop = FALSE]
#'         } else {
#'           d[d$derived_status == f, , drop = FALSE]
#'         }
#'       }
#'       
#'       q <- input$q
#'       if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
#'         cols <- intersect(c("order_number", "customer_name", "crop_name",
#'                             "variety_name", "current_stage"), names(d))
#'         hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
#'         d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
#'       }
#'       d
#'     })
#'     
#'     # The KPI row IS the tab bar, on `status_filter` - the same input the
#'     # dropdown used to set, so board() is unchanged.
#'     #
#'     # "Needs action" leads because it is the question the page exists to answer,
#'     # and it is the default. Its count is the sum of the three statuses board()
#'     # includes under that filter; the arithmetic is repeated here rather than
#'     # shared because board() filters rows and this counts them, but the two
#'     # definitions must be kept in step.
#'     #
#'     # "Rejected" earns a tab only when there is something in it. A permanent
#'     # tab reading 0 for a rare terminal state costs attention on every visit;
#'     # when it is non-zero it matters enough to surface. If it is hidden the
#'     # orders are still reachable under "All orders".
#'     #
#'     # isolate() on the active tab: the highlight moves client-side.
#'     # The tab bar, in quarantine's flow-stepper language. Awaiting approval ->
#'     # in progress -> completed is the order an order actually travels in, so
#'     # those three carry ordinals. "Needs action", "Rejected" and "All orders"
#'     # are views ACROSS that sequence rather than positions in it, so they are
#'     # marked instead of numbered - otherwise "Awaiting approval" would be
#'     # step 2 and the numbers would stop meaning anything.
#'     #
#'     # `status_filter` is the same input the dropdown used to set, so board() is
#'     # unchanged apart from its default.
#'     #
#'     # "Needs action" leads because it is the question the page exists to answer.
#'     # Its count is the sum of the three statuses board() includes under that
#'     # filter; the arithmetic is repeated here rather than shared because board()
#'     # filters rows and this counts them, but the two must be kept in step.
#'     #
#'     # "Rejected" earns a tab only when there is something in it. A permanent tab
#'     # reading 0 for a rare terminal state costs attention on every visit; when
#'     # non-zero it matters enough to surface. Hidden, those orders are still
#'     # reachable under "All orders".
#'     #
#'     # isolate() on the active tab: the highlight moves client-side.
#'     output$kpis <- shiny$renderUI({
#'       if (!is.null(trigger_refresh)) trigger_refresh()
#'       a <- order_counts()
#'       g <- function(s) { r <- a[a$derived_status == s, , drop = FALSE]; if (nrow(r)) as.integer(r$n[1]) else 0L }
#'       
#'       needs  <- g("pending_approval") + g("approved") + g("in_progress")
#'       reject <- g("rejected")
#'       total  <- if (nrow(a)) sum(as.integer(a$n)) else 0L
#'       cur    <- shiny$isolate(input$status_filter) %||% "needs_action"
#'       
#'       steps <- list(
#'         # list(title = "Needs action", sub = "everything not yet finished",
#'         #      value = "needs_action", num = "\u2691", count = needs, unit = "orders",
#'         #      active = identical(cur, "needs_action"), waiting = needs > 0),
#'         list(title = "Awaiting approval", sub = "registration to review",
#'              value = "pending_approval", count = g("pending_approval"), unit = "to approve",
#'              active = identical(cur, "pending_approval"), waiting = g("pending_approval") > 0),
#'         list(title = "In progress", sub = "services part delivered",
#'              value = "in_progress", count = g("in_progress"), unit = "running",
#'              active = identical(cur, "in_progress")),
#'         list(title = "Completed", sub = "all services fulfilled",
#'              value = "completed", count = g("completed"), unit = "done",
#'              active = identical(cur, "completed"))
#'       )
#'       if (reject > 0) {
#'         steps <- c(steps, list(
#'           list(title = "Rejected", sub = "registration declined",
#'                value = "rejected", num = "!", count = reject, unit = "rejected",
#'                active = identical(cur, "rejected"))))
#'       }
#'       steps <- c(steps, list(
#'         list(title = "All orders", sub = "every status", value = "all",
#'              num = "\u2211", count = total, unit = "orders",
#'              active = identical(cur, "all"))))
#'       
#'       order_theme$flow_stepper(steps, input_id = ns("status_filter"))
#'     })
#'     
#'     
#'     # THE instruction, per tab.
#'     output$guide <- shiny$renderUI({
#'       if (!is.null(trigger_refresh)) trigger_refresh()
#'       a <- order_counts()
#'       g <- function(s) { r <- a[a$derived_status == s, , drop = FALSE]; if (nrow(r)) as.integer(r$n[1]) else 0L }
#'       pend <- g("pending_approval")
#'       switch(
#'         input$status_filter %||% "needs_action",
#'         pending_approval = if (pend > 0)
#'           order_theme$guide(tone = "do",
#'                             shiny$strong(pend),
#'                             if (pend == 1) " order is waiting for approval."
#'                             else " orders are waiting for approval.",
#'                             " Open one to review its details, then approve or reject it.")
#'         else order_theme$guide("Nothing is waiting for approval."),
#'         in_progress = order_theme$guide(
#'           "Work has begun on these orders. Progress is measured by service ",
#'           "fulfilment, so an order reads 0% until material is allocated against ",
#'           "its service lines.",
#'           action = order_theme$goto("Quarantine", "quarantine")),
#'         completed = order_theme$guide(tone = "done",
#'                                       "Every service on these orders has been fulfilled."),
#'         rejected  = order_theme$guide("Registration was declined for these orders."),
#'         all       = order_theme$guide("Every order, whatever its status."),
#'         order_theme$guide(
#'           "Orders with work still outstanding. Select one to see its services, ",
#'           "progress and full history.")
#'       )
#'     })
#'     
#'     output$tbl <- renderReactable({
#'       d <- board()
#'       d$`__open` <- rep(NA, nrow(d))   # rep(): an empty board is the fresh-DB state
#'       
#'       reactable(
#'         d,
#'         columns = list(
#'           approval_state     = colDef(show = FALSE),
#'           sample_type_code   = colDef(show = FALSE),
#'           services_requested = colDef(show = FALSE),
#'           services_fulfilled = colDef(show = FALSE),
#'           order_kind         = colDef(show = FALSE),
#'           variety_name       = colDef(show = FALSE),
#'           sample_amount      = colDef(show = FALSE),
#'           
#'           order_number = colDef(name = "ORDER", width = 165, cell = function(v) {
#'             shiny$tags$strong(v)
#'           }),
#'           customer_name = colDef(name = "CUSTOMER", minWidth = 140),
#'           crop_name = colDef(name = "CROP", width = 100,
#'                              cell = function(v) if (is.na(v)) "\u2014" else v),
#'           
#'           derived_status = colDef(name = "STATUS", width = 140, cell = function(v) {
#'             order_theme$chip(toupper(gsub("_", " ", v)),
#'                              switch(v, completed = "brand", in_progress = "teal",
#'                                     pending_approval = "amber", rejected = "red",
#'                                     cancelled = "ink", "ink"))
#'           }),
#'           
#'           pct_complete = colDef(name = "PROGRESS", width = 115,
#'                                 cell = function(v) order_theme$mini_bar(v)),
#'           
#'           current_stage = colDef(name = "CURRENT", minWidth = 165, cell = function(v) {
#'             if (is.na(v)) {
#'               return(shiny$span(style = "color:var(--ink-faint); font-size:12px;",
#'                                 "No samples yet"))
#'             }
#'             shiny$span(style = "font-size:12px;", v)
#'           }),
#'           
#'           # RECOMMENDED, and the header says so. The database will accept any
#'           # legal transition; this column is the workflow's opinion.
#'           next_step = colDef(name = "NEXT (RECOMMENDED)", minWidth = 195, cell = function(v) {
#'             if (is.na(v)) {
#'               return(shiny$span(style = "color:var(--ink-faint); font-size:12px;", "\u2014"))
#'             }
#'             shiny$span(style = "font-size:12px; color:var(--teal); font-weight:500;", v)
#'           }),
#'           
#'           created_on = colDef(name = "REGISTERED", width = 105,
#'                               cell = function(v) format(as.Date(v), "%d %b %y")),
#'           
#'           `__open` = colDef(name = "", sortable = FALSE, width = 85, html = TRUE,
#'                             cell = JS(sprintf("function(ci){
#'               return '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + ci.row['order_number'] + '\\', n: Math.random()})\">Open</button>';
#'             }", ns("open_row"))))
#'         ),
#'         defaultPageSize = 12, compact = TRUE, highlight = TRUE,
#'         language = reactableLang(
#'           noData = "No orders yet \u2014 register one under NEW ORDER REGISTRATION."
#'         ),
#'         theme = order_theme$rt_theme()
#'       )
#'     })
#'     
#'     invisible(NULL)
#'   })
#' }
#' 
#' list_screen <- function(ns) {
#'   shiny$tagList(
#'     order_theme$page_header(
#'       title = "Order management",
#'       sub   = "Every order, its live status, and what happens next."
#'     ),
#'     shiny$uiOutput(ns("kpis")),
#'     shiny$uiOutput(ns("guide")),
#'     order_theme$toolbar(
#'       order_theme$search_box(ns("q"), "Search order, customer, crop, stage...")
#'     ),
#'     order_theme$table_card(reactableOutput(ns("tbl")))
#'   )
#' }
#' 
#' 
#' 
#' `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a