box::use(
  shiny,
  shinyjs[useShinyjs],
  shinyFeedback[useShinyFeedback],
  shinytoastr[useToastr, toastr_success, toastr_error],
  pool[poolWithTransaction],
  DBI[dbGetQuery, dbExecute, dbAppendTable],
  waiter[waiter_show, waiter_hide, useWaiter, spin_6],
)

box::use(
  app/view/orders/new_order,
  app/view/orders/new_order_details,
  app/view/shared/order_theme,
  app/logic/fct_conn[pool],
  app/logic/fct_file_upload[file_upload],
)

# ============================================================================
# ORDER REGISTRATION · the save
# ----------------------------------------------------------------------------
# WHAT THIS WRITES NOW (and what it no longer writes)
#
#   tbl_order          <- was tbl_project
#   tbl_order_detail   <- was tbl_project_details
#   tbl_order_service  <- was tbl_project_services. ONE ROW PER SERVICE now,
#                         not eight columns in one row.
#   tbl_order_test     <- was tbl_project_test_methods (which had no PK and
#                         no FK, so anything at all could be written into it)
#   tbl_file           <- was file_uploads
#   tbl_order_event    <- was tbl_project_log
#
#   tbl_project_status is GONE. Nothing replaces it. Order status is DERIVED
#   in view_order_progress from the service lines. The old code wrote
#   percentage_complete = 2.5 - a hardcoded number that was never updated by
#   anything, on a row that then had to be kept in step with reality by hand.
#
#   NO SAMPLES ARE CREATED HERE. At reception the lab has a consignment, not
#   samples: quarantine receives it onto a bench and initiation cuts explants
#   from it later. tbl_sample rows are born there, not here.
#
# THE ORDER NUMBER is minted by next_order_number() as the FIRST statement of
# the transaction, and threaded through every subsequent write. The old code
# read a COUNT(*) at form-render and wrote it at save, so two technicians
# registering in the same month produced the same code and the second lost
# their entire order to a primary-key violation.
# ============================================================================

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "rtb-intake",
    order_theme$head_orders(),
    useShinyjs(), useShinyFeedback(), useToastr(), useWaiter(),

    shiny$div(
      class = "intake-header",
      shiny$div(
        shiny$tags$h1("New sample intake & registration"),
        shiny$tags$p("Log incoming material and record the services requested."),
        order_theme$order_pill(shiny$textOutput(ns("header_order_code"), inline = TRUE))
      ),
      shiny$div(
        class = "actions",
        shiny$actionButton(ns("reset_form"), "Reset", icon = shiny$icon("undo"),
                           class = "btn btn-outline-secondary"),
        shiny$actionButton(ns("save_all"), "Save new order", icon = shiny$icon("check-double"),
                           class = "btn btn-info")
      )
    ),

    shiny$fluidRow(
      shiny$column(4, new_order$ui(ns("identity"))),
      shiny$column(8, new_order_details$ui(ns("details")))
    )
  )
}

#' @export
server <- function(id, res_auth, page, tab, trigger_refresh) {
  shiny$moduleServer(id, function(input, output, session) {

    step1 <- new_order$server(
      "identity", res_auth, page = page, tab = tab,
      trigger_refresh = trigger_refresh,
      clear_clicked = shiny$reactive(input$reset_form)
    )

    step2 <- new_order_details$server(
      "details", res_auth, page, tab,trigger_refresh, 
      clear_clicked = shiny$reactive(input$reset_form)
    )

    output$header_order_code <- shiny$renderText({
      code <- step1$preview_code()
      if (is.null(code) || !nzchar(code)) "\u2014" else code
    })

    saved_code <- shiny$reactiveVal(NULL)

    shiny$observeEvent(input$save_all, {
      shiny$req(page == tab())

      # validate both halves before opening a transaction
      if (!step1$is_valid()) {
        step1$show_errors()
        toastr_error("Fix the highlighted fields in Order identity.",
                     title = "Cannot save", position = "bottom-right")
        return()
      }
      if (!step2$is_valid()) {
        toastr_error(step2$validation_message() %||% "Complete the sample details.",
                     title = "Cannot save", position = "bottom-right")
        return()
      }

      waiter_show(html = spin_6(), color = "rgba(22,36,28,.6)")
      on.exit(waiter_hide(), add = TRUE)

      user <- shiny$reactiveValuesToList(res_auth)$user
      code <- NULL

      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {

          # 1. Make sure the actor exists, so every created_by FK resolves.
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user, isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))

          # 2. Mint the authoritative order number ATOMICALLY. Concurrent
          #    callers serialise on the counter row, so this cannot collide.
          code <<- dbGetQuery(conn, "SELECT next_order_number() AS code")$code[1]

          # 3. The order
          o <- step1$data()
          o$order_number <- code
          o$created_by   <- user
          dbAppendTable(conn, "tbl_order", o)

          # 4. Reception + incoming material
          d <- step2$data()
          d$order_number <- code
          dbAppendTable(conn, "tbl_order_detail", d)

          # 5. THE SERVICE LINES - one row per requested service.
          #    origin='customer_order' distinguishes these from work the lab
          #    adds later off its own bat (continuations, reflex subculture).
          svc <- step2$services()
          if (nrow(svc) > 0) {
            svc$order_number <- code
            svc$origin       <- "customer_order"
            svc$requested_by <- user
            dbAppendTable(conn, "tbl_order_service", svc)
          }

          # 6. Requested tests
          tests <- step2$tests()
          if (length(tests) > 0) {
            dbAppendTable(conn, "tbl_order_test",
                          data.frame(order_number = code,
                                     test_id      = as.integer(tests),
                                     stringsAsFactors = FALSE))
          }

          # 7. Attachment, if any. Passing `conn` (not pool) puts the row in
          #    THIS transaction, so a later failure takes it back out.
          f <- step2$file_name()
          if (!is.null(f) && is.data.frame(f) && nrow(f) > 0) {
            file_upload(
              file_input   = f,
              description  = step2$file_description(),
              max_size_mb  = 10,
              order_number = code,
              user         = user,
              conn         = conn
            )
          }

          # 8. The order-level event log (was tbl_project_log). Append-only:
          #    history is a by-product of doing the work, not a chore.
          dbAppendTable(conn, "tbl_order_event",
                        data.frame(order_number = code,
                                   module = "reception",
                                   action = "registered",
                                   actor  = user,
                                   stringsAsFactors = FALSE))
        })
        TRUE
      }, error = function(e) {
        toastr_error(paste("Save failed:", conditionMessage(e)),
                     title = "Error", position = "bottom-right", timeOut = 0)
        FALSE
      })

      if (!ok) return()

      saved_code(code)
      toastr_success(paste("Order", code, "registered."),
                     title = "Saved", position = "bottom-right")
      trigger_refresh(trigger_refresh() + 1)
    })

    list(saved_code = saved_code)
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
