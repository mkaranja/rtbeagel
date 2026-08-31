box::use(
  shiny,
  shinyWidgets[show_alert],
  stats[setNames],
  shinyjs[disabled, enable, disable],
  shinyFeedback[useShinyFeedback, feedbackDanger],
  shinytoastr[toastr_error],
)

box::use(
  app/logic/fct_conn[pool, load_data],
  app/view/shared/order_theme,
)

# ============================================================================
# NEW ORDER · identity + billing
# ----------------------------------------------------------------------------
# Rewritten for the new schema. What changed and why:
#
#   project_code -> order_number, minted by next_order_number() INSIDE the
#   save transaction. This module only ever shows a PREVIEW via
#   peek_order_number(), which does not advance the counter. The old code
#   read `SELECT count FROM current_month_project_count` at form-render and
#   wrote it at save, so two technicians registering at once computed the
#   same code and the second save died on the primary key.
#
#   `status = "Active"` is GONE. tbl_order has no status column - order
#   status is derived from its service lines in view_order_progress. The
#   old design stored it and then had to remember to keep it true.
#
#   payment_made is now a real column with a CHECK constraint behind it
#   (ck_billing_complete), so a paid order without a receipt is rejected by
#   the database, not merely by this form.
# ============================================================================

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    useShinyFeedback(),
    
    order_theme$section(
      "1", "Order identity",
      sub = "System-generated order number",
      
      shiny$textInput(
        ns("order_number"),
        shiny$HTML("Lab order no <span class='mandatory_star'>*</span> <span class='text-muted'>(provisional \u2014 confirmed on save)</span>"),
        value = ""
      ) |> disabled(),
      
      shiny$selectizeInput(
        ns("customer_id"),
        shiny$HTML("Customer <span class='mandatory_star'>*</span>"),
        choices = c("--SELECT--" = 0)
      ),
      # choices are filled from tbl_report_format / tbl_dispatch_method by
      # the server - they used to be two hardcoded strings with one real
      # option each.
      shiny$selectizeInput(
        ns("report_format_code"),
        shiny$HTML("Report format <span class='mandatory_star'>*</span>"),
        choices = c("--SELECT--" = "")
      ),
      shiny$selectizeInput(
        ns("dispatch_code"),
        shiny$HTML("Results dispatch <span class='mandatory_star'>*</span>"),
        choices = c("--SELECT--" = "")
      ),
      shiny$numericInput(
        ns("sample_amount"),
        shiny$HTML("Number of samples <span class='mandatory_star'>*</span>"),
        value = NULL, min = 1
      )
    ),
    
    order_theme$section(
      "2", "Billing",
      accent = "amber",
      sub = "Required only if payment has been made",
      
      shiny$selectizeInput(
        ns("payment_made"),
        shiny$HTML("Payment made? <span class='mandatory_star'>*</span>"),
        choices = c("No", "Yes")
      ),
      shiny$numericInput(
        ns("amount_charged"),
        shiny$HTML("Amount charged <span class='mandatory_star'>*</span>"),
        value = NULL, min = 0
      ) |> disabled(),
      shiny$textInput(
        ns("receipt_no"),
        shiny$HTML("Receipt number <span class='mandatory_star'>*</span>"),
        value = ""
      ) |> disabled()
    )
  )
}

#' @export
server <- function(id, res_auth, page, tab, dashboard, trigger_refresh, clear_clicked) {
  shiny$moduleServer(id, function(input, output, session) {
    
    update_feedback <- function(input_id, show_error, message = "required") {
      feedbackDanger(input_id, show = show_error, text = message, icon = NULL, color = "#A23A32")
    }
    
    # ---- provisional preview only; never authoritative ---------------
    next_order_no <- shiny$reactive({
      trigger_refresh()
      res <- tryCatch(load_data(pool, "SELECT peek_order_number() AS code"),
                      error = function(e) NULL)
      if (is.null(res) || nrow(res) == 0 || is.na(res$code[1])) {
        return(paste0("PQS-", format(Sys.Date(), "%Y"), "-",
                      toupper(format(Sys.Date(), "%b")), "-XXX"))
      }
      res$code[1]
    })
    
    shiny$observe({
      shiny$req(page == tab())
      shiny$updateTextInput(session, "order_number", value = next_order_no())
    })
    
    load_customers <- function() {
      load_data(pool, "
        SELECT customer_id, customer_name
        FROM tbl_customer
        WHERE active
        ORDER BY customer_name")
    }
    
    shiny$observe({
      shiny$req(page == tab())
      trigger_refresh()
      
      customers <- load_customers()
      if (nrow(customers) > 0) {
        shiny$updateSelectizeInput(session, "customer_id",
                                   choices = c("--SELECT--" = 0,
                                               setNames(customers$customer_id, customers$customer_name)))
      }
      
      rf <- load_data(pool, "SELECT format_code, label FROM tbl_report_format
                             WHERE active ORDER BY sort_order, label")
      if (nrow(rf) > 0) {
        shiny$updateSelectizeInput(session, "report_format_code",
                                   choices = c("--SELECT--" = "", setNames(rf$format_code, rf$label)))
      }
      
      dm <- load_data(pool, "SELECT dispatch_code, label FROM tbl_dispatch_method
                             WHERE active ORDER BY sort_order, label")
      if (nrow(dm) > 0) {
        shiny$updateSelectizeInput(session, "dispatch_code",
                                   choices = c("--SELECT--" = "", setNames(dm$dispatch_code, dm$label)))
      }
    })
    
    shiny$observeEvent(input$payment_made, {
      shiny$req(page == tab())
      if (isTRUE(input$payment_made == "Yes")) {
        enable("amount_charged"); enable("receipt_no")
      } else {
        disable("amount_charged"); disable("receipt_no")
        shiny$updateNumericInput(session, "amount_charged", value = NA)
        shiny$updateTextInput(session, "receipt_no", value = "")
      }
    })
    
    # ---- validation --------------------------------------------------
    validation_rules <- list(
      customer_id = list(
        condition = function() is.null(input$customer_id) || is.na(input$customer_id) ||
          input$customer_id == 0 || input$customer_id == "0",
        message = "required"),
      report_format_code = list(
        condition = function() is.null(input$report_format_code) || !nzchar(input$report_format_code),
        message = "required"),
      dispatch_code = list(
        condition = function() is.null(input$dispatch_code) || !nzchar(input$dispatch_code),
        message = "required"),
      sample_amount = list(
        condition = function() is.null(input$sample_amount) || is.na(input$sample_amount) ||
          input$sample_amount < 1,
        message = "must be at least 1"),
      amount_charged = list(
        condition = function() isTRUE(input$payment_made == "Yes") &&
          (is.null(input$amount_charged) || is.na(input$amount_charged) ||
             input$amount_charged <= 0),
        message = "required when payment made"),
      receipt_no = list(
        condition = function() isTRUE(input$payment_made == "Yes") &&
          (is.null(input$receipt_no) || nchar(trimws(input$receipt_no)) == 0),
        message = "required when payment made")
    )
    
    check_rule <- function(name) {
      r <- tryCatch(validation_rules[[name]]$condition(), error = function(e) TRUE)
      if (is.na(r)) TRUE else isTRUE(r)
    }
    
    is_valid <- shiny$reactive({
      !any(vapply(names(validation_rules), check_rule, logical(1)))
    })
    
    show_errors <- function() {
      for (nm in names(validation_rules)) {
        bad <- check_rule(nm)
        update_feedback(nm, bad, validation_rules[[nm]]$message)
      }
    }
    
    shiny$observeEvent(input$next_btn, {
      shiny$req(page == tab())
      show_errors()
      if (!is_valid()) {
        toastr_error("Please fix the errors below before proceeding.",
                     title = "Error:", position = "bottom-right", preventDuplicates = TRUE)
      }
    })
    
    # ---- the order row -----------------------------------------------
    # order_number is deliberately ABSENT: it is minted inside the save
    # transaction. approval_state/created_on come from column defaults.
    new_order_row <- shiny$reactive({
      shiny$req(page == tab())
      paid <- isTRUE(input$payment_made == "Yes")
      data.frame(
        customer_id      = as.integer(input$customer_id),
        sample_amount    = as.integer(input$sample_amount),
        report_format_code = input$report_format_code,
        dispatch_code      = input$dispatch_code,
        payment_made     = paid,
        amount_charged   = if (paid) as.numeric(input$amount_charged) else NA_real_,
        receipt_no       = if (paid) input$receipt_no else NA_character_,
        created_by       = shiny$reactiveValuesToList(res_auth)$user,
        stringsAsFactors = FALSE
      )
    })
    
    reset_form <- function() {
      customers <- load_customers()
      shiny$updateSelectizeInput(session, "customer_id",
                                 choices = c("--SELECT--" = 0, setNames(customers$customer_id, customers$customer_name)))
      shiny$updateTextInput(session, "order_number", value = next_order_no())
      shiny$updateSelectizeInput(session, "report_format_code", selected = "")
      shiny$updateSelectizeInput(session, "dispatch_code", selected = "")
      shiny$updateNumericInput(session, "sample_amount", value = NA)
      shiny$updateSelectizeInput(session, "payment_made", choices = c("No", "Yes"))
      shiny$updateNumericInput(session, "amount_charged", value = NA)
      shiny$updateTextInput(session, "receipt_no", value = "")
      disable("amount_charged"); disable("receipt_no")
      for (nm in names(validation_rules)) update_feedback(nm, FALSE)
    }
    
    shiny$observeEvent(trigger_refresh(), { shiny$req(page == tab()); reset_form() })
    shiny$observeEvent(clear_clicked(),   { shiny$req(page == tab()); reset_form() })
    
    list(
      data           = new_order_row,
      is_valid       = is_valid,
      show_errors    = show_errors,
      next_clicked   = shiny$reactive(input$next_btn),
      clear_clicked  = shiny$reactive(input$clear_btn),
      preview_code   = shiny$reactive(input$order_number)
    )
  })
}