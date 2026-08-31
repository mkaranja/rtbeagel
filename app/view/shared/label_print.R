box::use(
  shiny,
  shinytoastr[toastr_success, toastr_error, toastr_warning],
  DBI[dbExecute],
)

box::use(
  app/logic/fct_conn[pool],
  app/logic/fct_zpl[label_spec, zpl_batch, zpl_test_label],
  app/view/shared/order_theme,
)

# ============================================================================
# LABEL PRINT  -  reusable, drop into any module that mints a code
# ----------------------------------------------------------------------------
# USAGE  (three lines wherever a new code is created)
#
#   ui:      label_print$ui(ns("print"))
#
#   server:  printer <- label_print$server("print")
#            ...
#            printer$print(data.frame(
#              code  = new_codes,                    # required
#              title = "EXPLANT",
#              line1 = paste(crop, variety),
#              line2 = format(Sys.Date(), "%d %b %Y")
#            ))
#
# `printer$print()` may be called from anywhere - an observeEvent that has just
# INSERTed rows, a bulk action, a retry button. It does not need to be near the
# UI, which is the point: the code is minted deep in a transaction and the
# label has to follow it.
#
# ----------------------------------------------------------------------------
# WHY THE PRINTING HAPPENS IN THE BROWSER
#
# The app is deployed to shinyapps.io. The R process runs in Amazon's cloud, so
# it cannot see a USB cable plugged into a bench PC. Every server-side approach
# - system("lp"), writing to /dev/usb/lp0, opening a socket to port 9100 -
# addresses a machine in a data centre. There is no server-side option that
# works, so this module never tries: it sends ZPL to the BROWSER, and the
# browser hands it to Zebra Browser Print on localhost, which owns the USB.
#
# That means one install per workstation. It is the only supported route from a
# cloud-hosted web app to a locally attached Zebra, and it is Zebra's own tool.
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "print-ctl",
    shiny$actionButton(ns("print"), "Print label",
                       icon = shiny$icon("print"),
                       class = "btn btn-sm btn-primary"),
    shiny$uiOutput(ns("status"), inline = TRUE),
    shiny$downloadLink(ns("dl"), "Download .zpl", class = "print-dl")
  )
}

#' Reusable printing controller.
#'
#' @param id module id
#' @param spec a fct_zpl$label_spec(). Defaults to 50x25mm at 203dpi - CHECK
#'   THIS against the printer's configuration label before going live; a ZT411
#'   can be 203, 300 or 600 dpi and the wrong value silently mis-sizes
#'   every label.
#' @param module_name recorded in tbl_activity_log so a reprint can be traced
#' @param user reactive giving the username, for the audit row
#' @return list(print = function(df), pending = reactive)
#' @export
server <- function(id, spec = label_spec(), module_name = "label_print",
                   user = shiny$reactive("system")) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # The last batch, kept so the .zpl download and any retry use exactly the
    # bytes that were sent - not a regenerated approximation of them.
    pending <- shiny$reactiveVal(NULL)
    state   <- shiny$reactiveVal(list(state = "unknown", message = ""))
    
    # Prepare a batch WITHOUT sending it. The button in ui() then sends it.
    #
    # This exists because printing is not always something to do on the
    # operator's behalf. When a code is minted the operator may not be at the
    # printer, may want two copies, or may want to check the code first. Firing
    # the label automatically spends a label and the operator's attention on a
    # decision they did not make.
    prepare <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(invisible(FALSE))
      pending(list(df = df, zpl = zpl_batch(df, spec)))
      invisible(TRUE)
    }
    
    send <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        toastr_warning("Nothing to print.", title = "No labels"); return(invisible(FALSE))
      }
      if (is.null(df$code) || !any(nzchar(as.character(df$code)))) {
        toastr_error("These rows have no code to print.", title = "No code")
        return(invisible(FALSE))
      }
      zpl <- zpl_batch(df, spec)
      pending(list(df = df, zpl = zpl))
      session$sendCustomMessage("rtb_print", list(
        zpl = zpl, labels = nrow(df),
        subject = paste(utils::head(df$code, 5), collapse = ",")
      ))
      # Audit BEFORE the printer answers. A label that was sent and jammed
      # still left the system, and the physical sample may already be tagged;
      # a log that only records confirmed prints under-reports exactly the
      # cases somebody will later need to investigate.
      try(dbExecute(pool, "
        INSERT INTO tbl_activity_log (username, module, action, subject, detail)
        VALUES ($1, $2, 'print_labels', $3, $4)",
                    params = list(user() %||% "system", module_name,
                                  paste(utils::head(as.character(df$code), 5), collapse = ","),
                                  sprintf("%d label(s)", nrow(df)))), silent = TRUE)
      invisible(TRUE)
    }
    
    # Reprint the last batch from the button.
    shiny$observeEvent(input$print, {
      p <- pending()
      if (is.null(p)) {
        toastr_warning("No labels queued yet. Create or select a record first.",
                       title = "Nothing to print")
        return()
      }
      send(p$df)
    })
    
    # Status pushed up from zebra_print.js. Unnamespaced on purpose: the JS
    # bridge is a single page-wide service, not one per module instance.
    shiny$observeEvent(session$rootScope()$input$rtb_print_status, {
      s <- session$rootScope()$input$rtb_print_status
      if (is.null(s)) return()
      state(s)
      if (identical(s$state, "done")) toastr_success(s$message, title = "Printed")
      if (identical(s$state, "error")) toastr_error(s$message, title = "Print failed")
    }, ignoreInit = TRUE)
    
    output$status <- shiny$renderUI({
      s <- state()
      switch(s$state %||% "unknown",
             ready       = order_theme$chip("Printer ready", "brand"),
             printing    = order_theme$chip("Sending...", "teal"),
             done        = order_theme$chip("Printed", "brand"),
             error       = order_theme$chip("Print failed", "red"),
             unavailable = shiny$span(
               class = "print-warn",
               order_theme$chip("No printer", "amber"),
               shiny$tags$small(" \u2014 use Download .zpl")),
             NULL)
    })
    
    output$dl <- shiny$downloadHandler(
      filename = function() sprintf("labels-%s.zpl", format(Sys.time(), "%Y%m%d-%H%M%S")),
      content = function(file) {
        p <- pending()
        writeLines(if (is.null(p)) zpl_test_label(spec) else p$zpl, file, useBytes = TRUE)
      }
    )
    
    list(
      # send now - for unattended or bulk minting
      print   = function(df) send(df),
      # stage it for the operator to send - for a single code they are about
      # to attach to something in their hand
      queue   = function(df) prepare(df),
      clear   = function() pending(NULL),
      pending = shiny$reactive(pending()),
      state   = shiny$reactive(state())
    )
  })
}