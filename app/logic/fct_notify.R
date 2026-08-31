box::use(
  utils[URLencode],
)

box::use(
  app/logic/fct_tracking[approver_emails],
)

# ============================================================================
# NOTIFICATIONS · reminding administrators that something needs approval
# ----------------------------------------------------------------------------
# Raising an approval request and DELIVERING the reminder are separate acts.
# The request is written to tbl_order_event by the calling module, so it is a
# durable, auditable fact whether or not a message goes out. This file is the
# only place delivery is wired in.
#
# Recipients come from shinymanager's `credentials` table - administrators with
# an address whose access has not expired (approver_emails()).
#
# TWO DELIVERY ROUTES, in order of preference:
#
#   1. SERVER-SIDE SMTP. Used when RTB_SMTP_HOST and RTB_SMTP_FROM are set in
#      .Renviron AND the `emayili` package is installed. The app sends the mail
#      itself, so it works regardless of the technician's desktop setup.
#
#   2. MAILTO FALLBACK. With no SMTP configured, the module opens the
#      technician's own mail client with recipients, subject and body already
#      filled in. Needs no server configuration at all, and the reminder then
#      comes from a real person who can be replied to.
#
# If neither is possible - no administrator has an address - the request is
# still recorded and the caller says so plainly. Nothing ever reports a
# delivery that did not happen: someone would wait for mail that never arrives.
# ============================================================================

#' Is a server-side mail transport both configured and installed?
#' @export
email_configured <- function() {
  nzchar(Sys.getenv("RTB_SMTP_HOST", "")) &&
    nzchar(Sys.getenv("RTB_SMTP_FROM", "")) &&
    requireNamespace("emayili", quietly = TRUE)
}

#' Subject and body for an approval reminder.
#' @export
reminder_text <- function(sample_code, stage, order_number, requested_by) {
  stage_label <- gsub("_", " ", stage)
  list(
    subject = sprintf("RTB-EAGEL: %s awaiting %s approval", sample_code, stage_label),
    body = paste0(
      "A sample is waiting for administrator approval in RTB-EAGEL.\n\n",
      "  Sample:    ", sample_code, "\n",
      "  Order:     ", order_number, "\n",
      "  Stage:     ", stage_label, "\n",
      "  Requested: ", requested_by, " on ", format(Sys.time(), "%d %b %Y %H:%M"), "\n\n",
      "Open the ", stage_label, " module and review the sample to approve or reject it.\n")
  )
}

#' Build a mailto: URL addressed to the given recipients.
#' @export
mailto_url <- function(recipients, subject, body) {
  if (length(recipients) == 0) return("")
  sprintf("mailto:%s?subject=%s&body=%s",
          paste(recipients, collapse = ","),
          URLencode(subject, reserved = TRUE),
          URLencode(body, reserved = TRUE))
}

#' Notify administrators that a sample is waiting for approval.
#'
#' Returns a list the caller uses to decide what to tell the user and whether
#' to open a mail client:
#'   delivered  TRUE only if the app itself sent the mail
#'   mailto     a mailto: URL to open, or "" when not needed
#'   n          how many administrators were addressed
#'   recipients their email addresses
#' @export
notify_approvers <- function(sample_code, stage, order_number, requested_by) {
  force(sample_code); force(stage); force(order_number); force(requested_by)
  out <- list(delivered = FALSE, mailto = "", n = 0L, recipients = character(0))
  
  who <- tryCatch(approver_emails(), error = function(e) NULL)
  if (is.null(who) || nrow(who) == 0) {
    message("[notify] no administrator has an email address; request recorded only")
    return(out)
  }
  out$recipients <- who$email
  out$n <- nrow(who)
  txt <- reminder_text(sample_code, stage, order_number, requested_by)
  
  if (email_configured()) {
    sent <- tryCatch({
      smtp <- emayili::server(
        host = Sys.getenv("RTB_SMTP_HOST"),
        port = as.integer(Sys.getenv("RTB_SMTP_PORT", "587")),
        username = Sys.getenv("RTB_SMTP_USER", ""),
        password = Sys.getenv("RTB_SMTP_PASSWORD", ""))
      msg <- emayili::envelope(
        to = out$recipients,
        from = Sys.getenv("RTB_SMTP_FROM"),
        subject = txt$subject)
      msg <- emayili::text(msg, txt$body)
      smtp(msg, verbose = FALSE)
      TRUE
    }, error = function(e) {
      message("[notify] SMTP send failed: ", conditionMessage(e))
      FALSE
    })
    if (isTRUE(sent)) { out$delivered <- TRUE; return(out) }
  }
  
  # no server transport (or it failed): hand the caller a pre-filled mailto so
  # the technician's own mail client can send it
  out$mailto <- mailto_url(out$recipients, txt$subject, txt$body)
  out
}

