# app/view/shared/connectivity.R
#
# Surfaces two distinct "no internet" failure modes:
#   1. Client -> server: the browser's connection to the Shiny session drops
#      (WiFi/VPN blip, or the websocket itself dying).
#   2. Server -> database: the R process loses its route to Postgres. The
#      browser stays connected, but every write will start failing silently
#      unless this is caught separately.
#
# JS lives in app/js/connectivity.js, imported by app/js/index.js, and gets
# bundled into app/static/js/app.min.js by rhino::build_js() the same way
# zebra_print.js already is - so no <script> tag is added here.

box::use(
  shiny,
  shinyjs[useShinyjs],
  DBI[dbGetQuery],
)

box::use(
  app/logic/fct_conn[pool],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  # No visible UI - this module only emits notifications. Kept as its own
  # tagList so it can be dropped into dashboardBody() next to useShinyjs()
  # without needing a dedicated tabItem.
  shiny$tagList(
    useShinyjs()
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------
    # 1. CLIENT CONNECTIVITY
    #
    # input$rtb_online is set by app/js/connectivity.js via three signals:
    #   - the browser's online/offline events
    #   - a periodic fetch() ping (navigator.onLine only reflects whether a
    #     network interface exists, not whether anything is reachable)
    #   - Shiny's own shiny:disconnected event, for when the websocket dies
    #     even though the network itself is fine
    #
    # This input is deliberately unnamespaced (see rtb_goto in app_server.R
    # for the same pattern) so any module's JS can set it without knowing
    # this module's namespace.
    # ------------------------------------------------------------------
    shiny$observeEvent(session$input$rtb_online, {
      if (isFALSE(session$input$rtb_online)) {
        shiny$showNotification(
          "No internet connection \u2014 changes may not be saved until you reconnect.",
          type = "error", duration = NULL, id = "conn_lost"
        )
      } else {
        shiny$removeNotification(id = "conn_lost")
      }
    }, ignoreNULL = TRUE)

    # ------------------------------------------------------------------
    # 2. DATABASE CONNECTIVITY
    #
    # pool retries dead connections under the hood, so this isn't recovery
    # logic - it's purely to surface an outage to the user instead of
    # letting writes fail silently while the browser looks fine.
    # ------------------------------------------------------------------
    db_alive <- shiny$reactivePoll(
      intervalMillis = 10000,
      session = session,
      checkFunc = function() {
        tryCatch({
          dbGetQuery(pool, "SELECT 1")
          TRUE
        }, error = function(e) FALSE)
      },
      valueFunc = function() TRUE
    )

    shiny$observeEvent(db_alive(), {
      if (isFALSE(db_alive())) {
        shiny$showNotification(
          "Lost connection to the database.",
          type = "error", duration = NULL, id = "db_lost"
        )
      } else {
        shiny$removeNotification(id = "db_lost")
      }
    })

    invisible(NULL)
  })
}