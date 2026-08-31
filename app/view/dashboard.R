box::use(
  shiny,
  reactable[reactable, reactableOutput, renderReactable, colDef],
)

box::use(
  app/logic/fct_dashboard[dash_orders, dash_pipeline, dash_attention,
                          dash_indexing, dash_throughput, dash_activity],
  app/view/shared/order_theme,
)

# ============================================================================
# DASHBOARD - the overview screen
# ----------------------------------------------------------------------------
# Answers, in this order:
#
#   1. What needs doing?      <- leads, because it is the only actionable part
#   2. Where is the material?
#   3. How is testing going?
#   4. What has been happening?
#
# It leads with work rather than totals on purpose. A dashboard that opens with
# "11 orders" tells you something you already knew; one that opens with "3
# results awaiting review" tells you where to go next, and every attention row
# carries a button that takes you there.
#
# NOT a worklist: no tabs, no two-pane workbench, no row selection. The tiles
# here are plain stat_tile(), deliberately NOT the clickable flow_stepper the
# stage modules use - nothing on this page filters anything, and a tile that
# looks clickable but is not is worse than a plain one.
# ============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    order_theme$page_header(
      title = "Dashboard",
      sub   = "Where the lab stands right now."
    ),
    
    shiny$uiOutput(ns("kpis")),
    shiny$uiOutput(ns("guide")),
    
    # What needs doing - first, and full width.
    order_theme$section("1", "Needs attention",
                        sub = "act on these",
                        shiny$uiOutput(ns("attention"))),
    
    # Where the material is.
    order_theme$section("2", "Material in the pipeline",
                        sub = "by stage",
                        shiny$uiOutput(ns("pipeline"))),
    
    # How testing is going.
    order_theme$section("3", "Virus indexing coverage",
                        sub = "tests required vs started",
                        shiny$uiOutput(ns("indexing"))),
    
    # Intake trend.
    order_theme$section("4", "Intake, last 12 months",
                        sub = "orders and samples received",
                        shiny$uiOutput(ns("throughput"))),
    
    order_theme$section("5", "Recent activity",
                        sub = "latest first",
                        order_theme$table_card(reactableOutput(ns("activity"))))
  )
}

#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # One refresh dependency for the whole page. Every reactive below takes it,
    # so a change anywhere in the app refreshes the overview - but each query
    # stays separate, so one failing does not blank the rest.
    refresh <- shiny$reactive({
      if (!is.null(trigger_refresh)) trigger_refresh()
      invisible(TRUE)
    })
    
    orders     <- shiny$reactive({ refresh(); dash_orders() })
    pipeline   <- shiny$reactive({ refresh(); dash_pipeline() })
    attention  <- shiny$reactive({ refresh(); dash_attention() })
    indexing   <- shiny$reactive({ refresh(); dash_indexing() })
    throughput <- shiny$reactive({ refresh(); dash_throughput() })
    activity   <- shiny$reactive({ refresh(); dash_activity() })
    
    # ---- KPI tiles ----------------------------------------------------
    output$kpis <- shiny$renderUI({
      o <- orders()
      g <- function(s) {
        r <- o[o$derived_status == s, , drop = FALSE]
        if (nrow(r)) as.integer(r$n[1]) else 0L
      }
      p <- pipeline()
      a <- attention()
      todo <- if (nrow(a)) sum(a$n, na.rm = TRUE) else 0L
      
      order_theme$stat_row(list(
        list(value = todo, label = "Actions waiting",
             tone = if (todo > 0) "amber" else "brand"),
        list(value = if (nrow(o)) sum(o$n) else 0L, label = "Orders", tone = "ink"),
        list(value = g("in_progress"), label = "In progress", tone = "teal"),
        list(value = g("completed"), label = "Completed", tone = "brand"),
        list(value = if (nrow(p)) sum(p$n) else 0L, label = "Samples tracked", tone = "ink")
      ))
    })
    
    output$guide <- shiny$renderUI({
      a <- attention()
      todo <- if (nrow(a)) sum(a$n, na.rm = TRUE) else 0L
      if (todo == 0) {
        return(order_theme$guide(tone = "done",
                                 "Nothing is waiting on a decision. Everything in the lab is either ",
                                 "in progress or finished."))
      }
      hi <- if (nrow(a)) sum(a$n[a$severity == "high"], na.rm = TRUE) else 0L
      order_theme$guide(tone = "do",
                        shiny$strong(todo), if (todo == 1) " item needs" else " items need",
                        " someone to act",
                        if (hi > 0) shiny$tagList(", ", shiny$strong(hi), " of them time-sensitive") else NULL,
                        ". Each row below opens the bench it belongs to.")
    })
    
    # ---- needs attention ----------------------------------------------
    output$attention <- shiny$renderUI({
      a <- attention()
      a <- a[!is.na(a$n) & a$n > 0, , drop = FALSE]
      if (nrow(a) == 0) {
        return(order_theme$empty_state(
          title   = "Nothing waiting",
          message = "No approvals, reviews or overdue treatments outstanding.",
          icon    = "circle-check"))
      }
      # Time-sensitive first, then by size. Ordering by count alone would bury
      # a single overdue conviron under a pile of routine reviews.
      a <- a[order(a$severity != "high", -a$n), , drop = FALSE]
      shiny$div(class = "attn-list", lapply(seq_len(nrow(a)), function(i) {
        shiny$div(
          class = paste("attn-row", if (identical(a$severity[i], "high")) "high" else ""),
          shiny$div(class = "attn-n", as.character(a$n[i])),
          shiny$div(class = "attn-label", a$label[i]),
          shiny$div(class = "attn-go", order_theme$goto("Open", a$tab[i]))
        )
      }))
    })
    
    # ---- pipeline -----------------------------------------------------
    output$pipeline <- shiny$renderUI({
      p <- pipeline()
      if (nrow(p) == 0) {
        return(order_theme$empty_state(
          title   = "No material in the pipeline",
          message = "Register an order and receive it into quarantine to begin.",
          icon    = "seedling"))
      }
      mx <- max(p$n, na.rm = TRUE)
      shiny$div(class = "bar-list", lapply(seq_len(nrow(p)), function(i) {
        # Bars are scaled to the BIGGEST stage, not to the total. Stages hold
        # wildly different counts, and scaling to the total makes every bar a
        # sliver and the chart useless.
        pct <- if (mx > 0) round(100 * p$n[i] / mx) else 0
        shiny$div(
          class = "bar-row",
          shiny$div(class = "bar-label", p$label[i]),
          shiny$div(class = "bar-track",
                    shiny$div(class = "bar-fill", style = sprintf("width:%d%%;", pct))),
          shiny$div(class = "bar-val", as.character(p$n[i]),
                    shiny$tags$small(sprintf("%s units", p$units[i])))
        )
      }))
    })
    
    # ---- indexing coverage --------------------------------------------
    output$indexing <- shiny$renderUI({
      x <- indexing()
      req <- if (nrow(x)) as.integer(x$required[1]) else 0L
      ini <- if (nrow(x)) as.integer(x$initiated[1]) else 0L
      res <- if (nrow(x)) as.integer(x$resulted[1]) else 0L
      app <- if (nrow(x)) as.integer(x$approved[1]) else 0L
      if (req == 0) {
        return(order_theme$empty_state(
          title   = "No tests required yet",
          message = "Approve material in quarantine to make it available for indexing.",
          icon    = "vial"))
      }
      pc <- function(v) if (req > 0) round(100 * v / req) else 0
      shiny$tagList(
        order_theme$stat_row(list(
          list(value = req, label = "Tests required", tone = "ink"),
          list(value = req - ini, label = "Awaiting initiation",
               tone = if (req - ini > 0) "amber" else "brand"),
          list(value = ini, label = "Initiated", tone = "teal"),
          list(value = res, label = "Result recorded", tone = "brand"),
          list(value = app, label = "Approved", tone = "brand")
        )),
        shiny$div(class = "bar-list", lapply(
          list(list("Initiated", ini), list("Result recorded", res), list("Approved", app)),
          function(s) {
            shiny$div(class = "bar-row",
                      shiny$div(class = "bar-label", s[[1]]),
                      shiny$div(class = "bar-track",
                                shiny$div(class = "bar-fill",
                                          style = sprintf("width:%d%%;", pc(s[[2]])))),
                      shiny$div(class = "bar-val", paste0(pc(s[[2]]), "%")))
          }))
      )
    })
    
    # ---- throughput ----------------------------------------------------
    output$throughput <- shiny$renderUI({
      t <- throughput()
      if (nrow(t) == 0 || sum(t$orders, t$samples, na.rm = TRUE) == 0) {
        return(order_theme$empty_state(
          title = "No intake recorded yet", icon = "chart-column"))
      }
      mx <- max(c(t$orders, t$samples), na.rm = TRUE)
      shiny$div(
        class = "spark",
        lapply(seq_len(nrow(t)), function(i) {
          ho <- if (mx > 0) round(100 * t$orders[i] / mx) else 0
          hs <- if (mx > 0) round(100 * t$samples[i] / mx) else 0
          shiny$div(
            class = "spark-col",
            title = sprintf("%s: %d orders, %d samples", t$month[i], t$orders[i], t$samples[i]),
            shiny$div(class = "spark-bars",
                      shiny$div(class = "sb sb-o", style = sprintf("height:%d%%;", ho)),
                      shiny$div(class = "sb sb-s", style = sprintf("height:%d%%;", hs))),
            shiny$div(class = "spark-lbl", sub(" .*", "", t$month[i]))
          )
        })
      )
    })
    
    # ---- activity ------------------------------------------------------
    output$activity <- renderReactable({
      d <- activity()
      reactable(
        d,
        columns = list(
          occurred_on = colDef(name = "WHEN", width = 155, cell = function(v) {
            if (is.na(v)) "\u2014" else format(v, "%d %b %H:%M")
          }),
          username = colDef(name = "WHO", width = 120),
          module   = colDef(name = "WHERE", width = 150),
          action   = colDef(name = "WHAT", minWidth = 160),
          subject  = colDef(name = "SUBJECT", minWidth = 150,
                            cell = function(v) if (is.na(v)) "\u2014" else v)
        ),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang("Nothing has been recorded yet."),
        theme = order_theme$rt_theme())
    })
  })
}