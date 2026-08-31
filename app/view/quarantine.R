box::use(
  shiny,
  reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
  shinyjs[useShinyjs, runjs],
  shinytoastr[toastr_success, toastr_error],
  htmlwidgets[JS],
  pool[poolWithTransaction],
  DBI[dbExecute, dbGetQuery],
  stats[setNames],
  shinyWidgets[radioGroupButtons]
)

box::use(
  app/logic/fct_conn[pool, load_data],
  app/logic/fct_tracking[clearance_queue, tests_for_sample, pending_requests,
                         quarantine_stock],
  app/logic/fct_workflows[workflow_cache, next_options, order_context,
                          sample_context, record_event],
  app/view/shared/label_print,
  app/view/shared/order_theme,
)

# ============================================================================
# QUARANTINE · the whole life of a consignment on a bench, in one place
# ----------------------------------------------------------------------------
# A quarantine technician's job is one continuous sequence, and this module is
# organised as that sequence rather than as three separate screens:
#
#   1. RECEPTION   receive an approved consignment onto a bench   (per ORDER)
#   2. INITIATION  cut explants into individually tracked samples (per ORDER,
#                  creates the first tbl_sample rows)
#   3. CLEARANCE   review each sample and approve it to indexing  (per SAMPLE)
#
# Initiation lives here, not in its own menu, because it is physically a
# bench activity: it cuts explants from material sitting in quarantine, and
# the samples it creates are BORN at the quarantine stage (glasshouse
# 'established', growthroom 'received'). Splitting it out made one job span
# two menus. Together the three steps read left-to-right as the consignment's
# progress, and each shows how much work is waiting in it.
#
# NAVIGATION - the stepper IS the tab bar.
# ----------------------------------------------------------------------------
# There used to be two navigation surfaces stacked on top of each other: the
# flow stepper (which said where the work is) and a tabsetPanel strip (which
# said where you are). They carried the same three labels and the same three
# counts, and only one of them was clickable - so the eye went to the counts
# and the hand had to go somewhere else. Worse, the counts were duplicated:
# once in .fs-count and again in the tab_badge, two renderUI paths that could
# in principle disagree.
#
# Now there is ONE surface. Each step of the stepper is a real tab control
# (role="tab", keyboard reachable, aria-selected) that sets input$qtab; the
# three panels are conditionalPanels keyed off that input. The tabsetPanel and
# the three tab_badge outputs are gone - not hidden with CSS, gone - so there
# is no second nav to keep in sync and no orphan output.
#
# WHY conditionalPanel AND NOT A HIDDEN tabsetPanel
# Keeping tabsetPanel would have meant hiding its <ul class="nav-tabs"> from
# style.css, which trades a visible duplicate for an invisible one that the
# next person has to discover. conditionalPanel hides with display:none
# exactly as tabsetPanel does, so Shiny still suspends the outputs of the two
# panels you are not looking at - the reactables do not all run at once.
#
# WHAT EACH STEP TOUCHES
#   Reception  -> tbl_order_quarantine (per order), tbl_order_event. No samples.
#   Initiation -> tbl_sample + first tbl_sample_event per explant. NULL
#                 order_service_id (shared upstream stock, allocated later).
#   Clearance  -> one record_event() per sample, established/received ->
#                 approved (or rejected). Per-sample or whole-consignment.
# ============================================================================

WF_PATH <- file.path("app", "static", "workflows", "cassava.yaml")

DESTS <- c(quarantine_glasshouse = "Glasshouse", quarantine_growthroom = "Growthroom")

ESTABLISHED_STATE <- c(
  quarantine_glasshouse = "established",
  quarantine_growthroom = "received"
)

CODE_PREFIX <- "IN"

# Prefix for a virus-indexing TEST-SAMPLE. Must match virus_indexing.R's
# VT_PREFIX: both modules mint codes from the same tbl_code_counter series and
# a disagreement here would split one series into two.
# the conditionalPanel fallback, the server-side `cur` default, and the step
# marked active on first render.
DEFAULT_TAB <- "reception"

#' One tab panel, shown when input$qtab holds `value`.
#'
#' The `|| DEFAULT_TAB` fallback is what makes the module render correctly
#' before the first click: input$qtab is created by Shiny.setInputValue, so it
#' is undefined until a step is clicked, and an undefined input would leave
#' every panel hidden on load.
#'
#' NOTE: conditionalPanel's own `ns` argument is deliberately NOT used. The
#' condition below is already fully namespaced by the caller, which is the
#' convention the rest of this module follows (see the route1/batch panels).
panel <- function(qtab_id, value, ...) {
  shiny$conditionalPanel(
    condition = sprintf("(input['%s'] || '%s') == '%s'", qtab_id, DEFAULT_TAB, value),
    shiny$div(role = "tabpanel", style = "padding-top:14px;", ...)
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    order_theme$page_header(
      title = "Quarantine",
      sub   = "Receive a consignment, cut it into samples, clear them onward.",
      # Glasshouse and growthroom are two physical houses. This scopes the whole
      # module to one of them at once - every post-bench tab reads it - so an
      # operator working in the glasshouse is not shown growthroom material on
      # five tabs. "Both" is the default: filtering is a convenience, not a
      # wall, and reception (no bench yet) always shows in full.
      actions = radioGroupButtons(
        ns("house"), NULL, 
        direction = "horizontal",
        choiceNames  = list("Both", "Glasshouse", "Growthroom"),
        choiceValues = list("all", "quarantine_glasshouse", "quarantine_growthroom"),
        selected = "quarantine_glasshouse",
        justified = TRUE,
        individual = TRUE,
        status = "info"
        )
    ),
    
    shiny$uiOutput(ns("flow")),    # the stepper: three steps, and the tabs
    shiny$uiOutput(ns("guide")),   # ONE instruction - replaces three table
    # notes and five separate CTA banners
    
    panel(
      ns("qtab"), "reception",
      order_theme$toolbar(
        order_theme$search_box(ns("q"), "Search order, customer, crop..."),
        order_theme$filter_select(
          ns("filter"),
          choices = c("Awaiting bench" = "awaiting",
                      "In glasshouse"  = "quarantine_glasshouse",
                      "In growthroom"  = "quarantine_growthroom",
                      "All"            = "all"),
          selected = "awaiting"
        )
      ),
      order_theme$table_card(reactableOutput(ns("tbl")))
    ),
    
    panel(
      ns("qtab"), "initiation",
      order_theme$toolbar(
        order_theme$search_box(ns("init_q"), "Search order, customer, crop, bench..."),
        order_theme$filter_select(
          ns("init_filter"),
          choices = c("Awaiting initiation" = "awaiting",
                      "Initiated"           = "done",
                      "All on bench"        = "all"),
          selected = "awaiting"
        )
      ),
      order_theme$table_card(reactableOutput(ns("init_tbl")))
    ),
    
    panel(
      ns("qtab"), "requests",
      # ONE action panel, below both lists, showing whichever row is selected.
      # There were two, each permanently on screen and mostly empty: the
      # operator saw two sets of controls and had to work out which belonged to
      # what they had clicked.
      order_theme$table_card(
        order_theme$table_note(
          title = "Asked for by another bench. ",
          "Draw the sample and it is handed to whoever asked."),
        reactableOutput(ns("req_tbl"))),
      order_theme$table_card(
        order_theme$table_note(
          title = "Cleared and standing here. ",
          "Reception, initiation and clearance all leave the sample in ",
          "quarantine. Select any of it to route it onward."),
        reactableOutput(ns("stock_tbl"))),
      shiny$uiOutput(ns("action_panel"))
    ),
    
    panel(
      ns("qtab"), "clearance",
      order_theme$toolbar(
        order_theme$search_box(ns("clr_q"), "Search sample, order, customer..."),
        order_theme$filter_select(
          ns("clr_filter"),
          choices = c("All awaiting"  = "all",
                      "Glasshouse"    = "quarantine_glasshouse",
                      "Growthroom"    = "quarantine_growthroom"),
          selected = "all"
        )
      ),
      order_theme$table_card(reactableOutput(ns("clr_tbl")))
    )
  )
}



#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bump <- shiny$reactiveVal(0)
    MY_TAB <- "quarantine"
    if (!is.null(tab) && is.function(tab)) {
      shiny$observeEvent(tab(), { if (identical(tab(), MY_TAB)) bump(shiny$isolate(bump()) + 1) }, ignoreInit = TRUE)
    }
    user <- shiny$reactive(shiny$reactiveValuesToList(res_auth)$user)
    
    refresh_all <- function() {
      bump(bump() + 1)
      if (!is.null(trigger_refresh)) trigger_refresh(trigger_refresh() + 1)
    }
    
    # The house filter, applied to any frame that carries a bench column. Post-
    # bench tables (initiation, requests, cleared stock, clearance) all have
    # stage_code or source_stage; reception has no bench yet and is untouched.
    # Reading input$house here makes each table a dependent of the radio, so
    # switching house re-filters IN R without a re-query.
    by_house <- function(d) {
      h <- input$house %||% "all"
      if (identical(h, "all") || is.null(d) || nrow(d) == 0) return(d)
      col <- if ("source_stage" %in% names(d)) "source_stage"
      else if ("stage_code" %in% names(d)) "stage_code"
      else return(d)
      d[!is.na(d[[col]]) & d[[col]] == h, , drop = FALSE]
    }
    
    # ---- shared data -------------------------------------------------
    awaiting <- shiny$reactive({
      bump()
      load_data(pool, "
        SELECT o.order_number, cu.customer_name, c.crop_name, v.variety_name,
               st.label AS sample_type, d.sample_type_code,
               o.sample_amount, d.date_received, o.approved_on,
               NULL::text AS stage_code, NULL::text AS bench_no,
               NULL::date AS received_date, NULL::int AS quantity
        FROM tbl_order o
        JOIN tbl_customer cu           ON cu.customer_id = o.customer_id
        LEFT JOIN tbl_order_detail d   ON d.order_number = o.order_number
        LEFT JOIN tbl_crop c           ON c.crop_id = d.crop_id
        LEFT JOIN tbl_variety v        ON v.variety_id = d.variety_id
        LEFT JOIN tbl_sample_type st   ON st.sample_type_code = d.sample_type_code
        WHERE o.approval_state = 'approved'
          AND NOT EXISTS (SELECT 1 FROM tbl_order_quarantine q
                          WHERE q.order_number = o.order_number)
          -- a consignment received by another bench (thermotherapy takes
          -- cassava directly) already has samples; it is no longer awaiting
          -- reception here
          AND NOT EXISTS (SELECT 1 FROM tbl_sample s
                          WHERE s.order_number = o.order_number)
        ORDER BY o.approved_on NULLS LAST, o.created_on")
    })
    
    bench <- shiny$reactive({
      bump()
      load_data(pool, "
        SELECT q.order_number, q.stage_code, q.bench_no, q.received_date,
               q.quantity AS received_qty, q.quantity,
               cu.customer_name, c.crop_name, v.variety_name,
               st.label AS sample_type, d.sample_type_code, o.sample_amount,
               d.date_received, o.approved_on,
               COALESCE(s.n_samples, 0)::int AS n_samples,
               COALESCE(s.total_qty, 0)::int AS total_qty
        FROM tbl_order_quarantine q
        JOIN tbl_order o               ON o.order_number = q.order_number
        JOIN tbl_customer cu           ON cu.customer_id = o.customer_id
        LEFT JOIN tbl_order_detail d   ON d.order_number = o.order_number
        LEFT JOIN tbl_crop c           ON c.crop_id = d.crop_id
        LEFT JOIN tbl_variety v        ON v.variety_id = d.variety_id
        LEFT JOIN tbl_sample_type st   ON st.sample_type_code = d.sample_type_code
        LEFT JOIN (
            SELECT order_number, count(*) AS n_samples, sum(quantity) AS total_qty
            FROM tbl_sample GROUP BY order_number
        ) s ON s.order_number = q.order_number
        WHERE o.approval_state = 'approved'
        ORDER BY q.received_date DESC")
    })
    
    clr_all <- shiny$reactive({
      bump()
      clearance_queue()
    })
    
    counts <- shiny$reactive({
      a <- awaiting(); b <- bench(); cc <- clr_all()
      list(
        awaiting_bench = nrow(a),
        on_bench       = nrow(b),
        awaiting_init  = if (nrow(b)) sum(b$n_samples == 0) else 0L,
        awaiting_clr   = nrow(cc)
      )
    })
    
    # ---- the stepper, which is also the tab bar ----------------------
    # DELIBERATE: input$qtab is read under isolate(). The stepper therefore
    # re-renders when the WORK changes (counts), not when the tab changes -
    # clicking a step moves the `on` class in the browser, so there is no
    # round trip and no flash of the header on every switch. isolate() still
    # reads the CURRENT tab, so when a save triggers refresh_all() the
    # re-rendered stepper comes back with the right step highlighted.
    output$flow <- shiny$renderUI({
      k <- counts()
      cur <- shiny$isolate(input$qtab) %||% DEFAULT_TAB
      order_theme$flow_stepper(
        list(
          list(title = "Reception",  sub = "receive onto a bench", value = "reception",
               count = k$awaiting_bench, unit = "awaiting",
               active = identical(cur, "reception"), waiting = k$awaiting_bench > 0),
          list(title = "Initiation", sub = "cut explants into samples", value = "initiation",
               count = k$awaiting_init, unit = "to initiate",
               active = identical(cur, "initiation"), waiting = k$awaiting_init > 0),
          list(title = "Clearance",  sub = "approve samples to indexing", value = "clearance",
               count = k$awaiting_clr, unit = "to clear",
               active = identical(cur, "clearance"), waiting = k$awaiting_clr > 0),
          # Marked, not numbered. A request is not a stage a consignment passes
          # through - it is work another bench has asked this one to do, and it
          # can arrive at any point in the sequence.
          list(title = "Sample requests", sub = "draw from standing stock",
               value = "requests", num = "\u21a9",
               count = nrow(reqs()) + nrow(stock()), unit = "to route",
               active = identical(cur, "requests"), waiting = nrow(reqs()) > 0)
        ),
        input_id = ns("qtab")
      )
    })
    
    # ONE guide replaces the three table notes and the five separate CTA
    # banners this module used to carry. The recv_cta / init_cta / clr_cta
    # outputs below are kept and still render, but the UI no longer places
    # them - the guide says the same thing, once, in the place the eye is
    # already going.
    output$guide <- shiny$renderUI({
      k <- counts()
      switch(
        input$qtab %||% DEFAULT_TAB,
        initiation = if (k$awaiting_init > 0)
          order_theme$guide(tone = "do",
                            "Cut a consignment into individually coded explants. ",
                            shiny$strong("Per-sample tracking begins here"),
                            " - everything before this point is recorded against the order.")
        else if (k$awaiting_bench > 0)
          order_theme$guide("Receive a consignment onto a bench first - ",
                            shiny$strong(k$awaiting_bench), " waiting in Reception.")
        else order_theme$guide("Nothing is waiting to be initiated."),
        clearance = if (k$awaiting_clr > 0)
          order_theme$guide(tone = "do",
                            "Clear established explants so the indexing bench can pull them. ",
                            "You can clear one sample or the whole consignment.",
                            action = order_theme$goto("Virus Indexing", "vx"))
        else order_theme$guide(tone = "done",
                               "Nothing is waiting to be cleared.",
                               action = order_theme$goto("Virus Indexing", "vx")),
        if (k$awaiting_bench > 0)
          order_theme$guide(tone = "do",
                            shiny$strong(k$awaiting_bench),
                            if (k$awaiting_bench == 1) " approved consignment is waiting"
                            else " approved consignments are waiting",
                            " for a bench. Select one to assign it to the glasshouse or the growthroom.")
        else order_theme$guide("No consignments are waiting for a bench.")
      )
    })
    
    
    # ======================================================================
    # STEP 1 · RECEPTION
    # ======================================================================
    # Reception shows a FIXED set of columns whatever the filter. awaiting()
    # and bench() do not have identical columns, so after any union we coerce to
    # exactly this set - missing columns filled with rep(NA, nrow) (never <- NA,
    # which breaks on a zero-row frame). This is shape_frame's discipline applied
    # at the union point, and it stops the "All" filter erroring reactable.
    RECV_COLS <- c("order_number","customer_name","crop_name","variety_name",
                   "sample_type","sample_amount","stage_code","bench_no",
                   "received_date","quantity")
    
    recv_rows <- shiny$reactive({
      f <- input$filter %||% "awaiting"
      if (identical(f, "awaiting")) {
        d <- awaiting()
      } else if (identical(f, "all")) {
        a <- awaiting(); b <- bench()
        common <- intersect(names(a), names(b))
        d <- rbind(a[, common, drop = FALSE], b[, common, drop = FALSE])
      } else {
        b <- bench(); d <- b[b$stage_code == f, , drop = FALSE]
      }
      # coerce to the fixed reception shape
      for (col in RECV_COLS) if (is.null(d[[col]])) d[[col]] <- rep(NA, nrow(d))
      d <- d[, RECV_COLS, drop = FALSE]
      
      q <- input$q
      if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
        cols <- intersect(c("order_number","customer_name","crop_name","variety_name","bench_no"), names(d))
        hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    output$tbl <- renderReactable({
      d <- recv_rows(); d$`__act` <- rep(NA, nrow(d))
      reactable(d, columns = order_theme$rt_cols(list(
        order_number = colDef(name = "ORDER", width = 160, cell = function(v) shiny$tags$strong(v)),
        customer_name = colDef(name = "CUSTOMER", minWidth = 150),
        crop_name = colDef(name = "CROP", width = 105, cell = function(v) if (is.na(v)) "\u2014" else v),
        variety_name = colDef(name = "VARIETY", width = 115, cell = function(v) if (is.na(v)) "\u2014" else v),
        sample_type = colDef(name = "TYPE", width = 95, cell = function(v) if (is.na(v)) "\u2014" else v),
        sample_amount = colDef(name = "SAMPLES", width = 85),
        stage_code = colDef(name = "LOCATION", width = 125, cell = function(v) {
          if (is.na(v)) return(order_theme$chip("Awaiting", "amber"))
          order_theme$chip(unname(DESTS[v]) %||% v,
                           if (identical(v, "quarantine_glasshouse")) "brand" else "teal")
        }),
        bench_no = colDef(name = "BENCH", width = 80, cell = function(v) if (is.na(v)) "\u2014" else v),
        received_date = colDef(name = "RECEIVED", width = 105,
                               cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %y")),
        quantity = colDef(name = "QTY", width = 65, cell = function(v) if (is.na(v)) "\u2014" else v),
        `__act` = colDef(name = "", sortable = FALSE, width = 110, html = TRUE,
                         cell = JS(sprintf("function(ci){ var on = ci.row['stage_code'];
            var lbl = on ? 'Edit' : 'Receive';
            return '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + ci.row['order_number'] + '\\', n: Math.random()})\">' + lbl + '</button>';
          }", ns("receive_click"))))
      ), d),
      defaultPageSize = 10, compact = TRUE, highlight = TRUE,
      language = reactableLang(noData = "Nothing here. Approved orders appear once registered."),
      theme = order_theme$rt_theme())
    })
    
    recv_active <- shiny$reactiveVal(NULL)
    
    recommended_dest <- function(order_number) {
      wf <- tryCatch(workflow_cache(WF_PATH), error = function(e) NULL)
      if (is.null(wf)) return(list(stage = NULL))
      ctx  <- tryCatch(order_context(pool, order_number), error = function(e) list())
      opts <- tryCatch(next_options(wf, "reception", "approved", ctx), error = function(e) NULL)
      if (is.null(opts) || nrow(opts) == 0) return(list(stage = NULL))
      bench_opt <- opts$to_stage[opts$to_stage %in% names(DESTS)]
      list(stage = if (length(bench_opt)) bench_opt[1] else opts$to_stage[1])
    }
    
    shiny$observeEvent(input$receive_click, {
      code <- input$receive_click$id; shiny$req(code)
      d <- recv_rows(); row <- d[d$order_number == code, , drop = FALSE]
      shiny$req(nrow(row) == 1); recv_active(code)
      rec <- recommended_dest(code)
      shiny$showModal(shiny$modalDialog(
        title = paste("Receive", code, "into quarantine"),
        shiny$div(class = "rtb-intake",
                  # WHERE THE MATERIAL PHYSICALLY GOES, stated once, large, and
                  # not as a preselected dropdown the operator scrolls past.
                  # Putting a plant on the wrong bench is not a data error that
                  # can be corrected later - the plant is on the wrong bench.
                  shiny$div(
                    class = "dest-callout",
                    shiny$div(class = "dc-label", "Place this material in"),
                    shiny$div(class = "dc-value",
                              unname(DESTS[rec$stage %||% "quarantine_growthroom"]) %||%
                                (rec$stage %||% "quarantine_growthroom")),
                    shiny$div(class = "dc-why",
                              "Determined by the workflow",
                              if (!is.na(row$sample_type[1]))
                                paste0(" from sample type ", row$sample_type[1]) else NULL,
                              ".")),
                  # The destination is the workflow's, so it is submitted as a
                  # fixed value rather than chosen. A dropdown here was the
                  # mix-up risk: two benches one scroll apart.
                  shiny$div(style = "display:none;",
                            shiny$selectizeInput(ns("dest"), NULL,
                                                 choices = setNames(
                                                   rec$stage %||% "quarantine_growthroom",
                                                   unname(DESTS[rec$stage %||% "quarantine_growthroom"])),
                                                 selected = rec$stage %||% "quarantine_growthroom")),
                  order_theme$section("\u2713", "Reception",
                                      shiny$textInput(ns("bench_no"), shiny$HTML("Bench no <span class='mandatory_star'>*</span>"),
                                                      value = if (!is.na(row$bench_no[1])) row$bench_no[1] else ""),
                                      shiny$dateInput(ns("received_date"), "Date received",
                                                      value = if (!is.na(row$received_date[1])) as.Date(row$received_date[1]) else Sys.Date()),
                                      shiny$numericInput(ns("quantity"),
                                                         sprintf("Quantity received (order registered %s)",
                                                                 if (is.na(row$sample_amount[1])) "?" else row$sample_amount[1]),
                                                         value = if (!is.na(row$quantity[1])) row$quantity[1] else row$sample_amount[1],
                                                         min = 1,
                                                         max = if (is.na(row$sample_amount[1])) NA else as.integer(row$sample_amount[1])),
                                      shiny$textAreaInput(ns("notes"), "Notes"))
        ),
        footer = shiny$tagList(shiny$modalButton("Cancel"),
                               shiny$actionButton(ns("save"), "Save", class = "btn btn-success")),
        easyClose = FALSE, size = "m"))
    })
    
    shiny$observeEvent(input$save, {
      code <- recv_active(); shiny$req(code)
      if (is.null(input$bench_no) || !nzchar(trimws(input$bench_no))) {
        toastr_error("Bench number is required.", title = "Missing"); return() }
      if (is.null(input$quantity) || is.na(input$quantity) || input$quantity < 1) {
        toastr_error("Quantity must be at least 1.", title = "Invalid"); return() }
      # More received than was registered means the paperwork and the material
      # disagree, and receiving it silently makes the order untrue from here on.
      d0 <- recv_rows(); r0 <- d0[d0$order_number == code, , drop = FALSE]
      reg <- if (nrow(r0) && !is.na(r0$sample_amount[1])) as.integer(r0$sample_amount[1]) else NA_integer_
      if (!is.na(reg) && as.integer(input$quantity) > reg) {
        toastr_error(sprintf(
          "Cannot receive %d - the order registered %d unit%s. Amend the order first.",
          as.integer(input$quantity), reg, if (reg == 1) "" else "s"),
          title = "More than registered", timeOut = 0)
        return()
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_order_quarantine
              (order_number, stage_code, received_date, received_by, bench_no, quantity, notes)
            VALUES ($1,$2,$3,$4,$5,$6,$7)
            ON CONFLICT (order_number, stage_code) DO UPDATE SET
              received_date = EXCLUDED.received_date, received_by = EXCLUDED.received_by,
              bench_no = EXCLUDED.bench_no, quantity = EXCLUDED.quantity, notes = EXCLUDED.notes",
                    params = list(code, input$dest, as.character(input$received_date), user(),
                                  trimws(input$bench_no), as.integer(input$quantity),
                                  if (nzchar(input$notes %||% "")) input$notes else NA))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'quarantine', $2, $3, $4)",
                    params = list(code, paste("received into", unname(DESTS[input$dest]) %||% input$dest), user(),
                                  paste0("bench ", trimws(input$bench_no), " \u00b7 qty ", as.integer(input$quantity))))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Save failed", timeOut = 0); FALSE })
      if (ok) {
        shiny$removeModal()
        toastr_success(paste(code, "received into", unname(DESTS[input$dest]),
                             "\u2014 now cut explants in the Initiation step."))
        recv_active(NULL); refresh_all()
      }
    })
    
    # ======================================================================
    # STEP 2 · INITIATION
    # ======================================================================
    init_rows <- shiny$reactive({
      d <- by_house(bench()); f <- input$init_filter %||% "awaiting"
      if (nrow(d) > 0) d <- switch(f,
                                   awaiting = d[d$n_samples == 0, , drop = FALSE],
                                   done     = d[d$n_samples > 0, , drop = FALSE], d)
      q <- input$init_q
      if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
        cols <- intersect(c("order_number","customer_name","crop_name","variety_name","bench_no"), names(d))
        hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    output$init_tbl <- renderReactable({
      d <- init_rows(); d$`__act` <- rep(NA, nrow(d))
      reactable(d, columns = order_theme$rt_cols(list(
        total_qty = colDef(show = FALSE), sample_type_code = colDef(show = FALSE),
        approved_on = colDef(show = FALSE), date_received = colDef(show = FALSE),
        order_number = colDef(name = "ORDER", width = 160, cell = function(v) shiny$tags$strong(v)),
        customer_name = colDef(name = "CUSTOMER", minWidth = 140),
        crop_name = colDef(name = "CROP", width = 95, cell = function(v) if (is.na(v)) "\u2014" else v),
        variety_name = colDef(name = "VARIETY", width = 105, cell = function(v) if (is.na(v)) "\u2014" else v),
        sample_type = colDef(name = "TYPE", width = 90, cell = function(v) if (is.na(v)) "\u2014" else v),
        stage_code = colDef(name = "BENCH", width = 120, cell = function(v) {
          order_theme$chip(if (identical(v, "quarantine_glasshouse")) "Glasshouse" else "Growthroom",
                           if (identical(v, "quarantine_glasshouse")) "brand" else "teal")
        }),
        bench_no = colDef(name = "NO", width = 60, cell = function(v) if (is.na(v)) "\u2014" else v),
        received_date = colDef(name = "RECEIVED", width = 100,
                               cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %y")),
        received_qty = colDef(name = "IN", width = 55, cell = function(v) if (is.na(v)) "\u2014" else v),
        sample_amount = colDef(name = "EXPECTED", width = 85),
        n_samples = colDef(name = "ESTABLISHED", width = 105, cell = function(v) {
          if (v == 0) return(order_theme$chip("None", "amber"))
          order_theme$chip(paste(v, "samples"), "brand")
        }),
        `__act` = colDef(name = "", sortable = FALSE, width = 105, html = TRUE,
                         cell = JS(sprintf("function(ci){ var lbl = ci.row['n_samples'] > 0 ? 'Add more' : 'Establish';
            return '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + ci.row['order_number'] + '\\', n: Math.random()})\">' + lbl + '</button>';
          }", ns("establish_click"))))
      ), d),
      defaultPageSize = 10, compact = TRUE, highlight = TRUE,
      language = reactableLang(noData = "Nothing on the bench. Receive a consignment first."),
      theme = order_theme$rt_theme())
    })
    
    init_active <- shiny$reactiveVal(NULL)
    
    shiny$observeEvent(input$establish_click, {
      code <- input$establish_click$id; shiny$req(code)
      d <- init_rows(); row <- d[d$order_number == code, , drop = FALSE]
      shiny$req(nrow(row) == 1); init_active(code)
      bench_label <- if (identical(row$stage_code[1], "quarantine_glasshouse")) "glasshouse" else "growthroom"
      state_word  <- unname(ESTABLISHED_STATE[row$stage_code[1]])
      already     <- row$n_samples[1]
      suggested   <- max(1, row$sample_amount[1] - already)
      shiny$showModal(shiny$modalDialog(
        title = paste("Establish samples \u00b7", code),
        shiny$div(class = "rtb-intake", order_theme$head_orders(),
                  shiny$div(class = "update-hint",
                            "Each sample gets its own code and is tracked individually from here on.", shiny$br(),
                            shiny$strong("Bench: "), bench_label, "  \u00b7  ", shiny$strong("State: "), state_word,
                            if (already > 0) shiny$tagList(shiny$br(), shiny$span(style = "opacity:.8;",
                                                                                  sprintf("%d already established. New ones are added alongside.", already)))),
                  order_theme$section("\u2702", "Explants",
                                      shiny$numericInput(ns("n_samples"),
                                                         shiny$HTML("Number of samples to establish <span class='mandatory_star'>*</span>"),
                                                         value = suggested, min = 1, max = 500),
                                      shiny$numericInput(ns("qty_each"),
                                                         shiny$HTML("Units per sample <span class='mandatory_star'>*</span>"), value = 1, min = 1),
                                      shiny$dateInput(ns("established_on"), "Date established", value = Sys.Date()),
                                      shiny$textAreaInput(ns("init_notes"), "Notes")),
                  shiny$div(class = "text-muted", style = "font-size:11.5px;",
                            sprintf("Codes assigned automatically as %s%s001, %s%s002, ...",
                                    CODE_PREFIX, format(Sys.Date(), "%y"), CODE_PREFIX, format(Sys.Date(), "%y")))
        ),
        footer = shiny$tagList(shiny$modalButton("Cancel"),
                               shiny$actionButton(ns("init_save"), "Establish", class = "btn btn-success")),
        easyClose = FALSE, size = "m"))
    })
    
    shiny$observeEvent(input$init_save, {
      code <- init_active(); shiny$req(code)
      n <- input$n_samples; q <- input$qty_each
      if (is.null(n) || is.na(n) || n < 1) { toastr_error("Number of samples must be at least 1.", title = "Invalid"); return() }
      if (n > 500) { toastr_error("500 samples is the per-batch limit. Split it.", title = "Too many"); return() }
      if (is.null(q) || is.na(q) || q < 1) { toastr_error("Units per sample must be at least 1.", title = "Invalid"); return() }
      d <- init_rows(); row <- d[d$order_number == code, , drop = FALSE]
      shiny$req(nrow(row) == 1); stage <- row$stage_code[1]
      state <- unname(ESTABLISHED_STATE[stage])
      if (is.na(state)) { toastr_error(paste("No established-state for", stage), title = "Configuration"); return() }
      created <- character(0)
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          for (i in seq_len(as.integer(n))) {
            sc <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code", params = list(CODE_PREFIX))$code[1]
            dbExecute(conn, "
              INSERT INTO tbl_sample (sample_code, order_number, stage_code, quantity, created_by, created_on)
              VALUES ($1, $2, $3, $4, $5, $6)",
                      params = list(sc, code, stage, as.integer(q), user(), as.character(input$established_on)))
            dbExecute(conn, "
              INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, occurred_on, notes)
              VALUES ($1, $2, $3, $4, $5, $6)",
                      params = list(sc, stage, state, user(), as.character(input$established_on),
                                    if (nzchar(input$init_notes %||% "")) input$init_notes else NA))
            # `<<-`, not `<-`. poolWithTransaction() runs this body in its own
            # function, so a plain assignment writes to a LOCAL copy and the
            # outer `created` stays empty. Reads INSIDE the transaction saw the
            # local one and looked right; every read AFTER it got character(0).
            created <<- c(created, sc)
          }
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'initiation', $2, $3, $4)",
                    params = list(code, sprintf("established %d sample%s", n, if (n == 1) "" else "s"), user(),
                                  sprintf("%s \u2014 %s to %s", stage, created[1], created[length(created)])))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Establish failed", timeOut = 0); FALSE })
      if (ok) {
        shiny$removeModal()
        toastr_success(sprintf("%d sample%s established (%s\u2026%s) \u2014 review them in Clearance.",
                               n, if (n == 1) "" else "s", created[1], created[length(created)]))
        init_active(NULL); refresh_all()
      }
    })
    
    # ======================================================================
    # STEP 3 · CLEARANCE
    # ======================================================================
    clr_rows <- shiny$reactive({
      d <- by_house(clr_all()); f <- input$clr_filter %||% "all"
      if (nrow(d) > 0 && !identical(f, "all")) d <- d[d$stage_code == f, , drop = FALSE]
      q <- input$clr_q
      if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
        cols <- intersect(c("sample_code","order_number","customer_name","crop_name","bench_no"), names(d))
        hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    output$clr_tbl <- renderReactable({
      d <- clr_rows(); d$`__act` <- rep(NA, nrow(d))
      reactable(d, groupBy = "order_number", defaultExpanded = TRUE, columns = order_theme$rt_cols(list(
        state_code = colDef(show = FALSE), stage_label = colDef(show = FALSE), quantity = colDef(show = FALSE),
        order_number = colDef(name = "CONSIGNMENT", minWidth = 160, cell = function(v) shiny$tags$strong(v)),
        sample_code = colDef(name = "SAMPLE", width = 115, cell = function(v) shiny$tags$strong(v)),
        customer_name = colDef(name = "CUSTOMER", minWidth = 130),
        crop_name = colDef(name = "CROP", width = 95, cell = function(v) if (is.na(v)) "\u2014" else v),
        variety_name = colDef(name = "VARIETY", width = 100, cell = function(v) if (is.na(v)) "\u2014" else v),
        stage_code = colDef(name = "BENCH", width = 115, cell = function(v) {
          order_theme$chip(unname(DESTS[v]) %||% v,
                           if (identical(v, "quarantine_glasshouse")) "brand" else "teal")
        }),
        state_label = colDef(name = "STATE", width = 105),
        bench_no = colDef(name = "NO", width = 60, cell = function(v) if (is.na(v)) "\u2014" else v),
        since = colDef(name = "SINCE", width = 95,
                       cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %y")),
        `__act` = colDef(name = "", sortable = FALSE, width = 150, html = TRUE,
                         cell = JS(sprintf("function(ci){ var s = ci.row['sample_code'];
            return '<button class=\"btn btn-outline-success btn-sm\" style=\"margin-right:4px;\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + s + '\\', act:\\'approved\\', n: Math.random()})\">Approve</button>'
                 + '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + s + '\\', act:\\'rejected\\', n: Math.random()})\">Reject</button>';
          }", ns("clr_click"), ns("clr_click"))),
                         grouped = JS(sprintf("function(ci){ var on = ci.row['order_number'];
            var n = ci.subRows ? ci.subRows.length : 0;
            return '<button class=\"btn btn-success btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + on + '\\', n: Math.random()})\">Clear all ' + n + '</button>';
          }", ns("clr_batch_click"))))
      ), d),
      defaultPageSize = 12, compact = TRUE, highlight = TRUE,
      language = reactableLang(noData = "Nothing awaiting clearance. Samples appear here once initiation cuts explants."),
      theme = order_theme$rt_theme())
    })
    
    # route: "indexing" (default, crop-appropriate bench) or "thermotherapy"
    # (skip indexing - the operator judges the material dirty). Thermotherapy
    # is NOT the workflow recommendation from quarantine-approved, so it is an
    # off-workflow move: record_event flags it and a reason is required.
    do_clear <- function(codes, decision, notes = NA) {
      # QUARANTINE IS A HOLDING STAGE. Approval records a decision and nothing
      # else: the sample does not move, and no material is cut here.
      #
      # It previously did both. First it moved the source to the indexing bench,
      # which consumed the mother stock. Then (briefly) it drew a child and cut
      # one test-sample per required test - which preserved the source but put
      # the cutting decision in the wrong hands. The person clearing a
      # consignment does not decide how many tests to run or when; the indexing
      # bench does, and it does it per test, when it is ready to run that test.
      #
      # So approval leaves the sample at (quarantine_*, approved), which is the
      # state that makes it AVAILABLE. Virus indexing pulls from there and cuts
      # what it needs, one sample per test. Thermotherapy pulls the same way,
      # through its own Incoming tab.
      reason <- if (identical(decision, "approved"))
        "quarantine review: approved - available to the next stage"
      else "quarantine review: rejected"
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          for (sc in codes) {
            cur <- load_data(pool, "SELECT stage_code, state_code FROM view_sample_current WHERE sample_code = $1",
                             params = list(sc))
            if (nrow(cur) == 0) next
            ctx <- sample_context(conn, sc)
            record_event(conn, sc, cur$stage_code[1], decision, user(),
                         wf = wf, ctx = ctx,
                         notes = if (!is.na(notes) && nzchar(notes)) notes else reason)
          }
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Review failed", timeOut = 0); FALSE
      })
    }
    
    
    
    clr_active <- shiny$reactiveVal(NULL)
    # ---- SAMPLE REQUESTS ------------------------------------------------
    # Other benches ask for material; quarantine draws it. This is the ONLY
    # module that cuts from standing stock - that is what makes quarantine a
    # holding stage rather than another queue, and it means there is one place
    # to look when a count is wrong.
    reqs <- shiny$reactive({ bump(); by_house(pending_requests()) })
    
    # ---- CLEARED STOCK, READY TO ROUTE ----------------------------------
    # Reception, initiation and clearance all leave the sample HERE - that is
    # what a holding stage means. This is what is standing on the bench.
    #
    # Quarantine can route it without waiting to be asked. The pull rule says
    # no bench may CUT another's material; it does not say the holder must sit
    # on stock until someone remembers to ask for it.
    # You cannot draw more units than are standing there. Checked against the
    # DATABASE inside the transaction, not against the reactive snapshot the
    # panel was rendered from: that snapshot can be minutes old, and two
    # operators drawing at the same time would each see the full count.
    #
    # Returns the held quantity so the caller can name it in the error.
    
    # UNITS, not sample count.
    #
    # The rule was "an order needs two SAMPLES to draw one", which blocked a
    # single sample holding five units - there was plenty of material, just one
    # tube. What must survive a draw is a UNIT: draw at most quantity - 1, so
    # something is always left to draw from again.
    #
    # A sample down to its last unit is genuinely exhausted for drawing
    # purposes, and the honest answer is to cut more explants rather than to
    # take the last of it.
    check_units <- function(conn, sample_code, qty) {
      h <- dbGetQuery(conn, "SELECT quantity FROM tbl_sample WHERE sample_code = $1",
                      params = list(sample_code))
      if (nrow(h) == 0) stop("Source material not found: ", sample_code, call. = FALSE)
      have <- as.integer(h$quantity[1])
      if (have <= 1) {
        stop(sprintf(
          paste("%s holds %d unit and cannot be drawn from - one unit must stay",
                "for future requests. Cut more explants in the Initiation step,",
                "then draw again."),
          sample_code, have), call. = FALSE)
      }
      if (qty > have - 1L) {
        stop(sprintf(
          "Cannot draw %d of %s: it holds %d unit%s and one must stay. Draw at most %d.",
          qty, sample_code, have, if (have == 1) "" else "s", have - 1L), call. = FALSE)
      }
      invisible(have)
    }
    
    # Drawing SPENDS units. Without this the check above never bites: quantity
    # would stay at its original value and the same tube could be drawn from
    # for ever, which is what made a units rule meaningless.
    #
    # The WHERE clause is the real guard. Two operators can pass check_units()
    # at the same moment and both reach here; the row-level condition means the
    # second UPDATE matches nothing rather than taking the last unit.
    spend_units <- function(conn, sample_code, qty) {
      n <- dbExecute(conn, "
        UPDATE tbl_sample SET quantity = quantity - $2
         WHERE sample_code = $1 AND quantity - $2 >= 1",
                     params = list(sample_code, as.integer(qty)))
      if (n == 0) {
        stop(sprintf(
          paste("Could not take %d unit%s from %s - it no longer holds enough.",
                "Somebody else may have drawn from it. Refresh and try again."),
          qty, if (qty == 1) "" else "s", sample_code), call. = FALSE)
      }
      invisible(n)
    }
    
    # The bench that MINTS a code prints its label, next to the tube it goes
    # on. Quarantine draws material for every other bench and printed nothing,
    # so an operator cut a sample here and had to write on it by hand or carry
    # it unlabelled - meristem already does this correctly.
    printer <- label_print$server("print", module_name = "quarantine", user = user)
    
    stock <- shiny$reactive({ bump(); by_house(quarantine_stock()) })
    stock_sel <- shiny$reactiveVal(NULL)
    shiny$observeEvent(input$stock_pick, { stock_sel(input$stock_pick$code) })
    
    stock_row <- shiny$reactive({
      sc <- stock_sel(); if (is.null(sc)) return(NULL)
      d <- stock(); if (nrow(d) == 0) return(NULL)
      x <- d[d$sample_code == sc, , drop = FALSE]
      if (nrow(x) == 0) NULL else x
    })
    
    output$stock_tbl <- renderReactable({
      d <- stock()
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("stock_pick"), "sample_code")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        columns = order_theme$rt_cols(list(
          stage_code = colDef(show = FALSE), state_code = colDef(show = FALSE),
          since = colDef(show = FALSE), variety_name = colDef(show = FALSE),
          sample_code = colDef(name = "MATERIAL", minWidth = 120,
                               cell = function(v) shiny$tags$strong(v)),
          bench = colDef(name = "HELD IN", minWidth = 130),
          sample_type = colDef(name = "TYPE", width = 100,
                               cell = function(v) if (is.na(v)) "\u2014" else v),
          quantity = colDef(name = "UNITS", width = 75),
          draws_so_far = colDef(name = "DRAWS", width = 75),
          open_requests = colDef(name = "ASKED FOR", width = 100,
                                 cell = function(v) if (is.na(v) || v == 0) ""
                                 else order_theme$chip(as.character(v), "amber")),
          order_number = colDef(name = "ORDER", minWidth = 145),
          customer_name = colDef(name = "CUSTOMER", minWidth = 125),
          crop_name = colDef(name = "CROP", width = 90)
        ), d),
        defaultPageSize = 10, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "No cleared material is standing here. Clear a sample first."),
        theme = order_theme$rt_theme())
    })
    
    # Where cleared material may go, and the state it arrives in. NOT uniform:
    # tbl_stage_state has no `inprogress` at surface_sterilization - its entry
    # state is `established` - and the composite FK rejects a wrong pair.
    ROUTE_STATE <- c(molecular_virus_indexing = "inprogress",
                     grafting_virus_indexing  = "inprogress",
                     thermotherapy            = "inprogress",
                     meristem_culture         = "received",
                     surface_sterilization    = "established")
    ROUTE_LABEL <- c(molecular_virus_indexing = "Virus indexing (molecular)",
                     grafting_virus_indexing  = "Virus indexing (grafting)",
                     thermotherapy            = "Thermotherapy",
                     meristem_culture         = "Meristem culture",
                     surface_sterilization    = "Surface sterilization")
    
    # The workflow's recommendation for this material, shown as the default.
    stock_reco <- shiny$reactive({
      x <- stock_row(); if (is.null(x)) return(NA_character_)
      wf <- tryCatch(workflow_cache(WF_PATH, pool), error = function(e) NULL)
      if (is.null(wf)) return(NA_character_)
      ctx <- tryCatch(sample_context(pool, x$sample_code[1]), error = function(e) NULL)
      if (is.null(ctx)) return(NA_character_)
      o <- tryCatch(next_options(wf, x$stage_code[1], "approved", ctx),
                    error = function(e) NULL)
      if (is.null(o) || nrow(o) == 0) NA_character_ else o$to_stage[1]
    })
    
    output$stock_detail <- shiny$renderUI({
      x <- stock_row(); if (is.null(x)) return(NULL)
      rec <- stock_reco()
      order_theme$section(
        "\u2192", sprintf("Route %s", x$sample_code[1]),
        sub = "send material onward",
        order_theme$guide(
          if (!is.na(rec)) shiny$tagList(
            "The workflow recommends ",
            shiny$strong(unname(ROUTE_LABEL[rec]) %||% gsub("_", " ", rec)),
            " for this material. You can send it somewhere else \u2014 the ",
            "recommendation is advice, not a rule.")
          else shiny$tagList(
            "The workflow has no recommendation from ",
            shiny$strong(x$bench[1]), " for this material. Choose a destination."),
          if (as.integer(x$open_requests[1]) > 0)
            sprintf(" %d bench has already asked for this material.",
                    as.integer(x$open_requests[1])) else NULL),
        order_theme$prop_grid(
          order_theme$prop("Order", x$order_number[1]),
          order_theme$prop("Customer", x$customer_name[1]),
          order_theme$prop("Crop", x$crop_name[1]),
          order_theme$prop("Units held", as.character(x$quantity[1])),
          order_theme$prop("Drawn from", sprintf("%d time(s)", as.integer(x$draws_so_far[1])))
        ),
        shiny$selectizeInput(ns("route_to"), "Send to",
                             choices = stats::setNames(names(ROUTE_LABEL), unname(ROUTE_LABEL)),
                             selected = if (!is.na(rec)) rec else names(ROUTE_LABEL)[1],
                             width = "100%"),
        # max caps the spinner; the transaction re-checks anyway, because a
        # numericInput max is a hint the browser can be talked out of.
        shiny$numericInput(ns("route_qty"),
                           sprintf("Units to draw (%d held)", as.integer(x$quantity[1])),
                           value = 1, min = 1, max = as.integer(x$quantity[1]),
                           step = 1, width = "240px"),
        order_theme$detail_actions(
          shiny$actionButton(ns("route_go"), "Draw and send",
                             class = "btn btn-success"),
          label_print$ui(ns("print")),
          shiny$actionButton(ns("sel_cancel"), "Cancel",
                             class = "btn btn-sm btn-outline-secondary"))
      )
    })
    
    shiny$observeEvent(input$route_go, {
      x <- stock_row(); shiny$req(!is.null(x))
      to <- input$route_to %||% ""
      state <- unname(ROUTE_STATE[to])
      if (!nzchar(to) || is.na(state)) {
        toastr_error("Choose a destination first.", title = "No destination"); return()
      }
      qty <- max(1L, as.integer(input$route_qty %||% 1L))
      drawn <- NA_character_
      off_wf <- !identical(to, stock_reco())
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          check_units(conn, x$sample_code[1], qty)
          child <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                              params = list(CODE_PREFIX))$code[1]
          dbExecute(conn, "
            INSERT INTO tbl_sample (sample_code, order_number, parent_sample_code,
                                    stage_code, quantity, created_by, created_on)
            VALUES ($1,$2,$3,$4,$5,$6,now())",
                    params = list(child, x$order_number[1], x$sample_code[1], to, qty, user()))
          spend_units(conn, x$sample_code[1], qty)
          dbExecute(conn, "
            INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, notes)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(child, to, state, user(),
                                  sprintf("drawn from %s and routed to %s%s",
                                          x$sample_code[1], gsub("_", " ", to),
                                          if (off_wf) " (off the recommended path)" else "")))
          drawn <<- child
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not route", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(
          code  = drawn, title = toupper(gsub("_", " ", to)),
          line1 = sprintf("from %s", x$sample_code[1]),
          line2 = format(Sys.Date(), "%d %b %Y"), stringsAsFactors = FALSE))
        toastr_success(sprintf("%s drawn from %s and sent to %s.", drawn,
                               x$sample_code[1], unname(ROUTE_LABEL[to])),
                       title = "Routed")
        stock_sel(NULL); refresh_all()
      }
    })
    req_sel <- shiny$reactiveVal(NULL)
    
    output$req_tbl <- renderReactable({
      d <- reqs()
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("req_pick"), "request_id")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        columns = order_theme$rt_cols(list(
          request_id = colDef(show = FALSE), to_stage = colDef(show = FALSE),
          source_stage = colDef(show = FALSE), source_state = colDef(show = FALSE), source_units = colDef(show = FALSE),
          variety_name = colDef(show = FALSE), requested_on = colDef(show = FALSE),
          source_sample_code = colDef(name = "MATERIAL", minWidth = 120,
                                      cell = function(v) shiny$tags$strong(v)),
          to_stage_label = colDef(name = "NEEDED AT", minWidth = 150,
                                  cell = function(v) order_theme$chip(v, "amber")),
          source_bench = colDef(name = "HELD IN", minWidth = 130),
          reason = colDef(name = "WHY", minWidth = 210),
          requested_by = colDef(name = "ASKED BY", width = 110),
          draws_so_far = colDef(name = "DRAWS", width = 80),
          order_number = colDef(name = "ORDER", minWidth = 150),
          customer_name = colDef(name = "CUSTOMER", minWidth = 130),
          crop_name = colDef(name = "CROP", width = 95)
        ), d),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang("No bench has asked for material."),
        theme = order_theme$rt_theme())
    })
    
    shiny$observeEvent(input$req_pick, { req_sel(input$req_pick$code) })
    
    req_row <- shiny$reactive({
      id <- req_sel(); if (is.null(id)) return(NULL)
      d <- reqs(); if (nrow(d) == 0) return(NULL)
      x <- d[as.character(d$request_id) == as.character(id), , drop = FALSE]
      if (nrow(x) == 0) NULL else x
    })
    
    # Entry state per destination. NOT uniform, and not guessable:
    # tbl_stage_state has no `inprogress` at surface_sterilization - its entry
    # state is `established` - and the composite FK rejects the wrong pair.
    ENTRY_STATE <- c(
      thermotherapy            = "inprogress",
      surface_sterilization    = "established",
      molecular_virus_indexing = "inprogress",
      grafting_virus_indexing  = "inprogress",
      meristem_culture         = "received"
    )
    DEST_TAB <- c(
      thermotherapy            = "thermotherapy",
      molecular_virus_indexing = "vx",
      grafting_virus_indexing  = "vx",
      meristem_culture         = "meristem"
    )
    
    # Selecting in one list clears the other: there is one action panel, so
    # there can only be one thing selected. Without this the panel showed the
    # request while the stock row still looked selected.
    shiny$observeEvent(input$req_pick,   { stock_sel(NULL) }, priority = 10)
    shiny$observeEvent(input$stock_pick, { req_sel(NULL) },   priority = 10)
    
    shiny$observeEvent(input$sel_cancel, { req_sel(NULL); stock_sel(NULL) })
    
    # Collapsed until something is selected. An empty form permanently on
    # screen is noise, and a stale one is worse - it invites an action against
    # a row the operator has moved on from.
    output$action_panel <- shiny$renderUI({
      if (!is.null(req_row()))   return(shiny$uiOutput(ns("req_detail")))
      if (!is.null(stock_row())) return(shiny$uiOutput(ns("stock_detail")))
      order_theme$guide(
        "Select a request above to draw the sample somebody asked for, or a ",
        "cleared sample to route it onward.")
    })
    
    output$req_detail <- shiny$renderUI({
      x <- req_row(); if (is.null(x)) return(NULL)
      order_theme$section(
        "\u21a9", sprintf("Draw from %s", x$source_sample_code[1]),
        sub = x$to_stage_label[1],
        order_theme$guide(
          "Cutting a sample from ", shiny$strong(x$source_sample_code[1]),
          " for ", shiny$strong(x$to_stage_label[1]),
          ". The source stays on its bench - this draws FROM it, it does not move it.",
          if (as.integer(x$draws_so_far[1]) > 0)
            sprintf(" It has been drawn from %d time(s) already.",
                    as.integer(x$draws_so_far[1])) else NULL),
        order_theme$prop_grid(
          order_theme$prop("Held in", x$source_bench[1]),
          order_theme$prop("Order", x$order_number[1]),
          order_theme$prop("Crop", x$crop_name[1]),
          order_theme$prop("Asked by", x$requested_by[1]),
          order_theme$prop("Reason", x$reason[1])
        ),
        # The held quantity was not shown here at all, so the operator was
        # asked for a number with nothing to judge it against.
        shiny$numericInput(ns("req_qty"),
                           if (is.na(x$source_units[1]))
                             "Units to draw"
                           else sprintf("Units to draw (%d held)",
                                        as.integer(x$source_units[1])),
                           value = 1, min = 1,
                           max = if (is.na(x$source_units[1])) NA
                           else as.integer(x$source_units[1]),
                           step = 1, width = "240px"),
        order_theme$detail_actions(
          shiny$actionButton(ns("req_fulfil"),
                             sprintf("Add sample and send to %s", x$to_stage_label[1]),
                             class = "btn btn-success"),
          shiny$actionButton(ns("req_cancel"), "Cancel request",
                             class = "btn btn-sm btn-outline-secondary"),
          # Clears the SELECTION, not the request. Two different acts, and
          # conflating them is how a request gets cancelled by someone who only
          # meant to look away.
          shiny$actionButton(ns("sel_cancel"), "Close",
                             class = "btn btn-sm btn-outline-secondary")
        )
      )
    })
    
    shiny$observeEvent(input$req_fulfil, {
      x <- req_row(); shiny$req(!is.null(x))
      to <- x$to_stage[1]
      state <- unname(ENTRY_STATE[to])
      if (is.na(state)) {
        toastr_error(sprintf("No entry state is defined for %s.", to),
                     title = "Cannot draw"); return()
      }
      qty <- max(1L, as.integer(input$req_qty %||% 1L))
      drawn <- NA_character_
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          # A request that NAMES A TEST is asking for a test sample, not for
          # bulk material: it gets the VT series and carries the test_id, which
          # is what makes it a test sample everywhere else in the system. A
          # request with no test asks for material and gets the ordinary series.
          check_units(conn, x$source_sample_code[1], qty)
          # Deliver MATERIAL, never a test sample.
          #
          # A request that names a test used to be fulfilled with a VT sample
          # carrying that test_id. It looked right and broke the handover: the
          # indexing queue lists material with test_id IS NULL, so the thing
          # this bench had just handed over was invisible there, and the test
          # appeared already "initiated" against a source still standing in
          # quarantine. The requesting bench had nothing to work on.
          #
          # The test_id stays on the REQUEST, which is what narrows the
          # receiving bench's test plan to the one test that was asked for.
          tid <- NA_integer_
          pfx <- CODE_PREFIX
          child <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                              params = list(pfx))$code[1]
          dbExecute(conn, "
            INSERT INTO tbl_sample (sample_code, order_number, parent_sample_code,
                                    stage_code, test_id, quantity, created_by, created_on)
            VALUES ($1,$2,$3,$4,$5,$6,$7,now())",
                    params = list(child, x$order_number[1], x$source_sample_code[1],
                                  to, tid, qty, user()))
          spend_units(conn, x$source_sample_code[1], qty)
          dbExecute(conn, "
            INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, notes)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(child, to, state, user(),
                                  sprintf("drawn from %s for %s", x$source_sample_code[1],
                                          gsub("_", " ", to))))
          dbExecute(conn, "
            UPDATE tbl_sample_request
               SET status = 'fulfilled', fulfilled_sample_code = $1,
                   fulfilled_qty = $2, fulfilled_by = $3, fulfilled_on = now()
             WHERE request_id = $4",
                    params = list(child, qty, user(), as.integer(x$request_id[1])))
          drawn <<- child
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not draw", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(
          code  = drawn, title = "QUARANTINE DRAW",
          line1 = sprintf("from %s", x$source_sample_code[1]),
          line2 = format(Sys.Date(), "%d %b %Y"), stringsAsFactors = FALSE))
        toastr_success(sprintf("%s drawn from %s.", drawn, x$source_sample_code[1]),
                       title = "Sample created")
        req_sel(NULL); refresh_all()
        # Hand off to the bench that asked. The label is printed THERE, next to
        # the conviron the sample is about to go into - printing it here would
        # mean carrying a loose label across the lab and hoping it stays with
        # the right tube.
        tab <- unname(DEST_TAB[to])
        if (!is.na(tab)) {
          runjs(sprintf(
            "Shiny.setInputValue('rtb_goto', {tab: '%s', n: Math.random()}, {priority: 'event'})",
            tab))
        }
      }
    })
    
    shiny$observeEvent(input$req_cancel, {
      x <- req_row(); shiny$req(!is.null(x))
      ok <- tryCatch({
        dbExecute(pool, "
          UPDATE tbl_sample_request
             SET status = 'cancelled', cancelled_on = now(),
                 cancel_reason = 'cancelled in quarantine'
           WHERE request_id = $1", params = list(as.integer(x$request_id[1]))); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e)); FALSE })
      if (ok) { toastr_success("Request cancelled."); req_sel(NULL); refresh_all() }
    })
    
    
    shiny$observeEvent(input$clr_click, {
      sc <- input$clr_click$id; act <- input$clr_click$act
      shiny$req(sc, act)
      if (identical(act, "rejected")) {
        if (do_clear(sc, "rejected")$ok) {
          toastr_success(sprintf("%s rejected.", sc)); refresh_all()
        }
        return()
      }
      clr_active(sc)
      shiny$showModal(shiny$modalDialog(
        title = paste("Clear", sc, "to next step"),
        shiny$div(class = "rtb-intake",
                  order_theme$guide(
                    "Approving marks ", shiny$strong(sc),
                    " as available. It stays in quarantine as standing stock \u2014",
                    " virus indexing draws from it, one sample per test, when it",
                    " is ready to run that test.")
        ),
        footer = shiny$tagList(shiny$modalButton("Cancel"),
                               shiny$actionButton(ns("route1_go"), "Approve \u2014 keep in quarantine", class = "btn btn-success")),
        easyClose = FALSE, size = "m"))
    })
    
    shiny$observeEvent(input$route1_go, {
      sc <- clr_active(); shiny$req(sc)
      if (isTRUE(do_clear(sc, "approved"))) {
        shiny$removeModal()
        toastr_success(sprintf("%s approved and available to virus indexing.", sc),
                       title = "Available")
        clr_active(NULL); refresh_all()
      }
    })
    
    clr_batch_order <- shiny$reactiveVal(NULL)
    
    shiny$observeEvent(input$clr_batch_click, {
      on <- input$clr_batch_click$id; shiny$req(on)
      d <- clr_all(); codes <- d$sample_code[d$order_number == on]
      shiny$req(length(codes) >= 1)
      shiny$showModal(shiny$modalDialog(
        title = paste("Clear consignment", on),
        shiny$div(class = "rtb-intake", order_theme$head_orders(),
                  shiny$div(class = "update-hint",
                            sprintf("%d sample%s of this consignment are awaiting clearance. ",
                                    length(codes), if (length(codes) == 1) "" else "s"),
                            "Approving clears them all for indexing at once; each keeps its own bench."),
                  order_theme$section("\u2713", "Decision",
                                      shiny$radioButtons(ns("batch_decision"), NULL,
                                                         choices = c("Approve all" = "approved", "Reject all" = "rejected"),
                                                         selected = "approved"),
                                      shiny$textAreaInput(ns("batch_notes"), "Notes (applied to all)"))
        ),
        footer = shiny$tagList(shiny$modalButton("Cancel"),
                               shiny$actionButton(ns("batch_save"),
                                                  sprintf("Clear %d sample%s", length(codes), if (length(codes) == 1) "" else "s"),
                                                  class = "btn btn-success")),
        easyClose = FALSE, size = "m"))
      clr_batch_order(on)
    })
    
    shiny$observeEvent(input$batch_save, {
      on <- clr_batch_order(); shiny$req(on)
      d <- clr_all(); codes <- d$sample_code[d$order_number == on]
      shiny$req(length(codes) >= 1)
      dec <- input$batch_decision %||% "approved"
      notes <- if (nzchar(input$batch_notes %||% "")) input$batch_notes else NA
      
      if (isTRUE(do_clear(codes, dec, notes = notes))) {
        shiny$removeModal()
        toastr_success(sprintf("%d sample%s of %s %s%s.", length(codes),
                               if (length(codes) == 1) "" else "s", on,
                               if (identical(dec, "approved")) "cleared" else "rejected",
                               if (identical(dec, "approved"))
                                 paste0(" \u2014 to ", if (identical(route, "thermotherapy")) "thermotherapy" else "indexing")
                               else ""))
        clr_batch_order(NULL); refresh_all()
      }
    })
    
    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

