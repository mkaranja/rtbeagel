box::use(
  shiny,
  reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
  shinyjs[useShinyjs],
  shinytoastr[toastr_success, toastr_error],
  htmlwidgets[JS],
  pool[poolWithTransaction],
  DBI[dbExecute, dbGetQuery],
)

box::use(
  app/logic/fct_conn[pool, load_data],
  app/view/shared/order_theme,
)

# ============================================================================
# INITIATION · establishing samples from a quarantine consignment
# ----------------------------------------------------------------------------
# THIS IS WHERE SAMPLES ARE BORN. Everything upstream is order-level: an order
# is registered, approved, and its consignment is received onto a quarantine
# bench. No tbl_sample row exists yet, because until explants are cut there is
# nothing to track individually - there is a box of plant material.
#
# Initiation cuts that consignment into countable, trackable units. Each gets
# a code from next_sample_code(), a row in tbl_sample, and a first event. From
# here on the pipeline is per-sample.
#
# THE TWO BENCHES USE DIFFERENT WORDS FOR THE SAME ACT:
#   glasshouse  transferred -> established
#   growthroom  transferred -> received
# Both mean "the material is now on the bench as individual plants". The
# vocabulary is not ours to normalise - it is what the lab says - so
# tbl_stage_state carries both and this module picks the right one per stage.
# Getting it wrong is not a silent bug: the composite FK rejects the insert.
#
# SAMPLES ARE NOT ALLOCATED HERE. tbl_sample.order_service_id stays NULL:
# these are shared upstream stock, not yet earmarked for conservation or
# distribution. That decision happens later, at subculture or hardening, when
# there is something to allocate. NULL is the meaningful value.
# ============================================================================

# Sample codes: prefix + 2-digit year + 3-digit sequence, e.g. IN26001.
# Minted by next_sample_code(), which serialises on a counter row, so two
# technicians initiating at once cannot collide.
CODE_PREFIX <- "IN"

# Which state each bench reaches once established.
ESTABLISHED_STATE <- c(
  quarantine_glasshouse = "established",
  quarantine_growthroom = "received"
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    order_theme$page_header(
      title = "Initiation",
      sub   = "Establish individual samples from a received consignment."
    ),
    
    # The hint that used to sit in the header moves here, where it is one line
    # instead of a paragraph and sits next to the table it describes.
    order_theme$guide(
      shiny$strong("Sample codes begin here."),
      " Everything before this point is recorded against the order; from here ",
      "each plant is tracked on its own."
    ),
    
    shiny$uiOutput(ns("kpis")),
    
    order_theme$toolbar(
      order_theme$search_box(ns("q"), "Search order, customer, crop, bench..."),
      order_theme$filter_select(
        ns("filter"),
        choices = c("Awaiting initiation" = "awaiting",
                    "Initiated"           = "done",
                    "All on bench"        = "all"),
        selected = "awaiting", width = "200px"
      )
    ),
    
    order_theme$table_card(reactableOutput(ns("tbl")))
  )
}


#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bump <- shiny$reactiveVal(0)
    
    user <- shiny$reactive(shiny$reactiveValuesToList(res_auth)$user)
    
    # ---- the bench ---------------------------------------------------
    # Every consignment on a quarantine bench, with how many samples have
    # been established from it so far. The LEFT JOIN matters: a consignment
    # with zero samples is exactly the one we want to show.
    bench <- shiny$reactive({
      bump(); if (!is.null(trigger_refresh)) trigger_refresh()
      load_data(pool, "
        SELECT q.order_number, q.stage_code, q.bench_no, q.received_date,
               q.quantity AS received_qty,
               cu.customer_name, c.crop_name, v.variety_name,
               st.label AS sample_type, o.sample_amount,
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
    
    rows <- shiny$reactive({
      d <- bench()
      f <- input$filter %||% "awaiting"
      if (nrow(d) > 0) {
        d <- switch(f,
                    awaiting = d[d$n_samples == 0, , drop = FALSE],
                    done     = d[d$n_samples > 0, , drop = FALSE],
                    d)
      }
      q <- input$q
      if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
        cols <- intersect(c("order_number","customer_name","crop_name","variety_name","bench_no"),
                          names(d))
        hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    output$kpis <- shiny$renderUI({
      d <- bench()
      awaiting <- if (nrow(d)) sum(d$n_samples == 0) else 0L
      done     <- if (nrow(d)) sum(d$n_samples > 0) else 0L
      samples  <- if (nrow(d)) sum(d$n_samples) else 0L
      order_theme$stat_row(
        order_theme$stat_tile(awaiting, "Awaiting initiation",
                              tone = if (awaiting > 0) "amber" else "ink"),
        order_theme$stat_tile(done, "Initiated", tone = "brand"),
        order_theme$stat_tile(samples, "Samples established", tone = "teal"),
        order_theme$stat_tile(nrow(d), "On bench", tone = "ink")
      )
    })
    
    output$tbl <- renderReactable({
      d <- rows()
      d$`__act` <- rep(NA, nrow(d))   # rep(): an empty bench is normal
      
      reactable(
        d,
        columns = list(
          total_qty = colDef(show = FALSE),
          order_number = colDef(name = "ORDER", width = 165,
                                cell = function(v) shiny$tags$strong(v)),
          customer_name = colDef(name = "CUSTOMER", minWidth = 140),
          crop_name = colDef(name = "CROP", width = 100,
                             cell = function(v) if (is.na(v)) "\u2014" else v),
          variety_name = colDef(name = "VARIETY", width = 110,
                                cell = function(v) if (is.na(v)) "\u2014" else v),
          sample_type = colDef(name = "TYPE", width = 95,
                               cell = function(v) if (is.na(v)) "\u2014" else v),
          stage_code = colDef(name = "BENCH", width = 125, cell = function(v) {
            order_theme$chip(
              if (identical(v, "quarantine_glasshouse")) "Glasshouse" else "Growthroom",
              if (identical(v, "quarantine_glasshouse")) "brand" else "teal")
          }),
          bench_no = colDef(name = "NO", width = 70,
                            cell = function(v) if (is.na(v)) "\u2014" else v),
          received_date = colDef(name = "RECEIVED", width = 105,
                                 cell = function(v) if (is.na(v)) "\u2014" else format(as.Date(v), "%d %b %y")),
          received_qty = colDef(name = "IN", width = 60,
                                cell = function(v) if (is.na(v)) "\u2014" else v),
          sample_amount = colDef(name = "EXPECTED", width = 90),
          n_samples = colDef(name = "ESTABLISHED", width = 105, cell = function(v, i) {
            if (v == 0) return(order_theme$chip("None", "amber"))
            order_theme$chip(paste(v, "samples"), "brand")
          }),
          `__act` = colDef(name = "", sortable = FALSE, width = 105, html = TRUE,
                           cell = JS(sprintf("function(ci){
              var lbl = ci.row['n_samples'] > 0 ? 'Add more' : 'Establish';
              return '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + ci.row['order_number'] + '\\', n: Math.random()})\">' + lbl + '</button>';
            }", ns("establish_click"))))
        ),
        defaultPageSize = 12, compact = TRUE, highlight = TRUE,
        language = reactableLang(
          noData = "Nothing on the bench. Consignments appear once received into quarantine."
        ),
        theme = order_theme$rt_theme()
      )
    })
    
    # ---- establish ---------------------------------------------------
    active <- shiny$reactiveVal(NULL)
    
    shiny$observeEvent(input$establish_click, {
      code <- input$establish_click$id
      shiny$req(code)
      d <- rows()
      row <- d[d$order_number == code, , drop = FALSE]
      shiny$req(nrow(row) == 1)
      active(code)
      
      bench_label <- if (identical(row$stage_code[1], "quarantine_glasshouse")) "glasshouse" else "growthroom"
      state_word  <- unname(ESTABLISHED_STATE[row$stage_code[1]])
      already     <- row$n_samples[1]
      suggested   <- max(1, row$sample_amount[1] - already)
      
      shiny$showModal(shiny$modalDialog(
        title = paste("Establish samples \u00b7", code),
        shiny$div(
          class = "rtb-intake", order_theme$head_orders(),
          
          shiny$div(class = "update-hint",
                    "Each sample gets its own code and is tracked individually from here on. ",
                    shiny$br(),
                    shiny$strong("Bench: "), bench_label,
                    "  \u00b7  ", shiny$strong("State: "), state_word,
                    if (already > 0) shiny$tagList(
                      shiny$br(),
                      shiny$span(style = "opacity:.8;",
                                 sprintf("%d sample%s already established from this consignment. New ones are added alongside.",
                                         already, if (already == 1) "" else "s"))
                    )
          ),
          
          order_theme$section(
            "\u2702", "Explants",
            shiny$numericInput(ns("n_samples"),
                               shiny$HTML("Number of samples to establish <span class='mandatory_star'>*</span>"),
                               value = suggested, min = 1, max = 500),
            shiny$numericInput(ns("qty_each"),
                               shiny$HTML("Units per sample <span class='mandatory_star'>*</span>"),
                               value = 1, min = 1),
            shiny$dateInput(ns("established_on"), "Date established", value = Sys.Date()),
            shiny$textAreaInput(ns("notes"), "Notes")
          ),
          
          shiny$div(class = "text-muted", style = "font-size:11.5px;",
                    sprintf("Codes will be assigned automatically as %s%s001, %s%s002, ...",
                            CODE_PREFIX, format(Sys.Date(), "%y"),
                            CODE_PREFIX, format(Sys.Date(), "%y")))
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("save"), "Establish", class = "btn btn-success")
        ),
        easyClose = FALSE, size = "m"
      ))
    })
    
    shiny$observeEvent(input$save, {
      code <- active(); shiny$req(code)
      
      n <- input$n_samples
      q <- input$qty_each
      if (is.null(n) || is.na(n) || n < 1) {
        toastr_error("Number of samples must be at least 1.", title = "Invalid"); return()
      }
      if (n > 500) {
        toastr_error("500 samples is the per-batch limit. Split it.", title = "Too many"); return()
      }
      if (is.null(q) || is.na(q) || q < 1) {
        toastr_error("Units per sample must be at least 1.", title = "Invalid"); return()
      }
      
      d <- rows()
      row <- d[d$order_number == code, , drop = FALSE]
      shiny$req(nrow(row) == 1)
      stage <- row$stage_code[1]
      state <- unname(ESTABLISHED_STATE[stage])
      if (is.na(state)) {
        toastr_error(paste("No established-state defined for", stage), title = "Configuration"); return()
      }
      
      created <- character(0)
      
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          
          for (i in seq_len(as.integer(n))) {
            # One code per sample, minted inside the transaction. The counter
            # row locks, so concurrent initiations interleave safely rather
            # than colliding.
            sc <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                             params = list(CODE_PREFIX))$code[1]
            
            # order_service_id is deliberately absent -> NULL. These are
            # shared upstream stock; allocation happens downstream.
            dbExecute(conn, "
              INSERT INTO tbl_sample
                (sample_code, order_number, stage_code, quantity, created_by, created_on)
              VALUES ($1, $2, $3, $4, $5, $6)",
                      params = list(sc, code, stage, as.integer(q), user(),
                                    as.character(input$established_on)))
            
            # The first event. Composite FK to tbl_stage_state means an
            # illegal (stage, state) is rejected here, not discovered later.
            dbExecute(conn, "
              INSERT INTO tbl_sample_event
                (sample_code, stage_code, state_code, actor, occurred_on, notes)
              VALUES ($1, $2, $3, $4, $5, $6)",
                      params = list(sc, stage, state, user(),
                                    as.character(input$established_on),
                                    if (nzchar(input$notes %||% "")) input$notes else NA))
            
            # `<<-`, not `<-`. poolWithTransaction() runs this body in its own
            # function, so a plain assignment writes to a LOCAL copy and the
            # outer `created` stays empty. Reads INSIDE the transaction saw the
            # local one and looked right; every read AFTER it got character(0).
            created <<- c(created, sc)
          }
          
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'initiation', $2, $3, $4)",
                    params = list(code,
                                  sprintf("established %d sample%s", n, if (n == 1) "" else "s"),
                                  user(),
                                  sprintf("%s \u2014 %s to %s",
                                          stage, created[1], created[length(created)])))
        })
        TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Establish failed", timeOut = 0); FALSE
      })
      
      if (ok) {
        shiny$removeModal()
        toastr_success(sprintf("%d sample%s established (%s\u2026%s)",
                               n, if (n == 1) "" else "s",
                               created[1], created[length(created)]))
        active(NULL)
        bump(bump() + 1)
        if (!is.null(trigger_refresh)) trigger_refresh(trigger_refresh() + 1)
      }
    })
    
    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a