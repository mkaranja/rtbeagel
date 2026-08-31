box::use(
  shiny,
  reactable[reactable, reactableOutput, renderReactable, colDef, reactableTheme, reactableLang],
  shinyjs[useShinyjs],
  shinytoastr[toastr_success, toastr_error],
  htmlwidgets[JS],
  pool[poolWithTransaction],
  DBI[dbExecute],
)

box::use(
  app/logic/fct_conn[pool, load_data],
  app/logic/fct_tracking[scan_identity, scan_history, scan_options],
  app/logic/fct_workflows[workflow_cache, sample_context, record_event],
  app/view/shared/order_theme,
)

# ============================================================================
# BARCODE STATION · scan anything, see where it is and move it on
# ----------------------------------------------------------------------------
# A single global screen. Scan any code - explant, meristem tip, test-sample,
# or plant - and it shows what the code is, its full history, its
# workflow-recommended next step, and every legal alternative. Recording a step
# writes the event; because every stage module is a pull-queue keyed on
# (stage, state), the sample then appears in the queue of whichever module owns
# the stage it moved to. The station does not duplicate the stage modules - it
# is a fast path to move one known sample without hunting for it in a queue.
#
# A recommended step records clean. A legal-but-not-recommended alternative is
# off-workflow, so record_event flags it and a reason is required - the same
# PERMITTED-vs-RECOMMENDED distinction the rest of the system uses.
# ============================================================================

WF_PATH <- file.path("app", "static", "workflows", "cassava.yaml")

KIND_LABEL <- c(explant = "Explant", meristem_tip = "Meristem tip",
                test_sample = "Test sample", plant = "Plant")

# indexing/thermotherapy-style stages are entered at a conventional state; map
# a recommended target stage to the state a fresh arrival takes.
ENTRY_STATE <- c(
  molecular_virus_indexing = "inprogress",
  grafting_virus_indexing  = "inprogress",
  thermotherapy            = "inprogress",
  meristem_culture         = "established",
  surface_sterilization    = "established",
  subculture               = "established",
  hardening                = "established",
  in_vitro_conservation    = "established",
  in_vitro_distribution    = "established",
  in_vivo_conservation     = "established",
  mini_tubers_distribution = "established",
  quarantine_glasshouse    = "established",
  quarantine_growthroom    = "received"
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    order_theme$page_header(
      title = "Barcode Station",
      sub   = "Scan any sample or plant to see its history and move it on."
    ),
    
    order_theme$guide(
      "Scan or type a code and press ", shiny$strong("Enter"),
      ". Works for explants, meristem tips, test samples and plants."
    ),
    
    order_theme$toolbar(
      order_theme$scan_box(ns("code"), ns("lookup"),
                           placeholder = "Scan or type a code, then Enter...",
                           label = "Scan or type any sample, tip, test or plant code")
    ),
    
    # The scanned record, full width: it is the answer to the question the
    # operator just asked, so it does not share the row with anything.
    shiny$uiOutput(ns("card")),
    
    # History on the left, where this sample can go next on the right - the
    # same two-pane language every stage module uses.
    order_theme$workbench(
      list_ui = order_theme$table_card(
        shiny$uiOutput(ns("history_head")),
        reactableOutput(ns("history"))
      ),
      detail_ui = shiny$uiOutput(ns("next_steps"))
    )
  )
}


#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    user <- shiny$reactive(shiny$reactiveValuesToList(res_auth)$user)
    
    scanned <- shiny$reactiveVal(NULL)   # the resolved code
    bump    <- shiny$reactiveVal(0)
    
    # Enter in the field clicks Look up (wired via onkeydown in the UI); the
    # scanner types the code then sends Enter, so this is the scan trigger.
    shiny$observeEvent(input$lookup, { do_lookup() })
    
    do_lookup <- function() {
      code <- trimws(input$code %||% "")
      if (!nzchar(code)) { scanned(NULL); return() }
      scanned(code); bump(bump() + 1)
    }
    
    ident <- shiny$reactive({ bump(); code <- scanned(); if (is.null(code)) NULL else scan_identity(code) })
    
    output$card <- shiny$renderUI({
      code <- scanned(); if (is.null(code)) return(NULL)
      d <- ident()
      if (is.null(d) || nrow(d) == 0) {
        return(shiny$div(class = "flow-cta warn",
                         shiny$span(class = "fc-ico", shiny$icon("triangle-exclamation")),
                         shiny$span("No sample found for code ", shiny$strong(code), ".")))
      }
      r <- d[1, ]
      at <- if (is.na(r$stage_label)) "\u2014" else paste0(r$stage_label, " \u00b7 ", r$state_label)
      order_theme$stat_row(
        order_theme$stat_tile(order_theme$chip(KIND_LABEL[r$kind] %||% r$kind, "brand"), "Kind", tone = "ink"),
        order_theme$stat_tile(at, "Currently at", tone = "brand"),
        order_theme$stat_tile(r$order_number, "Order", tone = "ink"),
        order_theme$stat_tile(
          if (!is.na(r$crop_name)) paste0(r$crop_name,
                                          if (!is.na(r$variety_name)) paste0(" / ", r$variety_name) else "") else "\u2014",
          "Crop", tone = "teal")
      )
    })
    
    # ---- next-step options -------------------------------------------
    opts <- shiny$reactive({ bump(); code <- scanned(); if (is.null(code)) NULL else scan_options(code) })
    
    output$next_steps <- shiny$renderUI({
      code <- scanned(); if (is.null(code)) return(NULL)
      d <- ident(); if (is.null(d) || nrow(d) == 0) return(NULL)
      if (identical(d$kind[1], "test_sample")) {
        return(shiny$div(class = "update-hint", style = "margin-top:12px;",
                         "This is a test sample \u2014 it carries a single test and does not travel ",
                         "the pipeline. Record its result in the Virus Indexing module."))
      }
      o <- opts()
      if (is.null(o) || nrow(o) == 0) {
        return(shiny$div(class = "update-hint", style = "margin-top:12px;",
                         "No next step available \u2014 this sample may be at a terminal stage."))
      }
      rec <- o[o$recommended, , drop = FALSE]
      alt <- o[!o$recommended, , drop = FALSE]
      
      shiny$div(style = "margin-top:12px;",
                order_theme$subhead("Next step"),
                if (nrow(rec) > 0) shiny$div(
                  lapply(seq_len(nrow(rec)), function(i) {
                    shiny$div(style = "display:flex; align-items:center; gap:10px; padding:6px 0;",
                              order_theme$chip("Recommended", "brand"),
                              shiny$strong(rec$label[i]),
                              shiny$actionButton(ns(paste0("go_rec_", i)), "Record",
                                                 class = "btn btn-success btn-sm", style = "margin-left:auto;"))
                  })),
                if (nrow(alt) > 0) shiny$div(style = "margin-top:8px;",
                                             shiny$tags$details(
                                               shiny$tags$summary(style = "cursor:pointer; font-size:12.5px; opacity:.8;",
                                                                  sprintf("%d legal alternative%s", nrow(alt), if (nrow(alt) == 1) "" else "s")),
                                               shiny$div(style = "padding-top:6px;",
                                                         lapply(seq_len(nrow(alt)), function(i) {
                                                           shiny$div(style = "display:flex; align-items:center; gap:10px; padding:5px 0;",
                                                                     order_theme$chip("Legal", "ink"),
                                                                     shiny$span(alt$state_label[i]),
                                                                     shiny$actionButton(ns(paste0("go_alt_", i)), "Record",
                                                                                        class = "btn btn-outline-secondary btn-sm", style = "margin-left:auto;"))
                                                         }))))
      )
    })
    
    # record a recommended step: one observer per possible button index.
    lapply(1:6, function(i) {
      shiny$observeEvent(input[[paste0("go_rec_", i)]], {
        o <- opts(); rec <- o[o$recommended, , drop = FALSE]
        if (i > nrow(rec)) return()
        do_record(to_stage = rec$to_stage[i], to_state = NA, label = rec$label[i], recommended = TRUE)
      }, ignoreInit = TRUE)
      shiny$observeEvent(input[[paste0("go_alt_", i)]], {
        o <- opts(); alt <- o[!o$recommended, , drop = FALSE]
        if (i > nrow(alt)) return()
        do_record(to_stage = alt$to_stage[i], to_state = alt$to_state[i],
                  label = alt$state_label[i], recommended = FALSE)
      }, ignoreInit = TRUE)
    })
    
    pending_move <- shiny$reactiveVal(NULL)
    
    do_record <- function(to_stage, to_state, label, recommended) {
      code <- scanned(); shiny$req(code)
      state <- if (!is.na(to_state)) to_state else (ENTRY_STATE[to_stage] %||% "inprogress")
      pending_move(list(to_stage = to_stage, to_state = state, label = label, recommended = recommended))
      
      shiny$showModal(shiny$modalDialog(
        title = paste("Record:", label),
        shiny$div(class = "rtb-intake", order_theme$head_orders(),
                  shiny$div(class = "update-hint",
                            "Move ", shiny$strong(code), " to ", shiny$strong(gsub("_", " ", to_stage)),
                            " (", state, ")."),
                  if (!recommended) shiny$tagList(
                    shiny$div(class = "flow-cta warn",
                              shiny$span(class = "fc-ico", shiny$icon("triangle-exclamation")),
                              shiny$span("This is a legal move but not the workflow's recommendation. ",
                                         "It will be recorded as off-workflow.")),
                    shiny$textAreaInput(ns("move_reason"),
                                        shiny$HTML("Reason <span class='mandatory_star'>*</span>"))),
                  shiny$textAreaInput(ns("move_notes"), "Notes")
        ),
        footer = shiny$tagList(shiny$modalButton("Cancel"),
                               shiny$actionButton(ns("move_go"), "Record step", class = "btn btn-success")),
        easyClose = FALSE, size = "m"))
    }
    
    shiny$observeEvent(input$move_go, {
      mv <- pending_move(); code <- scanned(); shiny$req(mv, code)
      if (!mv$recommended) {
        rsn <- input$move_reason
        if (is.null(rsn) || !nzchar(trimws(rsn))) {
          toastr_error("A reason is required for an off-workflow move.", title = "Reason needed"); return()
        }
      }
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf  <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, code)
          record_event(conn, code, mv$to_stage, mv$to_state, user(),
                       wf = wf, ctx = ctx,
                       reason = if (!mv$recommended) trimws(input$move_reason) else NULL,
                       notes  = if (nzchar(input$move_notes %||% "")) input$move_notes else NA)
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ((SELECT order_number FROM tbl_sample WHERE sample_code = $1),
                    'barcode_station', $2, $3, $4)",
                    params = list(code, sprintf("moved %s to %s", code, gsub("_", " ", mv$to_stage)),
                                  user(), mv$label))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Record failed", timeOut = 0); FALSE })
      
      if (ok) {
        shiny$removeModal()
        toastr_success(sprintf("%s moved to %s.", code, gsub("_", " ", mv$to_stage)))
        pending_move(NULL); bump(bump() + 1)
        if (!is.null(trigger_refresh)) trigger_refresh(trigger_refresh() + 1)
      }
    })
    
    # ---- history -----------------------------------------------------
    output$history_head <- shiny$renderUI({
      if (is.null(scanned())) return(NULL); order_theme$subhead("History")
    })
    
    output$history <- renderReactable({
      code <- scanned()
      d <- if (is.null(code)) scan_history("") else scan_history(code)
      reactable(d, columns = list(
        occurred_on = colDef(name = "WHEN", width = 150,
                             cell = function(v) if (is.na(v)) "\u2014" else format(as.POSIXct(v), "%d %b %y %H:%M")),
        stage_label = colDef(name = "STAGE", minWidth = 150),
        state_label = colDef(name = "STATE", width = 130),
        actor = colDef(name = "BY", width = 110),
        is_override = colDef(name = "", width = 110, cell = function(v) {
          if (isTRUE(v)) order_theme$chip("Off-workflow", "amber") else ""
        }),
        notes = colDef(name = "NOTES", minWidth = 180,
                       cell = function(v) if (is.na(v)) "" else v)
      ),
      defaultPageSize = 8, compact = TRUE, highlight = TRUE,
      language = reactableLang(noData = "Scan a code to see its history."),
      theme = order_theme$rt_theme())
    })
    
    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a