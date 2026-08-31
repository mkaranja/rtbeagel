box::use(
  shiny,
  bs4Dash[bs4Card],
  reactable[reactable, reactableOutput, renderReactable, colDef,
            reactableTheme, reactableLang],
  shinyjs[useShinyjs, runjs],
  shinytoastr[toastr_success, toastr_error, toastr_warning],
  htmlwidgets[JS],
  pool[poolWithTransaction],
  DBI[dbExecute, dbGetQuery],
  stats[setNames],
)

box::use(
  app/logic/fct_conn[pool, load_data],
  app/logic/fct_tracking[meristem_queue, scan_identity, last_approval_request,
                         pending_requests, meristem_held],
  app/logic/fct_workflows[workflow_cache, next_options, sample_context, record_event],
  app/logic/fct_notify[notify_approvers],
  app/view/shared/label_print,
  app/view/shared/order_theme,
)

# ============================================================================
# MERISTEM CULTURE · clinical worklist (worklist + right detail panel)
# ----------------------------------------------------------------------------
# The child-derivation stage. An operator takes a heat-treated (or cleaned)
# explant and excises apical meristem tips - tiny growing points most likely to
# be virus-free. Each tip is a NEW sample: its own MC-series code, a
# parent_sample_code pointing back at the source explant, born directly at
# meristem_culture/established. The source's role ends when its tips are taken,
# so it is marked completed in the SAME transaction - source and tips can never
# disagree about whether excision happened.
#
# Two-pane worklist, matching virus indexing and thermotherapy. The bench holds
# two kinds of row, told apart by parent_sample_code:
#   SOURCE explant (parent NULL) -> detail panel offers EXCISE.
#   TIP           (parent set)   -> detail panel offers UPDATE then REVIEW.
#
# ACTS, mapped to the legal state machine (established / updated / completed +
# rejected):
#   Excise   create N child samples at meristem_culture/established (direct
#            insert - a birth, not a transition), then complete the source.
#   Update   established -> updated  (a tip checkpoint).
#   Review   updated -> completed (recommends re-indexing the clean tip) OR
#            updated -> rejected (contaminated / failed).
#
# PERFORMANCE: local refresh + tab-activation, never the global storm.
# ============================================================================

MY_TAB       <- "meristem"
WF_PATH      <- file.path("app", "static", "workflows", "cassava.yaml")
CHILD_PREFIX <- "MC"        # meristem tips get their own code series
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    order_theme$page_header(
      title = "Meristem Culture",
      sub   = "Excise meristem tips from source explants."
    ),
    
    shiny$uiOutput(ns("kpis")),
    shiny$uiOutput(ns("guide")),
    
    order_theme$toolbar(
      order_theme$search_box(ns("q"), "Search sample, order, customer..."),
      order_theme$scan_box(ns("scan"), ns("scan_go"))
    ),
    
    order_theme$workbench(
      # ONE Requests tab carrying everything that moves material off this
      # bench, in the order it happens: what was asked for, what is authorised
      # and waiting to be cut, and what is standing here ready to be sent.
      # Quarantine has the same shape - a holding stage is a holding stage.
      list_ui   = shiny$tagList(
        shiny$conditionalPanel(
          condition = sprintf("input['%s'] == 'requests'", ns("filter")),
          order_theme$table_card(
            order_theme$table_note(
              title = "1. Authorise. ",
              "Another bench has asked for a completed tip. Authorising allows ",
              "material to be taken; it does not cut anything."),
            reactableOutput(ns("req_tbl"))),
          order_theme$table_card(
            order_theme$table_note(
              title = "2. Enter the sample. ",
              "Record what was cut and its unit count. The tip stays on this ",
              "bench and can be drawn again."),
            reactableOutput(ns("init_tbl"))),
          order_theme$table_card(
            order_theme$table_note(
              title = "Completed tips standing here. ",
              "Cleaned material waiting to be tested. Select one to send it for ",
              "retesting without waiting to be asked."),
            reactableOutput(ns("held_tbl"))),
          shiny$uiOutput(ns("action_panel"))),
        shiny$conditionalPanel(
          condition = sprintf("input['%s'] != 'requests'", ns("filter")),
          order_theme$table_card(reactableOutput(ns("tbl"))))
      ),
      detail_ui = shiny$uiOutput(ns("detail"))
    )
  )
}



#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    user <- shiny$reactive(shiny$reactiveValuesToList(res_auth)$user)
    # approval is admin-only; shinymanager supplies the flag
    is_admin <- shiny$reactive(isTRUE(shiny$reactiveValuesToList(res_auth)$admin))
    
    # ---- scoped refresh ----------------------------------------------
    refresh <- shiny$reactiveVal(0)
    self_refresh <- function() refresh(shiny$isolate(refresh()) + 1)
    if (!is.null(tab) && is.function(tab)) {
      shiny$observeEvent(tab(), { if (identical(tab(), MY_TAB)) self_refresh() }, ignoreInit = TRUE)
    }
    signal_others <- function() {
      if (!is.null(trigger_refresh)) trigger_refresh(shiny$isolate(trigger_refresh()) + 1)
    }
    
    printer <- label_print$server("print", module_name = "meristem_culture",
                                  user = user)
    
    # Set the moment tips are cut, cleared when the operator moves on. Excision
    # creates several new codes at once and then the source leaves the tab -
    # without this the operator is returned to a list with no idea which codes
    # were just created or where they went.
    just_cut <- shiny$reactiveVal(NULL)
    
    queue <- shiny$reactive({ refresh(); meristem_queue() })
    
    # ---- REQUESTS FOR MERISTEM STOCK ------------------------------------
    # Completed tips are standing stock. Virus indexing asks for one; this bench
    # authorises it, and the sample is then entered under Initiation.
    #
    # Two acts, two people: a supervisor authorises that material may be taken,
    # a technician enters what was actually cut. Quarantine does both in one
    # click because one person does both there.
    MY_SOURCE_STAGE <- "meristem_culture"
    
    # The SAME draw rules quarantine applies. A holding stage is a holding
    # stage: it may not cut more than it holds, and one unit must always
    # survive so the material can answer a later request.
    #
    # These were inline here and only half-implemented - the cap was checked
    # but the units were never spent, so quantity never moved and the cap could
    # never actually bite.
    check_units <- function(conn, sample_code, qty) {
      h <- dbGetQuery(conn, "SELECT quantity FROM tbl_sample WHERE sample_code = $1",
                      params = list(sample_code))
      if (nrow(h) == 0) stop("Source material not found: ", sample_code, call. = FALSE)
      have <- as.integer(h$quantity[1])
      if (have <= 1) {
        stop(sprintf(
          paste("%s holds %d unit and cannot be drawn from - one unit must stay",
                "for future requests. Excise more tips, then draw again."),
          sample_code, have), call. = FALSE)
      }
      if (qty > have - 1L) {
        stop(sprintf(
          "Cannot draw %d of %s: it holds %d unit%s and one must stay. Draw at most %d.",
          qty, sample_code, have, if (have == 1) "" else "s", have - 1L), call. = FALSE)
      }
      invisible(have)
    }
    
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
    
    reqs <- shiny$reactive({ refresh(); pending_requests(MY_SOURCE_STAGE) })
    
    # ---- TIPS STANDING HERE, READY TO ROUTE -----------------------------
    # The mirror of quarantine's cleared stock. Culture, excision and review
    # all leave the tip HERE; this is what is on the bench, and this bench can
    # send it onward without waiting to be asked.
    held <- shiny$reactive({ refresh(); meristem_held() })
    held_sel <- shiny$reactiveVal(NULL)
    shiny$observeEvent(input$held_pick, { held_sel(input$held_pick$code); req_sel(NULL) })
    
    held_row <- shiny$reactive({
      sc <- held_sel(); if (is.null(sc)) return(NULL)
      d <- held(); if (nrow(d) == 0) return(NULL)
      x <- d[d$sample_code == sc, , drop = FALSE]
      if (nrow(x) == 0) NULL else x
    })
    
    output$held_tbl <- renderReactable({
      d <- held()
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("held_pick"), "sample_code")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        columns = order_theme$rt_cols(list(
          state_code = colDef(show = FALSE), since = colDef(show = FALSE),
          variety_name = colDef(show = FALSE),
          sample_code = colDef(name = "TIP", minWidth = 115,
                               cell = function(v) shiny$tags$strong(v)),
          parent_code = colDef(name = "CUT FROM", minWidth = 115),
          state_label = colDef(name = "STATE", width = 120,
                               cell = function(v) order_theme$chip(v %||% "", "brand")),
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
          "No completed tips are standing here. Finish a culture first."),
        theme = order_theme$rt_theme())
    })
    
    # A tip only ever goes back for testing, so there is no destination to
    # choose - which is why this has a button and not a dropdown. Quarantine
    # material can go to four benches; a cleaned tip has one purpose.
    output$held_detail <- shiny$renderUI({
      x <- held_row(); if (is.null(x)) return(NULL)
      order_theme$section(
        "\u2192", sprintf("Send %s for retesting", x$sample_code[1]),
        sub = "virus indexing",
        order_theme$guide(
          "Draws a sample from ", shiny$strong(x$sample_code[1]),
          " and sends it to virus indexing. The tip stays here and can be ",
          "drawn again.",
          if (as.integer(x$open_requests[1]) > 0)
            sprintf(" Indexing has already asked for it %d time(s).",
                    as.integer(x$open_requests[1])) else NULL),
        order_theme$prop_grid(
          order_theme$prop("Cut from", x$parent_code[1]),
          order_theme$prop("Order", x$order_number[1]),
          order_theme$prop("Units held", as.character(x$quantity[1])),
          order_theme$prop("Drawn from", sprintf("%d time(s)", as.integer(x$draws_so_far[1])))
        ),
        shiny$numericInput(ns("held_qty"),
                           sprintf("Units to draw (%d held)", as.integer(x$quantity[1])),
                           value = 1, min = 1, max = as.integer(x$quantity[1]),
                           step = 1, width = "240px"),
        order_theme$detail_actions(
          shiny$actionButton(ns("held_go"), "Draw and send to indexing",
                             class = "btn btn-success"),
          shiny$actionButton(ns("sel_cancel"), "Cancel",
                             class = "btn btn-sm btn-outline-secondary"))
      )
    })
    
    shiny$observeEvent(input$sel_cancel, { held_sel(NULL); req_sel(NULL) })
    
    # ONE action panel: whichever row is selected. Two panels permanently on
    # screen, each mostly empty, made the operator work out which set of
    # controls belonged to what they had clicked.
    output$action_panel <- shiny$renderUI({
      if (!is.null(req_row()))  return(shiny$uiOutput(ns("req_detail")))
      if (!is.null(held_row())) return(shiny$uiOutput(ns("held_detail")))
      order_theme$guide(
        "Select a request above to authorise or enter it, or a completed tip ",
        "to send it for retesting.")
    })
    
    shiny$observeEvent(input$held_go, {
      x <- held_row(); shiny$req(!is.null(x))
      qty <- max(1L, as.integer(input$held_qty %||% 1L))
      to <- "molecular_virus_indexing"
      drawn <- NA_character_
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          check_units(conn, x$sample_code[1], qty)
          child <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                              params = list(CHILD_PREFIX))$code[1]
          dbExecute(conn, "
            INSERT INTO tbl_sample (sample_code, order_number, parent_sample_code,
                                    stage_code, quantity, created_by, created_on)
            VALUES ($1,$2,$3,$4,$5,$6,now())",
                    params = list(child, x$order_number[1], x$sample_code[1], to, qty, user()))
          dbExecute(conn, "
            INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, notes)
            VALUES ($1,$2,'inprogress',$3,$4)",
                    params = list(child, to, user(),
                                  sprintf("drawn from %s for retesting", x$sample_code[1])))
          spend_units(conn, x$sample_code[1], qty)
          drawn <<- child
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not send", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(code = drawn, title = "MERISTEM SAMPLE",
                                 line1 = sprintf("from %s", x$sample_code[1]),
                                 line2 = format(Sys.Date(), "%d %b %Y"),
                                 stringsAsFactors = FALSE))
        toastr_success(sprintf("%s drawn from %s and sent to virus indexing.",
                               drawn, x$sample_code[1]), title = "Sent")
        held_sel(NULL); self_refresh(); signal_others()
      }
    })
    to_authorize <- shiny$reactive({
      d <- reqs(); if (nrow(d) == 0) return(d)
      d[d$status == "pending", , drop = FALSE]
    })
    to_enter <- shiny$reactive({
      d <- reqs(); if (nrow(d) == 0) return(d)
      d[d$status == "authorized", , drop = FALSE]
    })
    req_sel <- shiny$reactiveVal(NULL)
    
    shiny$observeEvent(input$req_pick, { req_sel(input$req_pick$code); just_cut(NULL) })
    
    # Tips this bench could satisfy a request from. The request names ONE tip,
    # because tbl_sample_request.source_sample_code is NOT NULL and something
    # has to be written there - but that name is a suggestion raised by another
    # bench, not an instruction. Which tip actually goes is a judgement made
    # here, looking at the material: a technician picks the healthy one.
    #
    # Eligibility is the same rule the retest queue applies - a completed tip
    # of this order - plus the draw rule, which is that a tip must keep one
    # unit back so it can answer a later request. A tip holding a single unit
    # is therefore listed as unavailable rather than hidden, so the operator
    # can see it exists and why it cannot be cut.
    eligible_tips <- shiny$reactive({
      x <- req_row(); if (is.null(x)) return(NULL)
      d <- held(); if (!is.data.frame(d) || nrow(d) == 0) return(d)
      d[d$order_number == x$order_number[1], , drop = FALSE]
    })
    
    tip_choices <- function(d) {
      if (!is.data.frame(d) || nrow(d) == 0) return(character(0))
      q <- as.integer(d$quantity)
      q[is.na(q)] <- 0L
      lab <- sprintf("%s  \u00b7  %d unit%s held%s", d$sample_code, q,
                     ifelse(q == 1, "", "s"),
                     ifelse(q <= 1, "  \u2014 cannot be drawn", ""))
      setNames(d$sample_code, lab)
    }
    
    req_row <- shiny$reactive({
      id <- req_sel(); if (is.null(id)) return(NULL)
      d <- reqs(); if (nrow(d) == 0) return(NULL)
      x <- d[as.character(d$request_id) == as.character(id), , drop = FALSE]
      if (nrow(x) == 0) NULL else x
    })
    
    req_cols <- function() list(
      request_id = colDef(show = FALSE), to_stage = colDef(show = FALSE),
      source_stage = colDef(show = FALSE), source_state = colDef(show = FALSE), source_units = colDef(show = FALSE),
      variety_name = colDef(show = FALSE), requested_on = colDef(show = FALSE),
      status = colDef(show = FALSE), authorized_on = colDef(show = FALSE),
      source_bench = colDef(show = FALSE),
      source_sample_code = colDef(name = "TIP", minWidth = 115,
                                  cell = function(v) shiny$tags$strong(v)),
      to_stage_label = colDef(name = "NEEDED AT", minWidth = 145,
                              cell = function(v) order_theme$chip(v, "amber")),
      test_id = colDef(show = FALSE), test_name = colDef(show = FALSE),
      # Which pathogen the retest is for. A request that named no test asks for
      # material rather than a diagnosis, so it shows an em-dash.
      test_acronym = colDef(name = "FOR", width = 90,
                            cell = function(v) if (is.na(v)) "\u2014"
                            else order_theme$chip(v, "teal")),
      reason = colDef(name = "WHY", minWidth = 200),
      requested_by = colDef(name = "ASKED BY", width = 105),
      authorized_by = colDef(name = "AUTHORISED BY", width = 130,
                             cell = function(v) if (is.na(v)) "\u2014" else v),
      draws_so_far = colDef(name = "DRAWS", width = 75),
      # Balance columns, added with partial fulfilment. A column with NO colDef
      # renders with defaults, so these would otherwise have appeared raw.
      qty_requested = colDef(show = FALSE), qty_sent = colDef(show = FALSE),
      n_deliveries = colDef(show = FALSE), last_sent_on = colDef(show = FALSE),
      # What is still owed. A tip request answered once and still short stays
      # in this queue now, and this is the number that says why it is here.
      qty_outstanding = colDef(name = "STILL OWED", width = 110,
                               cell = function(v) {
                                 n <- if (is.na(v)) 0L else as.integer(v)
                                 order_theme$chip(sprintf("%d", n),
                                                  if (n > 0) "amber" else "brand")
                               }),
      order_number = colDef(name = "ORDER", minWidth = 145),
      customer_name = colDef(name = "CUSTOMER", minWidth = 125),
      crop_name = colDef(name = "CROP", width = 90)
    )
    
    output$req_tbl <- renderReactable({
      reactable(to_authorize(),
                onClick = JS(order_theme$rt_click_js(ns("req_pick"), "request_id")),
                rowStyle = JS(order_theme$rt_pointer_js()),
                columns = req_cols(),
                defaultPageSize = 10, compact = TRUE, highlight = TRUE,
                language = order_theme$rt_lang("No bench has asked for a tip."),
                theme = order_theme$rt_theme())
    })
    
    output$init_tbl <- renderReactable({
      reactable(to_enter(),
                onClick = JS(order_theme$rt_click_js(ns("req_pick"), "request_id")),
                rowStyle = JS(order_theme$rt_pointer_js()),
                columns = req_cols(),
                defaultPageSize = 10, compact = TRUE, highlight = TRUE,
                language = order_theme$rt_lang("Nothing authorised is waiting to be entered."),
                theme = order_theme$rt_theme())
    })
    
    output$req_detail <- shiny$renderUI({
      x <- req_row(); if (is.null(x)) return(NULL)
      pending <- identical(x$status[1], "pending")
      order_theme$section(
        if (pending) "\u21a9" else "\u2713",
        sprintf("%s \u2014 %s", x$source_sample_code[1], x$to_stage_label[1]),
        sub = if (pending) "authorise" else "enter the sample",
        order_theme$guide(
          if (pending) shiny$tagList(
            "Authorising allows a sample to be taken from ",
            shiny$strong(x$source_sample_code[1]),
            ". It does not cut anything \u2014 the request then moves to ",
            shiny$strong("Initiation"), " for a technician to enter what was cut.")
          else shiny$tagList(
            "Choose the tip to cut from and enter what was taken. ",
            shiny$strong(x$source_sample_code[1]), " is what indexing suggested; ",
            "send whichever tip is healthiest. The tip stays on this bench and ",
            "can be drawn again.")),
        order_theme$prop_grid(
          order_theme$prop("Order", x$order_number[1]),
          order_theme$prop("Customer", x$customer_name[1]),
          order_theme$prop("Asked by", x$requested_by[1]),
          if (!is.na(x$test_acronym[1]))
            order_theme$prop("Retest for",
                             sprintf("%s \u2014 %s", x$test_acronym[1], x$test_name[1]))
          else NULL,
          order_theme$prop("Reason", x$reason[1]),
          if (!pending) order_theme$prop("Authorised by", x$authorized_by[1]) else NULL
        ),
        if (!pending) local({
          el  <- eligible_tips()
          ch  <- tip_choices(el)
          # Default to the suggested tip when it is still drawable, otherwise
          # to the first tip that is. Defaulting blindly to the suggestion put
          # a tip that cannot be cut in the box, and the only way to find out
          # was to press the button and read an error.
          ok_codes <- if (length(ch)) el$sample_code[as.integer(el$quantity) > 1L] else character(0)
          sel <- if (x$source_sample_code[1] %in% ok_codes) x$source_sample_code[1]
          else if (length(ok_codes)) ok_codes[1] else NULL
          if (length(ch) == 0)
            order_theme$guide(tone = "do",
                              "No completed tip of this order is standing on this bench. ",
                              "Complete a culture first, then enter this request.")
          else shiny$tagList(
            shiny$selectInput(ns("req_tip"), "Tip to draw from", choices = ch,
                              selected = sel, width = "100%"),
            if (length(ok_codes) == 0)
              order_theme$guide(tone = "do",
                                "Every tip of this order holds a single unit. One unit must ",
                                "stay so the material can answer a later request \u2014 excise ",
                                "more tips, then enter this request.") else NULL,
            shiny$numericInput(ns("req_qty"), "Units to draw",
                               value = 1, min = 1, step = 1, width = "240px"))
        }) else NULL,
        order_theme$detail_actions(
          if (pending)
            shiny$actionButton(ns("req_authorize"), "Authorise this request",
                               class = "btn btn-success")
          else
            shiny$actionButton(ns("req_enter"), "Add sample and send",
                               class = "btn btn-success"),
          shiny$actionButton(ns("req_cancel"), "Cancel request",
                             class = "btn btn-sm btn-outline-secondary"))
      )
    })
    
    shiny$observeEvent(input$req_authorize, {
      x <- req_row(); shiny$req(!is.null(x))
      if (!is_admin()) {
        toastr_error("Only an administrator can authorise a request.",
                     title = "Not permitted"); return()
      }
      ok <- tryCatch({
        dbExecute(pool, "
          UPDATE tbl_sample_request
             SET status = 'authorized', authorized_by = $1, authorized_on = now()
           WHERE request_id = $2 AND status = 'pending'",
                  params = list(user(), as.integer(x$request_id[1]))); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e)); FALSE })
      if (ok) {
        toastr_success(sprintf("%s authorised \u2014 now in Initiation for entry.",
                               x$source_sample_code[1]), title = "Authorised")
        req_sel(NULL); self_refresh()
      }
    })
    
    ENTRY_STATE <- c(molecular_virus_indexing = "inprogress",
                     grafting_virus_indexing  = "inprogress",
                     thermotherapy            = "inprogress",
                     surface_sterilization    = "established")
    DEST_TAB <- c(molecular_virus_indexing = "vx", grafting_virus_indexing = "vx",
                  thermotherapy = "thermotherapy",
                  surface_sterilization = "surface")
    
    shiny$observeEvent(input$req_enter, {
      x <- req_row(); shiny$req(!is.null(x))
      to <- x$to_stage[1]; state <- unname(ENTRY_STATE[to])
      if (is.na(state)) {
        toastr_error(sprintf("No entry state is defined for %s.", to),
                     title = "Cannot enter"); return()
      }
      qty <- max(1L, as.integer(input$req_qty %||% 1L))
      # Never send more than is still owed. The form caps this too, but the
      # form is client state and this is the write; over-delivering would leave
      # the request settled with material the asking bench never accounted for.
      owed <- as.integer(x$qty_outstanding[1] %||% qty)
      if (!is.na(owed) && owed > 0 && qty > owed) {
        toastr_error(sprintf("Only %d unit(s) are still owed on this request.", owed),
                     title = "More than is owed"); return()
      }
      # The tip the TECHNICIAN chose, not the one the request named. The
      # request's source_sample_code is a suggestion raised by another bench;
      # which tip is healthy enough to send is a call made here, at the bench,
      # looking at the material.
      src_tip <- input$req_tip %||% x$source_sample_code[1]
      if (!nzchar(src_tip %||% "")) {
        toastr_error("Choose a tip to draw from.", title = "No tip selected"); return()
      }
      # It must still be one of this order's completed tips. The dropdown only
      # offers those, but the dropdown is client state and this is the write.
      el <- eligible_tips()
      if (!is.data.frame(el) || !(src_tip %in% el$sample_code)) {
        toastr_error(sprintf("%s is not a completed tip standing on this bench for %s.",
                             src_tip, x$order_number[1]),
                     title = "Cannot draw", timeOut = 0); return()
      }
      drawn <- NA_character_
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          # The integrity check, inside the transaction: never cut more than is
          # held, and always leave one unit behind. check_units() raises with a
          # message naming the tip and the most that can be taken.
          check_units(conn, src_tip, qty)
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
          pfx <- CHILD_PREFIX
          child <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                              params = list(pfx))$code[1]
          dbExecute(conn, "
            INSERT INTO tbl_sample (sample_code, order_number, parent_sample_code,
                                    stage_code, test_id, quantity, created_by, created_on)
            VALUES ($1,$2,$3,$4,$5,$6,$7,now())",
                    params = list(child, x$order_number[1], src_tip,
                                  to, tid, qty, user()))
          spend_units(conn, src_tip, qty)
          dbExecute(conn, "
            INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, notes)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(child, to, state, user(),
                                  sprintf("drawn from %s for %s%s", src_tip,
                                          gsub("_", " ", to),
                                          if (identical(src_tip, x$source_sample_code[1])) ""
                                          else sprintf(" (requested against %s)",
                                                       x$source_sample_code[1]))))
          # PARTIAL FULFILMENT. A delivery is a row in its own right, not an
          # overwrite of the request, because a request for 40 is routinely
          # answered with the 12 that are ready today and the rest when they
          # are - and both benches have to be able to see the balance.
          #
          # source_sample_code is recorded per delivery rather than taken from
          # the request: which batch is drawn is decided here, at the bench,
          # and two deliveries against one request often come from two batches.
          dbExecute(conn, "
            INSERT INTO tbl_request_fulfilment
              (request_id, sample_code, source_sample_code, qty, fulfilled_by)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(as.integer(x$request_id[1]), child, src_tip,
                                  qty, user()))
          # Settled or still owing, decided from the LEDGER and not from what
          # this delivery happened to carry. Marking it fulfilled because a
          # delivery was made is how a request for 40 answered with 12 would
          # have disappeared from the holding bench's queue with 28 still owed.
          dbExecute(conn, "
            UPDATE tbl_sample_request r
               SET status = CASE WHEN p.qty_sent >= r.qty_requested
                                 THEN 'fulfilled' ELSE 'partial' END,
                   fulfilled_sample_code = $2, fulfilled_qty = $3,
                   fulfilled_by = $4, fulfilled_on = now()
              FROM view_request_progress p
             WHERE r.request_id = $1 AND p.request_id = r.request_id",
                    params = list(as.integer(x$request_id[1]), child, qty, user()))
          drawn <<- child
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not enter", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(code = drawn, title = "MERISTEM SAMPLE",
                                 line1 = sprintf("from %s", src_tip),
                                 line2 = format(Sys.Date(), "%d %b %Y"),
                                 stringsAsFactors = FALSE))
        toastr_success(sprintf("%s entered from %s.", drawn, src_tip),
                       title = "Sample created")
        req_sel(NULL); self_refresh(); signal_others()
        tab <- unname(DEST_TAB[to])
        if (!is.na(tab)) runjs(sprintf(
          "Shiny.setInputValue('rtb_goto', {tab: '%s', n: Math.random()}, {priority: 'event'})", tab))
      }
    })
    
    shiny$observeEvent(input$req_cancel, {
      x <- req_row(); shiny$req(!is.null(x))
      ok <- tryCatch({
        dbExecute(pool, "
          UPDATE tbl_sample_request
             SET status = 'cancelled', cancelled_on = now(),
                 cancel_reason = 'cancelled in meristem culture'
           WHERE request_id = $1", params = list(as.integer(x$request_id[1]))); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e)); FALSE })
      if (ok) { toastr_success("Request cancelled."); req_sel(NULL); self_refresh() }
    })
    
    rows <- shiny$reactive({
      d <- queue(); f <- input$filter %||% "all"
      if (nrow(d) > 0) {
        d <- switch(f,
                    incoming = d[d$state_code == "received", , drop = FALSE],
                    # role, NOT parentage. Drawn material always has a parent
                    # now - the quarantine source it came from - so parentage
                    # can no longer tell an explant from a tip.
                    # Completed and rejected explants are done - they stay
                    # visible under "On the bench" as history, but they are not
                    # work, and counting them made the tab look busy for ever.
                    # ONE arm for the work. "Source explants" and "Meristem
                    # tips" were separate tabs, but the table groups tips under
                    # the source they were cut from - so the two tabs split a
                    # view that is only useful together, and an operator had to
                    # switch tabs to see what came out of what.
                    #
                    # Finished explants drop out: once tips are taken the
                    # explant's job is done and it is history, visible under
                    # "On the bench" but not counted as work.
                    culture = d[d$state_code != "received" &
                                  !(d$role == "explant" &
                                      d$state_code %in% c("completed", "rejected")),
                                , drop = FALSE],
                    d)
      }
      q <- input$q
      if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
        cols <- intersect(c("sample_code","order_number","customer_name","crop_name","parent_sample_code"), names(d))
        hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    # The KPI row IS the tab bar. Each count equals the rows clicking it shows;
    # the four values mirror rows()' switch() arms exactly.
    #
    # "Tips taken" was dropped from this row. It is a SUM of n_children, not a
    # subset of the worklist - there is no set of rows it could filter to, so
    # as a tab it would be a lie. It moves to the table note, where an
    # aggregate belongs.
    #
    # isolate() on the active tab: the highlight moves client-side, so this
    # row must not re-render when a tab is clicked.
    # The tab bar, in quarantine's flow-stepper language. Incoming -> source
    # explants -> meristem tips is the real order of the work, so those three
    # carry ordinals; "On the bench" is a view across them and is marked.
    #
    # "Tips taken" is NOT a tab. It is a SUM of n_children, not a subset of the
    # worklist - there is no set of rows it could filter to, so as a tab it
    # would be a lie. It moves to the table note, where an aggregate belongs.
    #
    # `filter` is the same input the dropdown used to set, so rows() is
    # unchanged. isolate() on the active tab: the highlight moves client-side.
    output$kpis <- shiny$renderUI({
      d <- queue()
      incoming <- if (nrow(d)) sum(d$state_code == "received") else 0L
      culture <- if (nrow(d)) sum(d$state_code != "received" &
                                    !(d$role == "explant" &
                                        d$state_code %in% c("completed", "rejected"))) else 0L
      cur <- shiny$isolate(input$filter) %||% "all"
      order_theme$flow_stepper(
        list(
          list(title = "On the bench", sub = "every sample here", value = "all",
               num = "\u2211", count = nrow(d), unit = "samples",
               active = identical(cur, "all")),
          list(title = "Incoming", sub = "receive onto this bench", value = "incoming",
               count = incoming, unit = "to receive",
               active = identical(cur, "incoming"), waiting = incoming > 0),
          # Named for the WORK. Sources and their tips are one tab because the
          # table groups them together anyway.
          list(title = "Culture & excise", sub = "grow on, cut tips, review",
               value = "culture", count = culture, unit = "in culture",
               active = identical(cur, "culture"), waiting = culture > 0),
          # Marked, not numbered: a request is work another bench has asked
          # for, arriving at any point, not a position a tip passes through.
          # Authorising and entering are two steps of ONE request, so they are
          # one tab with two lists rather than two tabs.
          list(title = "Requests", sub = "authorise, then enter", value = "requests",
               num = "\u21a9",
               count = nrow(to_authorize()) + nrow(to_enter()), unit = "to handle",
               active = identical(cur, "requests"),
               waiting = (nrow(to_authorize()) + nrow(to_enter())) > 0)
        ),
        input_id = ns("filter")
      )
    })
    
    
    # The aggregate that is no longer a tab.
    
    # ---- worklist ----------------------------------------------------
    # THE instruction: one sentence, specific to the tab, ending in the handoff.
    output$guide <- shiny$renderUI({
      d <- queue()
      incoming <- if (nrow(d)) sum(d$state_code == "received") else 0L
      culture  <- if (nrow(d)) sum(d$state_code != "received" &
                                     !(d$role == "explant" &
                                         d$state_code %in% c("completed", "rejected"))) else 0L
      excised  <- if (nrow(d)) sum(d$n_children, na.rm = TRUE) else 0L
      switch(
        input$filter %||% "all",
        incoming = if (incoming > 0)
          order_theme$guide(tone = "do",
                            shiny$strong(incoming),
                            if (incoming == 1) " explant has arrived from thermotherapy."
                            else " explants have arrived from thermotherapy.",
                            " Receive it onto this bench, then culture it before cutting tips.")
        else order_theme$guide("Nothing new has arrived from thermotherapy."),
        culture = if (culture > 0)
          order_theme$guide(tone = "do",
                            "Culture each explant on, then excise meristem tips from it \u2014 the ",
                            "tips appear under the explant they came from. Grow each tip on, ",
                            "then review and approve it; approved tips go back to indexing to ",
                            "be tested clean.",
                            action = order_theme$goto("Virus Indexing", "vx"))
        else order_theme$guide("Nothing is in culture."),
        requests = local({
          a <- nrow(to_authorize()); e <- nrow(to_enter())
          if (a == 0 && e == 0)
            order_theme$guide("No bench has asked for a tip.")
          else order_theme$guide(tone = "do",
                                 if (a > 0) shiny$tagList(shiny$strong(a),
                                                          if (a == 1) " request to authorise. " else " requests to authorise. ") else NULL,
                                 if (e > 0) shiny$tagList(shiny$strong(e),
                                                          if (e == 1) " authorised request waiting for its sample to be entered."
                                                          else " authorised requests waiting for their samples to be entered.") else NULL)
        }),
        order_theme$guide(
          "Every sample on the culture bench, grouped by the source it came from. ",
          if (excised > 0) shiny$tagList(shiny$strong(excised),
                                         if (excised == 1) " tip excised so far." else " tips excised so far.") else NULL)
      )
    })
    
    output$tbl <- renderReactable({
      d <- rows()
      d$since_fmt <- if (nrow(d)) ifelse(is.na(d$since), "\u2014",
                                         format(as.Date(d$since), "%d %b %y")) else character(0)
      
      # Locate the EXPLANT within a group. Looking for a parentless row stopped
      # working once material began arriving as a draw from quarantine stock:
      # every row has a parent now, so it found nothing and fell back to
      # rows[0] - whichever tip happened to sort first.
      find_src <- "var s = rows.find(function(r){ return r['role'] === 'explant'; }) || rows[0]; "
      
      # Order and customer, crop and variety are IDENTICAL for every row inside
      # a group. Printed on each row they were most of the ink on the table and
      # none of the information: what actually differs between two rows is the
      # sample code, what it is, and where it has got to.
      #
      # So each value is shown ONCE, at the level it is constant across:
      #   order group   -> customer
      #   lineage group -> crop and variety, and the explant's own status
      #   leaf row      -> sample, role, status, tips, since
      # `cell` blanks the leaf; `aggregate` fills the group row above it.
      blank <- function(v, i) ""
      
      reactable(
        d,
        groupBy = c("order_number", "group_key"),
        onClick = JS(sprintf("function(rowInfo){ if (rowInfo.subRows && rowInfo.subRows.length) { return; } Shiny.setInputValue('%s', {code: rowInfo.row['sample_code'], n: Math.random()}); }", ns("pick"))),
        rowStyle = JS("function(rowInfo){ return rowInfo.subRows && rowInfo.subRows.length ? {} : {cursor:'pointer'}; }"),
        rowClass = JS(sprintf("function(rowInfo){ return (!rowInfo.subRows || !rowInfo.subRows.length) && rowInfo.row['sample_code'] === '%s' ? 'wl-selected' : null; }",
                              selected() %||% "")),
        columns = order_theme$rt_cols(list(
          quantity = colDef(show = FALSE), state_code = colDef(show = FALSE),
          since = colDef(show = FALSE), parent_sample_code = colDef(show = FALSE),
          # folded into CROP on the lineage header rather than given a column
          # of its own, where it repeated on every row
          variety_name = colDef(show = FALSE),
          
          order_number = colDef(name = "ORDER", minWidth = 150),
          
          group_key = colDef(name = "LINEAGE", minWidth = 135,
                             cell = function(v) shiny$tags$strong(v)),
          
          # ---- leaf rows: what actually differs -------------------------
          sample_code = colDef(name = "SAMPLE", width = 110,
                               cell = function(v) shiny$tags$strong(v),
                               aggregate = JS("function(){ return ''; }")),
          
          role = colDef(name = "ROLE", width = 95, cell = function(v, i) {
            if (identical(v, "tip")) return(order_theme$chip("Tip", "ink"))
            if (identical(d$state_code[i], "received")) order_theme$chip("Incoming", "amber")
            else order_theme$chip("Explant", "brand")
          }, aggregate = JS("function(){ return ''; }")),
          
          state_label = colDef(name = "STATUS", minWidth = 150, cell = function(v, i) {
            st <- d$state_code[i]; is_tip <- identical(d$role[i], "tip")
            info <- if (!is_tip) switch(st,
                                        received    = list("Receive it", "amber"),
                                        established = list("Ready to culture", "brand"),
                                        updated     = list("In culture", "brand"),
                                        completed   = list("Tips taken", "ink"),
                                        list(d$state_label[i], "ink"))
            else switch(st,
                        established = list("Culturing", "brand"),
                        updated     = list("Culturing \u00b7 checkpoint", "brand"),
                        completed   = list("Awaiting review", "amber"),
                        approved    = list("Approved", "teal"),
                        rejected    = list("Rejected", "amber"),
                        list(d$state_label[i], "ink"))
            order_theme$chip(info[[1]], info[[2]])
          },
          # the lineage header carries the EXPLANT's status, so a collapsed
          # group still says what the operator has to do next
          aggregate = JS(paste0("function(values, rows){ ", find_src,
                                "if(!s) return '';",
                                "var st = s['state_code'];",
                                "if(st === 'received') return 'Receive it';",
                                "if(st === 'established') return 'Ready to culture';",
                                "if(st === 'updated') return 'In culture';",
                                "if(st === 'completed') return 'Tips taken';",
                                "return s['state_label'] || ''; }"))),
          
          n_children = colDef(name = "TIPS", width = 70, cell = function(v, i) {
            # role, not parentage. A drawn explant HAS a parent - the quarantine
            # source - so a parentage test returned the em-dash here instead of
            # the count, and excising appeared to do nothing.
            if (!identical(d$role[i], "explant")) return("")
            if (is.na(v) || v == 0) return(order_theme$chip("0", "ink"))
            order_theme$chip(as.character(v), "teal")
          },
          aggregate = JS(paste0("function(values, rows){ ", find_src,
                                "return s ? s['n_children'] : ''; }"))),
          
          since_fmt = colDef(name = "SINCE", width = 90,
                             aggregate = JS(paste0("function(values, rows){ ", find_src,
                                                   "return s ? s['since_fmt'] : ''; }"))),
          
          # ---- group rows only: constant within the group ---------------
          customer_name = colDef(name = "CUSTOMER", minWidth = 130, cell = blank,
                                 aggregate = JS("function(values){ return values[0] || ''; }")),
          
          crop_name = colDef(name = "CROP", minWidth = 130, cell = blank,
                             aggregate = JS(paste0(
                               "function(values, rows){ var r = rows[0]; if(!r) return '';",
                               "var c = r['crop_name'] || '';",
                               "var v = r['variety_name'];",
                               "return v ? c + ' \u00b7 ' + v : c; }")))
        ), d),
        defaultExpanded = TRUE,
        defaultPageSize = 20, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "No samples in meristem culture. Cleaned or heat-treated explants arrive here."),
        theme = order_theme$rt_theme())
    })
    
    # ---- selection + scan --------------------------------------------
    selected <- shiny$reactiveVal(NULL)
    
    shiny$observeEvent(input$pick, { selected(input$pick$code); just_cut(NULL) })
    shiny$observeEvent(input$detail_close, { selected(NULL); just_cut(NULL) })
    
    shiny$observeEvent(input$scan_go, {
      code <- trimws(input$scan %||% "")
      if (!nzchar(code)) return()
      id <- scan_identity(code)
      if (nrow(id) == 0) { toastr_warning(sprintf("No sample found for %s.", code), title = "Not found"); return() }
      if (!(code %in% queue()$sample_code)) {
        toastr_warning(sprintf("%s is not on the meristem bench (currently %s).",
                               code, id$stage_label[1] %||% id$kind[1]), title = "Not on bench"); return()
      }
      selected(code)
      toastr_success(sprintf("Showing %s.", code))
      runjs(sprintf("var el=document.getElementById('%s'); if(el) el.value='';", ns("scan")))
    })
    
    sel_row <- shiny$reactive({
      sc <- selected(); if (is.null(sc)) return(NULL)
      d <- queue(); r <- d[d$sample_code == sc, , drop = FALSE]
      if (nrow(r) == 1) r else NULL
    })
    
    # ---- detail panel ------------------------------------------------
    output$detail <- shiny$renderUI({
      # Confirmation first: the explant it would look up has just been completed
      # and left this tab, so a record lookup would return nothing and the pane
      # would blank at the moment the new codes matter most.
      jc <- just_cut()
      if (!is.null(jc)) {
        return(shiny$div(
          class = "wl-detail-inner",
          order_theme$detail_head(
            sprintf("%d tip%s excised", length(jc$codes),
                    if (length(jc$codes) == 1) "" else "s"),
            sub = paste("from", jc$source),
            close_input = ns("detail_close")),
          order_theme$guide(tone = "done",
                            "Each tip is a new sample under ", shiny$strong("Meristem tips"),
                            ". ", shiny$strong(jc$source), " is complete \u2014 its role ends here."),
          shiny$div(class = "code-list",
                    lapply(jc$codes, function(cd) shiny$span(class = "code-chip", cd))),
          order_theme$detail_actions(
            label_print$ui(ns("print")),
            shiny$actionButton(ns("see_tips"), "Show the tips",
                               class = "btn btn-sm btn-outline-secondary"),
            shiny$actionButton(ns("cut_next"), "Back to the bench",
                               class = "btn btn-sm btn-outline-secondary"))
        ))
      }
      
      r <- sel_row()
      if (is.null(r)) {
        return(shiny$div(class = "wl-detail-inner",
                         shiny$div(class = "wl-empty",
                                   shiny$div(class = "wl-empty-ico", shiny$icon("hand-pointer")),
                                   shiny$div(class = "wl-empty-title",
                                             "Select a sample to begin"),
                                   shiny$div(class = "wl-empty-body",
                                             "Click a source explant to excise meristem tips, or a tip to record ",
                                             "and review its culture.", shiny$br(),
                                             "You can also scan a barcode to jump straight to a sample."))))
      }
      sc <- r$sample_code[1]; st <- r$state_code[1]
      # role, not parentage - see meristem_queue(). Getting this wrong is what
      # showed a freshly received explant a tip's "Update" action instead of
      # culture and excision.
      is_tip <- identical(r$role[1], "tip")
      
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = sc,
                  sub = sprintf("%s \u00b7 %s", r$order_number[1], r$customer_name[1] %||% ""),
                  close_input = ns("detail_close")),
                
                shiny$div(class = "wl-statusbar",
                          if (is_tip) order_theme$chip(paste("from", r$parent_sample_code[1]), "brand")
                          else order_theme$chip("Source explant", "amber"),
                          order_theme$chip(r$state_label[1], switch(st,
                                                                    received = "amber", updated = "amber", completed = "brand", rejected = "amber", "ink"))),
                
                # ---- RECEIVED (pushed from thermotherapy): receive before excising ----
                if (identical(st, "received")) shiny$tagList(
                  order_theme$subhead("Receive into meristem culture"),
                  shiny$div(class = "flow-cta warn",
                            shiny$span(class = "fc-ico", shiny$icon("inbox")),
                            shiny$span("This sample was pushed from thermotherapy and is awaiting ",
                                       "receipt. Receiving it establishes the culture, ready to excise.")),
                  shiny$textAreaInput(ns("receive_notes"), "Notes", width = "100%"),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("receive"), "Receive sample", class = "btn btn-success",
                                               icon = shiny$icon("inbox"))))
                
                # ---- SOURCE (established): excise ----
                else if (!is_tip) shiny$tagList(
                  # TWO acts on an explant, in order: culture it on, then cut
                  # tips from it. Only excision was offered before, so an
                  # explant that had just arrived and was not yet ready to cut
                  # had nothing to record against it.
                  order_theme$subhead("1. Culture the explant"),
                  shiny$div(class = "update-hint",
                            "Record a checkpoint while the explant grows on. Do this as often as ",
                            "needed - it does not end the explant's life on this bench."),
                  shiny$textAreaInput(ns("culture_notes"), "Culture notes", width = "100%"),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("culture"), "Record culture checkpoint",
                                               class = "btn btn-outline-success",
                                               icon = shiny$icon("seedling"))),
                  
                  order_theme$subhead("2. Excise meristem tips"),
                  shiny$div(class = "update-hint",
                            "Cut apical meristem tips from this explant. Each tip becomes a NEW ",
                            "sample with its own code, listed under Meristem tips. Excising ",
                            "completes the explant - its role ends once its tips are taken."),
                  if (!is.na(r$n_children[1]) && r$n_children[1] > 0)
                    shiny$div(class = "wl-note",
                              sprintf("%d tip(s) already taken from this explant.", r$n_children[1])) else NULL,
                  shiny$numericInput(ns("n_tips"),
                                     shiny$HTML("Number of tips to excise <span class='mandatory_star'>*</span>"),
                                     value = 1, min = 1, max = 200),
                  shiny$numericInput(ns("qty_each"), "Units per tip", value = 1, min = 1),
                  shiny$dateInput(ns("excised_on"), "Date excised", value = Sys.Date()),
                  shiny$textAreaInput(ns("notes"), "Notes", width = "100%"),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("excise"), "Excise tips", class = "btn btn-success")))
                
                # ---- TIP established: update ----
                else if (identical(st, "established")) shiny$tagList(
                  order_theme$subhead("Update"),
                  shiny$div(class = "update-hint",
                            "Record a culture checkpoint. Moves the tip to \u2018updated\u2019."),
                  shiny$textAreaInput(ns("notes"), "Notes", width = "100%"),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("update"), "Mark updated", class = "btn btn-success")))
                
                # ---- TIP updated: review & approve (admin only) ----
                # Approval comes BEFORE completion. cassava.yaml clears
                # meristem_culture/completed to re-indexing, so completing a
                # tip is what releases it - and nothing should be releasable
                # that a reviewer has not already passed.
                else if (identical(st, "updated")) {
                  admin <- is_admin()
                  req <- if (!admin) last_approval_request(sc) else NULL
                  shiny$tagList(
                    order_theme$subhead("Review & approve"),
                    shiny$div(class = "update-hint",
                              "An administrator reviews the cultured tip. Approving clears it ",
                              "to be completed; a contaminated or failed tip is rejected instead."),
                    if (!admin) shiny$tagList(
                      shiny$div(class = "flow-cta warn",
                                shiny$span(class = "fc-ico", shiny$icon("user-shield")),
                                shiny$span("Only an administrator can approve. Send a reminder so one reviews it.")),
                      if (!is.null(req) && nrow(req) > 0)
                        shiny$div(class = "wl-meta-note",
                                  sprintf("Last requested by %s on %s.", req$actor[1],
                                          format(as.POSIXct(req$requested_on[1]), "%d %b %Y %H:%M")))
                      else NULL,
                      shiny$div(class = "wl-actions",
                                shiny$actionButton(ns("request_approval"), "Send approval reminder",
                                                   class = "btn btn-primary", icon = shiny$icon("envelope"))))
                    else shiny$tagList(
                      shiny$textAreaInput(ns("approve_notes"), "Review comments", width = "100%"),
                      shiny$div(class = "wl-actions",
                                shiny$actionButton(ns("reject"), "Reject", class = "btn btn-outline-secondary"),
                                shiny$actionButton(ns("approve"), "Approve", class = "btn btn-success"))))
                }
                
                # ---- TIP approved: complete the culture ----
                else if (identical(st, "approved")) shiny$tagList(
                  order_theme$subhead("Complete the culture"),
                  shiny$div(class = "update-hint",
                            "Approved. Completing the culture ends the tip's growing period and ",
                            "makes the cleaned tissue available to virus indexing, which requests ",
                            "it for the pathogens that came back positive."),
                  shiny$textAreaInput(ns("notes"), "Notes", width = "100%"),
                  shiny$div(class = "wl-actions",
                            shiny$actionButton(ns("finish"), "Complete culture", class = "btn btn-success"),
                            shiny$actionButton(ns("reject"), "Reject", class = "btn btn-outline-secondary")))
                
                # ---- TIP completed: standing stock, awaiting a request ----
                else if (identical(st, "completed")) shiny$tagList(
                  order_theme$subhead("Available for re-indexing"),
                  # No push button. The tip STAYS here as stock - that is what
                  # makes this a holding stage - and virus indexing requests a
                  # draw from it. Pushing moved the tip onto the indexing bench,
                  # which took it out of `meristem_culture` and so removed it
                  # from the very queue indexing pulls from: the tip vanished
                  # instead of arriving.
                  shiny$div(class = "flow-cta ok",
                            shiny$span(class = "fc-ico", shiny$icon("circle-check")),
                            shiny$span("Complete. This tip is standing stock: virus indexing asks ",
                                       "for it, and a technician on this bench chooses which tip ",
                                       "to send.")))
                
                else if (identical(st, "rejected")) shiny$div(class = "update-hint",
                                                              "This tip was rejected and will not proceed.")
                
                else shiny$div(class = "update-hint", "This tip has left meristem culture.")
      )
    })
    
    active <- function() { r <- sel_row(); if (is.null(r)) NULL else r }
    
    # ---- receive: meristem_culture/received -> established -----------
    shiny$observeEvent(input$receive, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "meristem_culture", "established", user(),
                       wf = wf, ctx = ctx,
                       notes = if (nzchar(input$receive_notes %||% "")) input$receive_notes
                       else "received from thermotherapy")
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'meristem_culture', $2, $3, $4)",
                    params = list(r$order_number[1], sprintf("received %s", sc), user(),
                                  "established in meristem culture"))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Receive failed", timeOut = 0); FALSE })
      if (ok) { toastr_success(sprintf("%s received \u2014 ready to excise.", sc)); self_refresh() }
    })
    
    # ---- culture an explant: a checkpoint that does NOT end its life ----
    shiny$observeEvent(input$culture, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      if (advance(sc, "updated",
                  if (nzchar(input$culture_notes %||% "")) input$culture_notes
                  else "culture checkpoint"))
        toastr_success(sprintf("Culture checkpoint recorded for %s.", sc))
    })
    
    # ================================================================
    # EXCISE · derive child tips, complete the source (one transaction)
    # ================================================================
    shiny$observeEvent(input$excise, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      n <- input$n_tips; q <- input$qty_each
      if (is.null(n) || is.na(n) || n < 1) { toastr_error("Number of tips must be at least 1.", title = "Invalid"); return() }
      if (n > 200) { toastr_error("200 tips is the per-batch limit. Split it.", title = "Too many"); return() }
      if (is.null(q) || is.na(q) || q < 1) { toastr_error("Units per tip must be at least 1.", title = "Invalid"); return() }
      created <- character(0)
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          on <- r$order_number[1]
          for (i in seq_len(as.integer(input$n_tips))) {
            child <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                                params = list(CHILD_PREFIX))$code[1]
            # child: parent = the source explant, born at meristem_culture/established,
            # order_service_id NULL (shared upstream stock, allocated later).
            dbExecute(conn, "
              INSERT INTO tbl_sample
                (sample_code, order_number, parent_sample_code, stage_code,
                 quantity, created_by, created_on)
              VALUES ($1, $2, $3, 'meristem_culture', $4, $5, $6)",
                      params = list(child, on, sc, as.integer(input$qty_each),
                                    user(), as.character(input$excised_on)))
            # birth event, written directly (not a transition). The composite
            # FK still enforces (meristem_culture, established) is legal.
            dbExecute(conn, "
              INSERT INTO tbl_sample_event
                (sample_code, stage_code, state_code, actor, occurred_on, notes)
              VALUES ($1, 'meristem_culture', 'established', $2, $3, $4)",
                      params = list(child, user(), as.character(input$excised_on),
                                    if (nzchar(input$notes %||% "")) input$notes else sprintf("excised from %s", sc)))
            # `<<-`, not `<-`. poolWithTransaction() runs this body in its own
            # function, so a plain assignment writes to a LOCAL copy and the
            # outer `created` stays empty. Reads INSIDE the transaction saw the
            # local one and looked right; every read AFTER it got character(0).
            created <<- c(created, child)
          }
          # the source's role ends: complete it in the same transaction
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "meristem_culture", "completed", user(),
                       wf = wf, ctx = ctx,
                       notes = sprintf("%d tip%s excised", length(created),
                                       if (length(created) == 1) "" else "s"))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'meristem_culture', $2, $3, $4)",
                    params = list(on, sprintf("excised %d from %s", length(created), sc), user(),
                                  sprintf("tips %s to %s", created[1], created[length(created)])))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Excision failed", timeOut = 0); FALSE })
      if (ok && length(created) == 0) {
        # Belt and braces. The transaction reported success but produced no
        # codes, which means something upstream changed shape rather than
        # errored. data.frame(code = character(0), title = "...") raises
        # "arguments imply differing number of rows: 0, 1" - a confusing crash
        # a long way from the cause - so say what actually happened instead.
        toastr_error(paste("The transaction succeeded but returned no tip codes.",
                           "Nothing has been labelled; check the bench before",
                           "cutting again."),
                     title = "No codes returned", timeOut = 0)
        self_refresh()
      } else if (ok) {
        # Queue the labels; do NOT send them. The operator may be cutting a
        # batch away from the printer, or want to check the codes first.
        printer$queue(data.frame(
          code  = created,
          title = "MERISTEM TIP",
          line1 = sprintf("from %s", sc),
          line2 = format(as.Date(input$excised_on), "%d %b %Y"),
          stringsAsFactors = FALSE))
        just_cut(list(codes = created, source = sc, order = r$order_number[1]))
        toastr_success(sprintf("%d tip%s excised from %s.", length(created),
                               if (length(created) == 1) "" else "s", sc),
                       title = "Tips created")
        selected(NULL); self_refresh()
      }
    })
    
    shiny$observeEvent(input$see_tips, {
      just_cut(NULL)
      # Click the real tab so the input and the stepper highlight move together.
      # Setting the input alone leaves the stepper on the old step, because it
      # reads the tab with isolate().
      runjs(sprintf(
        "var s=document.querySelector('#%s .flow-step[data-value=\"tips\"]'); if(s) s.click();",
        ns("kpis")))
    })
    
    shiny$observeEvent(input$cut_next, { just_cut(NULL) })
    
    # ---- update a tip ------------------------------------------------
    shiny$observeEvent(input$update, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      if (advance(sc, "updated",
                  if (nzchar(input$notes %||% "")) input$notes else NA))
        toastr_success(sprintf("%s marked updated.", sc))
    })
    
    # ---- complete the culture: approved -> completed ------------------
    # This is the RELEASE. cassava.yaml routes meristem_culture/completed to
    # re-indexing, so a completed tip is available stock. Approval happens
    # first, at `updated`, so nothing is released unreviewed.
    shiny$observeEvent(input$finish, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      if (advance(sc, "completed", if (nzchar(input$notes %||% "")) input$notes else NA,
                  order_note = "culture completed; available for re-indexing"))
        toastr_success(sprintf("%s complete \u2014 available for re-indexing.", sc))
    })
    
    # ---- review gate: updated -> approved (+ tbl_review) -------------
    # ---- request approval: a technician asks an admin to review -------
    shiny$observeEvent(input$request_approval, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'approval', 'approval requested', $2, $3)",
                    params = list(r$order_number[1], user(),
                                  sprintf("%s awaiting meristem culture approval", sc)))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Request failed", timeOut = 0); FALSE })
      if (ok) {
        res <- notify_approvers(sample_code = sc, stage = "meristem_culture",
                                order_number = r$order_number[1], requested_by = user())
        plural <- if (identical(res$n, 1L)) "" else "s"
        if (isTRUE(res$delivered)) {
          toastr_success(sprintf("Approval reminder emailed to %d administrator%s.", res$n, plural))
        } else if (nzchar(res$mailto %||% "")) {
          # no server transport: open the technician's own mail client, already
          # addressed and filled in. The request is recorded either way.
          runjs(sprintf("window.location.href = '%s';", res$mailto))
          toastr_success(sprintf("Approval requested \u2014 opening an email to %d administrator%s.",
                                 res$n, plural))
        } else {
          toastr_warning(paste("Approval requested, but no administrator has an email",
                               "address on file."), title = "Recorded, not sent")
        }
        self_refresh()
      }
    })
    
    shiny$observeEvent(input$approve, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      if (!is_admin()) {
        toastr_error("Only an administrator can approve.", title = "Not permitted"); return()
      }
      note <- if (nzchar(input$approve_notes %||% "")) input$approve_notes else NA
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_review (sample_code, stage_code, decision, comments, reviewed_by)
            VALUES ($1, 'meristem_culture', 'approved', $2, $3)",
                    params = list(sc, note, user()))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "meristem_culture", "approved", user(),
                       wf = wf, ctx = ctx, notes = "review: approved")
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Approve failed", timeOut = 0); FALSE })
      if (ok) {
        toastr_success(sprintf("%s approved \u2014 complete the culture to release it.", sc))
        self_refresh()
      }
    })
    
    # reject fires from review (updated) and from completion (approved)
    shiny$observeEvent(input$reject, {
      r <- active(); if (is.null(r)) return(); sc <- r$sample_code[1]
      note <- if (nzchar(input$approve_notes %||% "")) input$approve_notes
      else if (nzchar(input$notes %||% "")) input$notes else NA
      if (advance(sc, "rejected", note, order_note = "rejected"))
        toastr_success(sprintf("%s rejected.", sc))
    })
    
    # ---- push: approved -> virus indexing (crop-routed) --------------
    
    # shared: advance a sample's state, with an optional order-event note
    advance <- function(sc, to_state, note, order_note = NULL) {
      r <- active()
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, "meristem_culture", to_state, user(), wf = wf, ctx = ctx, notes = note)
          if (!is.null(order_note) && !is.null(r)) {
            dbExecute(conn, "
              INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
              VALUES ($1, 'meristem_culture', $2, $3, $4)",
                      params = list(r$order_number[1], sprintf("%s %s", to_state, sc), user(), order_note))
          }
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Failed", timeOut = 0); FALSE })
      if (ok) self_refresh()
      ok
    }
    
    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
