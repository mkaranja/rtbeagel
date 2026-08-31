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
  app/logic/fct_tracking[index_queue, indexing_tests, tests_for_sample,
                         result_recorders, scan_identity, indexing_material],
  app/logic/fct_workflows[workflow_cache, next_options, sample_context, record_event],
  app/logic/fct_file_upload[file_upload],
  app/view/shared/label_print,
  app/view/shared/order_theme,
)

# ============================================================================
# VIRUS INDEXING · clinical worklist (worklist + right detail panel)
# ----------------------------------------------------------------------------
# Modelled on a hospital LIS worklist: a persistent list of explants on the
# left, and a detail panel on the right that opens when you select one - both
# visible, so the technician keeps the bench in view while working a sample.
# No modals, no nested tables.
#
# The detail panel shows the selected explant's test-samples (one per test) and
# records each result + evidence file INLINE (a real Shiny panel, so the file
# input works). The explant advances through the review state machine:
#   inprogress -> results_available -> approved/rejected -> completed
# with segregation of duty at approval.
#
# PERFORMANCE
#   Data reactives depend on a LOCAL refresh counter plus a tab-activation
#   signal - NOT the global trigger_refresh. A write here refreshes only this
#   module; other modules re-read when the user navigates to them. This removes
#   the app-wide re-query storm that made every write invalidate every module.
# ============================================================================

MY_TAB    <- "virus_index"
WF_PATH   <- file.path("app", "static", "workflows", "cassava.yaml")
VT_PREFIX <- "VT"
OUTCOMES  <- c("Positive" = "positive", "Negative" = "negative", "Inconclusive" = "inconclusive")

outcome_tone <- function(o) switch(o %||% "",
                                   positive = "amber", negative = "brand", inconclusive = "teal", "ink")

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    useShinyjs(),
    
    # Title + one line. The stepper says where you are; guide() says what to
    # do. Nothing else on this screen explains itself.
    order_theme$page_header(
      title = "Virus Indexing",
      sub   = "Record test results for explants on the indexing bench."
    ),
    
    shiny$uiOutput(ns("kpis")),    # the stepper: where you are, and the tabs
    shiny$uiOutput(ns("guide")),   # the one instruction, changes with the tab
    
    order_theme$toolbar(
      order_theme$search_box(ns("q"), "Search sample, order, customer..."),
      order_theme$scan_box(ns("scan"), ns("scan_go"),
                           placeholder = "Scan explant or test id..."),
      # The bench dropdown is gone. It filtered `stage_code`, which used to be
      # the indexing bench but is now where the MATERIAL sits - quarantine for
      # anything not yet drawn. Choosing "Molecular" therefore hid every row
      # whose material is still in quarantine, which is most of the awaiting
      # queue. The bench is shown as a column instead, and the tabs carry the
      # dimension that actually matters here.
      NULL
    ),
    
    order_theme$workbench(
      list_ui   = shiny$tagList(
        # The main worklist FIRST. It was second, under the cleaned-tips table,
        # so on the awaiting tab the operator met an empty tip list and stopped
        # reading - the queue they wanted was below the fold.
        order_theme$table_card(reactableOutput(ns("tbl"))),
        # Cleaned meristem tips, on the awaiting tab only. A tip is not more
        # material of the same kind: it is the tissue the thermotherapy and
        # meristem sequence existed to produce, and the only test worth running
        # on it is the one that came back positive on the material it was
        # cleaned from. That is a different question from the quarantine queue
        # above, so it gets its own list.
        #
        # Rendered server-side rather than wrapped in a conditionalPanel:
        # flow_stepper sets no initial value, so `vstate` is NULL until the
        # operator clicks a tab, and a client-side condition comparing it to
        # 'awaiting' would be false on the tab that loads by default.
        shiny$uiOutput(ns("mer_card"))
      ),
      detail_ui = shiny$uiOutput(ns("detail"))
    )
  )
}



#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Label printer. One instance per module; the returned controller is called
    # from wherever a code is minted, not from the UI.
    # The printer is back, for material this bench holds. A test sample cut
    # HERE gets its label HERE. One drawn by quarantine or meristem is labelled
    # there, next to the tube - which is why this is not unconditional.
    printer <- label_print$server("print", module_name = "virus_indexing",
                                  user = user)
    
    # Columns the detail panel needs. Declared, because the panel is fed by
    # indexing_material() and the two have drifted apart twice now - each time
    # surfacing as "argument is of length zero" from inside htmltools, with a
    # line number pointing at a div and no hint which column was missing.
    #
    # A missing column makes r$x NULL, so r$x[1] is NULL, and `if (NULL == 0)`
    # is logical(0) - which is what that error actually means.
    DETAIL_COLS <- c("acronym", "bench", "customer_name", "init_status",
                     "n_resulted", "n_tests", "order_number", "pathogen_name",
                     "sample_code", "source_code", "stage_code", "state_code",
                     "state_label", "test_name", "test_stage")
    
    # Read one field with a fallback. Never returns length zero, so no `if()`
    # downstream can be handed an empty condition.
    fld <- function(r, name, default = NA) {
      if (is.null(r) || is.null(r[[name]]) || length(r[[name]]) == 0) return(default)
      v <- r[[name]][1]
      if (length(v) == 0) default else v
    }
    
    user <- shiny$reactive(shiny$reactiveValuesToList(res_auth)$user)
    
    # ---- scoped refresh: local bump + re-query on tab activation -----
    refresh <- shiny$reactiveVal(0)
    self_refresh <- function() refresh(shiny$isolate(refresh()) + 1)
    if (!is.null(tab) && is.function(tab)) {
      shiny$observeEvent(tab(), {
        if (identical(tab(), MY_TAB)) self_refresh()
      }, ignoreInit = TRUE)
    }
    # cross-module: when THIS module changes something other benches show,
    # signal the global trigger once (does not re-query us). Kept minimal.
    signal_others <- function() {
      if (!is.null(trigger_refresh)) trigger_refresh(shiny$isolate(trigger_refresh()) + 1)
    }
    
    # ---- data (depend only on local refresh) -------------------------
    # The worklist is now (available material x required test), not a list of
    # explants somebody already moved here. Quarantine is a holding stage: it
    # approves material and keeps it. This bench decides what to cut and when,
    # one sample per test, and a test nobody has started yet is a ROW reading
    # "awaiting initiation" rather than a row that does not exist.
    
    
    
    queue <- shiny$reactive({
      refresh()
      d <- indexing_material()
      d
    })
    tests_all <- shiny$reactive({ refresh(); indexing_tests() })
    
    rows <- shiny$reactive({
      d <- queue()
      # The action column has to EXIST before a colDef can name it - reactable
      # rejects the whole table otherwise.
      if (nrow(d) > 0 || !is.null(d$row_id)) {
        d$act <- rep(NA_character_, nrow(d))
        d$ord_test <- rep(NA_character_, nrow(d))
      }
      # These arms MUST stay identical to the counts in output$kpis; if they
      # diverge a tab reads 7 and the table shows something else.
      f <- input$vstate %||% "awaiting"
      if (nrow(d) > 0) {
        d <- switch(f,
                    awaiting  = d[d$init_status == "awaiting_initiation", , drop = FALSE],
                    # A REJECTED test belongs here, not in a tab of its own.
                    # The reviewer sent it back because the result needs
                    # redoing, and this is the tab where results are entered -
                    # a separate "rejected" tab would be a queue of work whose
                    # only action is to come back to this one.
                    recording = d[d$init_status %in% c("initiated", "rejected"), , drop = FALSE],
                    review    = d[d$init_status == "resulted", , drop = FALSE],
                    approved  = d[d$init_status == "approved", , drop = FALSE],
                    completed = d[d$init_status == "completed", , drop = FALSE],
                    # "All tests" = every test that has actually been started.
                    # A test nobody has requested yet has no sample, no result
                    # and no history - it belongs in Awaiting initiation, which
                    # is the queue for bringing it into existence, and listing
                    # it here made the tab a mix of work and intentions.
                    all = d[d$init_status != "awaiting_initiation", , drop = FALSE],
                    d)
      }
      q <- input$q
      if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
        cols <- intersect(c("source_code","test_sample_code","order_number",
                            "customer_name","crop_name","variety_name","acronym"), names(d))
        hay <- apply(d[, cols, drop = FALSE], 1, function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    # ---- the awaiting tab is TWO lists, split by lineage ----------------
    # is_mer marks material excised on the meristem bench, or drawn from
    # something that was. It is a property of the TISSUE, not of where the
    # tissue is standing: a tip already delivered here belongs with the other
    # tips, because the question it answers - did the cleaning work - is the
    # same one, and the action column already says whether to request it or
    # initiate it.
    #
    # Both frames come from the SAME queue() call. A second query would be a
    # second thing to keep in step with the stepper counts, and the two would
    # drift the first time either was touched.
    on_awaiting <- shiny$reactive(identical(input$vstate %||% "awaiting", "awaiting"))
    
    split_rows <- function(d, want_mer) {
      if (!is.data.frame(d) || nrow(d) == 0) return(d)
      if (!"is_mer" %in% names(d)) return(if (want_mer) d[0, , drop = FALSE] else d)
      keep <- as.integer(d$is_mer) == (if (want_mer) 1L else 0L)
      keep[is.na(keep)] <- !want_mer
      d[keep, , drop = FALSE]
    }
    
    # Off the awaiting tab there is one list again. The split exists to separate
    # two ways of GETTING material; once a test is running, which bench the
    # tissue came from stops deciding anything.
    rows_main <- shiny$reactive({
      if (!on_awaiting()) rows() else split_rows(rows(), FALSE)
    })
    rows_mer <- shiny$reactive({
      d <- rows()
      if (!on_awaiting()) d[0, , drop = FALSE] else split_rows(d, TRUE)
    })
    
    # The KPI row IS the tab bar, on `vstate` - a NEW input, not the existing
    # `stage` one. The two are orthogonal and both are needed: `vstate` is
    # WHERE IN THE WORK a sample is (recording / awaiting review / ready), while
    # `stage` is WHICH BENCH it sits on (molecular / grafting). Collapsing them
    # into one control would make "everything awaiting review" unaskable.
    # Quarantine already pairs a stepper with a per-panel dropdown the same way.
    #
    # Counts come from queue(), which already honours `stage`, so the numbers
    # track the bench selection. Each count mirrors an arm of rows()' filter
    # exactly - "Recording" is state inprogress AND n_tests > 0 in both places,
    # and the two must be changed together or the tile stops matching the table.
    #
    # isolate() on the active tab: the highlight moves client-side.
    # The tab bar, in the same flow-stepper language quarantine uses. Recording
    # -> awaiting review -> ready to complete IS the order the work happens in,
    # so those three carry ordinals. "On the bench" is a view across all three
    # rather than a position within them, so it is marked, not numbered.
    #
    # `vstate` is a NEW input, not the existing `stage` one. The two are
    # orthogonal and both are needed: vstate is WHERE IN THE WORK a sample is,
    # stage is WHICH BENCH it sits on (molecular / grafting). Collapsing them
    # would make "everything awaiting review, either bench" unaskable.
    #
    # Counts come from queue(), which already honours `stage`, so the numbers
    # track the bench selection. Each count mirrors an arm of rows()' filter
    # exactly - "Recording" is state inprogress AND n_tests > 0 in both places,
    # and the two must be changed together or the tab stops matching the table.
    #
    # isolate() on the active tab is deliberate: flow_pick_js moves the `on`
    # class in the browser, so this must NOT re-render on a tab change.
    # Awaiting initiation -> recording -> awaiting review -> ready to complete
    # is the order the work happens in, so all four carry ordinals. "All tests"
    # is a view across them and is marked.
    #
    # AWAITING INITIATION LEADS, and is the default. It is the tab that did not
    # previously exist: a test nobody has started was invisible, because the old
    # queue could only list samples that already existed.
    output$kpis <- shiny$renderUI({
      d <- queue()
      n <- function(s) if (nrow(d)) sum(d$init_status %in% s) else 0L
      await <- n("awaiting_initiation")
      rec   <- n(c("initiated", "rejected"))   # must match rows()' arm exactly
      rej   <- n("rejected")
      rev   <- n("resulted")
      appr  <- n("approved")
      done  <- n("completed")
      cur <- shiny$isolate(input$vstate) %||% "awaiting"
      order_theme$flow_stepper(
        list(
          list(title = "All tests", sub = "every test started", value = "all",
               num = "\u2211",
               count = if (nrow(d)) sum(d$init_status != "awaiting_initiation") else 0L,
               unit = "tests", active = identical(cur, "all")),
          # ONE tab for getting material. "Incoming" and "Awaiting initiation"
          # ONE queue for getting material, whatever bench holds it. Quarantine
          # stock, cleaned meristem tips and material already delivered here
          # all answer the same question - what still needs testing, and how do
          # I get it - so they are one list, distinguished by a column rather
          # than by which table the operator remembered to look in.
          list(title = "Awaiting initiation", sub = "request, then initiate",
               value = "awaiting", count = await, unit = "to start",
               active = identical(cur, "awaiting"), waiting = await > 0),
          # The value stays "recording" while the label changes. It is never
          # user-visible, and it is referenced in four places - rows()' switch,
          # here, the guide switch, and the runjs tab selector - so renaming it
          # is four chances to leave one behind for no visible gain.
          list(title = "Update results", sub = "enter the result", value = "recording",
               count = rec, unit = "to record",
               active = identical(cur, "recording"), waiting = rec > 0),
          list(title = "Awaiting review", sub = "result needs approval", value = "review",
               count = rev, unit = "to review",
               active = identical(cur, "review"), waiting = rev > 0),
          list(title = "Ready to complete", sub = "approved", value = "approved",
               count = appr, unit = "ready",
               active = identical(cur, "approved")),
          # Marked, not numbered. Finished tests are an ARCHIVE, not a step
          # with work in it - numbering it would put a queue that needs no
          # action at the end of the sequence and imply somebody must clear it.
          list(title = "Completed", sub = "indexing finished", value = "completed",
               num = "\u2713", count = done, unit = "done",
               active = identical(cur, "completed"))
        ),
        input_id = ns("vstate")
      )
    })
    
    
    # ---- worklist table (row click selects) --------------------------
    # THE instruction. One sentence, specific to the tab you are on, ending in
    # where the work goes next. This replaces the header hint and the table
    # note, both of which described the screen in general and so described no
    # particular moment.
    output$guide <- shiny$renderUI({
      d <- queue()
      n <- function(s) if (nrow(d)) sum(d$init_status %in% s) else 0L
      await <- n("awaiting_initiation")
      rej   <- n("rejected")
      switch(
        input$vstate %||% "awaiting",
        awaiting = if (await > 0) local({
          d0 <- queue()
          hh <- if (nrow(d0)) sum(d0$init_status == "awaiting_initiation" &
                                    as.integer(d0$held_here) == 1L) else 0L
          mm <- if (nrow(d0) && "is_mer" %in% names(d0))
            sum(d0$init_status == "awaiting_initiation" &
                  as.integer(d0$is_mer) == 1L, na.rm = TRUE) else 0L
          order_theme$guide(tone = "do",
                            shiny$strong(await),
                            if (await == 1) " test has not been started. " else " tests have not been started. ",
                            if (hh > 0) shiny$tagList(
                              shiny$strong(hh),
                              if (hh == 1) " is on material already delivered to this bench \u2014 "
                              else " are on material already delivered to this bench \u2014 ",
                              "initiate those directly. ") else NULL,
                            if (await - hh > 0) shiny$tagList(
                              "The rest are on material held by quarantine or meristem culture; ",
                              "request those and the holding bench draws them. ") else NULL,
                            if (mm > 0) shiny$tagList(
                              shiny$strong(mm),
                              if (mm == 1) " is a retest on a cleaned meristem tip"
                              else " are retests on cleaned meristem tips",
                              " \u2014 listed separately below the main queue.") else NULL)
        })
        else order_theme$guide(tone = "done", "Every required test has been started."),
        recording = if (rej > 0)
          order_theme$guide(tone = "do",
                            shiny$strong(rej),
                            if (rej == 1) " result was sent back" else " results were sent back",
                            " by the reviewer and need redoing. Correct the outcome and submit again.")
        else order_theme$guide(
          "Select a test sample, record its outcome, attach any evidence, then ",
          "submit it for review."),
        review = order_theme$guide(
          "Check the recorded result, then approve or reject it."),
        approved = order_theme$guide(tone = "done",
                                     "Approved. Completing these releases the material to the next bench.",
                                     action = order_theme$goto("Thermotherapy", "thermotherapy")),
        completed = order_theme$guide(tone = "done",
                                      "Indexing is finished for these tests. They stay here as a record; ",
                                      "nothing further is required."),
        order_theme$guide(
          "Every test required for material currently available in quarantine, ",
          "started or not.")
      )
    })
    
    output$tbl <- renderReactable({
      d <- rows_main()
      
      # The visible column set for this tab, computed ONCE.
      keep <- switch(
        input$vstate %||% "awaiting",
        # asked for, and whether it has arrived. NO material code: while it is
        # in quarantine there is no tube on this bench to name.
        awaiting  = c("ord_test", "acronym", "test_name", "bench",
                      "init_status", "act"),
        recording = c("test_sample_code", "acronym", "order_number",
                      "init_status", "bench"),
        review    = c("test_sample_code", "acronym", "last_status",
                      "order_number", "init_status"),
        approved  = c("test_sample_code", "acronym", "last_status",
                      "order_number", "init_status"),
        completed = c("test_sample_code", "acronym", "last_status",
                      "order_number", "customer_name"),
        c("source_code", "test_sample_code", "acronym", "test_name",
          "pathogen_name", "init_status", "last_status", "order_number",
          "customer_name", "crop_name", "bench", "act")
      )
      
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick"), "row_id")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(selected(), "row_id")),
        # Group ONLY where the grouping column is visible.
        #
        # This was an unconditional groupBy = "source_code", and rt_only hides
        # source_code on every tab but "all". Grouping by a hidden column gives
        # collapsed group rows with nothing rendered in them - the rows are all
        # there, behind headers that cannot be seen or clicked. The table reads
        # as empty while the counts, which never look at columns, stay correct.
        groupBy = if ("source_code" %in% keep) "source_code" else NULL,
        defaultExpanded = TRUE,
        # ONE table, six tabs. Each tab shows only what exists and matters at
        # that point: an awaiting row has no test sample, no result and no
        # bench of its own, and showing those columns sent operators looking
        # for things that are not there yet.
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          row_id = colDef(show = FALSE), test_id = colDef(show = FALSE),
          stage_code = colDef(show = FALSE), test_state = colDef(show = FALSE),
          # `last_status`, not `outcome`. The column was renamed in
          # indexing_material() to match what the detail panel reads, and this
          # colDef was left pointing at the old name - reactable rejects the
          # whole table when a declared column is absent, so one stale name
          # blanks the entire worklist.
          n_resulted = colDef(show = FALSE), last_status = colDef(show = FALSE),
          n_tests = colDef(show = FALSE), state_label = colDef(show = FALSE),
          variety_name = colDef(show = FALSE), since = colDef(show = FALSE),
          # A column with NO colDef is not hidden - it renders with defaults.
          # rt_only() can only set show = FALSE on a colDef that already
          # exists, so a data column absent from this list appears on every
          # tab regardless of `keep`. These four were doing exactly that.
          test_stage = colDef(show = FALSE), requested = colDef(show = FALSE),
          origin = colDef(show = FALSE), is_mer = colDef(show = FALSE),
          source_state = colDef(show = FALSE),
          source_state_label = colDef(show = FALSE),
          tips_available = colDef(show = FALSE),
          
          source_code = colDef(name = "MATERIAL", minWidth = 130,
                               cell = function(v, i) {
                                 if (identical(as.integer(d$held_here[i]), 1L))
                                   shiny$tags$strong(v)
                                 else shiny$span(class = "wl-meta-note",
                                                 "in quarantine")
                               }),
          # Shown only on the awaiting tab, where the material has no code on
          # this bench yet and the row IS an order and a test.
          ord_test = colDef(name = "ORDER", minWidth = 160,
                            cell = function(v, i) shiny$tags$strong(d$order_number[i])),
          acronym = colDef(name = "TEST", width = 100,
                           cell = function(v) shiny$tags$strong(v)),
          test_name = colDef(name = "METHOD", minWidth = 150),
          pathogen_name = colDef(name = "PATHOGEN", minWidth = 130,
                                 cell = function(v) if (is.na(v)) "\u2014" else v),
          
          # The status the whole rebuild exists to show. "Awaiting initiation"
          # is a real row here; under the old queue it had no row at all.
          init_status = colDef(name = "STATUS", width = 150, cell = function(v, i) {
            switch(v,
                   awaiting_initiation = if (identical(as.integer(d$held_here[i]), 1L))
                     order_theme$chip("Ready to initiate", "amber")
                   else order_theme$chip("Not initiated", "ink"),
                   initiated           = order_theme$chip("Initiated", "teal"),
                   resulted            = order_theme$chip("Result recorded", "brand"),
                   approved            = order_theme$chip("Approved", "brand"),
                   order_theme$chip(v, "ink"))
          }),
          test_sample_code = colDef(name = "TEST SAMPLE", width = 125,
                                    cell = function(v) if (is.na(v)) "\u2014" else v),
          order_number = colDef(name = "ORDER", minWidth = 150),
          customer_name = colDef(name = "CUSTOMER", minWidth = 130),
          crop_name = colDef(name = "CROP", width = 95,
                             cell = function(v) if (is.na(v)) "\u2014" else v),
          held_here = colDef(show = FALSE),
          act = colDef(name = "", width = 165, sortable = FALSE, cell = function(v, i) {
            if (!identical(d$init_status[i], "awaiting_initiation")) return("")
            if (identical(as.integer(d$held_here[i]), 1L))
              shiny$tags$button(class = "btn btn-success btn-sm", type = "button",
                                onclick = sprintf(
                                  "Shiny.setInputValue('%s', {code: '%s', n: Math.random()})",
                                  ns("row_act"), d$row_id[i]),
                                "Initiate")
            else if (as.integer(d$requested[i]) > 0)
              order_theme$chip("Requested", "amber")
            else
              shiny$tags$button(class = "btn btn-outline-success btn-sm", type = "button",
                                onclick = sprintf(
                                  "Shiny.setInputValue('%s', {code: '%s', n: Math.random()})",
                                  ns("row_act"), d$row_id[i]),
                                "Request sample")
          }),
          bench = colDef(name = "HELD IN", minWidth = 150, cell = function(v, i) {
            # Whose material this is, which decides what the operator can do.
            # Where the material is, which is what decides whether the row's
            # action is "request" or "initiate". The merged queue makes this
            # the column that carries the distinction the second table used to.
            switch(d$origin[i],
                   bench    = order_theme$chip("On this bench", "brand"),
                   meristem = order_theme$chip("Meristem tip", "teal"),
                   order_theme$chip(v, "ink"))
          })
        ), keep), d),
        defaultPageSize = 14, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "No material is available. Approve a consignment in quarantine first."),
        theme = order_theme$rt_theme())
    })
    
    # ---- cleaned meristem tips, awaiting tab only ----------------------
    # The card is always drawn on this tab, even with nothing in it. An empty
    # list that explains what will appear in it is how the operator learns the
    # route exists; a card that only appears once material arrives can only be
    # discovered by accident.
    output$mer_card <- shiny$renderUI({
      if (!on_awaiting()) return(NULL)
      n <- nrow(rows_mer())
      order_theme$table_card(
        order_theme$table_note(
          title = "Cleaned meristem tips. ",
          if (n > 0)
            shiny$tagList(
              shiny$strong(n),
              if (n == 1) " retest is outstanding on tissue that came through "
              else " retests are outstanding on tissue that came through ",
              "thermotherapy and meristem culture. One row per consignment and ",
              "test: only the tests that came back POSITIVE on the material a ",
              "tip was cleaned from are listed, since clearing those is what the ",
              "sequence was run for. Requesting asks meristem culture for the ",
              "consignment \u2014 a technician there chooses which tip to send.")
          else
            "Retests appear here once meristem culture completes a tip. Only the tests that came back positive on the material it was cleaned from are offered, since clearing those is what the sequence was run for."
        ),
        reactableOutput(ns("mer_tbl")))
    })
    
    output$mer_tbl <- renderReactable({
      d <- rows_mer()
      # An ORDER and a TEST, not a tip. One row per consignment per outstanding
      # retest: naming a tip here listed the same order and the same test once
      # for every tip standing on the meristem bench, which for one cassava
      # consignment was thirteen identical-looking rows.
      #
      # Which tip actually goes is not this bench's call. Indexing asks for the
      # consignment's retest; meristem culture picks the healthy tip. So the
      # useful number here is HOW MANY it has to choose from.
      keep <- c("order_number", "acronym", "test_name", "pathogen_name",
                "tips_available", "bench", "init_status", "act")
      reactable(
        d,
        onClick = JS(order_theme$rt_click_js(ns("pick"), "row_id")),
        rowStyle = JS(order_theme$rt_pointer_js()),
        rowClass = JS(order_theme$rt_selected_js(selected(), "row_id")),
        columns = order_theme$rt_cols(order_theme$rt_only(list(
          row_id = colDef(show = FALSE), test_id = colDef(show = FALSE),
          stage_code = colDef(show = FALSE), test_state = colDef(show = FALSE),
          test_stage = colDef(show = FALSE),
          n_resulted = colDef(show = FALSE), last_status = colDef(show = FALSE),
          n_tests = colDef(show = FALSE), state_label = colDef(show = FALSE),
          variety_name = colDef(show = FALSE), since = colDef(show = FALSE),
          held_here = colDef(show = FALSE), is_mer = colDef(show = FALSE),
          origin = colDef(show = FALSE), ord_test = colDef(show = FALSE),
          customer_name = colDef(show = FALSE), crop_name = colDef(show = FALSE),
          test_sample_code = colDef(show = FALSE),
          requested = colDef(show = FALSE), source_code = colDef(show = FALSE),
          source_state = colDef(show = FALSE),
          source_state_label = colDef(show = FALSE),
          
          order_number = colDef(name = "ORDER", minWidth = 160,
                                cell = function(v) shiny$tags$strong(v)),
          acronym = colDef(name = "RETEST", width = 100,
                           cell = function(v) shiny$tags$strong(v)),
          test_name = colDef(name = "METHOD", minWidth = 150),
          pathogen_name = colDef(name = "PATHOGEN", minWidth = 130,
                                 cell = function(v) if (is.na(v)) "\u2014" else v),
          tips_available = colDef(name = "TIPS", width = 110, cell = function(v) {
            n <- if (is.na(v)) 0L else as.integer(v)
            if (n == 0) order_theme$chip("none yet", "ink")
            else order_theme$chip(sprintf("%d ready", n), "teal")
          }),
          bench = colDef(name = "HELD IN", minWidth = 150, cell = function(v, i) {
            if (identical(as.integer(d$held_here[i]), 1L))
              order_theme$chip("On this bench", "brand")
            else order_theme$chip("Meristem culture", "teal")
          }),
          init_status = colDef(name = "STATUS", width = 150, cell = function(v, i) {
            if (identical(as.integer(d$held_here[i]), 1L))
              order_theme$chip("Ready to initiate", "amber")
            else order_theme$chip("Not initiated", "ink")
          }),
          act = colDef(name = "", width = 165, sortable = FALSE, cell = function(v, i) {
            if (identical(as.integer(d$held_here[i]), 1L))
              shiny$tags$button(class = "btn btn-success btn-sm", type = "button",
                                onclick = sprintf(
                                  "Shiny.setInputValue('%s', {code: '%s', n: Math.random()})",
                                  ns("row_act"), d$row_id[i]),
                                "Initiate")
            else if (as.integer(d$requested[i]) > 0)
              order_theme$chip("Requested", "amber")
            else if (is.na(d$tips_available[i]) || as.integer(d$tips_available[i]) == 0)
              order_theme$chip("No tip ready", "ink")
            else
              shiny$tags$button(class = "btn btn-outline-success btn-sm", type = "button",
                                onclick = sprintf(
                                  "Shiny.setInputValue('%s', {code: '%s', n: Math.random()})",
                                  ns("row_act"), d$row_id[i]),
                                "Request tip")
          })
        ), keep), d),
        defaultPageSize = 8, compact = TRUE, highlight = TRUE,
        language = order_theme$rt_lang(
          "No retests are outstanding. Meristem culture releases a tip when its culture is completed."),
        theme = order_theme$rt_theme())
    })
    
    # ---- selection ---------------------------------------------------
    selected   <- shiny$reactiveVal(NULL)   # the explant sample_code
    focus_test <- shiny$reactiveVal(NULL)   # a VT test-sample to highlight
    
    shiny$observeEvent(input$pick, {
      selected(input$pick$code)
      focus_test(NULL)
      just_done(NULL)
    })
    shiny$observeEvent(input$detail_close, {
      selected(NULL); focus_test(NULL); just_done(NULL)
    })
    
    # ---- sel_grid: the selected (material, test) row -------------------
    # `selected` now holds a row_id of the form "<source_code>:<test_id>",
    # because a row is a TEST on a material, not a sample. It is composite by
    # necessity: before initiation there is no sample_code to key on, which is
    # the whole point of the awaiting-initiation row.
    # Set the moment a test is initiated, cleared as soon as the operator moves
    # on. While it is set the detail pane shows the confirmation rather than a
    # record, because the record it would show has just left this tab.
    just_done <- shiny$reactiveVal(NULL)
    
    
    
    
    
    sel_grid <- shiny$reactive({
      id <- selected(); if (is.null(id)) return(NULL)
      d <- queue(); if (nrow(d) == 0) return(NULL)
      r <- d[d$row_id == id, , drop = FALSE]
      if (nrow(r) == 0) NULL else r
    })
    
    # Switch tab from the confirmation. Setting the input is enough: the
    # stepper reads it with isolate(), and the refresh that follows initiation
    # re-renders the stepper, so the highlight lands on Recording.
    shiny$observeEvent(input$go_recording, {
      just_done(NULL); selected(NULL)
      # CLICK the real tab rather than set the input directly.
      #
      # Setting input$vstate alone would filter the table but leave the stepper
      # highlighting "Awaiting initiation", because the stepper reads the tab
      # with isolate() and only re-renders when the QUEUE changes - which
      # already happened, back at initiation. Clicking the step runs the same
      # handler a human click would, so the input and the highlight move
      # together and cannot disagree.
      runjs(sprintf(
        "var s=document.querySelector('#%s .flow-step[data-value=\"recording\"]'); if(s) s.click();",
        ns("kpis")))
    })
    
    shiny$observeEvent(input$init_next, {
      just_done(NULL); selected(NULL)
    })
    
    
    # ---- initiate a test: cut here, or ask the bench that holds it -------
    # ONE button, two behaviours, decided by who owns the material:
    #
    #   held_here = 1  it is on this bench - cut the test sample now
    #   held_here = 0  it is standing in quarantine - ask for it
    #
    # That is the pull rule stated properly. It was never "indexing must not
    # cut"; it is "no bench cuts another bench's stock". Material delivered
    # here belongs to this bench.
    # Named, because two controls do this: the button in the detail panel and
    # the button on the row. Duplicating the body is how the two drift apart.
    do_initiate <- function(r) {
      if (is.null(r) || nrow(r) == 0) return(invisible(FALSE))
      if (!identical(fld(r, "init_status", ""), "awaiting_initiation")) {
        toastr_warning("That test already has a sample.", title = "Already started"); return()
      }
      here <- as.integer(fld(r, "held_here", 0L)) == 1L
      src_code <- fld(r, "source_code", ""); tid <- as.integer(r$test_id[1])
      held_at  <- fld(r, "stage_code", "")
      held_state <- fld(r, "source_state", "approved")
      
      if (!here) {
        # ---- not ours: request it ----
        if (as.integer(fld(r, "requested", 0L)) > 0) {
          toastr_warning("This material has already been requested for that test.",
                         title = "Already requested"); return()
        }
        ok <- tryCatch({
          poolWithTransaction(pool, function(conn) {
            dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                      params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
            on <- dbGetQuery(conn, "SELECT order_number FROM tbl_sample WHERE sample_code = $1",
                             params = list(src_code))
            if (nrow(on) == 0) stop("Source material not found: ", src_code, call. = FALSE)
            wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
            ctx <- sample_context(conn, src_code)
            bench <- NA_character_
            if (!is.null(wf)) {
              # The state the material is ACTUALLY in. Hardcoding 'approved'
              # was right for quarantine, whose stock only becomes available
              # once approved, but a meristem tip is offered at 'completed'
              # too - and cassava.yaml has different transitions for the two.
              opts <- tryCatch(next_options(wf, held_at, held_state, ctx),
                               error = function(e) NULL)
              if (!is.null(opts) && nrow(opts) > 0) {
                idx <- opts$to_stage[grepl("virus_indexing", opts$to_stage)]
                if (length(idx) > 0) bench <- idx[1]
              }
            }
            if (is.na(bench)) bench <- "molecular_virus_indexing"
            dbExecute(conn, "
              INSERT INTO tbl_sample_request
                (order_number, source_sample_code, to_stage, test_id, reason, requested_by)
              VALUES ($1,$2,$3,$4,$5,$6)",
                      params = list(on$order_number[1], src_code, bench, tid,
                                    sprintf("virus indexing: sample needed for %s",
                                            fld(r, "acronym", "")), user()))
          }); TRUE
        }, error = function(e) {
          toastr_error(conditionMessage(e), title = "Could not request", timeOut = 0); FALSE
        })
        if (ok) {
          holder <- if (identical(held_at, "meristem_culture")) "Meristem culture" else "Quarantine"
          just_done(list(code = NA_character_, acronym = fld(r, "acronym", ""),
                         test = fld(r, "test_name", ""), source = src_code,
                         order = fld(r, "order_number", ""), bench = holder))
          toastr_success(sprintf("%s requested from %s for %s.", src_code, holder,
                                 fld(r, "acronym", "")), title = "Requested")
          self_refresh()
          runjs(sprintf(
            "Shiny.setInputValue('rtb_goto', {tab: '%s', n: Math.random()}, {priority: 'event'})",
            if (identical(held_at, "meristem_culture")) "meristem" else "quarantine"))
        }
        return()
      }
      
      # ---- ours: cut the test sample here ----
      new_code <- NA_character_
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dup <- dbGetQuery(conn, "
            SELECT 1 FROM tbl_sample
            WHERE parent_sample_code = $1 AND test_id = $2 LIMIT 1",
                            params = list(src_code, tid))
          if (nrow(dup) > 0) stop("This test has already been initiated for ",
                                  src_code, call. = FALSE)
          on <- dbGetQuery(conn, "
            SELECT order_number, quantity FROM tbl_sample WHERE sample_code = $1",
                           params = list(src_code))
          if (nrow(on) == 0) stop("Material not found: ", src_code, call. = FALSE)
          # Same rule as every other draw: never take more units than are held.
          if (as.integer(on$quantity[1]) < 1L) {
            stop("No units are held for ", src_code, call. = FALSE)
          }
          vt <- dbGetQuery(conn, "SELECT next_sample_code($1) AS code",
                           params = list(VT_PREFIX))$code[1]
          dbExecute(conn, "
            INSERT INTO tbl_sample (sample_code, order_number, parent_sample_code,
                                    stage_code, test_id, quantity, created_by, created_on)
            VALUES ($1,$2,$3,$4,$5,1,$6,now())",
                    params = list(vt, on$order_number[1], src_code, held_at, tid, user()))
          dbExecute(conn, "
            INSERT INTO tbl_sample_event (sample_code, stage_code, state_code, actor, notes)
            VALUES ($1,$2,'inprogress',$3,$4)",
                    params = list(vt, held_at, user(),
                                  sprintf("%s cut from %s for test %s", vt, src_code,
                                          fld(r, "acronym", ""))))
          new_code <<- vt
        }); TRUE
      }, error = function(e) {
        toastr_error(conditionMessage(e), title = "Could not initiate", timeOut = 0); FALSE
      })
      if (ok) {
        printer$queue(data.frame(
          code  = new_code, title = "VIRUS TEST",
          line1 = sprintf("%s  \u00b7  from %s", fld(r, "acronym", ""), src_code),
          line2 = format(Sys.Date(), "%d %b %Y"), stringsAsFactors = FALSE))
        just_done(list(code = new_code, acronym = fld(r, "acronym", ""),
                       test = fld(r, "test_name", ""), source = src_code,
                       order = fld(r, "order_number", ""), bench = fld(r, "bench", "")))
        toastr_success(sprintf("%s initiated \u2014 sample cut from %s.",
                               fld(r, "acronym", ""), src_code),
                       title = "Test initiated")
        self_refresh()
      }
    }
    
    shiny$observeEvent(input$initiate, { do_initiate(sel_grid()) })
    
    # Same act from the worklist row. Select first so the detail panel follows
    # along and the operator can see what just happened.
    shiny$observeEvent(input$row_act, {
      just_done(NULL)
      selected(input$row_act$code)
      d <- queue()
      r <- d[d$row_id == input$row_act$code, , drop = FALSE]
      do_initiate(if (nrow(r)) r else NULL)
    })
    
    
    
    
    # ---- scan: resolve any code to the explant, select + highlight ---
    shiny$observeEvent(input$scan_go, {
      code <- trimws(input$scan %||% "")
      if (!nzchar(code)) return()
      id <- scan_identity(code)
      if (nrow(id) == 0) {
        toastr_warning(sprintf("No sample found for %s.", code), title = "Not found"); return()
      }
      kind <- id$kind[1]
      if (identical(kind, "test_sample")) {
        # a VT test id: focus its parent explant and the test itself
        explant <- id$parent_sample_code[1]
        if (is.na(explant)) { toastr_warning("Test sample has no parent explant.", title = "Orphan"); return() }
        # is that explant on THIS bench?
        onbench <- explant %in% queue()$sample_code
        if (!onbench) {
          toastr_warning(sprintf("%s belongs to %s, which is not on the indexing bench.", code, explant),
                         title = "Not on bench"); return()
        }
        selected(explant); focus_test(code)
        toastr_success(sprintf("Showing %s \u2014 test %s highlighted.", explant, code))
      } else {
        # an explant (or other pipeline sample): select if on this bench
        if (!(code %in% queue()$sample_code)) {
          toastr_warning(sprintf("%s is not on the indexing bench (kind: %s).", code, kind),
                         title = "Not on bench"); return()
        }
        selected(code); focus_test(NULL)
        toastr_success(sprintf("Showing %s.", code))
      }
      shinyjs_reset_scan()
    })
    
    shinyjs_reset_scan <- function() {
      runjs(sprintf("var el=document.getElementById('%s'); if(el) el.value='';", ns("scan")))
    }
    
    # explant currently selected, resolved from the live queue
    # The rest of this panel was written against the OLD queue, whose rows were
    # explants keyed by sample_code. Rows are now (material x test). Rather than
    # rewrite every reference, the grid row is given the two columns the panel
    # reads - sample_code and state_code - mapped onto the TEST SAMPLE, which is
    # the thing those parts of the panel actually operate on.
    #
    # Both are NA until the test is initiated. That is correct and is why the
    # awaiting-initiation branch below returns before any of it runs.
    sel_row <- shiny$reactive({
      r <- sel_grid(); if (is.null(r)) return(NULL)
      r$sample_code <- r$test_sample_code
      r$state_code  <- r$test_state
      r
    })
    
    # ---- the detail panel --------------------------------------------
    output$detail <- shiny$renderUI({
      # CONFIRMATION comes first, before any record lookup. The row it would
      # look up has already moved to Recording, so sel_row() would return
      # nothing and the pane would go blank at exactly the moment the operator
      # needs the new code most.
      jd <- just_done()
      if (!is.null(jd)) {
        return(shiny$div(
          class = "wl-detail-inner",
          order_theme$detail_head("Sample requested",
                                  sub = jd$acronym,
                                  close_input = ns("detail_close")),
          
          # The code, large and selectable. This is the thing the operator has
          # to get onto a tube; it is the largest thing on the panel.
          # No code yet: the holding bench mints it when it draws. An empty
          # code box would read as a failed request.
          if (!is.na(jd$code))
            shiny$div(class = "new-code",
                      shiny$div(class = "nc-label", "New sample code"),
                      shiny$div(class = "nc-value", jd$code))
          else NULL,
          
          order_theme$prop_grid(
            order_theme$prop("Test", jd$test),
            order_theme$prop("Cut from", jd$source),
            order_theme$prop("Order", jd$order),
            order_theme$prop("Bench", jd$bench)
          ),
          
          order_theme$guide(tone = "done",
                            shiny$strong(jd$source), " has been requested from ",
                            shiny$strong(jd$bench), " for ", shiny$strong(jd$acronym),
                            ". Once that bench draws it, the sample appears here under ",
                            shiny$strong("Update results"), ". The source stays where it is and ",
                            "can be drawn again for its other tests."),
          
          # No label here: the code is minted by the bench that draws, so the
          # label prints there, next to the material.
          order_theme$detail_actions(
            # The label, on the panel that shows the code it belongs to.
            # Initiating queues it; without a button here it stayed queued and
            # the operator had a freshly cut tube and no way to tag it.
            #
            # Only when a code was actually minted. On the REQUEST path there
            # is no sample yet - the holding bench mints it, and prints it
            # there, next to the tube.
            if (!is.na(jd$code)) label_print$ui(ns("print")) else NULL,
            shiny$actionButton(ns("go_recording"), "Go to Update results",
                               class = "btn btn-sm btn-outline-secondary"),
            shiny$actionButton(ns("init_next"), "Initiate another",
                               class = "btn btn-sm btn-outline-secondary")
          )
        ))
      }
      
      r <- sel_row()
      
      # Fail LOUDLY and by name. fld() above stops a missing column crashing
      # the panel, but silently substituting a default would hide a real
      # mismatch between this module and indexing_material() - which is how
      # the first version of this shipped with an always-empty results table.
      if (!is.null(r)) {
        missing <- setdiff(DETAIL_COLS, names(r))
        if (length(missing) > 0) {
          return(shiny$div(
            class = "wl-detail-inner",
            order_theme$detail_head("Cannot show this record",
                                    close_input = ns("detail_close")),
            order_theme$guide(tone = "do",
                              "The worklist query is missing ",
                              shiny$strong(paste(missing, collapse = ", ")),
                              ". This module and fct_tracking$indexing_material() are out of ",
                              "step \u2014 check that both files are deployed from the same build.")
          ))
        }
      }
      if (is.null(r)) {
        # nothing selected yet: guide the user
        return(shiny$div(class = "wl-detail-inner",
                         shiny$div(class = "wl-empty",
                                   shiny$div(class = "wl-empty-ico", shiny$icon("hand-pointer")),
                                   shiny$div(class = "wl-empty-title",
                                             "Select a sample to begin"),
                                   shiny$div(class = "wl-empty-body",
                                             "Click any row in the worklist to record results, submit for review, ",
                                             "or complete indexing.", shiny$br(),
                                             "You can also scan a barcode \u2014 an explant or a ",
                                             shiny$strong("VT"), " test id \u2014 to jump straight to it."))))
      }
      # AWAITING INITIATION: there is no test sample yet, so there is nothing to
      # record against. The only action is to cut one.
      if (identical(fld(r, "init_status", ""), "awaiting_initiation")) {
        return(shiny$div(
          class = "wl-detail-inner",
          order_theme$detail_head(fld(r, "acronym", ""),
                                  sub = paste("on", fld(r, "source_code", "")),
                                  close_input = ns("detail_close")),
          if (as.integer(fld(r, "held_here", 0L)) == 1L)
            order_theme$guide(tone = "do",
                              shiny$strong(fld(r, "source_code", "")),
                              " is on this bench. Initiating cuts a sample from it for this ",
                              "test alone; the material stays here for its other tests.")
          else
            order_theme$guide(tone = "do",
                              "This material is standing in ",
                              shiny$strong(if (identical(fld(r, "stage_code", ""), "meristem_culture"))
                                "meristem culture" else "quarantine"),
                              ". Requesting asks that bench to draw a sample from ",
                              shiny$strong(fld(r, "source_code", "")),
                              " for this test \u2014 no bench cuts another's stock."),
          order_theme$prop_grid(
            order_theme$prop("Test", fld(r, "test_name", "")),
            order_theme$prop("Pathogen",
                             if (is.na(fld(r, "pathogen_name", NA_character_))) "\u2014" else fld(r, "pathogen_name", NA_character_)),
            order_theme$prop("Material", fld(r, "source_code", "")),
            order_theme$prop("Held in", fld(r, "bench", "")),
            order_theme$prop("Order", fld(r, "order_number", "")),
            order_theme$prop("Customer", fld(r, "customer_name", ""))
          ),
          order_theme$detail_actions(
            # Same button, named for what it will actually do.
            if (as.integer(fld(r, "held_here", 0L)) == 1L)
              shiny$tagList(
                shiny$actionButton(ns("initiate"),
                                   paste("Initiate", fld(r, "acronym", "")),
                                   class = "btn btn-success"),
                label_print$ui(ns("print")))
            else if (as.integer(fld(r, "requested", 0L)) > 0)
              order_theme$chip("Requested \u2014 awaiting the holding bench", "amber")
            else
              shiny$actionButton(ns("initiate"),
                                 paste("Request a sample for", fld(r, "acronym", "")),
                                 class = "btn btn-success")
          )
        ))
      }
      
      sc <- fld(r, "sample_code", ""); st <- fld(r, "state_code", "")
      
      # A row is ONE test now, so the panel shows one test - this one. The old
      # filter was ta$explant == sc, which selected every test taken from an
      # explant. `sc` is now the TEST SAMPLE code, so that comparison would
      # match nothing and the results table would render empty without erroring:
      # a silent blank, not a crash.
      ta <- tests_all()
      sub <- if (nrow(ta)) ta[ta$sample_code == sc, , drop = FALSE] else ta[0, ]
      ft <- focus_test()   # a scanned VT test-sample to highlight, if any
      
      method <- if (identical(fld(r, "test_stage", ""), "grafting_virus_indexing")) "Grafting" else "Molecular"
      editable <- identical(st, "inprogress")
      
      shiny$div(class = "wl-detail-inner",
                order_theme$detail_head(
                  title = sc,
                  sub = sprintf("%s \u00b7 %s \u00b7 %s indexing", fld(r, "order_number", ""),
                                fld(r, "customer_name", "") %||% "", method),
                  close_input = ns("detail_close")),
                
                # state banner
                shiny$div(class = "wl-statusbar",
                          order_theme$chip(fld(r, "state_label", ""), switch(st,
                                                                             results_available = "amber", approved = "brand", rejected = "amber", "ink"))),
                
                # ---- no tests yet: take them ----
                # The old "Take tests" branch lived here, for a row with no
                # test samples yet. It is unreachable now: a row with no test
                # sample has init_status "awaiting_initiation" and returns at
                # the branch above. Worse, it cut one sample per test in one
                # go - the behaviour the per-test Awaiting tab replaced -
                # so leaving it gave two routes to create test samples, one of
                # which ignores the operator's choice of test.
                shiny$tagList(
                  order_theme$subhead("Test samples"),
                  shiny$div(
                    lapply(seq_len(nrow(sub)), function(i) {
                      tsc <- sub$sample_code[i]
                      lab <- paste0(sub$acronym[i],
                                    if (!is.na(sub$pathogen_name[i])) paste0(" \u00b7 ", sub$pathogen_name[i]) else "")
                      is_focus <- !is.null(ft) && identical(tsc, ft)
                      shiny$div(class = if (is_focus) "wl-item wl-item-focus" else "wl-item",
                                shiny$div(class = "wl-item-head",
                                          shiny$strong(tsc),
                                          if (is_focus) order_theme$chip("Scanned", "brand") else NULL,
                                          shiny$span(style = "opacity:.7; font-size:12px;", lab),
                                          shiny$span(style = "margin-left:auto;",
                                                     if (is.na(sub$outcome[i])) order_theme$chip("Pending", "ink")
                                                     else order_theme$chip(toupper(sub$outcome[i]), outcome_tone(sub$outcome[i])))),
                                shiny$div(class = "wl-item-meta",
                                          if (!is.na(sub$n_files[i]) && sub$n_files[i] > 0) sprintf("%d file(s) \u00b7 ", sub$n_files[i]) else "",
                                          if (!is.na(sub$tested_by[i])) paste("by", sub$tested_by[i]) else "not recorded"),
                                # inline recorder, only while editable
                                if (editable) shiny$div(style = "margin-top:8px; display:flex; gap:8px; align-items:flex-start; flex-wrap:wrap;",
                                                        shiny$div(style = "min-width:150px;",
                                                                  shiny$selectizeInput(ns(paste0("out_", tsc)), NULL,
                                                                                       choices = c("Set result..." = "", OUTCOMES),
                                                                                       selected = if (!is.na(sub$outcome[i])) sub$outcome[i] else "", width = "150px")),
                                                        shiny$div(style = "flex:1; min-width:170px;",
                                                                  shiny$fileInput(ns(paste0("file_", tsc)), NULL,
                                                                                  accept = c(".pdf", ".png", ".jpg", ".jpeg", ".csv", ".txt", ".docx"),
                                                                                  width = "100%")),
                                                        shiny$actionButton(ns(paste0("save_", tsc)), "Save",
                                                                           class = "btn btn-outline-secondary btn-sm")))
                    })),
                  
                  # ---- explant-level actions by state ----
                  shiny$div(class = "wl-actions",
                            if (identical(st, "inprogress")) {
                              all_in <- fld(r, "n_resulted", 0L) >= fld(r, "n_tests", 0L) && fld(r, "n_tests", 0L) > 0
                              shiny$actionButton(ns("submit"), "Submit for review",
                                                 class = if (all_in) "btn btn-primary" else "btn btn-outline-secondary")
                            },
                            if (identical(st, "results_available")) shiny$tagList(
                              shiny$actionButton(ns("reject"), "Reject", class = "btn btn-outline-secondary"),
                              if (!(user() %in% result_recorders(sc)))
                                shiny$actionButton(ns("approve"), "Approve", class = "btn btn-primary")
                              else shiny$span(class = "wl-warn-inline",
                                              "You recorded results \u2014 another reviewer must approve.")
                            ),
                            if (identical(st, "approved"))
                              shiny$actionButton(ns("complete"), "Complete indexing", class = "btn btn-primary"),
                            if (identical(st, "rejected"))
                              shiny$actionButton(ns("reopen"), "Re-record", class = "btn btn-outline-secondary")
                  ),
                  if (identical(st, "results_available"))
                    shiny$div(style = "margin-top:10px;",
                              shiny$textAreaInput(ns("review_comments"), "Review comments", width = "100%"))
                )
      )
    })
    
    # ================================================================
    # ACTIONS
    # ================================================================
    active <- function() { r <- sel_row(); if (is.null(r)) NULL else r }
    
    # save one test-sample's result (+ optional file), inline
    save_result <- function(tsc) {
      r <- active(); if (is.null(r)) return()
      ta <- tests_all(); trow <- ta[ta$sample_code == tsc, , drop = FALSE]
      if (nrow(trow) != 1) return()
      outcome <- input[[paste0("out_", tsc)]]
      if (is.null(outcome) || !nzchar(outcome)) { toastr_warning("Choose a result first.", title = "No result"); return() }
      fin <- input[[paste0("file_", tsc)]]
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_test_result (sample_code, test_id, outcome, tested_by, tested_on)
            VALUES ($1, $2, $3, $4, now())",
                    params = list(tsc, as.integer(trow$test_id[1]), outcome, user()))
          if (!is.null(fin) && is.data.frame(fin) && nrow(fin) > 0) {
            file_upload(file_input = fin, description = sprintf("%s evidence (%s)", trow$acronym[1], outcome),
                        sample_code = tsc, user = user(), conn = conn)
          }
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Save failed", timeOut = 0); FALSE })
      if (ok) { toastr_success(sprintf("Result saved for %s.", tsc)); self_refresh() }
    }
    
    # one observer per possible test-sample button (bounded); wire lazily
    observed <- shiny$reactiveVal(character(0))
    shiny$observe({
      ta <- tests_all()
      for (tsc in ta$sample_code) {
        if (!(tsc %in% observed())) {
          local({
            code <- tsc
            shiny$observeEvent(input[[paste0("save_", code)]], { save_result(code) }, ignoreInit = TRUE)
          })
          observed(c(observed(), tsc))
        }
      }
    })
    
    
    shiny$observeEvent(input$submit, {
      r <- active(); if (is.null(r)) return(); sc <- fld(r, "sample_code", "")
      if (fld(r, "n_resulted", 0L) < fld(r, "n_tests", 0L) || fld(r, "n_tests", 0L) == 0) {
        toastr_warning("Record every test result before submitting.", title = "Not ready"); return()
      }
      ok <- advance(sc, fld(r, "test_stage", ""), "results_available", "all results recorded; submitted for review")
      if (ok) { toastr_success(sprintf("%s submitted for review.", sc)) }
    })
    
    do_review <- function(decision) {
      r <- active(); if (is.null(r)) return(); sc <- fld(r, "sample_code", "")
      if (identical(decision, "approved") && user() %in% result_recorders(sc)) {
        toastr_error("You recorded these results and cannot approve them.", title = "Segregation of duty"); return()
      }
      new_state <- if (identical(decision, "approved")) "approved" else "rejected"
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          dbExecute(conn, "
            INSERT INTO tbl_review (sample_code, stage_code, decision, comments, reviewed_by)
            VALUES ($1,$2,$3,$4,$5)",
                    params = list(sc, fld(r, "test_stage", ""), decision,
                                  if (nzchar(input$review_comments %||% "")) input$review_comments else NA, user()))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, fld(r, "test_stage", ""), new_state, user(),
                       wf = wf, ctx = ctx, notes = sprintf("review: %s", decision))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Review failed", timeOut = 0); FALSE })
      if (ok) { toastr_success(sprintf("%s %s.", sc, new_state)); self_refresh() }
    }
    shiny$observeEvent(input$approve, { do_review("approved") })
    shiny$observeEvent(input$reject,  { do_review("rejected") })
    
    shiny$observeEvent(input$reopen, {
      r <- active(); if (is.null(r)) return()
      if (advance(fld(r, "sample_code", ""), fld(r, "test_stage", ""), "inprogress", "reopened after rejection"))
        toastr_success(sprintf("%s reopened.", fld(r, "sample_code", "")))
    })
    
    shiny$observeEvent(input$complete, {
      r <- active(); if (is.null(r)) return(); sc <- fld(r, "sample_code", "")
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, fld(r, "test_stage", ""), "completed", user(),
                       wf = wf, ctx = ctx, notes = "indexing approved; completed")
          dbExecute(conn, "
            INSERT INTO tbl_order_event (order_number, module, action, actor, notes)
            VALUES ($1, 'virus_indexing', $2, $3, $4)",
                    params = list(fld(r, "order_number", ""), sprintf("indexing completed for %s", sc), user(),
                                  sprintf("roll-up: %s", r$last_status[1])))
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Complete failed", timeOut = 0); FALSE })
      if (ok) {
        toastr_success(sprintf("Indexing complete for %s.", sc))
        selected(NULL); focus_test(NULL)
        self_refresh(); signal_others()   # completed explant leaves for the next bench
      }
    })
    
    # shared: advance the explant's state via record_event
    advance <- function(sc, stage, to_state, note) {
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, "SELECT ensure_app_user($1, $2)",
                    params = list(user(), isTRUE(shiny$reactiveValuesToList(res_auth)$admin)))
          wf <- tryCatch(workflow_cache(WF_PATH, conn), error = function(e) NULL)
          ctx <- sample_context(conn, sc)
          record_event(conn, sc, stage, to_state, user(), wf = wf, ctx = ctx, notes = note)
        }); TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Failed", timeOut = 0); FALSE })
      if (ok) self_refresh()
      ok
    }
    
    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

