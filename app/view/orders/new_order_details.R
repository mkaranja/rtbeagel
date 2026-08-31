box::use(
  shiny,
  shinyjs[useShinyjs, disable, enable],
  shinyWidgets[prettyCheckbox, updatePrettyCheckbox, awesomeCheckboxGroup, updateAwesomeCheckboxGroup],
  shinyFeedback[useShinyFeedback, feedbackDanger],
  stats[setNames],
)

box::use(
  app/logic/fct_conn[pool, load_data],
  app/view/shared/order_theme,
  app/view/shared/view_files,
)

# ============================================================================
# NEW ORDER DETAILS · reception, material, services, tests
# ----------------------------------------------------------------------------
# THE BIG CHANGE: services are no longer eight hardcoded columns.
#
# The old module carried a `metrics` list in R mirroring the eight columns of
# tbl_project_services (pathogen_detection, in_vitro_conservation, ...). Adding
# a service meant editing R, editing the table, and redeploying. Worse, the
# table had UNIQUE (project_code), so an order could request each service at
# most ONCE - which made "subculture 200 for sale AND 50 for conservation"
# impossible to even express.
#
# Now the rows are rendered FROM tbl_service_catalog and returned as LINE
# ITEMS: one row per requested service. Adding a service is an INSERT.
#
# Lookups (sample type, condition, part, bag) also come from the database
# instead of R constants, so an admin can add one without a redeploy.
# ============================================================================

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    useShinyjs(),
    useShinyFeedback(),
    
    order_theme$section(
      "3", "Reception", accent = "teal", sub = "Field & receipt details",
      shiny$fluidRow(
        shiny$column(6, shiny$textInput(ns("ref_no"), "LOT / REF NO")),
        shiny$column(6, shiny$textInput(ns("sampler"), "SAMPLED BY")),
        shiny$column(6, shiny$dateInput(ns("date_sampled"), "DATE SAMPLED", value = Sys.Date())),
        shiny$column(6, shiny$dateInput(ns("date_received"), "DATE RECEIVED", value = Sys.Date()))
      )
    ),
    
    order_theme$section(
      "4", "Sample details", accent = "teal",
      shiny$fluidRow(
        shiny$column(6, sel(ns("crop_id"), "CROP", 0)),
        shiny$column(6, sel(ns("variety_id"), "SAMPLE VARIETY", 0)),
        shiny$column(6, sel(ns("sample_type_code"), "SAMPLE TYPE", "")),
        shiny$column(6, sel(ns("origin_country_code"), "SAMPLE ORIGIN", "")),
        shiny$column(6, sel(ns("condition_code"), "SAMPLE CONDITION", "")),
        shiny$column(6, shiny$textAreaInput(ns("sample_description"), "SAMPLE DESCRIPTION")),
        shiny$column(6, sel(ns("part_id"), "PART SUBMITTED", 0)),
        shiny$column(6, sel(ns("bag_id"), "SAMPLING BAG", 0))
      )
    ),
    
    order_theme$section(
      "5", "Lab services", accent = "teal", sub = "What the customer is asking for",
      
      order_theme$subhead("Diagnostics"),
      shiny$uiOutput(ns("diagnostic_rows")),
      shiny$conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("svc_pathogen_detection")),
        shiny$div(
          style = "margin-left:26px;",
          shiny$uiOutput(ns("test_picker"))
        )
      ),
      
      order_theme$subhead("Conservation"),
      shiny$uiOutput(ns("conservation_rows")),
      
      order_theme$subhead("Distribution"),
      shiny$uiOutput(ns("distribution_rows"))
    ),
    
    order_theme$section(
      "6", "Other details", sub = "Notes & attachments",
      shiny$textAreaInput(ns("additional_info"), "ADDITIONAL DETAILS", width = "100%"),
      shiny$fileInput(ns("file"), "Upload a file [PDF, Word, text, images]",
                      multiple = TRUE,
                      accept = c(".pdf", ".doc", ".docx", ".txt", ".csv", "image/*"),
                      buttonLabel = "Browse...", placeholder = "No file selected"),
      shiny$textAreaInput(ns("description"), "FILE DESCRIPTION"),
      view_files$ui(ns("display_file"))
    )
  )
}

#' @export
server <- function(id, res_auth, page, tab, trigger_refresh, clear_clicked) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- the catalogue drives the UI --------------------------------
    lookups <- shiny$reactiveVal(list())
    
    catalog <- shiny$reactive({
      trigger_refresh()
      load_data(pool, "
        SELECT service_code, service_label, service_kind, unit, sort_order
        FROM tbl_service_catalog
        WHERE active
        ORDER BY sort_order, service_label")
    })
    
    # A service row: checkbox + quantity. Rendered per catalogue entry, so
    # nothing here names a specific service.
    render_rows <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(shiny$div(class = "text-muted", style = "font-size:12px;", "No services configured."))
      }
      shiny$tagList(lapply(seq_len(nrow(df)), function(i) {
        code <- df$service_code[i]
        shiny$div(
          class = "metric-row",
          shiny$div(
            style = "flex:1;",
            prettyCheckbox(ns(paste0("svc_", code)), df$service_label[i],
                           status = "success", shape = "curve")
          ),
          shiny$div(
            style = "width:150px;",
            shiny$conditionalPanel(
              condition = sprintf("input['%s'] == true", ns(paste0("svc_", code))),
              shiny$numericInput(ns(paste0("qty_", code)), NULL,
                                 value = 1, min = 1,
                                 width = "100%")
            )
          ),
          shiny$div(style = "width:70px; font-size:11px; color:var(--ink-faint);", df$unit[i])
        )
      }))
    }
    
    output$diagnostic_rows <- shiny$renderUI({
      df <- catalog(); render_rows(df[df$service_kind == "diagnostic", , drop = FALSE])
    })
    output$conservation_rows <- shiny$renderUI({
      df <- catalog()
      render_rows(df[grepl("conservation|cold_room", df$service_code), , drop = FALSE])
    })
    output$distribution_rows <- shiny$renderUI({
      df <- catalog()
      keep <- df$service_kind == "fulfilment" & !grepl("conservation|cold_room", df$service_code)
      render_rows(df[keep, , drop = FALSE])
    })
    
    # ---- lookups, all from the database -----------------------------
    shiny$observe({
      shiny$req(page == tab())
      trigger_refresh()
      
      crops <- load_data(pool, "SELECT crop_id, crop_name FROM tbl_crop WHERE active ORDER BY crop_name")
      shiny$updateSelectizeInput(session, "crop_id",
                                 choices = c("--SELECT--" = 0, setNames(crops$crop_id, crops$crop_name)))
      
      st <- load_data(pool, "SELECT sample_type_code, label FROM tbl_sample_type WHERE active ORDER BY sort_order, label")
      shiny$updateSelectizeInput(session, "sample_type_code",
                                 choices = c("--SELECT--" = "", setNames(st$sample_type_code, st$label)))
      
      sc <- load_data(pool, "SELECT condition_code, label FROM tbl_sample_condition WHERE active ORDER BY sort_order, label")
      shiny$updateSelectizeInput(session, "condition_code",
                                 choices = c("--SELECT--" = "", setNames(sc$condition_code, sc$label)))
      
      sp <- load_data(pool, "SELECT part_id, part_name FROM tbl_sample_part WHERE active ORDER BY part_name")
      shiny$updateSelectizeInput(session, "part_id",
                                 choices = c("--SELECT--" = 0, setNames(sp$part_id, sp$part_name)))
      
      sb <- load_data(pool, "SELECT bag_id, bag_name FROM tbl_sampling_bag WHERE active ORDER BY bag_name")
      shiny$updateSelectizeInput(session, "bag_id",
                                 choices = c("--SELECT--" = 0, setNames(sb$bag_id, sb$bag_name)))
      
      # 256 ISO 3166 countries. server-side selectize so the browser is not
      # handed the whole list on every render.
      co <- load_data(pool, "SELECT country_code, country_name FROM tbl_country
                             WHERE active ORDER BY country_name")
      shiny$updateSelectizeInput(session, "origin_country_code",
                                 choices = c("--SELECT--" = "", setNames(co$country_code, co$country_name)),
                                 server = TRUE)
    })
    
    # variety cascades from crop
    shiny$observeEvent(input$crop_id, {
      shiny$req(page == tab())
      if (is.null(input$crop_id) || input$crop_id == 0) {
        shiny$updateSelectizeInput(session, "variety_id", choices = c("--SELECT--" = 0))
        return()
      }
      v <- load_data(pool, sprintf("
        SELECT variety_id, variety_name FROM tbl_variety
        WHERE active AND crop_id = %d ORDER BY variety_name", as.integer(input$crop_id)))
      shiny$updateSelectizeInput(session, "variety_id",
                                 choices = c("--SELECT--" = 0, setNames(v$variety_id, v$variety_name)))
    })
    
    # tests cascade from crop too - tbl_test_method.crop_id is 1:1
    output$test_picker <- shiny$renderUI({
      shiny$req(input$crop_id, input$crop_id != 0)
      tm <- load_data(pool, sprintf("
        SELECT t.test_id, t.acronym, t.test_name, p.pathogen_name
        FROM tbl_test_method t
        LEFT JOIN tbl_pathogen p ON p.pathogen_id = t.pathogen_id
        WHERE t.active AND t.crop_id = %d
        ORDER BY t.test_name", as.integer(input$crop_id)))
      if (is.null(tm) || nrow(tm) == 0) {
        return(shiny$div(class = "text-muted", style = "font-size:12px;",
                         "No test methods configured for this crop."))
      }
      labels <- ifelse(is.na(tm$pathogen_name), tm$test_name,
                       paste0(tm$test_name, " \u00b7 ", tm$pathogen_name))
      awesomeCheckboxGroup(ns("test_ids"), NULL,
                           choices = setNames(tm$test_id, labels))
    })
    
    # ---- returns -----------------------------------------------------
    
    # one row for tbl_order_detail
    detail_row <- shiny$reactive({
      shiny$req(page == tab())
      data.frame(
        ref_no             = nz(input$ref_no),
        sampler            = nz(input$sampler),
        date_sampled       = input$date_sampled,
        date_received      = input$date_received,
        crop_id            = int_or_na(input$crop_id),
        variety_id         = int_or_na(input$variety_id),
        sample_type_code   = nz(input$sample_type_code),
        condition_code     = nz(input$condition_code),
        part_id            = int_or_na(input$part_id),
        bag_id             = int_or_na(input$bag_id),
        origin_country_code = nz(input$origin_country_code),
        sample_description = nz(input$sample_description),
        additional_info    = nz(input$additional_info),
        stringsAsFactors   = FALSE
      )
    })
    
    # N rows for tbl_order_service - the line items.
    # This is the shape the old wide table could not hold.
    service_lines <- shiny$reactive({
      df <- catalog()
      shiny$req(df)
      picked <- lapply(seq_len(nrow(df)), function(i) {
        code <- df$service_code[i]
        on <- isTRUE(input[[paste0("svc_", code)]])
        if (!on) return(NULL)
        qty <- input[[paste0("qty_", code)]]
        if (is.null(qty) || is.na(qty) || qty < 1) qty <- 1
        data.frame(service_code = code, target_qty = as.integer(qty),
                   stringsAsFactors = FALSE)
      })
      picked <- Filter(Negate(is.null), picked)
      if (!length(picked)) {
        return(data.frame(service_code = character(0), target_qty = integer(0),
                          stringsAsFactors = FALSE))
      }
      do.call(rbind, picked)
    })
    
    test_ids <- shiny$reactive({
      if (!isTRUE(input$svc_pathogen_detection)) return(integer(0))
      ids <- input$test_ids
      if (is.null(ids)) integer(0) else as.integer(ids)
    })
    
    # An order must ask for SOMETHING. The old app happily saved orders with
    # no services at all, which then sat in the queue forever with nothing
    # to complete.
    is_valid <- shiny$reactive({
      nrow(service_lines()) > 0 &&
        !is.null(input$crop_id) && input$crop_id != 0 &&
        (!isTRUE(input$svc_pathogen_detection) || length(test_ids()) > 0)
    })
    
    validation_message <- shiny$reactive({
      if (is.null(input$crop_id) || input$crop_id == 0) return("Select a crop.")
      if (nrow(service_lines()) == 0) return("Select at least one service.")
      if (isTRUE(input$svc_pathogen_detection) && length(test_ids()) == 0) {
        return("Pathogen detection requires at least one test method.")
      }
      NULL
    })
    
    shiny$observeEvent(c(trigger_refresh(), clear_clicked()), {
      shiny$req(page == tab())
      df <- catalog()
      if (!is.null(df)) for (code in df$service_code) {
        updatePrettyCheckbox(session, paste0("svc_", code), value = FALSE)
      }
      shiny$updateTextInput(session, "ref_no", value = "")
      shiny$updateTextInput(session, "sampler", value = "")
      shiny$updateTextAreaInput(session, "sample_description", value = "")
      shiny$updateTextAreaInput(session, "additional_info", value = "")
      shiny$updateTextAreaInput(session, "description", value = "")
      shiny$updateSelectizeInput(session, "crop_id", selected = 0)
      shiny$updateSelectizeInput(session, "origin_country_code", selected = "")
    }, ignoreInit = TRUE)
    
    #files <- view_files$server("display_file", project_code)
    
    list(
      data               = detail_row,
      services           = service_lines,
      tests              = test_ids,
      is_valid           = is_valid,
      validation_message = validation_message,
      lookups            = lookups,
      file_name          = shiny$reactive(input$file),
      file_description   = shiny$reactive(input$description)
    )
  })
}

# helpers ---------------------------------------------------------------

# selectizeInput that renders its dropdown as a child of <body> instead of a
# child of the input's own container.
#
# WHY: section() draws each card as `.section > .section-body`. A dropdown
# opened on the LAST row of a card (PART SUBMITTED / SAMPLING BAG) is taller
# than the space left inside that card, so it is either clipped by the card's
# overflow or painted underneath the next card ("5 Lab services"), which is a
# later sibling in the same stacking context. No amount of z-index on the
# dropdown fixes that while the dropdown lives inside the card - the card
# itself is the clipping/stacking box.
#
# dropdownParent = "body" moves the dropdown out of the card entirely.
# Selectize then positions it absolutely against the input, so it floats over
# everything below it. This is a UI-only change: the input id, the choices
# contract and every updateSelectizeInput() call site are untouched.
sel <- function(id, label, empty_value) {
  shiny$selectizeInput(
    id, label,
    choices = c("--SELECT--" = empty_value),
    options = list(dropdownParent = "body")
  )
}

nz <- function(x) if (is.null(x) || length(x) == 0 || !nzchar(as.character(x)[1])) NA_character_ else as.character(x)[1]
int_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  v <- suppressWarnings(as.integer(x))
  if (is.na(v) || v == 0) NA_integer_ else v
}