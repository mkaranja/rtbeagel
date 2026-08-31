box::use(
  shiny,
  reactable[reactable, reactableOutput, renderReactable, colDef, getReactableState, reactableTheme, reactableLang],
  shinyjs[useShinyjs],
  shinytoastr[toastr_success, toastr_error],
  htmlwidgets[JS],
  pool[poolWithTransaction],
  stats[setNames],
)

box::use(
  app/logic/fct_conn[pool],
  app/logic/fct_admin[ADMIN, admin_fields, admin_list, admin_fk_choices,
                      admin_insert, admin_update, admin_set_active, admin_usage],
  app/view/shared/order_theme,
)

# ============================================================================
# ADMIN CRUD · one module for every reference table
# ----------------------------------------------------------------------------
# Replaces 13 files / 2191 lines: add_customer.R + customer.R,
# add_variety.R + variety.R, add_laboratory.R + laboratory.R,
# add_pathogen.R + pathogen.R, add_test_method.R + test_method.R,
# add_sample_part.R + sample_part.R, sampling_bag.R.
#
# Every one of them listed rows, opened a form, and saved. The forms differed
# only in their field labels - which is data, so it now lives in fct_admin.R.
#
# Usage, in app_ui.R:
#     admin_crud$ui(ns("customers"), ADMIN$customer)
#     admin_crud$server("customers", ADMIN$customer, res_auth)
#
# Adding a reference table is an entry in ADMIN, not a new pair of modules.
# ============================================================================

#' @export
ui <- function(id, cfg) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "rtb-intake",
    order_theme$head_orders(),
    useShinyjs(),
    
    shiny$div(
      class = "page-head",
      shiny$div(
        shiny$tags$h1(cfg$title),
        if (!is.null(cfg$warn)) {
          shiny$div(class = "update-hint", style = "margin-top:8px; max-width:620px;", cfg$warn)
        } else {
          shiny$tags$p(paste("Manage the", tolower(cfg$title), "available across the system."))
        }
      ),
      shiny$actionButton(ns("add"), paste("Add", cfg$singular),
                         icon = shiny$icon("plus"), class = "btn btn-primary")
    ),
    
    shiny$div(
      class = "toolbar",
      shiny$div(
        class = "search-wrap",
        shiny$span(class = "ico", shiny$icon("search")),
        shiny$tags$input(id = ns("q"), type = "text", class = "form-control form-control-sm",
                         placeholder = "Search...")
      ),
      shiny$checkboxInput(ns("show_inactive"), "Show inactive", value = FALSE, width = "160px")
    ),
    
    shiny$div(class = "table-card", reactableOutput(ns("tbl")))
  )
}

#' @export
server <- function(id, cfg, res_auth) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    refresh  <- shiny$reactiveVal(0)
    editing  <- shiny$reactiveVal(NULL)   # NULL = adding; otherwise the pk value
    
    rows <- shiny$reactive({
      refresh()
      d <- admin_list(cfg, include_inactive = isTRUE(input$show_inactive))
      q <- input$q
      if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
        hay <- apply(d[, intersect(cfg$display, names(d)), drop = FALSE], 1,
                     function(r) paste(r, collapse = " "))
        d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
      }
      d
    })
    
    # ---- resolve FK columns to their labels for display --------------
    fk_maps <- shiny$reactive({
      refresh()
      out <- list()
      for (f in cfg$fields) if (identical(f$type, "fk")) out[[f$name]] <- admin_fk_choices(f)
      out
    })
    
    output$tbl <- renderReactable({
      d <- rows()
      maps <- fk_maps()
      
      cols <- list()
      for (nm in names(d)) {
        if (nm == cfg$pk && !is_code_pk(cfg)) { cols[[nm]] <- colDef(show = FALSE); next }
        if (nm == "active") {
          cols[[nm]] <- colDef(name = "STATUS", width = 110, cell = function(v) {
            if (isTRUE(v)) order_theme$chip("Active", "brand") else order_theme$chip("Inactive", "ink")
          })
          next
        }
        fld <- field_by_name(cfg, nm)
        if (!is.null(fld) && identical(fld$type, "fk")) {
          m <- maps[[nm]]
          cols[[nm]] <- colDef(name = toupper(fld$label), cell = function(v) {
            lbl <- names(m)[match(as.character(v), unname(m))]
            if (length(lbl) && !is.na(lbl)) lbl else "\u2014"
          })
          next
        }
        if (!nm %in% cfg$display) { cols[[nm]] <- colDef(show = FALSE); next }
        cols[[nm]] <- colDef(name = toupper(if (!is.null(fld)) fld$label else nm))
      }
      
      cols[["__act"]] <- colDef(
        name = "", sortable = FALSE, width = 170,
        cell = JS(sprintf("function(ci){
          var id = ci.row['%s'];
          var on = ci.row['active'];
          return '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + id + '\\', n: Math.random()})\">Edit</button> ' +
                 '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + id + '\\', to: ' + (on ? 'false' : 'true') + ', n: Math.random()})\">' + (on ? 'Deactivate' : 'Reactivate') + '</button>';
        }", cfg$pk, ns("edit_row"), ns("toggle_row"))),
        html = TRUE
      )
      
      # rep(NA, nrow(d)), NOT NA. On a fresh database every reference table
      # is empty, and `d$x <- NA` on a zero-row frame raises
      #   "replacement has 1 row, data has 0"
      # so the very first page load crashed. rep() yields logical(0) and
      # assigns cleanly.
      d$`__act` <- rep(NA, nrow(d))
      
      reactable(d, columns = cols, defaultPageSize = 12, highlight = TRUE,
                searchable = FALSE, compact = TRUE,
                language = reactableLang(
                  noData = sprintf("No %s yet \u2014 use \u201cAdd %s\u201d to create the first one.",
                                   tolower(cfg$title), cfg$singular)
                ),
                theme = order_theme$rt_theme())
    })
    
    # ---- the form ----------------------------------------------------
    build_form <- function(current = NULL) {
      lapply(cfg$fields, function(f) {
        val <- if (!is.null(current)) current[[f$name]] else f$default
        lbl <- shiny$HTML(paste0(f$label,
                                 if (isTRUE(f$required)) " <span class='mandatory_star'>*</span>" else ""))
        # A code IS the primary key and the workflow refers to it by name.
        # Editable at creation, frozen forever after.
        locked <- identical(f$type, "code") && !is.null(current)
        inp <- switch(f$type,
                      text     = shiny$textInput(ns(f$name), lbl, value = chr(val)),
                      code     = shiny$textInput(ns(f$name), lbl, value = chr(val)),
                      textarea = shiny$textAreaInput(ns(f$name), lbl, value = chr(val)),
                      number   = shiny$numericInput(ns(f$name), lbl, value = if (is.null(val)) 0 else val),
                      select   = shiny$selectizeInput(ns(f$name), lbl, choices = f$choices, selected = chr(val)),
                      fk       = shiny$selectizeInput(ns(f$name), lbl, choices = admin_fk_choices(f),
                                                      selected = chr(val)),
                      shiny$textInput(ns(f$name), lbl, value = chr(val))
        )
        shiny$tagList(
          if (locked) shinyjs::disabled(inp) else inp,
          if (!is.null(f$help)) shiny$div(class = "text-muted",
                                          style = "font-size:11px; margin-top:-8px; margin-bottom:12px;",
                                          if (locked) "Code cannot be changed once created." else f$help)
        )
      })
    }
    
    open_modal <- function(current = NULL) {
      shiny$showModal(shiny$modalDialog(
        title = paste(if (is.null(current)) "Add" else "Edit", cfg$singular),
        shiny$div(class = "rtb-intake", order_theme$head_orders(),
                  order_theme$section("\u270e", cfg$title, build_form(current))),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("save"), "Save", class = "btn btn-success")
        ),
        easyClose = FALSE, size = "m"
      ))
    }
    
    shiny$observeEvent(input$add, { editing(NULL); open_modal(NULL) })
    
    shiny$observeEvent(input$edit_row, {
      d <- admin_list(cfg, include_inactive = TRUE)
      row <- d[as.character(d[[cfg$pk]]) == as.character(input$edit_row$id), , drop = FALSE]
      shiny$req(nrow(row) == 1)
      editing(row[[cfg$pk]][1])
      open_modal(as.list(row))
    })
    
    # ---- deactivate, never delete ------------------------------------
    shiny$observeEvent(input$toggle_row, {
      id <- input$toggle_row$id
      to <- isTRUE(input$toggle_row$to)
      n  <- if (!to) admin_usage(cfg, id) else NA_integer_
      
      shiny$showModal(shiny$modalDialog(
        title = if (to) "Reactivate" else "Deactivate",
        shiny$div(
          class = "rtb-intake", order_theme$head_orders(),
          shiny$p(if (to) {
            paste("Make this", cfg$singular, "selectable again?")
          } else {
            paste("Hide this", cfg$singular, "from new orders?",
                  "Nothing is deleted and existing records keep working.")
          }),
          if (!to && !is.na(n) && n > 0) {
            shiny$div(class = "update-hint",
                      sprintf("%d existing record%s already reference%s this. They are unaffected.",
                              n, if (n == 1) "" else "s", if (n == 1) "s" else ""))
          }
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("confirm_toggle"), if (to) "Reactivate" else "Deactivate",
                             class = "btn btn-success")
        )
      ))
      editing(id)
    })
    
    shiny$observeEvent(input$confirm_toggle, {
      id <- editing(); to <- isTRUE(input$toggle_row$to)
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) admin_set_active(conn, cfg, id, to))
        TRUE
      }, error = function(e) { toastr_error(conditionMessage(e), title = "Failed"); FALSE })
      shiny$removeModal()
      if (ok) { toastr_success(if (to) "Reactivated." else "Deactivated."); refresh(refresh() + 1) }
    })
    
    # ---- save --------------------------------------------------------
    shiny$observeEvent(input$save, {
      vals <- list()
      missing <- character(0)
      for (f in cfg$fields) {
        v <- input[[f$name]]
        if (identical(f$type, "code") && !is.null(editing())) next   # frozen
        if (is.null(v) || (is.character(v) && !nzchar(trimws(v))) ||
            (is.numeric(v) && is.na(v))) {
          if (isTRUE(f$required)) missing <- c(missing, f$label)
          vals[[f$name]] <- NA
          next
        }
        if (identical(f$type, "fk")) {
          if (!nzchar(v)) {
            if (isTRUE(f$required)) missing <- c(missing, f$label)
            vals[[f$name]] <- NA
          } else vals[[f$name]] <- as.integer(v)
          next
        }
        if (identical(f$type, "code")) {
          v <- tolower(trimws(v))
          if (grepl("[^a-z0-9_]", v)) {
            toastr_error("Code must be lowercase letters, numbers and underscores only.",
                         title = "Invalid code")
            return()
          }
        }
        vals[[f$name]] <- if (identical(f$type, "number")) as.numeric(v) else v
      }
      
      if (length(missing)) {
        toastr_error(paste("Required:", paste(missing, collapse = ", ")), title = "Missing fields")
        return()
      }
      
      ok <- tryCatch({
        poolWithTransaction(pool, function(conn) {
          if (is.null(editing())) admin_insert(conn, cfg, vals)
          else admin_update(conn, cfg, editing(), vals)
        })
        TRUE
      }, error = function(e) {
        msg <- conditionMessage(e)
        # turn the two constraint failures users actually hit into English
        if (grepl("duplicate key|unique", msg, ignore.case = TRUE)) {
          toastr_error(paste("That", cfg$singular, "already exists."), title = "Duplicate")
        } else if (grepl("foreign key", msg, ignore.case = TRUE)) {
          toastr_error("A linked record is missing or inactive.", title = "Invalid reference")
        } else {
          toastr_error(msg, title = "Save failed", timeOut = 0)
        }
        FALSE
      })
      
      if (ok) {
        shiny$removeModal()
        toastr_success("Saved.")
        editing(NULL)
        refresh(refresh() + 1)
      }
    })
    
    list(refresh = refresh)
  })
}

# helpers ---------------------------------------------------------------
field_by_name <- function(cfg, nm) {
  for (f in cfg$fields) if (identical(f$name, nm)) return(f)
  NULL
}
is_code_pk <- function(cfg) {
  for (f in cfg$fields) if (identical(f$name, cfg$pk) && identical(f$type, "code")) return(TRUE)
  FALSE
}
chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)[1]

#' box::use(
#'   shiny,
#'   reactable[reactable, reactableOutput, renderReactable, colDef, getReactableState, reactableTheme, reactableLang],
#'   shinyjs[useShinyjs],
#'   shinytoastr[toastr_success, toastr_error],
#'   htmlwidgets[JS],
#'   pool[poolWithTransaction],
#'   stats[setNames],
#' )
#' 
#' box::use(
#'   app/logic/fct_conn[pool],
#'   app/logic/fct_admin[ADMIN, admin_fields, admin_list, admin_fk_choices,
#'                       admin_insert, admin_update, admin_set_active, admin_usage],
#'   app/view/shared/order_theme,
#' )
#' 
#' # ============================================================================
#' # ADMIN CRUD · one module for every reference table
#' # ----------------------------------------------------------------------------
#' # Replaces 13 files / 2191 lines: add_customer.R + customer.R,
#' # add_variety.R + variety.R, add_laboratory.R + laboratory.R,
#' # add_pathogen.R + pathogen.R, add_test_method.R + test_method.R,
#' # add_sample_part.R + sample_part.R, sampling_bag.R.
#' #
#' # Every one of them listed rows, opened a form, and saved. The forms differed
#' # only in their field labels - which is data, so it now lives in fct_admin.R.
#' #
#' # Usage, in app_ui.R:
#' #     admin_crud$ui(ns("customers"), ADMIN$customer)
#' #     admin_crud$server("customers", ADMIN$customer, res_auth)
#' #
#' # Adding a reference table is an entry in ADMIN, not a new pair of modules.
#' # ============================================================================
#' 
#' #' @export
#' ui <- function(id, cfg) {
#'   ns <- shiny$NS(id)
#'   shiny$div(
#'     class = "rtb-intake",
#'     order_theme$head_orders(),
#'     useShinyjs(),
#'     
#'     shiny$div(
#'       class = "page-head",
#'       shiny$div(
#'         shiny$tags$h1(cfg$title),
#'         if (!is.null(cfg$warn)) {
#'           shiny$div(class = "update-hint", style = "margin-top:8px; max-width:620px;", cfg$warn)
#'         } else {
#'           shiny$tags$p(paste("Manage the", tolower(cfg$title), "available across the system."))
#'         }
#'       ),
#'       shiny$actionButton(ns("add"), paste("Add", cfg$singular),
#'                          icon = shiny$icon("plus"), class = "btn btn-primary")
#'     ),
#'     
#'     shiny$div(
#'       class = "toolbar",
#'       shiny$div(
#'         class = "search-wrap",
#'         shiny$span(class = "ico", shiny$icon("search")),
#'         shiny$tags$input(id = ns("q"), type = "text", class = "form-control form-control-sm",
#'                          placeholder = "Search...")
#'       ),
#'       shiny$checkboxInput(ns("show_inactive"), "Show inactive", value = FALSE, width = "160px")
#'     ),
#'     
#'     shiny$div(class = "table-card", reactableOutput(ns("tbl")))
#'   )
#' }
#' 
#' #' @export
#' server <- function(id, cfg, res_auth) {
#'   shiny$moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'     
#'     refresh  <- shiny$reactiveVal(0)
#'     editing  <- shiny$reactiveVal(NULL)   # NULL = adding; otherwise the pk value
#'     
#'     rows <- shiny$reactive({
#'       refresh()
#'       d <- admin_list(cfg, include_inactive = isTRUE(input$show_inactive))
#'       q <- input$q
#'       if (!is.null(q) && nzchar(q) && nrow(d) > 0) {
#'         hay <- apply(d[, intersect(cfg$display, names(d)), drop = FALSE], 1,
#'                      function(r) paste(r, collapse = " "))
#'         d <- d[grepl(q, hay, ignore.case = TRUE), , drop = FALSE]
#'       }
#'       d
#'     })
#'     
#'     # ---- resolve FK columns to their labels for display --------------
#'     fk_maps <- shiny$reactive({
#'       refresh()
#'       out <- list()
#'       for (f in cfg$fields) if (identical(f$type, "fk")) out[[f$name]] <- admin_fk_choices(f)
#'       out
#'     })
#'     
#'     output$tbl <- renderReactable({
#'       d <- rows()
#'       maps <- fk_maps()
#'       
#'       cols <- list()
#'       for (nm in names(d)) {
#'         if (nm == cfg$pk && !is_code_pk(cfg)) { cols[[nm]] <- colDef(show = FALSE); next }
#'         if (nm == "active") {
#'           cols[[nm]] <- colDef(name = "STATUS", width = 110, cell = function(v) {
#'             if (isTRUE(v)) order_theme$chip("Active", "brand") else order_theme$chip("Inactive", "ink")
#'           })
#'           next
#'         }
#'         fld <- field_by_name(cfg, nm)
#'         if (!is.null(fld) && identical(fld$type, "fk")) {
#'           m <- maps[[nm]]
#'           cols[[nm]] <- colDef(name = toupper(fld$label), cell = function(v) {
#'             lbl <- names(m)[match(as.character(v), unname(m))]
#'             if (length(lbl) && !is.na(lbl)) lbl else "\u2014"
#'           })
#'           next
#'         }
#'         if (!nm %in% cfg$display) { cols[[nm]] <- colDef(show = FALSE); next }
#'         cols[[nm]] <- colDef(name = toupper(if (!is.null(fld)) fld$label else nm))
#'       }
#'       
#'       cols[["__act"]] <- colDef(
#'         name = "", sortable = FALSE, width = 170,
#'         cell = JS(sprintf("function(ci){
#'           var id = ci.row['%s'];
#'           var on = ci.row['active'];
#'           return '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + id + '\\', n: Math.random()})\">Edit</button> ' +
#'                  '<button class=\"btn btn-outline-secondary btn-sm\" onclick=\"Shiny.setInputValue(\\'%s\\', {id: \\'' + id + '\\', to: ' + (on ? 'false' : 'true') + ', n: Math.random()})\">' + (on ? 'Deactivate' : 'Reactivate') + '</button>';
#'         }", cfg$pk, ns("edit_row"), ns("toggle_row"))),
#'         html = TRUE
#'       )
#'       
#'       # rep(NA, nrow(d)), NOT NA. On a fresh database every reference table
#'       # is empty, and `d$x <- NA` on a zero-row frame raises
#'       #   "replacement has 1 row, data has 0"
#'       # so the very first page load crashed. rep() yields logical(0) and
#'       # assigns cleanly.
#'       d$`__act` <- rep(NA, nrow(d))
#'       
#'       reactable(d, columns = cols, defaultPageSize = 12, highlight = TRUE,
#'                 searchable = FALSE, compact = TRUE,
#'                 language = reactableLang(
#'                   noData = sprintf("No %s yet \u2014 use \u201cAdd %s\u201d to create the first one.",
#'                                    tolower(cfg$title), cfg$singular)
#'                 ),
#'                 theme = reactableTheme(borderColor = "#E9EFE6", highlightColor = "#F4F7F2"))
#'     })
#'     
#'     # ---- the form ----------------------------------------------------
#'     build_form <- function(current = NULL) {
#'       lapply(cfg$fields, function(f) {
#'         val <- if (!is.null(current)) current[[f$name]] else f$default
#'         lbl <- shiny$HTML(paste0(f$label,
#'                                  if (isTRUE(f$required)) " <span class='mandatory_star'>*</span>" else ""))
#'         # A code IS the primary key and the workflow refers to it by name.
#'         # Editable at creation, frozen forever after.
#'         locked <- identical(f$type, "code") && !is.null(current)
#'         inp <- switch(f$type,
#'                       text     = shiny$textInput(ns(f$name), lbl, value = chr(val)),
#'                       code     = shiny$textInput(ns(f$name), lbl, value = chr(val)),
#'                       textarea = shiny$textAreaInput(ns(f$name), lbl, value = chr(val)),
#'                       number   = shiny$numericInput(ns(f$name), lbl, value = if (is.null(val)) 0 else val),
#'                       select   = shiny$selectizeInput(ns(f$name), lbl, choices = f$choices, selected = chr(val)),
#'                       fk       = shiny$selectizeInput(ns(f$name), lbl, choices = admin_fk_choices(f),
#'                                                       selected = chr(val)),
#'                       shiny$textInput(ns(f$name), lbl, value = chr(val))
#'         )
#'         shiny$tagList(
#'           if (locked) shinyjs::disabled(inp) else inp,
#'           if (!is.null(f$help)) shiny$div(class = "text-muted",
#'                                           style = "font-size:11px; margin-top:-8px; margin-bottom:12px;",
#'                                           if (locked) "Code cannot be changed once created." else f$help)
#'         )
#'       })
#'     }
#'     
#'     open_modal <- function(current = NULL) {
#'       shiny$showModal(shiny$modalDialog(
#'         title = paste(if (is.null(current)) "Add" else "Edit", cfg$singular),
#'         shiny$div(class = "rtb-intake", order_theme$head_orders(),
#'                   order_theme$section("\u270e", cfg$title, build_form(current))),
#'         footer = shiny$tagList(
#'           shiny$modalButton("Cancel"),
#'           shiny$actionButton(ns("save"), "Save", class = "btn btn-success")
#'         ),
#'         easyClose = FALSE, size = "m"
#'       ))
#'     }
#'     
#'     shiny$observeEvent(input$add, { editing(NULL); open_modal(NULL) })
#'     
#'     shiny$observeEvent(input$edit_row, {
#'       d <- admin_list(cfg, include_inactive = TRUE)
#'       row <- d[as.character(d[[cfg$pk]]) == as.character(input$edit_row$id), , drop = FALSE]
#'       shiny$req(nrow(row) == 1)
#'       editing(row[[cfg$pk]][1])
#'       open_modal(as.list(row))
#'     })
#'     
#'     # ---- deactivate, never delete ------------------------------------
#'     shiny$observeEvent(input$toggle_row, {
#'       id <- input$toggle_row$id
#'       to <- isTRUE(input$toggle_row$to)
#'       n  <- if (!to) admin_usage(cfg, id) else NA_integer_
#'       
#'       shiny$showModal(shiny$modalDialog(
#'         title = if (to) "Reactivate" else "Deactivate",
#'         shiny$div(
#'           class = "rtb-intake", order_theme$head_orders(),
#'           shiny$p(if (to) {
#'             paste("Make this", cfg$singular, "selectable again?")
#'           } else {
#'             paste("Hide this", cfg$singular, "from new orders?",
#'                   "Nothing is deleted and existing records keep working.")
#'           }),
#'           if (!to && !is.na(n) && n > 0) {
#'             shiny$div(class = "update-hint",
#'                       sprintf("%d existing record%s already reference%s this. They are unaffected.",
#'                               n, if (n == 1) "" else "s", if (n == 1) "s" else ""))
#'           }
#'         ),
#'         footer = shiny$tagList(
#'           shiny$modalButton("Cancel"),
#'           shiny$actionButton(ns("confirm_toggle"), if (to) "Reactivate" else "Deactivate",
#'                              class = "btn btn-success")
#'         )
#'       ))
#'       editing(id)
#'     })
#'     
#'     shiny$observeEvent(input$confirm_toggle, {
#'       id <- editing(); to <- isTRUE(input$toggle_row$to)
#'       ok <- tryCatch({
#'         poolWithTransaction(pool, function(conn) admin_set_active(conn, cfg, id, to))
#'         TRUE
#'       }, error = function(e) { toastr_error(conditionMessage(e), title = "Failed"); FALSE })
#'       shiny$removeModal()
#'       if (ok) { toastr_success(if (to) "Reactivated." else "Deactivated."); refresh(refresh() + 1) }
#'     })
#'     
#'     # ---- save --------------------------------------------------------
#'     shiny$observeEvent(input$save, {
#'       vals <- list()
#'       missing <- character(0)
#'       for (f in cfg$fields) {
#'         v <- input[[f$name]]
#'         if (identical(f$type, "code") && !is.null(editing())) next   # frozen
#'         if (is.null(v) || (is.character(v) && !nzchar(trimws(v))) ||
#'             (is.numeric(v) && is.na(v))) {
#'           if (isTRUE(f$required)) missing <- c(missing, f$label)
#'           vals[[f$name]] <- NA
#'           next
#'         }
#'         if (identical(f$type, "fk")) {
#'           if (!nzchar(v)) {
#'             if (isTRUE(f$required)) missing <- c(missing, f$label)
#'             vals[[f$name]] <- NA
#'           } else vals[[f$name]] <- as.integer(v)
#'           next
#'         }
#'         if (identical(f$type, "code")) {
#'           v <- tolower(trimws(v))
#'           if (grepl("[^a-z0-9_]", v)) {
#'             toastr_error("Code must be lowercase letters, numbers and underscores only.",
#'                          title = "Invalid code")
#'             return()
#'           }
#'         }
#'         vals[[f$name]] <- if (identical(f$type, "number")) as.numeric(v) else v
#'       }
#'       
#'       if (length(missing)) {
#'         toastr_error(paste("Required:", paste(missing, collapse = ", ")), title = "Missing fields")
#'         return()
#'       }
#'       
#'       ok <- tryCatch({
#'         poolWithTransaction(pool, function(conn) {
#'           if (is.null(editing())) admin_insert(conn, cfg, vals)
#'           else admin_update(conn, cfg, editing(), vals)
#'         })
#'         TRUE
#'       }, error = function(e) {
#'         msg <- conditionMessage(e)
#'         # turn the two constraint failures users actually hit into English
#'         if (grepl("duplicate key|unique", msg, ignore.case = TRUE)) {
#'           toastr_error(paste("That", cfg$singular, "already exists."), title = "Duplicate")
#'         } else if (grepl("foreign key", msg, ignore.case = TRUE)) {
#'           toastr_error("A linked record is missing or inactive.", title = "Invalid reference")
#'         } else {
#'           toastr_error(msg, title = "Save failed", timeOut = 0)
#'         }
#'         FALSE
#'       })
#'       
#'       if (ok) {
#'         shiny$removeModal()
#'         toastr_success("Saved.")
#'         editing(NULL)
#'         refresh(refresh() + 1)
#'       }
#'     })
#'     
#'     list(refresh = refresh)
#'   })
#' }
#' 
#' # helpers ---------------------------------------------------------------
#' field_by_name <- function(cfg, nm) {
#'   for (f in cfg$fields) if (identical(f$name, nm)) return(f)
#'   NULL
#' }
#' is_code_pk <- function(cfg) {
#'   for (f in cfg$fields) if (identical(f$name, cfg$pk) && identical(f$type, "code")) return(TRUE)
#'   FALSE
#' }
#' chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)[1]