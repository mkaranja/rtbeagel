box::use(
  shiny,
  shinyWidgets[radioGroupButtons],
)

box::use(
  app/logic/fct_admin[ADMIN],
  app/view/administration/admin_crud,
  app/view/shared/order_theme,
)

# ============================================================================
# ADMINISTRATION · one page for every reference table
# ----------------------------------------------------------------------------
# WHY THIS EXISTS
#   admin_crud$ui() takes (id, cfg), which does not match how every other
#   module in this app is called - X$ui(ns("id")). Wiring it straight into
#   app_ui.R produced:
#       Error in administration$ui: argument "cfg" is missing, with no default
#   This module restores the convention: administration$ui(ns("administration"))
#   and administration$server("administration", res_auth, page, tab, ...).
#   The config is chosen here rather than at the call site.
#
#   It also collapses five sidebar entries - Customer, Crop Varieties, Test
#   Method, Sample Part, Sampling Bag - into ONE. They were five doors into
#   the same room. Eleven reference tables now live behind one menu item,
#   grouped by what they are for.
#
# LAZY: an entity's server is instantiated the first time it is opened, and
# once only. Eleven reactables built at startup for a page nobody may visit
# is eleven queries wasted, and re-instantiating a module server on every
# switch would stack duplicate observers.
# ============================================================================

GROUPS <- list(
  "Customers & crops" = c(customer = "Customers", crop = "Crops", variety = "Varieties"),
  "Lab setup"         = c(laboratory = "Laboratories", pathogen = "Pathogens",
                          test_method = "Test methods"),
  "Sample vocabulary" = c(sample_type = "Sample types", sample_condition = "Conditions",
                          sample_part = "Parts", sampling_bag = "Bags"),
  "Services"          = c(service_catalog = "Services")
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  order_theme$page(
    order_theme$page_header(
      title = "Administration",
      sub   = "Manage the reference data that every order and sample refers to."
    ),
    
    order_theme$toolbar(
      stack = TRUE,
      shiny$tagList(lapply(names(GROUPS), function(g) {
        shiny$div(
          class = "admin-group",
          shiny$span(class = "admin-group-label", g),
          radioGroupButtons(
            inputId  = ns(paste0("grp_", make.names(g))),
            label    = NULL,
            choices  = stats::setNames(names(GROUPS[[g]]), unname(GROUPS[[g]])),
            selected = character(0),
            status   = "outline-secondary",
            size     = "sm"
          )
        )
      }))
    ),
    
    shiny$uiOutput(ns("panel"))
  )
}


#' @export
server <- function(id, res_auth, page, tab, trigger_refresh = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    entity  <- shiny$reactiveVal("customer")
    started <- shiny$reactiveVal(character(0))
    
    # The four pill groups behave as ONE selector: picking in any group
    # clears the others, so exactly one entity is ever active.
    grp_ids <- vapply(names(GROUPS), function(g) paste0("grp_", make.names(g)), character(1))
    
    lapply(names(GROUPS), function(g) {
      gid <- paste0("grp_", make.names(g))
      shiny$observeEvent(input[[gid]], {
        shiny$req(input[[gid]])
        entity(input[[gid]])
        for (other in grp_ids) {
          if (!identical(other, gid)) {
            shinyWidgets::updateRadioGroupButtons(session, other, selected = character(0))
          }
        }
      }, ignoreInit = TRUE)
    })
    
    output$panel <- shiny$renderUI({
      key <- entity()
      shiny$req(key, !is.null(ADMIN[[key]]))
      admin_crud$ui(ns(key), ADMIN[[key]])
    })
    
    # Instantiate each entity's server once, on first open.
    shiny$observeEvent(entity(), {
      key <- entity()
      shiny$req(key, !is.null(ADMIN[[key]]))
      if (!key %in% started()) {
        admin_crud$server(key, ADMIN[[key]], res_auth)
        started(c(started(), key))
      }
    })
    
    invisible(NULL)
  })
}