box::use(
  shiny,
  bs4Dash[dashboardPage, dashboardSidebar, dashboardBody, sidebarMenu, menuItem,
          tabItems, tabItem, bs4DashNavbar, dashboardFooter, updateTabItems, tabBox,
          menuSubItem],
  shinyjs[useShinyjs, runjs, delay],
  waiter[spin_4],
  shinyFeedback[useShinyFeedback],
  DBI[dbExecute],
  future[plan, multisession],
)

box::use(
  app/view/barcode_station,
  app/view/orders/order_management,
  app/view/orders/order_registration,
  app/view/dashboard,
  app/view/quarantine,
  app/view/initiation/initiation,
  app/view/virus_indexing,
  app/view/thermotherapy,
  app/view/meristem_culture,
  app/view/surface_sterilization,
  app/view/subculture,
  app/view/hardening,
  app/view/administration/administration,
  app/view/shared/connectivity,
)

box::use(
  app/logic/fct_conn[pool],
  app/logic/fct_workflows[workflow_cache],
  app/view/shared/order_theme,
)

shiny$onStop(function() {
  if (exists("pool") && !is.null(pool)) {
    print("Draining database pool...")
    pool::poolClose(pool)
  }
})

plan(multisession)


#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  
  dashboardPage(
    preloader = list(html = shiny$tagList(spin_4(), "PLEASE WAIT..."), color = "#3d9970"),
    title = "RTB-EAGEL",
    dark = NULL,
    help = NULL,
    
    header = bs4DashNavbar(
      border = FALSE,
      rightUi = shiny$uiOutput(ns("user_profile_ui"))
    ),
    
    dashboardSidebar(
      skin = "light", status = "olive", elevation = 4, width = 4,
      
      shiny$div(
        style = "padding: 20px 15px; border-bottom: 1px solid #e9ecef; text-align: center;",
        shiny$img(src = "static/images/kephis.png", height = "60px"),
        shiny$div(
          shiny$tags$p("RTB-EAGEL",
                       style = "margin:0; font-weight:500; font-size:20px; letter-spacing: 4px;"),
          shiny$tags$span("LABORATORY PLATFORM",
                          style = "font-size:11px; letter-spacing: 1.5px; color:#6c757d;")
        )
      ),
      sidebarMenu(
        id = ns("sidebar"),
        
        #----------------------------------------------------
        # HOME
        #----------------------------------------------------
        menuItem(
          "DASHBOARD",
          tabName = "dashboard",
          icon = shiny$icon("gauge-high")
        ),
        
        #----------------------------------------------------
        # ORDERS
        #----------------------------------------------------
        menuItem(
          "ORDERS",
          icon = shiny$icon("clipboard-list"),
          
          menuSubItem(
            "Manage Orders",
            tabName = "tab_command",
            icon = shiny$icon("list-check")
          ),
          
          menuSubItem(
            "New Registration",
            tabName = "tab_reg"
          )
        ),
        
        #----------------------------------------------------
        # LABORATORY WORKFLOW
        #----------------------------------------------------
        menuItem(
          "LABORATORY",
          icon = shiny$icon("flask-vial"),
          startExpanded = FALSE,
          
          menuSubItem(
            "Quarantine",
            tabName = "quarantine",
            icon = shiny$icon("shield-virus")
          ),
          
          menuSubItem(
            "Virus Indexing",
            tabName = "vx",
            icon = shiny$icon("virus")
          ),
          
          menuSubItem(
            "Thermotherapy",
            tabName = "thermotherapy",
            icon = shiny$icon("temperature-high")
          ),
          
          menuSubItem(
            "Meristem Culture",
            tabName = "meristem",
            icon = shiny$icon("seedling")
          ),
          
          menuSubItem(
            "Surface Sterilization",
            tabName = "surface",
            icon = shiny$icon("flask")
          ),
          
          menuSubItem(
            "Multiplication/ Subculture",
            tabName = "subculture",
            icon = shiny$icon("layer-group")
          ),
          
          menuSubItem(
            "Hardening",
            tabName = "hardening",
            icon = shiny$icon("tree")
          ),
          
          menuSubItem(
            "Conservation",
            tabName = "conservation",
            icon = shiny$icon("warehouse")
          ),
          
          menuSubItem(
            "Distribution",
            tabName = "distribution",
            icon = shiny$icon("truck-fast")
          )
        ),
        
        #----------------------------------------------------
        # TOOLS
        #----------------------------------------------------
        menuItem(
          "TOOLS",
          icon = shiny$icon("toolbox"),
          
          menuSubItem(
            "Sample Search",
            tabName = "search",
            icon = shiny$icon("barcode")
          )
        ),
        
        #----------------------------------------------------
        # ADMINISTRATION
        #----------------------------------------------------
        menuItem(
          "ADMINISTRATION",
          icon = shiny$icon("users-gear"),
          
          menuSubItem(
            "Manage",
            tabName = "administration",
            icon = shiny$icon("gear")
          )
        )
      )
    ),
    
    dashboardBody(
      
      # ------------------------------------------------------------------
      # The design system, linked ONCE for the whole app.
      #
      # It used to be ~16KB of CSS inlined by head_orders(), which every
      # module calls - so the same stylesheet was re-sent on every render of
      # every page and could never be cached. It now lives in
      # app/static/css/style.css, served at static/css/style.css (the same
      # mount the sidebar logo already uses).
      #
      # head_orders() still exists and still works - it returns the font
      # links now - so no module call site had to change.
      # ------------------------------------------------------------------
      order_theme$theme_css(),
      
      # NOTE: the browser -> USB Zebra bridge is NOT script-tagged here.
      # It lives in app/js/zebra_print.js, is imported by app/js/index.js, and
      # `rhino::build_js()` bundles it into app/static/js/app.min.js which
      # Rhino includes automatically. Adding a <script> tag as well would load
      # the module twice and register the custom message handlers twice, so
      # every print job would be sent to the printer twice.
      
      useShinyjs(),
      useShinyFeedback(),
      connectivity$ui(ns("connectivity")),
      
      tabItems(
        
        tabItem(
          tabName = "search",
          barcode_station$ui(ns("barcode"))
        ),
        
        # ---- ORDERS ---------------------------------------------------
        # The tabBox sits DIRECTLY in the tabItem:
        #   ORDER MANAGEMENT       -> order_management (list -> detail)
        #   NEW ORDER REGISTRATION -> order_registration

        tabItem(
          tabName = "tab_command",
          order_management$ui(ns("order_mgmt"))
        ),
        tabItem(
          tabName = "tab_reg",
          order_registration$ui(ns("reg"))
        ),
        # ---- QUARANTINE -----------------------------------------------
        # ONE tab, not two. Glasshouse and growthroom were identical tables
        # and identical modules; they are now one module with a destination
        # picker, discriminated by stage_code.
        # ---- DASHBOARD -------------------------------------------------
        tabItem(
          tabName = "dashboard",
          dashboard$ui(ns("dashboard"))
        ),
        
        tabItem(
          tabName = "quarantine",
          quarantine$ui(ns("quarantine"))
        ),
        
        # ---- INITIATION -----------------------------------------------
        # Where per-sample tracking begins: a consignment on a bench becomes
        # individually coded explants.
        tabItem(
          tabName = "initiation",
          initiation$ui(ns("initiation"))
        ),
        
        tabItem(
          tabName = "vx",
          virus_indexing$ui(ns("virus"))
        ),
        
        # ---- THERMOTHERAPY --------------------------------------------
        # Conviron heat treatment; samples arrive from positive indexing,
        # the cassava express lane, or a restart.
        tabItem(
          tabName = "thermotherapy",
          thermotherapy$ui(ns("thermotherapy"))
        ),
        
        
        tabItem(
          tabName = "meristem",
          meristem_culture$ui(ns("m_culture"))
        ),
        
        
        tabItem(
          tabName = "surface",
          surface_sterilization$ui(ns("sterilization"))
        ),
        
        
        tabItem(
          tabName = "subculture",
          subculture$ui(ns("subculture"))
        ),
        
        tabItem(
          tabName = "hardening",
          hardening$ui(ns("hardening"))
        ),
        
        # ---- ADMINISTRATION -------------------------------------------
        tabItem(
          tabName = "administration",
          administration$ui(ns("admin"))
        )
      )
    ),
    
    footer = dashboardFooter(
      right = shiny$tags$small(
        class = "text-muted",
        style = "letter-spacing: 0.5px;",
        paste("Copyright \u00a9 ", format(Sys.Date(), "%Y"), "KEPHIS. All Rights Reserved.")
      )
    )
  )
}


#' @export
server <- function(id, res_auth) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    connectivity$server("connectivity")
    
    initialized_modules <- shiny$reactiveVal(character(0))
    trigger_refresh     <- shiny$reactiveVal(0)
    update_page         <- shiny$reactiveValues(go = 0, tab_name = NULL)
    
    current_tab <- shiny$reactive({
      shiny$req(input$sidebar)
      input$sidebar
    })
    
    
    
    # ------------------------------------------------------------------
    # SESSION INIT - once, as soon as we know who is logged in.
    #
    # 1. ensure_app_user(): shinymanager owns `credentials`; this app owns
    #    tbl_app_user. Every created_by / actor / reviewed_by FKs to
    #    tbl_app_user, so a never-seen user would fail on their FIRST write -
    #    including the activity-log insert below. The two are deliberately
    #    NOT FK'd together: shinymanager DELETEs from credentials when an
    #    account is removed, and lab history must outlive the account.
    #
    # 2. validate_workflow(): checks every (stage, state) in the YAML against
    #    tbl_stage_state and every target against tbl_stage. The old engine
    #    shipped with both `invitro_conservation` and `in_vitro_conservation`
    #    live and nothing noticed for months - it just quietly said "no next
    #    step defined". A typo now stops the app at startup, in front of
    #    whoever deployed it.
    # ------------------------------------------------------------------
    app_init <- shiny$reactive({
      shiny$req(res_auth$user)
      tryCatch(
        dbExecute(pool, "SELECT ensure_app_user($1, $2)",
                  params = list(res_auth$user, isTRUE(res_auth$admin))),
        error = function(e) message("ensure_app_user failed: ", conditionMessage(e))
      )
      tryCatch(
        workflow_cache(file.path("app", "static", "workflows", "cassava.yaml"), conn = pool),
        error = function(e) {
          message("WORKFLOW INVALID: ", conditionMessage(e))
          shiny$showNotification(
            paste("Workflow definition is invalid:", conditionMessage(e)),
            type = "error", duration = NULL
          )
        }
      )
      TRUE
    })
    
    shiny$observe({ app_init() })
    
    current_user <- shiny$reactive({
      shiny$req(res_auth$user)
      app_init()
      res_auth$user
    })
    
    output$user_profile_ui <- shiny$renderUI({
      shiny$req(res_auth$user)
      nm <- if (!is.null(res_auth$firstname) && nzchar(res_auth$firstname)) {
        res_auth$firstname
      } else {
        res_auth$user
      }
      shiny$div(
        class = "d-flex justify-content-between align-items-center",
        style = "padding: 10px 0;",
        shiny$div(
          shiny$tags$p(paste("Hi,", nm),
                       style = "margin:0; font-weight:400; font-size:14px; letter-spacing: 1px;")
        )
      )
    })
    
    # --- ACTIVITY LOGGING -----------------------------------------------
    log_user_activity <- function(tab_name) {
      tryCatch({
        # tbl_activity_log replaces user_activity_logs:
        #   user_name -> username, module_name -> module,
        #   access_time -> occurred_on (defaults to now(), so not sent).
        # `action` is NOT NULL, hence 'view'.
        dbExecute(pool,
                  "INSERT INTO tbl_activity_log (username, module, action) VALUES ($1, $2, 'view')",
                  params = list(current_user(), tab_name))
      }, error = function(e) {
        message("Activity Log Error: ", conditionMessage(e))
      })
    }
    
    # --- LAZY LOADING ---------------------------------------------------
    shiny$observeEvent(current_tab(), {
      tab <- current_tab()
      
      log_user_activity(tab)
      
      if (tab %in% initialized_modules()) {
        runjs("setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 200);")
        return()
      }
      
      tryCatch({
        switch(tab,
               "search" = {
                 barcode_station$server(
                   "barcode",
                   res_auth = res_auth,
                   page = "search",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               # Both panels live in one tab, so both servers start together.
               "tab_command" = {
                 order_management$server(
                   "order_mgmt",
                   res_auth = res_auth,
                   page = "tab_command",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "tab_reg" = {
                 order_registration$server(
                   "reg",
                   res_auth = res_auth,
                   page = "tab_reg",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "quarantine" = {
                 quarantine$server(
                   "quarantine",
                   res_auth = res_auth,
                   page = "quarantine",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "dashboard" = {
                 dashboard$server(
                   "dashboard",
                   res_auth = res_auth,
                   page = "dashboard",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "initiation" = {
                 initiation$server(
                   "initiation",
                   res_auth = res_auth,
                   page = "initiation",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "vx" = {
                 virus_indexing$server(
                   "virus",
                   res_auth = res_auth,
                   page = "initiation",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },# res_auth, page, tab, trigger_refresh
               
               "thermotherapy" = {
                 thermotherapy$server(
                   "thermotherapy",
                   res_auth = res_auth,
                   page = "thermotherapy",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "meristem" = {
                 meristem_culture$server(
                   "m_culture",
                   res_auth = res_auth,
                   page = "meristem",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "surface" = {
                 surface_sterilization$server(
                   "sterilization",
                   res_auth = res_auth,
                   page = "surface",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "subculture" = {
                 subculture$server(
                   "subculture",
                   res_auth = res_auth,
                   page = "subculture",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "hardening" = {
                 hardening$server(
                   "hardening",
                   res_auth = res_auth,
                   page = "hardening",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               },
               
               "administration" = {
                 administration$server(
                   "admin",
                   res_auth = res_auth,
                   page = "administration",
                   tab = current_tab,
                   trigger_refresh = trigger_refresh
                 )
               }
        )
        
        initialized_modules(c(initialized_modules(), tab))
        delay(450, runjs("window.dispatchEvent(new Event('resize'));"))
        
      }, error = function(e) {
        shiny$showNotification(paste("Critical Error loading module:", tab),
                               type = "error", duration = NULL)
        message("Server Error: ", conditionMessage(e))
      })
    })
    
    # --- CROSS-MODULE NAVIGATION ----------------------------------------
    shiny$observeEvent(update_page$go, {
      shiny$req(update_page$go > 0)
      updateTabItems(session, "sidebar", selected = update_page$tab_name)
    })
    
    # order_theme$goto() buttons land here.
    #
    # update_page above is the older path and nothing has ever set it - no
    # module receives it, because `page` is passed to modules as a static
    # string naming their own tab, not as this reactiveValues. So the receiving
    # half of cross-module navigation existed while the sending half did not.
    #
    # goto() sets ONE unnamespaced input instead. That is what makes it work
    # from any module without threading a reactiveValues through every server
    # signature, and it will work for modules not yet written.
    #
    # The tab name is checked against the menu before use: it arrives from the
    # browser, and an unrecognised value would otherwise blank the body by
    # selecting a tab that does not exist.
    shiny$observeEvent(input$rtb_goto, {
      req <- input$rtb_goto
      shiny$req(!is.null(req$tab))
      # Checked against the LIVE sidebar, not the tabItems. "initiation" has a
      # tabItem but its menuItem is commented out, so selecting it would leave
      # the sidebar with nothing highlighted. Initiation is reached through
      # quarantine's own tab strip instead.
      known <- c("dashboard", "orders", "quarantine", "vx", "thermotherapy",
                 "meristem", "surface", "subculture", "search", "administration")
      if (!req$tab %in% known) {
        message("rtb_goto: unknown tab '", req$tab, "' ignored")
        return(invisible(NULL))
      }
      updateTabItems(session, "sidebar", selected = req$tab)
    })
  })
}

