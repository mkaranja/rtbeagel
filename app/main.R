
box::use(
  shiny,
  shinymanager[secure_app, secure_server, check_credentials, set_labels],
  bslib[bs_theme]
)

box::use(
  app/view/layout
)


set_labels(
  language = "en",
  "Please authenticate" = "Roots, Tubers & Banana East Africa Germplasm Exchange Laboratory"
)

#' @export
ui <- 
  secure_app(
    enable_admin = TRUE,
    status = "success",
    background="olive",
    
    tags_top = shiny$tags$div(
      style = "
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 20px 40px;
    ",
      
      # Logo
      shiny$tags$div(
        shiny$img(src = "static/images/kephis.png", style = "height: 50px;")
      ),
      
      # Environment badge
      shiny$tags$span(
        "RTB-EAGEL",
        style = "
        background: #e9f7ef;
        color: #1e7e34;
        padding: 5px 12px;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 500;
      "
      )
    ),
    
    tags_bottom = shiny$tags$div(
      style = "
      text-align: center;
      padding: 15px;
      font-size: 14px;
      color: #6c757d;
    ",
      
      "© 2026 RTB-EAGEL • ",
      "Need help? ",
      shiny$tags$a(
        href = "mailto:m.karanja@cgiar.org",
        "Contact Support",
        style = "color:#0d6efd;"
      )
    ),
    
    shiny$bootstrapPage(
      layout$ui("app")
    )
  )

#' @export
server <- function(input, output, session) {
  lab_manager_email <- "m.karanja@cgiar.org"
  
  res_auth <- secure_server(
    check_credentials = check_credentials(db = "app/static/pg_template.yml"),
    inputs_list = list(
      laboratory = list(
        fun = "selectInput",
        args = list(choices = c("RTBEAGEL", "VIROLOGY", "TISSUECULTURE"), multiple = TRUE)),
      designation = list(
        fun = "selectInput",
        args = list(choices = c("admin","manager", "technician", "receptionist", "reviewer"), multiple = FALSE))
    ),
    timeout = 15,
    keep_token = TRUE
  )
  
  # res_auth <- shiny$reactiveValues(
  #   user = "mkaranja",
  #   firstname="Margaret",
  #   lastname="Karanja",
  #   admin = TRUE,
  #   role = "admin",
  #   email="mkaranja@gmail.com",
  #   laboratory = "rtbeagel"
  # )
  
  layout$server("app", res_auth = res_auth)
}