library(shiny)
library(shinymanager)

#### init the SQL Database
# first edit the .yml configuration file
system.file(paste0(getwd(),"/app/static/pg_template.yml"), package = "shinymanager")


# Init Credentials data
credentials <- data.frame(
  user = c("mkaranja", "jane"),
  password = c("azerty", "12345"), # password will automatically be hashed
  admin = c(TRUE, FALSE),
  email = c("wambuikaranja003@gmail.com", "jdoe@gmail.com"),
  stringsAsFactors = FALSE
)

# Create SQL database
create_sql_db(
  credentials_data = credentials,
  config_path = paste0(getwd(),"/app/static/pg_template.yml")
)

### Use in shiny
ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output")
)

# Wrap your UI with secure_app
ui <- secure_app(ui, choose_language = TRUE)


server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(db = paste0(getwd(),"/app/static/pg_template.yml"))
  )
  
  # your classic server logic
  
}

shinyApp(ui, server)
