box::use(
  shiny,
  pool[poolClose]
)

box::use(
  app/logic/fct_conn[pool, load_data,],
  app/view/shared/order_theme,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  
  # The theme scope matters: every class this module emits is defined under
  # .rtb-intake, and this module is embedded inside view_order rather than
  # mounted as its own page, so it cannot rely on a parent page() wrapper.
  shiny$div(
    class = "rtb-intake",
    shiny$uiOutput(ns("fileUI"))
  )
}

#' @export
server <- function(id, project_code) {
  shiny$moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Get files for selected project
    project_files <- shiny$reactive({
      shiny$req(project_code)
      files <- load_data(pool, glue::glue( "SELECT * FROM file_uploads WHERE project_code = '{project_code}' ORDER BY upload_timestamp DESC"))
      files$file_path <- normalizePath(files$file_path, mustWork = FALSE)
      files
    })
    
    # Reactive value to store current preview file
    current_preview <- shiny$reactiveValues(file = NULL)
    
    # Main files list UI
    output$fileUI <- shiny$renderUI({
      shiny$req(project_code)
      files <- project_files()
      
      if (nrow(files) == 0) {
        return(order_theme$empty_state(
          title   = "No files attached",
          message = "Evidence, certificates and lab reports uploaded against this order will appear here.",
          icon    = "paperclip"
        ))
      }
      
      shiny$tagList(
        order_theme$subhead("Attached files"),
        shiny$div(
          class = "file-list",
          lapply(seq_len(nrow(files)), function(i) {
            file <- files[i, ]
            shiny$div(
              class = "file-row",
              shiny$div(
                class = "fr-main",
                shiny$downloadLink(
                  ns(paste0("download_", i)),
                  file$original_filename,
                  class = "fr-name"
                ),
                if (!is.na(file$description) && nzchar(file$description)) {
                  shiny$div(class = "fr-desc", file$description)
                }
              ),
              shiny$div(
                class = "fr-when",
                format(file$upload_timestamp, "%d %b %Y %H:%M")
              )
            )
          })
        )
      )
    })
    
    # output$fileUI <- shiny$renderUI({
    #   shiny$req(project_code)
    #   files <- project_files()
    #   
    #   if (nrow(files) == 0) {
    #     return(shiny$tags$div(
    #       style = "background-color: #fff3cd; color: #856404; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
    #       "No files found for this project"))
    #   }
    #   
    #   shiny$tagList(
    #     shiny$tags$p(style = "background-color: #F2F0EF; color: #3d9970; padding: 10px; border-radius: 5px; border: 1px solid #F2F0EF;",
    #              paste("Files for project:", project_code)),
    #     lapply(seq_len(nrow(files)), function(i) {
    #       file <- files[i, ]
    #       
    #       shiny$tags$div(
    #         style = "display: flex; align-items: center; justify-content: space-between; padding: 20px 0; border-bottom: 1px solid #eee;",
    #         
    #         
    #         # Download button (right side)
    #         shiny$downloadLink(
    #           ns(paste0("download_", i)),
    #           shiny$tags$p(file$original_filename) 
    #         ),
    #         
    #         # File metadata (center)
    #         shiny$tags$div(
    #           style = "display: flex; gap: 20px; flex: 2; font-size: 0.9em; color: #666;",
    #           shiny$tags$span(paste("Comments: ", file$description)),
    #           shiny$tags$span(paste("Date:", format(file$upload_timestamp, "%Y-%m-%d %H:%M")))
    #         )
    #       )
    #     })
    #   )
    # 
    # })
    
    # Observe preview button clicks for each file
    shiny$observe({
      files <- project_files()
      shiny$req(nrow(files) > 0)
      
      lapply(seq_len(nrow(files)), function(i) {
        shiny$observeEvent(input[[paste0("preview_btn_", i)]], {
          current_preview$file <- files[i, ]
          shiny$showModal(previewModal())
        })
      })
    })
    
    # Preview modal dialog
    previewModal <- function() {
      file <- current_preview$file
      ext <- tolower(tools::file_ext(file$file_path))
      
      modalContent <- if (!file.exists(file$file_path)) {
        shiny$tags$div(class = "alert alert-danger", "File not found")
      } else if (ext %in% c("png", "jpg", "jpeg", "gif")) {
        shiny$tags$img(src = file$file_path, class = "modal-img")
      } else if (ext == "pdf") {
        shiny$tags$iframe(src = file$file_path, class = "modal-pdf")
      } else if (ext %in% c("txt", "csv")) {
        shiny$tags$div(class = "modal-text", paste(readLines(file$file_path, warn = FALSE), collapse = "\n"))
      } else {
        shiny$tags$div("Preview not available for this file type")
      }
      
      shiny$modalDialog(
        title = file$original_filename,
        modalContent,
        easyClose = TRUE,
        footer = shiny$modalButton("Close"),
        size = "l"
      )
    }
    
    # Create dynamic download handlers
    shiny$observe({
      files <- project_files()
      shiny$req(nrow(files) > 0)
      
      lapply(seq_len(nrow(files)), function(i) {
        file <- files[i, ]
        output_name <- paste0("download_", i)
        
        output[[output_name]] <- shiny$downloadHandler(
          filename = function() {
            file$original_filename
          },
          content = function(file_out) {
            shiny$req(file.exists(file$file_path))
            file.copy(file$file_path, file_out)
          }
        )
      })
    })
    
    # Close database connection on app stop
    # shiny$onStop(function() {
    #   poolClose(pool)
    # })
  })
}

#' box::use(
#'   shiny,
#'   pool[poolClose]
#' )
#' 
#' box::use(
#'   app/logic/fct_conn[pool, load_data,],
#'   app/view/shared/order_theme,
#' )
#' 
#' #' @export
#' ui <- function(id) {
#'   ns <- shiny$NS(id)
#'   
#'   # The theme scope matters: every class this module emits is defined under
#'   # .rtb-intake, and this module is embedded inside view_order rather than
#'   # mounted as its own page, so it cannot rely on a parent page() wrapper.
#'   shiny$div(
#'     class = "rtb-intake",
#'     shiny$uiOutput(ns("fileUI"))
#'   )
#' }
#' 
#' #' @export
#' server <- function(id, project_code) {
#'   shiny$moduleServer(id, function(input, output, session) {
#'     
#'     ns <- session$ns
#'     
#'     # Get files for selected project
#'     project_files <- shiny$reactive({
#'       shiny$req(project_code)
#'       files <- load_data(pool, glue::glue( "SELECT * FROM file_uploads WHERE project_code = '{project_code}' ORDER BY upload_timestamp DESC"))
#'       files$file_path <- normalizePath(files$file_path, mustWork = FALSE)
#'       files
#'     })
#'     
#'     # Reactive value to store current preview file
#'     current_preview <- shiny$reactiveValues(file = NULL)
#'     
#'     # Main files list UI
#'     output$fileUI <- shiny$renderUI({
#'       shiny$req(project_code)
#'       files <- project_files()
#'       
#'       if (nrow(files) == 0) {
#'         return(order_theme$empty_state(
#'           title   = "No files attached",
#'           message = "Evidence, certificates and lab reports uploaded against this order will appear here.",
#'           icon    = "paperclip"
#'         ))
#'       }
#'       
#'       shiny$tagList(
#'         order_theme$subhead("Attached files"),
#'         shiny$div(
#'           class = "file-list",
#'           lapply(seq_len(nrow(files)), function(i) {
#'             file <- files[i, ]
#'             shiny$div(
#'               class = "file-row",
#'               shiny$div(
#'                 class = "fr-main",
#'                 shiny$downloadLink(
#'                   ns(paste0("download_", i)),
#'                   file$original_filename,
#'                   class = "fr-name"
#'                 ),
#'                 if (!is.na(file$description) && nzchar(file$description)) {
#'                   shiny$div(class = "fr-desc", file$description)
#'                 }
#'               ),
#'               shiny$div(
#'                 class = "fr-when",
#'                 format(file$upload_timestamp, "%d %b %Y %H:%M")
#'               )
#'             )
#'           })
#'         )
#'       )
#'     })
#'     
#'     # output$fileUI <- shiny$renderUI({
#'     #   shiny$req(project_code)
#'     #   files <- project_files()
#'     #   
#'     #   if (nrow(files) == 0) {
#'     #     return(shiny$tags$div(
#'     #       style = "background-color: #fff3cd; color: #856404; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
#'     #       "No files found for this project"))
#'     #   }
#'     #   
#'     #   shiny$tagList(
#'     #     shiny$tags$p(style = "background-color: #F2F0EF; color: #3d9970; padding: 10px; border-radius: 5px; border: 1px solid #F2F0EF;",
#'     #              paste("Files for project:", project_code)),
#'     #     lapply(seq_len(nrow(files)), function(i) {
#'     #       file <- files[i, ]
#'     #       
#'     #       shiny$tags$div(
#'     #         style = "display: flex; align-items: center; justify-content: space-between; padding: 20px 0; border-bottom: 1px solid #eee;",
#'     #         
#'     #         
#'     #         # Download button (right side)
#'     #         shiny$downloadLink(
#'     #           ns(paste0("download_", i)),
#'     #           shiny$tags$p(file$original_filename) 
#'     #         ),
#'     #         
#'     #         # File metadata (center)
#'     #         shiny$tags$div(
#'     #           style = "display: flex; gap: 20px; flex: 2; font-size: 0.9em; color: #666;",
#'     #           shiny$tags$span(paste("Comments: ", file$description)),
#'     #           shiny$tags$span(paste("Date:", format(file$upload_timestamp, "%Y-%m-%d %H:%M")))
#'     #         )
#'     #       )
#'     #     })
#'     #   )
#'     # 
#'     # })
#'     
#'     # Observe preview button clicks for each file
#'     shiny$observe({
#'       files <- project_files()
#'       shiny$req(nrow(files) > 0)
#'       
#'       lapply(seq_len(nrow(files)), function(i) {
#'         shiny$observeEvent(input[[paste0("preview_btn_", i)]], {
#'           current_preview$file <- files[i, ]
#'           shiny$showModal(previewModal())
#'         })
#'       })
#'     })
#'     
#'     # Preview modal dialog
#'     previewModal <- function() {
#'       file <- current_preview$file
#'       ext <- tolower(tools::file_ext(file$file_path))
#'       
#'       modalContent <- if (!file.exists(file$file_path)) {
#'         shiny$tags$div(class = "alert alert-danger", "File not found")
#'       } else if (ext %in% c("png", "jpg", "jpeg", "gif")) {
#'         shiny$tags$img(src = file$file_path, class = "modal-img")
#'       } else if (ext == "pdf") {
#'         shiny$tags$iframe(src = file$file_path, class = "modal-pdf")
#'       } else if (ext %in% c("txt", "csv")) {
#'         shiny$tags$div(class = "modal-text", paste(readLines(file$file_path, warn = FALSE), collapse = "\n"))
#'       } else {
#'         shiny$tags$div("Preview not available for this file type")
#'       }
#'       
#'       shiny$modalDialog(
#'         title = file$original_filename,
#'         modalContent,
#'         easyClose = TRUE,
#'         footer = shiny$modalButton("Close"),
#'         size = "l"
#'       )
#'     }
#'     
#'     # Create dynamic download handlers
#'     shiny$observe({
#'       files <- project_files()
#'       shiny$req(nrow(files) > 0)
#'       
#'       lapply(seq_len(nrow(files)), function(i) {
#'         file <- files[i, ]
#'         output_name <- paste0("download_", i)
#'         
#'         output[[output_name]] <- shiny$downloadHandler(
#'           filename = function() {
#'             file$original_filename
#'           },
#'           content = function(file_out) {
#'             shiny$req(file.exists(file$file_path))
#'             file.copy(file$file_path, file_out)
#'           }
#'         )
#'       })
#'     })
#'     
#'     # Close database connection on app stop
#'     # shiny$onStop(function() {
#'     #   poolClose(pool)
#'     # })
#'   })
#' }
#' 
#' #' box::use(
#' #'   shiny,
#' #'   pool[poolClose]
#' #' )
#' #' 
#' #' box::use(
#' #'   app/logic/fct_conn[pool, load_data,],
#' #' )
#' #' 
#' #' #' @export
#' #' ui <- function(id) {
#' #'   ns <- shiny$NS(id)
#' #'   
#' #'   shiny$uiOutput(ns("fileUI"))
#' #' }
#' #' 
#' #' #' @export
#' #' server <- function(id, project_code) {
#' #'   shiny$moduleServer(id, function(input, output, session) {
#' #'     
#' #'     ns <- session$ns
#' #'     
#' #'     # Get files for selected project
#' #'     project_files <- shiny$reactive({
#' #'       shiny$req(project_code)
#' #'       files <- load_data(pool, glue::glue( "SELECT * FROM file_uploads WHERE project_code = '{project_code}' ORDER BY upload_timestamp DESC"))
#' #'       files$file_path <- normalizePath(files$file_path, mustWork = FALSE)
#' #'       files
#' #'     })
#' #'     
#' #'     # Reactive value to store current preview file
#' #'     current_preview <- shiny$reactiveValues(file = NULL)
#' #'     
#' #'     # Main files list UI
#' #'     output$fileUI <- shiny$renderUI({
#' #'       shiny$req(project_code)
#' #'       files <- project_files()
#' #'       
#' #'       if (nrow(files) == 0) {
#' #'         return(shiny$tags$div(
#' #'           style = "background-color: #fff3cd; color: #856404; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
#' #'           "No files found for this project"))
#' #'       }
#' #'       
#' #'       shiny$tagList(
#' #'         shiny$tags$p(
#' #'           style = "background-color: #F2F0EF; color: #3d9970; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
#' #'           "Project files"
#' #'         ),
#' #'         
#' #'         shiny$tags$table(
#' #'           style = "width: 100%; border-collapse: collapse;",
#' #'           
#' #'           # Table header
#' #'           shiny$tags$thead(
#' #'             shiny$tags$tr(
#' #'               shiny$tags$th(style = "padding: 10px; border-bottom: 2px solid #dee2e6; text-align: left;", "File Name"),
#' #'               shiny$tags$th(style = "padding: 10px; border-bottom: 2px solid #dee2e6; text-align: left;", "Comments"),
#' #'               shiny$tags$th(style = "padding: 10px; border-bottom: 2px solid #dee2e6; text-align: left;", "Upload Date")
#' #'             )
#' #'           ),
#' #'           
#' #'           # Table body
#' #'           shiny$tags$tbody(
#' #'             lapply(seq_len(nrow(files)), function(i) {
#' #'               file <- files[i, ]
#' #'               
#' #'               shiny$tags$tr(
#' #'                 style = if (i %% 2 == 0) "background-color: #f8f9fa;" else "",
#' #'                 
#' #'                 shiny$tags$td(
#' #'                   style = "padding: 10px; border-bottom: 1px solid #eee;",
#' #'                   shiny$downloadLink(
#' #'                     ns(paste0("download_", i)),
#' #'                     file$original_filename,
#' #'                     style = "color: #007bff; text-decoration: none;"
#' #'                   )
#' #'                 ),
#' #'                 
#' #'                 shiny$tags$td(
#' #'                   style = "padding: 10px; border-bottom: 1px solid #eee; color: #666;",
#' #'                   file$description
#' #'                 ),
#' #'                 
#' #'                 shiny$tags$td(
#' #'                   style = "padding: 10px; border-bottom: 1px solid #eee; color: #666;",
#' #'                   format(file$upload_timestamp, "%Y-%m-%d %H:%M")
#' #'                 )
#' #'               )
#' #'             })
#' #'           )
#' #'         )
#' #'       )
#' #'     })
#' #'     # output$fileUI <- shiny$renderUI({
#' #'     #   shiny$req(project_code)
#' #'     #   files <- project_files()
#' #'     #   
#' #'     #   if (nrow(files) == 0) {
#' #'     #     return(shiny$tags$div(
#' #'     #       style = "background-color: #fff3cd; color: #856404; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
#' #'     #       "No files found for this project"))
#' #'     #   }
#' #'     #   
#' #'     #   shiny$tagList(
#' #'     #     shiny$tags$p(style = "background-color: #F2F0EF; color: #3d9970; padding: 10px; border-radius: 5px; border: 1px solid #F2F0EF;",
#' #'     #              paste("Files for project:", project_code)),
#' #'     #     lapply(seq_len(nrow(files)), function(i) {
#' #'     #       file <- files[i, ]
#' #'     #       
#' #'     #       shiny$tags$div(
#' #'     #         style = "display: flex; align-items: center; justify-content: space-between; padding: 20px 0; border-bottom: 1px solid #eee;",
#' #'     #         
#' #'     #         
#' #'     #         # Download button (right side)
#' #'     #         shiny$downloadLink(
#' #'     #           ns(paste0("download_", i)),
#' #'     #           shiny$tags$p(file$original_filename) 
#' #'     #         ),
#' #'     #         
#' #'     #         # File metadata (center)
#' #'     #         shiny$tags$div(
#' #'     #           style = "display: flex; gap: 20px; flex: 2; font-size: 0.9em; color: #666;",
#' #'     #           shiny$tags$span(paste("Comments: ", file$description)),
#' #'     #           shiny$tags$span(paste("Date:", format(file$upload_timestamp, "%Y-%m-%d %H:%M")))
#' #'     #         )
#' #'     #       )
#' #'     #     })
#' #'     #   )
#' #'     # 
#' #'     # })
#' #'     
#' #'     # Observe preview button clicks for each file
#' #'     shiny$observe({
#' #'       files <- project_files()
#' #'       shiny$req(nrow(files) > 0)
#' #'       
#' #'       lapply(seq_len(nrow(files)), function(i) {
#' #'         shiny$observeEvent(input[[paste0("preview_btn_", i)]], {
#' #'           current_preview$file <- files[i, ]
#' #'           shiny$showModal(previewModal())
#' #'         })
#' #'       })
#' #'     })
#' #'     
#' #'     # Preview modal dialog
#' #'     previewModal <- function() {
#' #'       file <- current_preview$file
#' #'       ext <- tolower(tools::file_ext(file$file_path))
#' #'       
#' #'       modalContent <- if (!file.exists(file$file_path)) {
#' #'         shiny$tags$div(class = "alert alert-danger", "File not found")
#' #'       } else if (ext %in% c("png", "jpg", "jpeg", "gif")) {
#' #'         shiny$tags$img(src = file$file_path, class = "modal-img")
#' #'       } else if (ext == "pdf") {
#' #'         shiny$tags$iframe(src = file$file_path, class = "modal-pdf")
#' #'       } else if (ext %in% c("txt", "csv")) {
#' #'         shiny$tags$div(class = "modal-text", paste(readLines(file$file_path, warn = FALSE), collapse = "\n"))
#' #'       } else {
#' #'         shiny$tags$div("Preview not available for this file type")
#' #'       }
#' #'       
#' #'       shiny$modalDialog(
#' #'         title = file$original_filename,
#' #'         modalContent,
#' #'         easyClose = TRUE,
#' #'         footer = shiny$modalButton("Close"),
#' #'         size = "l"
#' #'       )
#' #'     }
#' #'     
#' #'     # Create dynamic download handlers
#' #'     shiny$observe({
#' #'       files <- project_files()
#' #'       shiny$req(nrow(files) > 0)
#' #'       
#' #'       lapply(seq_len(nrow(files)), function(i) {
#' #'         file <- files[i, ]
#' #'         output_name <- paste0("download_", i)
#' #'         
#' #'         output[[output_name]] <- shiny$downloadHandler(
#' #'           filename = function() {
#' #'             file$original_filename
#' #'           },
#' #'           content = function(file_out) {
#' #'             shiny$req(file.exists(file$file_path))
#' #'             file.copy(file$file_path, file_out)
#' #'           }
#' #'         )
#' #'       })
#' #'     })
#' #'     
#' #'     # Close database connection on app stop
#' #'     # shiny$onStop(function() {
#' #'     #   poolClose(pool)
#' #'     # })
#' #'   })
#' #' }