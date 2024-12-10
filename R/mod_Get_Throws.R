#' Get_Throws UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import reticulate
mod_Get_Throws_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("fetch_data"), "Fetch Game Data"),
    uiOutput(ns("loading_indicator")),
    tableOutput(ns("game_table"))
  )
}
    
#' Get_Throws Server Functions
#'
#' @noRd 
mod_Get_Throws_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    use_python("/Library/Frameworks/Python.framework/Versions/3.9/bin/python3", required = TRUE)
    py_config()
    py_install(c("pandas"), pip=TRUE)

    ns <- session$ns
    source_python("R/throws_data.py")
    
    game_data <- reactiveVal(data.frame())
    
    observeEvent(input$fetch_data, {
      output$loading_indicator <- renderUI({
        tagList("Fetching data... Please wait.")
      })
      
      tryCatch({
        ALL_GAMES <- fetch_game_data()
        game_data(as.data.frame(ALL_GAMES))
      }, error = function(e) {
        message("Error fetching data: ", e$message)
      })
      
      output$loading_indicator <- renderUI(NULL) # Remove loading message after completion
    })
    
    output$game_table <- renderTable({
      game_data()
    })
  })
}
    
## To be copied in the UI
# mod_Get_Throws_ui("Get_Throws_1")
    
## To be copied in the server
# mod_Get_Throws_server("Get_Throws_1")
