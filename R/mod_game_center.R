#' game_center UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_game_center_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("game_id"), "Select Game ID:", choices = NULL),
    plotlyOutput(ns("win_prob_plot"))  |> 
      withSpinner() |> 
      bslib::as_fill_carrier()
  )
}
    
#' game_center Server Functions
#'
#' @noRd 
#' @importFrom stringr str_split
mod_game_center_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    pool <- get_db_pool()
    throws_data <- get_table_from_db(pool, table_name = "throws")
    updateSelectInput(session, "game_id", choices = unique(throws_data$gameID), selected = throws_data$gameID[1])
    throws_data <- get_win_probabilities(throws_data)


    output$win_prob_plot <- renderPlotly({
      req(input$game_id)
      game_id_to_plot <- input$game_id
      game_info <- get_game_info(pool, input$game_id)
      away_team <- game_info$awayTeamID %>% tools::toTitleCase()
      home_team <- game_info$homeTeamID %>% tools::toTitleCase()
      
      game_data <- throws_data %>%
        filter(gameID == game_id_to_plot,
        game_quarter != 6,
        !is.na(graphing_win_prob)) %>%
        arrange(game_time_left)

      game_data <- game_data %>%
        mutate(
          minutes = as.integer(floor(time_left / 60)),  # Ensure integer
          seconds = as.integer(time_left %% 60),  # Ensure integer
          formatted_time = sprintf("%d:%02d left in Q%d", minutes, seconds, game_quarter)  # Format string
        )

      
      # Create custom hover text
      game_data$hover_text <- paste(game_data$formatted_time,
                                    "<br>Possession:", ifelse(game_data$is_home_team, home_team, away_team),
                                    "<br>Yards To Go:", 100 - round(game_data$thrower_y),
                                    "<br>", str_split(input$game_id, "-")[[1]][5], game_data$home_team_score, str_split(input$game_id, "-")[[1]][4], game_data$away_team_score)
      game_data$point_color <- ifelse(game_data$possession_num == 1 & game_data$possession_throw == 1, " #446e9b", "#333333")

      
      # Create the plotly plot
      plot_ly(game_data, x = ~game_time_left, y = ~graphing_win_prob, type = 'scatter', mode = 'lines+markers',
          text = ~hover_text, hoverinfo = 'text', 
          line = list(color = 'black'), 
          marker = list(color = ~point_color)) %>%  # Apply point color
      layout(title = paste(away_team, "at", home_team),
          xaxis = list(title = "Time Left", tickvals = c(0, 720, 1440, 2160, 2880), 
          ticktext = c("Final", "Q4", "Q3", "Q2", "Q1"), autorange = "reversed", zeroline = FALSE),
          yaxis = list(title = "Win Probability", tickvals = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1), 
                        ticktext = c("100%", "75%", "50%", "25%", "0%", "25%", "50%", "75%", "100%"),
                        range = c(-1.1, 1.1), zeroline = FALSE), 
                        shapes = list(
                          list(type = "line", x0 = 720, x1 = 720, y0 = -1.1, y1 = 1.1, 
                               line = list(color = "#999999", dash = "dot", width = 2), opacity = 0.8, layer = "below"),
                          list(type = "line", x0 = 1440, x1 = 1440, y0 = -1.1, y1 = 1.1, 
                               line = list(color = "#999999", dash = "dot", width = 2), opacity = 0.8, layer = "below"),
                          list(type = "line", x0 = 2160, x1 = 2160, y0 = -1.1, y1 = 1.1, 
                               line = list(color = "#999999", dash = "dot", width = 2), opacity = 0.8, layer = "below"),
                          list(type = "line", x0 = 2880, x1 = 2880, y0 = -1.1, y1 = 1.1, 
                               line = list(color = "#999999", dash = "dot", width = 2), opacity = 0.8, layer = "below"),
                          list(type = "line", x0 = 0, x1 = 0, y0 = -1.1, y1 = 1.1, 
                               line = list(color = "#999999", dash = "dot", width = 2), opacity = 0.8, layer = "below")
                        ), dragmode = FALSE) %>% config(displayModeBar = FALSE)
    })
  })
}

