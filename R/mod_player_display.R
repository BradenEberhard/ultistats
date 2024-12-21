#' player_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_player_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      h1(textOutput(ns("player_name"))),
      sidebar = sidebar(
        selectInput(ns("year_selector"), "Year", choices = NULL), 
        open=FALSE
      ),
      layout_column_wrap(
        create_stat_value_box(title = "Goals", output_id = "goals", ns = ns),
        create_stat_value_box(title = "Assists", output_id = "assists", ns = ns),
        create_stat_value_box(title = "Hockey Assists", output_id = "hockeyAssists", ns = ns),
        create_stat_value_box(title = "Completions", output_id = "completions", ns = ns),
        create_stat_value_box(title = "Turnovers", output_id = "throwaways", ns = ns),
        create_stat_value_box(title = "Receiving Yards", output_id = "yardsReceived", ns = ns),
        create_stat_value_box(title = "Throwing Yards", output_id = "yardsThrown", ns = ns),
        create_stat_value_box(title = "Blocks", output_id = "blocks", ns = ns)
      ),
      card(
        plotOutput(ns("radial_histogram_plot"))
      )
    )
  )
}

    
#' player_display Server Functions
#' @import ggplot2
#' @noRd 
mod_player_display_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    player_id <- "jkerr"  # You can dynamically set this player ID based on user input or another source
    db_path <- get_golem_config("db_path")
    
    # Reactive expression to get player stats
    player_stats <- reactive({
      conn <- open_db_connection(db_path)
      stats <- get_player_stats(conn, player_id)  # Assuming this function fetches the player data
      close_db_connection(conn)
      stats
    })


    filtered_stats <- reactive({
      stats <- player_stats()
      if (input$year_selector == "Career") {
        return(stats)  # Return all stats for the player
      } else {
        return(stats[stats$year == input$year_selector, ])  # Filter stats by year
      }
    })

    filtered_throws <- reactive({
      req(player_stats)
      conn <- open_db_connection(db_path)
      player_throws <- get_player_throws(conn, player_stats()$playerID[[1]])
      close_db_connection(conn)

      player_throws$adjusted_angle <- (player_throws$throw_angle + 90) %% 360 - 180
      player_throws$year <- substr(player_throws$gameID, 1, 4)

      if (input$year_selector == "Career") {
        return(player_throws)  # Return all stats for the player
      } else {
        return(player_throws[player_throws$year == input$year_selector, ])  # Filter stats by year
      }
  })
    output$player_name <- renderText({
      stats <- player_stats()
      paste(stats$firstName[[1]], stats$lastName[[1]])
    })
    output$goals <- render_stat("goals", filtered_stats)
    output$assists <- render_stat("assists", filtered_stats)
    output$hockeyAssists <- render_stat("hockeyAssists", filtered_stats)
    output$completions <- render_stat("completions", filtered_stats)
    output$throwaways <- render_stat("throwaways", filtered_stats)
    output$yardsReceived <- render_stat("yardsReceived", filtered_stats)
    output$yardsThrown <- render_stat("yardsThrown", filtered_stats)
    output$blocks <- render_stat("blocks", filtered_stats)


    observe({
      stats <- player_stats()
      updateSelectInput(session, "year_selector", choices = c("Career", sort(stats$year)), selected = "Career")
    })

    output$radial_histogram_plot <- renderPlot({
      test_data <- data.frame(
        angle = c(0, 90, 180, 270, 360),
        value = c(1, 2, 3, 4, 5)
      )
      bin_cutoffs <- seq(-180, 180, by = 24)
      throws <- na.omit(filtered_throws()$adjusted_angle)
      radial_plot <- ggplot(data.frame(throws), aes(x = throws)) +
        geom_histogram(breaks = bin_cutoffs, fill = "blue", color = "white", boundary = 0) +
        coord_polar(start = pi) +
        scale_x_continuous(limits = c(-180, 180), breaks = bin_cutoffs) +
        theme_minimal() +
        labs(
          title = "Radial Histogram of Throw Angles",
          x = "Angle (Degrees)",
          y = "Frequency"
        )

      radial_plot
    })
  })
}

    
## To be copied in the UI
# mod_player_display_ui("player_display_1")
    
## To be copied in the server
# mod_player_display_server("player_display_1")
