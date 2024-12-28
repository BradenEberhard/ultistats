#' player_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
#' @import plotly
mod_player_display_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(get_HTML()),
    includeCSS("inst/app/www/full-screen-config.css"),
    bslib::page_sidebar(
      h1(textOutput(ns("player_name"))),
      sidebar = sidebar(
        selectizeInput(ns("player_selector"), "Player:", choices = NULL, options = list(maxItems = 1)),
        open=FALSE
      ),
      layout_sidebar(
        sidebar = sidebar(
          title="Controls",
          selectInput(ns("year_selector"), "Year", choices = NULL), 
          selectInput(ns("stat_category"), "Category", choices = c("Total", "Per Game", "Per Possession"), selected = "Total"),
          uiOutput(ns("handler_switch")),
          uiOutput(ns("offense_switch"))
        ),
        h2("Skill Percentiles"),
        withSpinner(plotlyOutput(ns("percentiles_plot")))
      ),
      card(
        card_header("Thrower Grade"),
        card_body(
          h1("A+")
        ),
        card_body(
          class = "thrower-card-full-screen",
          h1(textOutput(ns("player_name1"))),
          fluidRow(
            column(3, selectInput(ns("thrower_year_selector"), "Year", choices = NULL)),
            column(3, selectInput(ns("thrower_stat_category"), "Category", choices = c("Total", "Per Game", "Per Possession"), selected = "Total")),
          ),
          fluidRow(
            column(1, uiOutput(ns("thrower_handler_switch"))),
            column(1, uiOutput(ns("thrower_offense_switch")))
          ),
          layout_column_wrap(
            plotOutput(ns("radial_histogram_plot")) |> withSpinner() |> bslib::as_fill_carrier(),
            plotlyOutput(ns("thrower_usage_plot")) |> withSpinner() |> bslib::as_fill_carrier(),
            plotlyOutput(ns("thrower_efficiency_plot")) |> withSpinner() |> bslib::as_fill_carrier(),
            plotlyOutput(ns("thrower_metrics_plot")) |> withSpinner() |> bslib::as_fill_carrier(),
            min_height="1000px"
          )
        ),
        fill=FALSE,
        full_screen=TRUE
      )
    )
  )
}

    
#' player_display Server Functions
#' @import ggplot2
#' @import plotly
#' @importFrom tidyr gather
#' @noRd 
mod_player_display_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    db_path <- get_golem_config("db_path")
    
    conn <- open_db_connection(db_path)
    all_player_stats <- get_all_player_stats(conn)  
    close_db_connection(conn)

    output$player_name <- renderText({
      req(input$player_selector)
      stats <- all_player_stats %>% filter(fullName == input$player_selector)
      paste(stats$firstName[[1]], stats$lastName[[1]])
    })

    output$player_name1 <- renderText({
      req(input$player_selector)
      stats <- all_player_stats %>% filter(fullName == input$player_selector)
      paste(stats$firstName[[1]], stats$lastName[[1]])
    })

    selected_player_stats <- reactive({
      req(input$player_selector, input$year_selector)
      all_player_stats %>% 
        filter(fullName == input$player_selector & year == input$year_selector)
    })

    thrower_selected_player_stats <- reactive({
      req(input$player_selector, input$thrower_year_selector)
      all_player_stats %>% 
        filter(fullName == input$player_selector & year == input$thrower_year_selector)
    })

    observe({
      req(input$player_selector)
      
      # Freeze the value of the switches when a new player is selected
      freezeReactiveValue(input, "handler_switch_value")
      freezeReactiveValue(input, "offense_switch_value")
    })
    

    output$handler_switch <- renderUI({
      req(selected_player_stats())
      player_stats <- selected_player_stats()
      handler_label <- ifelse(player_stats$handler == TRUE, "Handler", "Cutter")
      input_switch(
        id = ns("handler_switch_value"),
        label = handler_label,
        value = FALSE
      )
    })

    output$offense_switch <- renderUI({
      req(selected_player_stats())
      player_stats <- selected_player_stats()
      offense_label <- ifelse(player_stats$offense == TRUE, "Offense", "Defense")
      input_switch(
        id = ns("offense_switch_value"),
        label = offense_label,
        value = FALSE
      )
    })

    output$thrower_handler_switch <- renderUI({
      req(thrower_selected_player_stats())
      player_stats <- thrower_selected_player_stats()
      handler_label <- ifelse(player_stats$handler == TRUE, "Handler", "Cutter")
      input_switch(
        id = ns("thrower_handler_switch_value"),
        label = handler_label,
        value = FALSE
      )
    })

    output$thrower_offense_switch <- renderUI({
      req(thrower_selected_player_stats())
      player_stats <- thrower_selected_player_stats()
      offense_label <- ifelse(player_stats$offense == TRUE, "Offense", "Defense")
      input_switch(
        id = ns("thrower_offense_switch_value"),
        label = offense_label,
        value = FALSE
      )
    })
    
    observeEvent(input$player_selector, {
      req(input$player_selector)
      freezeReactiveValue(input, "year_selector")
      freezeReactiveValue(input, "thrower_year_selector")
      stats <- all_player_stats %>% filter(fullName == input$player_selector)
      updateSelectInput(session, "year_selector", 
        choices = sort(stats$year), 
        selected = max(stats$year))
      updateSelectInput(session, "thrower_year_selector", 
        choices = sort(stats$year), 
        selected = max(stats$year))
    })
    
    output$percentiles_plot <- renderPlotly({
      req(input$player_selector, input$year_selector)
      addition <- ifelse(input$stat_category == "Per Possession", "(Per Possession)", ifelse(input$stat_category == "Per Game", "(Per Game)", ""))
      plot_data <- convert_to_metric_df(all_player_stats, input$stat_category, input$player_selector, input$year_selector, input$handler_switch_value, input$offense_switch_value) %>% rename_metrics()
      percentiles_plot <- create_percentiles_plot(plot_data, addition)
    })

    output$radial_histogram_plot <- renderPlot({
      req(input$player_selector, input$thrower_year_selector)
      player_id <- get_playerID_by_fullName(all_player_stats, input$player_selector)
      throws <- get_filtered_throws(db_path, player_id)
      throws <- if (input$thrower_year_selector == "Career") {
        throws  
      } else {
        throws %>% filter(year == input$thrower_year_selector)  
      }
      throws <- na.omit(throws$adjusted_angle)
      radial_histogram_plot(throws)
    })

    output$thrower_usage_plot <- renderPlotly({
      req(input$player_selector)
      percentiles_plot <- get_thrower_usage_plot(all_player_stats, input$player_selector, input$thrower_handler_switch_value, input$thrower_offense_switch_value)
    })

    output$thrower_efficiency_plot <- renderPlotly({
      req(input$player_selector)
      percentiles_plot <- get_thrower_efficiency_plot(all_player_stats, input$player_selector, input$thrower_handler_switch_value, input$thrower_offense_switch_value)
    })

    output$thrower_metrics_plot <- renderPlotly({
      req(input$player_selector)
      percentiles_plot <- get_thrower_metrics_plot(all_player_stats, input$player_selector, input$thrower_handler_switch_value, input$thrower_offense_switch_value)
    })

    observe({
      freezeReactiveValue(input, "player_selector")
      updateSelectizeInput(
        session, "player_selector", server = TRUE, 
        choices = unique(all_player_stats$fullName),
        selected = "Jordan Kerr"
      )
    })
  })
}

