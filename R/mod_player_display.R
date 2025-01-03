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
#' @importFrom ggiraph girafeOutput
#' @importFrom plotly plotlyOutput
mod_player_display_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(get_HTML()),
    bslib::page_sidebar(
      sidebar = sidebar(
        title="Controls",
        selectInput(ns("year_selector"), "Year", choices = NULL), 
        selectInput(ns("stat_category"), "Category", choices = c("Total", "Per Game", "Per Possession"), selected = "Total"),
        uiOutput(ns("handler_switch")),
        uiOutput(ns("offense_switch"))
      ),
      fluidRow(
        column(6, h1(textOutput(ns("player_name")))), 
        column(6, selectizeInput(ns("player_selector"), "Search for Player:", choices = NULL, options = list(maxItems = 1, hideSelected = TRUE)))  # Set the selectizeInput to take 6 columns
      ),
      fluidRow(
        card(
          h2("Skill Percentiles"),
          withSpinner(plotlyOutput(ns("skill_percentiles_plot")))
        )
      ),
      bslib::accordion(
        id = ns("player_accordion"),
        open = FALSE,
        generate_grade_panel(ns, "thrower", c("Usage", "Contribution", "Scores", "Efficiency")),
        generate_grade_panel(ns, "receiver", c("Usage", "Contribution", "Efficiency")),
        bslib::accordion_panel(
          title = "Defense Grade:",
          id = ns("defense_grade"),
          page_fluid(
            div(
              fluidRow(
                generate_grade_card(ns, "Overall", "defense")
              ),
              fluidRow(
                layout_column_wrap(
                  class = "mx-0 px-0",
                  width = 1/2,
                  !!!lapply(c("Usage", "Efficiency"), function(category) generate_grade_card(ns, category, "defense"))
                )
              )
            ),
            layout_columns(
              !!!lapply(c("Usage", "Efficiency"), function(category) {
                girafeOutput(ns(paste0("defense_", tolower(category), "_plot"))) |> 
                  withSpinner() |> 
                  bslib::as_fill_carrier()
              })
            )  
          )
        )
      )
    )
  )
}

    
#' player_display Server Functions
#' @import ggplot2
#' @importFrom plotly renderPlotly config layout add_trace add_segments plot_ly 
#' @importFrom tidyr gather
#' @importFrom ggiraph renderGirafe
#' @noRd 
mod_player_display_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ### Variables
    ns <- session$ns
    db_path <- get_golem_config("db_path")
    thrower_plot_config <- list(
      usage = list(
        metrics = c("completions", "completions_per_possession", "games", "offensive_points_per_game"),
        label = "usage",
        title = "Usage"
      ),
      efficiency = list(
        metrics = c("completion_percentage", "xcp", "cpoe", "offensive_efficiency"),
        label = "efficiency",
        title = "Efficiency"
      ),
      scores = list(
        metrics = c("assists_per_possession", "hockeyAssists_per_possession", "turnovers_per_possession"),
        label = "scores",
        title = "Scoring Per 100 Possessions"
      ),
      contribution = list(
        metrics = c("yardsThrown_per_possession", "thrower_ec_per_possession", "thrower_aec_per_possession"),
        label = "contribution",
        title = "Contribution Per 100 Possessions"
      )
    )

    receiver_plot_config <- list(
      usage = list(
        metrics = c("receptions", "receptions_per_possession", "games", "offensive_points_per_game"),
        label = "usage",
        title = "Usage"
      ),
      efficiency = list(
        metrics = c("offensive_efficiency", "receiver_aec_per_possession"),
        label = "efficiency",
        title = "Efficiency"
      ),
      contribution = list(
        metrics = c("yardsReceived_per_possession", "goals_per_possession", "drops_per_possession"),
        label = "contribution",
        title = "Contribution Per 100 Possessions"
      )
    )

    defense_plot_config <- list(
      usage = list(
        metrics = c("games", "defensive_points_per_game"),
        label = "usage",
        title = "Usage"
      ),
      efficiency = list(
        metrics = c("defensive_efficiency", "blocks", "blocks_per_possession"),
        label = "efficiency",
        title = "Efficiency"
      )
    )
    
    conn <- open_db_connection(db_path)
    session$onSessionEnded(function() {close_db_connection(conn)})

    all_player_stats <- get_all_player_stats(conn)  
    
    output$player_name <- renderText({
      req(input$player_selector, all_player_stats)
      stats <- all_player_stats %>% filter(.data$fullName == input$player_selector)
      paste(stats$firstName[[1]], stats$lastName[[1]])
    })

    ### Inputs
    output$handler_switch <- renderUI(create_switch("handler_switch_value", selected_player_stats()$handler, "Handler", "Cutter", selected_player_stats, ns))
    output$offense_switch <- renderUI(create_switch("offense_switch_value", selected_player_stats()$offense, "Offense", "Defense", selected_player_stats, ns))

    observeEvent(input$player_selector, update_year_selector(input$player_selector, all_player_stats, session))
    observe({
      freezeReactiveValue(input, "player_selector")
      updateSelectizeInput(session, "player_selector", server = TRUE, choices = unique(all_player_stats$fullName), selected = "Jordan Kerr")
    })

    ### Reactive Variables
    selected_player_stats <- reactive(get_selected_player_stats(input$player_selector, input$year_selector, all_player_stats))

    thrower_percentiles <- reactive({
      req(input$player_selector, input$year_selector)
      get_thrower_grade(
        input = input,
        df = all_player_stats
      )
    })

    receiver_percentiles <- reactive({
      req(input$player_selector, input$year_selector)
      get_receiver_grade(
        input = input,
        df = all_player_stats
      )
    })

    defense_percentiles <- reactive({
      req(input$player_selector, input$year_selector)
      get_defense_grade(
        input = input,
        df = all_player_stats
      )
    })

    ### Plots
    output$skill_percentiles_plot <- renderPlotly(generate_skill_percentiles_plot(input, session, all_player_stats))
  
    output$thrower_radial_histogram_plot <- renderPlot({
      generate_radial_histogram_plot(
        input = input,
        player_selector = input$player_selector,
        year_selector = input$year_selector,
        all_player_stats = all_player_stats,
        db_path = db_path,
        role = "thrower"
      )
    })

    output$receiver_radial_histogram_plot <- renderPlot({
      generate_radial_histogram_plot(
        input = input,
        player_selector = input$player_selector,
        year_selector = input$year_selector,
        all_player_stats = all_player_stats,
        db_path = db_path,
        role = "receiver"
      )
    })

    generate_plot_outputs("thrower", all_player_stats, input, output, thrower_plot_config)

    generate_plot_outputs("receiver", all_player_stats, input, output, receiver_plot_config)

    generate_plot_outputs("defense", all_player_stats, input, output, defense_plot_config)

    ### Grades

    lapply(c("Overall", "Contribution", "Usage", "Efficiency", "Scores"), function(category) {
      category_lower <- tolower(category)
      
      output[[paste0("thrower_", category_lower, "_grade")]] <- renderText({
        get_letter_grade(thrower_percentiles()[[paste0(category_lower, "_percentile")]])
      })
      
      output[[paste0("thrower_", category_lower, "_percentile")]] <- renderText({
        paste0("(", thrower_percentiles()[[paste0(category_lower, "_percentile")]], "th Percentile)")
      })
    })

    lapply(c("Overall", "Contribution", "Usage", "Efficiency"), function(category) {
      category_lower <- tolower(category)
      
      output[[paste0("receiver_", category_lower, "_grade")]] <- renderText({
        get_letter_grade(receiver_percentiles()[[paste0(category_lower, "_percentile")]])
      })
      
      output[[paste0("receiver_", category_lower, "_percentile")]] <- renderText({
        paste0("(", receiver_percentiles()[[paste0(category_lower, "_percentile")]], "th Percentile)")
      })
    })

    lapply(c("Overall", "Usage", "Efficiency"), function(category) {
      category_lower <- tolower(category)
      
      output[[paste0("defense_", category_lower, "_grade")]] <- renderText({
        get_letter_grade(defense_percentiles()[[paste0(category_lower, "_percentile")]])
      })
      
      output[[paste0("defense_", category_lower, "_percentile")]] <- renderText({
        paste0("(", defense_percentiles()[[paste0(category_lower, "_percentile")]], "th Percentile)")
      })
    })
  })
}

