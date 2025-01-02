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
        generate_thrower_grade_panel(ns),
        bslib::accordion_panel(
          title = "Receiver Grade: A+"
        ),
        bslib::accordion_panel(
          title = "Defense Grade: A+"
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
        metrics = c("completions", "completions_per_possession", "games", "oOpportunities"),
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
    
    conn <- open_db_connection(db_path)
    session$onSessionEnded(function() {close_db_connection(conn)})

    all_player_stats <- get_all_player_stats(conn)  
    
    output$player_name <- renderText({
      req(input$player_selector)
      stats <- all_player_stats %>% filter(fullName == input$player_selector)
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

    overall_score <- reactive({
      req(thrower_percentiles)
      percentiles <- thrower_percentiles()
      median(c(
        percentiles$usage_percentile,
        percentiles$efficiency_percentile,
        percentiles$scoring_percentile,
        percentiles$contribution_percentile
      ))
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

    generate_plot_outputs("thrower", all_player_stats, input, output, thrower_plot_config)

    ### Grades
    output$overall_percentile <- renderText(paste0("(", round(overall_score(), 0), "th Percentile)"))
    output$overall_grade <- renderText(get_letter_grade(overall_score()))

    lapply(c("Contribution", "Usage", "Efficiency", "Scoring"), function(category) {
      category_lower <- tolower(category)
      
      output[[paste0(category_lower, "_grade")]] <- renderText({
        get_letter_grade(thrower_percentiles()[[paste0(category_lower, "_percentile")]])
      })
      
      output[[paste0(category_lower, "_percentile")]] <- renderText({
        paste0("(", thrower_percentiles()[[paste0(category_lower, "_percentile")]], "th Percentile)")
      })
    })
  })
}

