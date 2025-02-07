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
      shinybrowser::detect(),
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
          girafeOutput(ns("skill_percentiles_plot"))  |> 
            withSpinner() |> 
            bslib::as_fill_carrier()
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
mod_player_display_server <- function(id, player_name) {
  moduleServer(id, function(input, output, session) {
    ### Variables
    ns <- session$ns
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
    
    pool <- get_db_pool()
    all_player_stats <- get_all_player_stats(pool)  

    cached_player_name <- reactiveVal(NULL)
    observeEvent(input$player_selector, {
      if(input$player_selector != ""){
        cached_player_name(input$player_selector)
      }
    })

    cached_player_year <- reactiveVal(NULL)
    observeEvent(input$year_selector, {
      cached_player_year(input$year_selector)
    })
    
    observeEvent(cached_player_name(), {
      req(cached_player_name(), all_player_stats)
      stats <- all_player_stats %>% filter(.data$fullName == cached_player_name())
      output$player_name <- renderText({
        paste(stats$firstName[[1]], stats$lastName[[1]])
      })
      output$handler_switch <- renderUI({
        create_switch("handler_switch_value", selected_player_stats()$handler, "Handler", "Cutter", selected_player_stats, ns)
      })
      output$offense_switch <- renderUI({
        create_switch("offense_switch_value", selected_player_stats()$offense, "Offense", "Defense", selected_player_stats, ns)
      })
    })
    
    ### Inputs
    
    observeEvent(cached_player_name(), {
      update_year_selector(cached_player_name(), all_player_stats, session)
    })
    
    observe({
      req(all_player_stats, player_name)
      relevant_players <- subset(all_player_stats, year >= 2021) %>% .[!duplicated(.$fullName), ] %>% pull(fullName)
      updateSelectizeInput(session, "player_selector", server = TRUE, choices = relevant_players, selected = player_name())
    })
    
    ### Reactive Variables
    selected_player_stats <- reactive(get_selected_player_stats(cached_player_name(), cached_player_year(), all_player_stats))
    
    thrower_percentiles <- reactive({
      req(cached_player_name(), cached_player_year(), !is.null(input$handler_switch_value), !is.null(input$offense_switch_value))
      get_thrower_grade(
        input = list(player_selector = cached_player_name(), year_selector = cached_player_year(), handler_switch_value = input$handler_switch_value, offense_switch_value = input$offense_switch_value),
        df = all_player_stats,
        selected_player = cached_player_name()
      )
    })
    
    receiver_percentiles <- reactive({
      req(cached_player_name(), cached_player_year(), !is.null(input$handler_switch_value), !is.null(input$offense_switch_value))
      get_receiver_grade(
        input = list(player_selector = cached_player_name(), year_selector = cached_player_year(), handler_switch_value = input$handler_switch_value, offense_switch_value = input$offense_switch_value),
        df = all_player_stats,
        selected_player = cached_player_name()
      )
    })
    
    defense_percentiles <- reactive({
      req(cached_player_name(), cached_player_year(), !is.null(input$handler_switch_value), !is.null(input$offense_switch_value))
      get_defense_grade(
        input = list(player_selector = cached_player_name(), year_selector = cached_player_year(), handler_switch_value = input$handler_switch_value, offense_switch_value = input$offense_switch_value),
        df = all_player_stats,
        selected_player = cached_player_name()
      )
    })
    
    ### Plots

    output$skill_percentiles_plot <- renderGirafe({
      req(cached_player_name(), cached_player_year(), input$stat_category, !is.null(input$handler_switch_value), !is.null(input$offense_switch_value))
      isolate({
        input_list = list(
          player_selector = cached_player_name(),
          year_selector = cached_player_year(),
          stat_category = input$stat_category,
          handler_switch_value = input$handler_switch_value,
          offense_switch_value = input$offense_switch_value
        )
        plot <- generate_skill_percentiles_plot(input_list, session, all_player_stats)
      })
      plot
    })
    
    
    output$thrower_radial_histogram_plot <- renderPlot({
      generate_radial_histogram_plot(
        input = list(player_selector = cached_player_name(), year_selector = cached_player_year()),
        player_selector = cached_player_name(),
        year_selector = cached_player_year(),
        all_player_stats = all_player_stats,
        role = "thrower"
      )
    })
    
    output$receiver_radial_histogram_plot <- renderPlot({
      generate_radial_histogram_plot(
        input = list(player_selector = cached_player_name(), year_selector = cached_player_year()),
        player_selector = cached_player_name(),
        year_selector = cached_player_year(),
        all_player_stats = all_player_stats,
        role = "receiver"
      )
    })
    
    generate_plot_outputs(
      "thrower", all_player_stats, 
      list(
        player_selector = cached_player_name(), 
        year_selector = cached_player_year(), 
        handler_switch_value = input$handler_switch_value, 
        offense_switch_value = input$offense_switch_value
      ), 
      output, 
      thrower_plot_config
    )
    
    generate_plot_outputs(
      "receiver", all_player_stats, 
      list(
        player_selector = cached_player_name(), 
        year_selector = cached_player_year(), 
        handler_switch_value = input$handler_switch_value, 
        offense_switch_value = input$offense_switch_value
      ), 
      output, 
      thrower_plot_config
    )   
    
    generate_plot_outputs(
      "defense", all_player_stats, 
      list(
        player_selector = cached_player_name(), 
        year_selector = cached_player_year(), 
        handler_switch_value = input$handler_switch_value, 
        offense_switch_value = input$offense_switch_value
      ), 
      output, 
      thrower_plot_config
    )    
    
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
  
  