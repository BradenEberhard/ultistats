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
        column(6, h1(textOutput(ns("player_name")))),  # Set the h1 to take 6 columns
        column(6, selectizeInput(ns("player_selector"), "Search for Player:", choices = NULL, options = list(maxItems = 1, hideSelected = TRUE)))  # Set the selectizeInput to take 6 columns
      ),
      fluidRow(
        card(
          h2("Skill Percentiles"),
          withSpinner(plotlyOutput(ns("percentiles_plot")))
        )
      ),
      bslib::accordion(
        id = ns("accordion"),
        open = FALSE,
        bslib::accordion_panel(
          title = "Thrower Grade:",
          id = ns("thrower_grade"),  # ID for this accordion item
          page_fluid(
            layout_column_wrap(
              fillable=FALSE,
              div(
                fluidRow(
                  card(
                    class = "mb-1 text-center",
                    card_header("Overall Grade:"),
                    div(
                      style = "display: flex; justify-content: center; align-items: baseline; gap: 10px; padding: 0;",
                      h2(
                        textOutput(ns("overall_grade")),
                        style = "margin: 0;"  # Remove extra space around the grade text
                      ),
                      div(
                        textOutput(ns("overall_percentile")),
                        style = "font-size: smaller; margin: 0;"  # Make the percentile smaller
                      )
                    )
                  )
                ),
                fluidRow(
                  layout_column_wrap(
                    class="mx-0 px-0",
                    width=1/2,
                    !!!lapply(c("Contribution", "Efficiency", "Scoring", "Usage"), function(category) {
                      card(
                        class = "mx-0 px-0 ml-0 pl-0 text-center", 
                        card_header(paste0(category, ":"), style = "padding: 5px; margin-bottom: 0 px;"),
                        div(
                          style = "display: flex; justify-content: center; align-items: baseline; gap: 10px; padding: 0;",
                          h2(
                            textOutput(ns(paste0(tolower(category), "_grade"))),
                            style = "margin: 0;"  # Remove extra space around the grade text
                          ),
                          div(
                            textOutput(ns(paste0(tolower(category), "_percentile"))),
                            style = "font-size: smaller; margin: 0;"  # Make the percentile smaller
                          )
                        )
                      )
                    })
                  )
                )
              ),
              plotOutput(ns("radial_histogram_plot")) |> withSpinner() |> bslib::as_fill_carrier(),
              width=1/2
            ),
            layout_column_wrap(
              width=1/4,
              girafeOutput(ns("thrower_contribution_plot")) |> withSpinner() |> bslib::as_fill_carrier(),
              girafeOutput(ns("thrower_efficiency_plot")) |> withSpinner() |> bslib::as_fill_carrier(),
              girafeOutput(ns("thrower_scores_plot")) |> withSpinner() |> bslib::as_fill_carrier(),
              girafeOutput(ns("thrower_usage_plot")) |> withSpinner() |> bslib::as_fill_carrier()
            )
          )
        ),
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
  
    observeEvent(input$player_selector, {
      req(input$player_selector)
      freezeReactiveValue(input, "year_selector")
      stats <- all_player_stats %>% filter(fullName == input$player_selector)
      updateSelectInput(session, "year_selector", 
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
      req(input$player_selector, input$year_selector)
      player_id <- get_playerID_by_fullName(all_player_stats, input$player_selector)
      throws <- get_filtered_throws(db_path, player_id)
      throws <- if (input$year_selector == "Career") {
        throws  
      } else {
        throws %>% filter(year == input$year_selector)  
      }
      throws <- na.omit(throws$adjusted_angle)
      radial_histogram_plot(throws)
    })

    output$thrower_usage_plot <- renderGirafe({
      req(input$player_selector)
      percentiles_plot <- get_thrower_usage_plot(all_player_stats, input$player_selector, input$handler_switch_value, input$offense_switch_value)
    })

    output$thrower_efficiency_plot <- renderGirafe({
      req(input$player_selector)
      percentiles_plot <- get_thrower_efficiency_plot(all_player_stats, input$player_selector, input$handler_switch_value, input$offense_switch_value)
    })

    output$thrower_scores_plot <- renderGirafe({
      req(input$player_selector)
      percentiles_plot <- get_thrower_scores_plot(all_player_stats, input$player_selector, input$handler_switch_value, input$offense_switch_value)
    })

    output$thrower_contribution_plot <- renderGirafe({
      req(input$player_selector)
      percentiles_plot <- get_thrower_contribution_plot(all_player_stats, input$player_selector, input$handler_switch_value, input$offense_switch_value)
    })

    observe({
      freezeReactiveValue(input, "player_selector")
      updateSelectizeInput(
        session, "player_selector", server = TRUE, 
        choices = unique(all_player_stats$fullName),
        selected = "Jordan Kerr"
      )
    })

    thrower_percentiles <- reactive({
      req(input$player_selector, input$year_selector)
      get_thrower_grade(
        df = all_player_stats, 
        player_full_name = input$player_selector, 
        handler_value = input$handler_switch_value, 
        offense_value = input$offense_switch_value,
        player_year = input$year_selector
      )
    })
    
    output$overall_percentile <- renderText({
      req(thrower_percentiles)
      percentiles <- thrower_percentiles()
      overall_score <- median(
        c(
          percentiles$usage_percentile,
          percentiles$efficiency_percentile,
          percentiles$scoring_percentile,
          percentiles$contribution_percentile
        ),
        na.rm = TRUE
      )
      paste0("(", round(overall_score, 0), "th Percentile)")
    })

    output$overall_grade <- renderText({
      req(thrower_percentiles)
      percentiles <- thrower_percentiles()
      overall_score <- median(
        c(
          percentiles$usage_percentile,
          percentiles$efficiency_percentile,
          percentiles$scoring_percentile,
          percentiles$contribution_percentile
        ),
        na.rm = TRUE
      )
      get_letter_grade(overall_score)
    })

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

