#' GameEvents Class
#'
#' @description The `GameEvents` class provides a structured way to manage game-related events,
#' including teams, scores, and events for both home and away teams. It interacts with an external
#' API using a proxy instance.
#'
#' @details This class encapsulates the following functionalities:
#' - Stores details about a specific game (e.g., teams, scores, events).
#' - Handles communication with an API through a provided proxy instance.
#' - Provides fields and methods to retrieve and process game events.
#'
#' @noRd
GameEvents <- R6::R6Class(
  "GameEvents",
  
  public = list(
    gameID = NULL,
    event_handlers = NULL,
    home_team = NULL,
    away_team = NULL,
    home_teamID = NULL,
    away_teamID = NULL,
    start_time = NULL,
    home_score = NULL,
    away_score = NULL,
    home_events = NULL,
    away_events = NULL,

    #' @description Initializes the GameEvents class.
    #' 
    #' @param gameID A string representing the game ID.
    #' @param proxy An instance of the GameEventsProxy class to handle API requests.
    #' @export
    initialize = function(gameID) {
      self$gameID <- gameID
      self$event_handlers <- EventHandlers$new()
    },

    #' @description Processes the game events for both home and away teams.
    #' 
    #' This function processes the game events for both the home and away teams by calling
    #' the respective handler functions for each event. It ensures that all events are handled
    #' according to the event type.
    #' @export
    process_game_events = function() {
      source('./classes/team_events.R')
      self$home_team <- TeamEvents$new(self$home_events, TRUE)  # Assuming TeamEvents is an R6 class
      stopifnot(length(self$home_team$game_events) > 0)  # Ensure there are game events
      for (i in 1:nrow(self$home_team$game_events)) {
        event <- self$home_team$game_events[i, ]
        event_type <- self$event_handlers$event_dict[[event$type]]
        if (!(event_type %in% names(self$event_handlers$handle_function_dict))) {
          stop('Event type is not coded yet')
        }
        self$event_handlers$handle_function_dict[[event_type]](self$home_team, event)
      }
      processed_data <- list()
      # Loop over each column in processed_events
      for (col_name in names(self$home_team$processed_events[[1]])) {
        # Extract the column data
        column_data <- sapply(self$home_team$processed_events, function(event) {
          if (is.null(event[[col_name]])) NA else event[[col_name]]
        })
        if (col_name == "current_line") {
          processed_data[[col_name]] <- I(column_data)
        } else {
          processed_data[[col_name]] <- column_data
        }
      }
      self$home_team$processed_events <- as.data.frame(processed_data)

      self$away_team <- TeamEvents$new(self$away_events, FALSE)
      for (i in 1:nrow(self$away_team$game_events)) {
        event <- self$away_team$game_events[i, ]
        event_type <- self$event_handlers$event_dict[[event$type]]
        if (!(event_type %in% names(self$event_handlers$handle_function_dict))) {
          stop('Event type is not coded yet')
        }
        self$event_handlers$handle_function_dict[[event_type]](self$away_team, event)
      }
      processed_data <- list()
      # Loop over each column in processed_events
      for (col_name in names(self$away_team$processed_events[[1]])) {
        # Extract the column data
        column_data <- sapply(self$away_team$processed_events, function(event) {
          if (is.null(event[[col_name]])) NA else event[[col_name]]
        })
        if (col_name == "current_line") {
          processed_data[[col_name]] <- I(column_data)
        } else {
          processed_data[[col_name]] <- column_data
        }
      }
      self$away_team$processed_events <- as.data.frame(processed_data)
    },

    #' @description Combines the processed events from both teams into a single data frame.
    #' 
    #' This function combines the processed events for both home and away teams into a single
    #' data frame, allowing for further analysis and filtering. The resulting data frame can be
    #' customized by choosing which columns to include or exclude.
    #' 
    #' @param gameID A logical indicating whether to include the gameID column in the final data frame.
    #' @param home_team A logical indicating whether to include the home_teamID column in the final data frame.
    #' @param away_team A logical indicating whether to include the away_teamID column in the final data frame.
    #' @param start_time A logical indicating whether to include the start_time column in the final data frame.
    #' @param drop_cols A character vector of column names to exclude from the final data frame.
    #' @return A data frame with the processed game events.
    #' @export
    get_events_df = function(gameID = TRUE, home_team = FALSE, away_team = FALSE, start_time = FALSE, drop_cols = list()) {
      if (is.null(self$home_team)) {
        print('Events not processed yet. Please run process_game_events() first')
        return(NULL)
      }

      final_df <- bind_rows(self$home_team$processed_events, self$away_team$processed_events) %>%
        as.data.frame() %>%
        {if (!is.null(drop_cols) && length(drop_cols) > 0) dplyr::select(., -all_of(drop_cols)) else .} %>%
        {if (!is.null(gameID) && gameID) mutate(., gameID = self$gameID) else .} %>%
        {if (!is.null(home_team) && home_team) mutate(., home_teamID = self$home_teamID) else .} %>%
        {if (!is.null(away_team) && away_team) mutate(., away_teamID = self$away_teamID) else .} %>%
        {if (!is.null(start_time) && start_time) mutate(., start_time = self$start_time) else .}


      final_df$total_points <- final_df$home_team_score + final_df$away_team_score
      final_df <- final_df %>%
        arrange(total_points, possession_num, desc(start_on_offense), possession_throw) %>%
        select(-total_points)

      return(final_df)
    }
  )
)
