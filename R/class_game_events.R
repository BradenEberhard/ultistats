#' GameEvents Class
#'
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


    initialize = function(gameID) {
      self$gameID <- gameID
      self$event_handlers <- EventHandlers$new()
    },


    process_game_events = function() {
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
