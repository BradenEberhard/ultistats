#' GameEvents Class
#'
#' This class handles the game events for a specific game by retrieving game information and events,
#' processing the events, and providing a data frame of the processed events.
#'
#' @import dplyr
#' @export
#' 

ThrowEvent = R6::R6Class(
  "ThrowEvent",
  
  public = list(
    thrower = NULL,
    thrower_x = NULL,
    thrower_y = NULL,
    receiver = NULL,
    receiver_x = NULL,
    receiver_y = NULL,
    turnover = NULL,
    
    #' @description Constructor for ThrowEvent
    #' @param thrower The player who threw the disc.
    #' @param thrower_x The x-coordinate of the thrower.
    #' @param thrower_y The y-coordinate of the thrower.
    #' @param receiver The player who received the disc.
    #' @param receiver_x The x-coordinate of the receiver.
    #' @param receiver_y The y-coordinate of the receiver.
    #' @param turnover Flag indicating whether the throw resulted in a turnover.
    initialize = function(thrower, thrower_x, thrower_y, receiver, receiver_x, receiver_y, turnover) {
      self$thrower <- thrower
      self$thrower_x <- thrower_x
      self$thrower_y <- thrower_y
      self$receiver <- receiver
      self$receiver_x <- receiver_x
      self$receiver_y <- receiver_y
      self$turnover <- turnover
    }
  )
)


PullEvent = R6::R6Class(
  "PullEvent",
  
  public = list(
    puller = NULL,
    pull_x = NULL,
    pull_y = NULL,
    pull_ms = NULL,
    
    #' @description Constructor for PullEvent
    #' @param puller The player who made the pull.
    #' @param pull_x The x-coordinate of the pull.
    #' @param pull_y The y-coordinate of the pull.
    #' @param pull_ms The timestamp of the pull in milliseconds.
    initialize = function(puller = NULL, pull_x = NULL, pull_y = NULL, pull_ms = NULL) {
      self$puller <- puller
      self$pull_x <- pull_x
      self$pull_y <- pull_y
      self$pull_ms <- pull_ms
    }
  )
)

EventHandlers <- R6::R6Class("EventHandlers",
  public = list(
    event_dict = NULL,
    handle_function_dict = NULL,

    #' @description Initializes the EventHandlers class.
    initialize = function() {
      self$event_dict <- list(
        '1' = 'start D point',
        '2' = 'start O point',
        '3' = 'midpoint timeout - recording team',
        '4' = 'endpoint timeout - recording team',
        '5' = 'midpoint timeout - opposing team',
        '6' = 'endpoint timeout - opposing team',
        '7' = 'inbounds pull',
        '8' = 'outbounds pull',
        '9' = 'offsides - recording team',
        '10' = 'offsides - opposing team',
        '11' = 'block',
        '12' = 'callahan caught',
        '13' = 'throwaway - opposing team',
        '14' = 'stall - opposing team',
        '15' = 'score - opposing team',
        '16' = 'penalty - recording team',
        '17' = 'penalty - opposing team',
        '18' = 'pass',
        '19' = 'score - recording team',
        '20' = 'drop',
        '21' = 'dropped pull',
        '22' = 'throwaway - recording team',
        '23' = 'callahan thrown',
        '24' = 'stall - recording team',
        '25' = 'injury',
        '26' = 'pmf',
        '27' = 'player ejected',
        '28' = 'end of first',
        '29' = 'end of second',
        '30' = 'end of third',
        '31' = 'end of fourth',
        '32' = 'end of ot',
        '33' = 'end of double ot',
        '34' = 'delayed',
        '35' = 'postponed'
      )
      self$handle_function_dict <- list(
        'start D point' = self$handle_start_D_point, 'start O point' = self$handle_start_O_point,
        'inbounds pull' = self$handle_inbounds_pull, 'score - opposing team' = self$handle_score_opposing_team,
        'score - recording team' = self$handle_score_recording_team, 'pass' = self$handle_pass, 
        'throwaway - recording team' = self$handle_throwaway_recording_team,
        'throwaway - opposing team' = self$handle_throwaway_opposing_team, 'block' = self$handle_block,
        'callahan thrown' = self$handle_callahan_thrown, 'drop' = self$handle_drop, 
        'end of first' = self$handle_end_of_quarter, 'end of second' = self$handle_end_of_quarter,
        'end of third' = self$handle_end_of_quarter, 'end of fourth' = self$handle_end_of_quarter,
        'injury' = self$handle_line_change, 'midpoint timeout - opposing team' = self$handle_line_change,
        'midpoint timeout - recording team' = self$handle_line_change, 'outbounds pull' = self$handle_outbounds_pull,
        'penalty - recording team' = self$handle_penalty_recording_team, 'penalty - opposing team' = self$handle_penalty_opposing_team,
        'endpoint timeout - recording team' = self$handle_endpoint_timeout, 'endpoint timeout - opposing team' = self$handle_endpoint_timeout,
        'offsides - recording team' = self$handle_penalty_recording_team, 'offsides - opposing team' = self$handle_penalty_opposing_team,
        'callahan caught' = self$handle_callahan_caught, 'stall - opposing team' = self$handle_stall_opposing_team,
        'stall - recording team' = self$handle_stall_opposing_team, 'dropped pull' = self$handle_dropped_pull,
        'pmf' = self$handle_pmf, 'player ejected' = self$handle_player_ejected, 'end of ot' = self$handle_end_of_quarter,
        'end of double ot' = self$handle_end_of_quarter, 'delayed' = self$handle_delayed, 
        'postponed' = self$handle_postponed
      )
    },
    
    #' @description Starts the point with appropriate settings.
    #'
    #' @param team_events A TeamEvents object.
    #' @param event A list containing event data.
    #' @param on_offense A boolean flag indicating if the team is on offense.
    start_point = function(team_events, event, on_offense) {
      team_events$current_line <- event$line
      if (team_events$possession_num != 0) return()
      team_events$start_on_offense <- on_offense
      team_events$point_start_time <- event$time
      if (on_offense) team_events$possession_num <- 1
    },

    #' @description Handler for 'start D point' event.
    handle_start_D_point = function(team_events, event) {
      self$start_point(team_events, event, FALSE)
    },

    #' @description Handler for 'start O point' event.
    handle_start_O_point = function(team_events, event) {
      self$start_point(team_events, event, TRUE)
    },

    #' @description Handler for 'inbounds pull' event.
    handle_inbounds_pull = function(team_events, event) {
      pull_event <- PullEvent$new(event$puller, event$pullX, event$pullY, event$pullMs)
      team_events$add_pull(pull_event)
    },

    #' @description Handler for 'outbounds pull' event.
    handle_outbounds_pull = function(team_events, event) {
      pull_event <- PullEvent$new(event$puller)
      team_events$add_pull(pull_event)
    },

    #' @description Handler for 'score - opposing team' event.
    handle_score_opposing_team = function(team_events, event) {
      team_events$end_point()
      if (team_events$home_team) {
        team_events$away_team_score <- team_events$away_team_score + 1
      } else {
        team_events$home_team_score <- team_events$home_team_score + 1
      }
    },

    #' @description Handler for 'score - recording team' event.
    handle_score_recording_team = function(team_events, event) {
      throw_event <- ThrowEvent$new(event$thrower, event$throwerX, event$throwerY, 
                                            event$receiver, event$receiverX, event$receiverY, 0)
      team_events$possession_throw <- team_events$possession_throw + 1
      team_events$add_event(throw_event)
      team_events$end_point()
      if (team_events$home_team) {
        team_events$home_team_score <- team_events$home_team_score + 1
      } else {
        team_events$away_team_score <- team_events$away_team_score + 1
      }
    },

    #' @description Handler for 'pass' event.
    handle_pass = function(team_events, event) {
      throw_event <- ThrowEvent$new(event$thrower, event$throwerX, event$throwerY,
                                            event$receiver, event$receiverX, event$receiverY, 0)
      team_events$possession_throw <- team_events$possession_throw + 1
      team_events$add_event(throw_event)
    },

    #' @description Handler for 'throwaway - recording team' event.
    handle_throwaway_recording_team = function(team_events, event) {
      throw_event <- ThrowEvent$new(event$thrower, event$throwerX, event$throwerY,
                                            NULL, event$turnoverX, event$turnoverY, 1)
      team_events$possession_throw <- team_events$possession_throw + 1
      team_events$add_event(throw_event)
    },

    #' @description Handler for 'throwaway - opposing team' event.
    handle_throwaway_opposing_team = function(team_events, event) {
      team_events$possession_num <- team_events$possession_num + 1
      team_events$possession_throw <- 0
    },

    #' @description Handler for 'block' event.
    handle_block = function(team_events, event) {
      team_events$possession_num <- team_events$possession_num + 1
      team_events$possession_throw <- 0
    },

    #' @description Handler for 'callahan thrown' event.
    handle_callahan_thrown = function(team_events, event) {
      throw_event <- ThrowEvent$new(event$thrower, event$throwerX, event$throwerY,
                                            NULL, event$turnoverX, event$turnoverY, 1)
      team_events$add_event(throw_event)
      self$handle_score_opposing_team(team_events, event)
    },

    #' @description Handler for 'drop' event.
    handle_drop = function(team_events, event) {
      throw_event <- ThrowEvent$new(event$thrower, event$throwerX, event$throwerY,
                                            event$receiver, event$receiverX, event$receiverY, 1)
      team_events$add_event(throw_event)
    },

    #' @description Handler for 'end of quarter' events.
    handle_end_of_quarter = function(team_events, event) {
      team_events$end_quarter()
    },

    #' @description Handler for 'line change' events.
    handle_line_change = function(team_events, event) {
      team_events$current_line <- event$line
    },

    #' @description Handler for 'penalty - recording team' event.
    handle_penalty_recording_team = function(team_events, event) {
      team_events$recording_team_penalty <- team_events$recording_team_penalty + 1
    },

    #' @description Handler for 'penalty - opposing team' event.
    handle_penalty_opposing_team = function(team_events, event) {
      team_events$opposing_team_penalty <- team_events$opposing_team_penalty + 1
    },

    #' @description Handler for 'endpoint timeout' events.
    handle_endpoint_timeout = function(team_events, event) {
      # No action required
    },

    #' @description Handler for 'callahan caught' event.
    handle_callahan_caught = function(team_events, event) {
      team_events$end_point()
      if (team_events$home_team) {
        team_events$home_team_score <- team_events$home_team_score + 1
      } else {
        team_events$away_team_score <- team_events$away_team_score + 1
      }
    },

    #' @description Handler for 'stall' event (opposing team).
    handle_stall_opposing_team = function(team_events, event) {
      team_events$possession_num <- team_events$possession_num + 1
      team_events$possession_throw <- 0
    },

    #' @description Handler for 'stall' event (recording team).
    handle_stall_recording_team = function(team_events, event) {
      throw_event <- ThrowEvent$new(event$thrower, event$throwerX, event$throwerY,
                                            NULL, event$turnoverX, event$turnoverY, 1)
      team_events$add_event(throw_event)
    },

    #' @description Handler for 'dropped pull' event.
    handle_dropped_pull = function(team_events, event) {
      team_events$add_pull(PullEvent(event$puller))
    },

    #' @description Handler for 'pmf' event.
    handle_pmf = function(team_events, event) {
      # Placeholder for PMF event handling
    },

    #' @description Handler for 'player ejected' event.
    handle_player_ejected = function(team_events, event) {
      # Placeholder for player ejection handling
    },

    #' @description Handler for 'delayed' event.
    handle_delayed = function(team_events, event) {
      # Placeholder for delayed event handling
    },

    #' @description Handler for 'postponed' event.
    handle_postponed = function(team_events, event) {
      # Placeholder for postponed event handling
    }
  )
)

#' Endpoint Class
#'
#' This class is used to manage the fetching of data from a specified API endpoint.
#' It allows for setting a base URL, constructing a complete URL, and fetching data in the form of data frames.
#' 
#' @description
#' The `Endpoint` class enables you to interact with an API by constructing a complete URL based on a base URL and endpoint. 
#' It includes methods for fetching data from the URL and processing it into data frames.
#'
#' @import R6
#' @importFrom httr GET
#' @importFrom xml2 read_html
#' @export
Endpoint <- R6::R6Class(
  "Endpoint",
  
  public = list(
    
    #' @field base_url A character string representing the base URL for the API
    base_url = NULL,
    
    #' @field endpoint A character string representing the specific endpoint
    endpoint = NULL,
    
    #' @field url A character string representing the full URL to the API resource
    url = NULL,
    
    #' @description
    #' Initializes the Endpoint object with a given base URL.
    #' 
    #' @param base_url A character string representing the base URL.
    initialize = function(base_url) {
      self$base_url <- base_url
      self$endpoint <- NULL
      self$url <- NULL
    },
    
    #' @description
    #' Fetches data from the constructed URL and returns it as a list of data frames.
    #'
    #' @return A list of data frames fetched from the URL.
    fetch_dfs_from_url = function() {
      if (is.null(self$url)) {
        stop("URL is not set. Please set the endpoint first.")
      }
      raw_data <- xml2::read_html(self$url)
      # Assuming the data comes in a table format, this will parse it
      return(as.data.frame(raw_data))
    },
    
    #' @description
    #' Constructs and returns the full URL by combining the base URL and endpoint.
    #'
    #' @return A character string representing the full URL.
    get_url = function() {
      self$url <- paste0(self$base_url, self$endpoint)
      return(self$url)
    },
    
    #' @description
    #' This is a placeholder method for setting the endpoint.
    #' In a real implementation, this would be used to specify the API endpoint.
    get_endpoint = function() {
      # Implementation for retrieving endpoint can go here
    }
  )
)


GameEvents <- R6::R6Class(
  "GameEvents",
  
  public = list(
    game_events_proxy = NULL,
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
    initialize = function(gameID, proxy) {
      self$game_events_proxy <- proxy
      self$gameID <- gameID
      self$event_handlers <- EventHandlers$new()
      self$get_game_info()
      self$get_game_events()
    },

    #' @description Retrieves the game information (home and away teams, scores, etc.).
    #' 
    #' This function calls the `get_request` method on the `game_events_proxy` to fetch
    #' game information using the `gameID`.
    #' @export
    get_game_info = function() {
      endpoint <- paste0('games?gameIDs=', self$gameID)
      self$game_events_proxy$get_request(endpoint)
      self$home_teamID <- self$game_events_proxy$current_request$homeTeamID
      self$away_teamID <- self$game_events_proxy$current_request$awayTeamID
      self$start_time <- self$game_events_proxy$current_request$startTimestamp
      self$home_score <- self$game_events_proxy$current_request$homeScore
      self$away_score <- self$game_events_proxy$current_request$awayScore
    },

    #' @description Retrieves the game events for the specified game.
    #' 
    #' This function calls the `get_request` method on the `game_events_proxy` to fetch
    #' game events using the `gameID`.
    #' @export
    get_game_events = function() {
      self$game_events_proxy$get_request(paste0('gameEvents?gameID=', self$gameID))
      self$home_events <- self$game_events_proxy$current_request$homeEvents
      self$away_events <- self$game_events_proxy$current_request$awayEvents
    },

    #' @description Processes the game events for both home and away teams.
    #' 
    #' This function processes the game events for both the home and away teams by calling
    #' the respective handler functions for each event. It ensures that all events are handled
    #' according to the event type.
    #' @export
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

#' TeamEvents Class
#' 
#' This class handles the events of a game, such as throws and pulls, and processes them for both home and away teams. It tracks the scores, penalties, and various game events.
#' 
#' @export
TeamEvents <- R6::R6Class(
  "TeamEvents",
  
  public = list(
    home_team = NULL,
    game_events = NULL,
    processed_events = NULL,
    processed_pulls = NULL,
    game_quarter = NULL,
    home_team_score = NULL,
    away_team_score = NULL,
    current_line = NULL,
    start_on_offense = NULL,
    point_start_time = NULL,
    possession_num = NULL,
    possession_throw = NULL,
    point_timeouts = NULL,
    recording_team_penalty = NULL,
    opposing_team_penalty = NULL,
    quarter_point = NULL,
    
    #' @description Constructor for TeamEvents
    #' @param game_events A list of game events to process.
    #' @param home_team A logical value indicating whether the team is the home team.
    initialize = function(game_events, home_team) {
      self$home_team <- home_team
      self$game_events <- game_events
      self$processed_events <- list()
      self$processed_pulls <- list()
      self$game_quarter <- 0
      self$end_quarter()
      self$home_team_score <- 0
      self$away_team_score <- 0
    },
    
    # Function to add a throw event
    #' @description Add a throw event to the processed events.
    #' @param throw_event A `ThrowEvent` object to be added.
    add_event = function(throw_event) {
      row <- list(
        thrower = throw_event$thrower,
        thrower_x = throw_event$thrower_x,
        thrower_y = throw_event$thrower_y,
        receiver = throw_event$receiver,
        receiver_x = throw_event$receiver_x,
        receiver_y = throw_event$receiver_y,
        turnover = throw_event$turnover,
        start_on_offense = self$start_on_offense,
        point_start_time = self$point_start_time,
        current_line = self$current_line,
        possession_num = self$possession_num,
        possession_throw = self$possession_throw,
        game_quarter = self$game_quarter,
        quarter_point = self$quarter_point,
        point_timeouts = self$point_timeouts,
        coming_from_recording_team_penalty = self$recording_team_penalty,
        coming_from_opposing_team_penalty = self$opposing_team_penalty,
        is_home_team = self$home_team,
        home_team_score = self$home_team_score,
        away_team_score = self$away_team_score
      )
      self$processed_events <- append(self$processed_events, list(row))
      self$recording_team_penalty <- 0
      self$opposing_team_penalty <- 0
    },

        # Function to add a throw event
    #' @description Add a throw event to the processed events.
    #' @param throw_event A `ThrowEvent` object to be added.
    set_pull_event = function(throw_event) {
      
    },
    
    # Function to add a pull event
    #' @description Add a pull event to the processed pulls.
    #' @param pull_event A `PullEvent` object to be added.
    add_pull = function(pull_event) {
      row <- list(
        puller = pull_event$puller,
        pull_x = pull_event$pull_x,
        pull_y = pull_event$pull_y,
        pull_ms = pull_event$pull_ms,
        home_team_score = self$home_team_score,
        away_team_score = self$away_team_score
      )
      self$processed_pulls <- append(self$processed_pulls, list(row))
    },
    
    # Function to end a point and reset relevant variables
    #' @description Reset the state at the end of a point.
    end_point = function() {
      self$current_line <- NULL
      self$start_on_offense <- NULL
      self$point_start_time <- NULL
      self$possession_num <- 0
      self$possession_throw <- 0
      self$point_timeouts <- 0
      self$recording_team_penalty <- 0
      self$opposing_team_penalty <- 0
    },
    
    # Function to end a quarter and reset quarter-related variables
    #' @description Reset the state at the end of a quarter.
    end_quarter = function() {
      self$end_point()
      self$quarter_point <- 0
      self$game_quarter <- self$game_quarter + 1
    }
  )
)




# Nested PullEvent class
PullEvent = R6::R6Class(
  "PullEvent",
  
  public = list(
    puller = NULL,
    pull_x = NULL,
    pull_y = NULL,
    pull_ms = NULL,
    
    #' @description Constructor for PullEvent
    #' @param puller The player who made the pull.
    #' @param pull_x The x-coordinate of the pull.
    #' @param pull_y The y-coordinate of the pull.
    #' @param pull_ms The timestamp of the pull in milliseconds.
    initialize = function(puller = NULL, pull_x = NULL, pull_y = NULL, pull_ms = NULL) {
      self$puller <- puller
      self$pull_x <- pull_x
      self$pull_y <- pull_y
      self$pull_ms <- pull_ms
    }
  )
)

# Nested ThrowEvent class
ThrowEvent = R6::R6Class(
  "ThrowEvent",
  
  public = list(
    thrower = NULL,
    thrower_x = NULL,
    thrower_y = NULL,
    receiver = NULL,
    receiver_x = NULL,
    receiver_y = NULL,
    turnover = NULL,
    
    #' @description Constructor for ThrowEvent
    #' @param thrower The player who threw the disc.
    #' @param thrower_x The x-coordinate of the thrower.
    #' @param thrower_y The y-coordinate of the thrower.
    #' @param receiver The player who received the disc.
    #' @param receiver_x The x-coordinate of the receiver.
    #' @param receiver_y The y-coordinate of the receiver.
    #' @param turnover Flag indicating whether the throw resulted in a turnover.
    initialize = function(thrower, thrower_x, thrower_y, receiver, receiver_x, receiver_y, turnover) {
      self$thrower <- thrower
      self$thrower_x <- thrower_x
      self$thrower_y <- thrower_y
      self$receiver <- receiver
      self$receiver_x <- receiver_x
      self$receiver_y <- receiver_y
      self$turnover <- turnover
    }
  )
)



