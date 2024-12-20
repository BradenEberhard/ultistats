
#' TeamEvents Class
#' 
#' This class handles the events of a game, such as throws and pulls, and processes them for both home and away teams. It tracks the scores, penalties, and various game events.
#' 
#' @noRd
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
