#' Event Handlers
#' @import dplyr
#' @noRd
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
      # No action required
    },

    #' @description Handler for 'outbounds pull' event.
    handle_outbounds_pull = function(team_events, event) {
      # No action required
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
      # Placeholder for dropped pull
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