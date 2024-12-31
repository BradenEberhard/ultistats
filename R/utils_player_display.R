#' Retrieve and Process Player Throws
#'
#' Fetches throw data for a player from the database and processes it to add adjusted angles and year.
#'
#' @param db_path Path to the database file.
#' @param player_full_name Player's fullName
#' @return A data frame with processed throw data, including `adjusted_angle` and `year`.
#' @examples
#' \dontrun{
#'   get_filtered_throws("path/to/database.sqlite", 12345)
#' }
#' @export
get_filtered_throws <- function(db_path, player_id) {
  conn <- open_db_connection(db_path)
  player_throws <- get_player_throws(conn, player_id)
  close_db_connection(conn)
  
  player_throws$adjusted_angle <- (as.integer(player_throws$throw_angle) + 90) %% 360 - 180
  player_throws$year <- substr(player_throws$gameID, 1, 4)
  
  return(player_throws)
}

get_HTML <- function() {
  output <- HTML("
  .selectize-input {
    color: white; /* Text color inside the input box */
  }
  .selectize-input input {
    color: white; /* Typed text color */
  }
  .selectize-dropdown {
    color: white; /* Text color in the dropdown options */
  }
  .selectize-dropdown .option:hover {
    background-color: #1ABC9C !important;
  }
  .selectize-dropdown .active {
    background-color: #1ABC9C !important; 
    color: white !important; /* Text color of the active option */
  }
")
  return(output)
}

calc_percentile <- function(player_value, all_values) {
  return(mean(all_values <= player_value, na.rm = TRUE) * 100)
}

get_metrics <- function(category) {
  addition <- ifelse(category == "Per Possession", "_per_possession", 
                     ifelse(category == "Per Game", "_per_game", ""))
  counting_metrics <- c("goals", "assists", "blocks", "completions", "hockeyAssists", 
                        "yardsThrown", "yardsReceived")
  counting_metrics <- paste0(counting_metrics, addition)
  percentage_metrics <- c("offensive_efficiency", "defensive_efficiency", "completion_percentage", "cpoe", "xcp")
  return(c(counting_metrics, percentage_metrics))
}

filter_year <- function(df, year) {
  return(df[df$year == year, ])
}

rename_metrics <- function(data, keep_category=FALSE, column = "metric") {
  if (keep_category){
    rename_map <- c(
      games = "G",
      yardsThrown = "TY",
      yardsThrown_per_game = "TY PG",
      yardsThrown_per_possession = "TY PP",
      yardsReceived = "RY",
      yardsReceived_per_game = "RY PG",
      yardsReceived_per_possession = "RY PP",
      completions = "C",
      completions_per_game = "C PG",
      completions_per_possession = "C PP",
      oOpportunities = "P",
      completion_percentage = "CP",
      cpoe = "CPOE",
      xcp = "xCP",
      assists = "A",
      assists_per_possession = "A PP",
      hockeyAssists = "HA",
      hockeyAssists_per_possession = "HA PP",
      turnovers = "Turns",
      turnovers_per_possession = "Turns PP",
      thrower_ec_per_possession = "T-EC PP",
      thrower_aec_per_possession = "T-aEC PP",
      thrower_ec = "T-EC",
      thrower_aec = "T-aEC",
      offensive_efficiency = "OE"
    )
  
    # Mapping for full names
    full_metric_map <- c(
      games = "Games Played",
      yardsThrown = "Total Yards Thrown",
      yardsThrown_per_game = "Yards Thrown Per Game",
      yardsThrown_per_possession = "Yards Thrown Per Possession",
      yardsReceived = "Total Yards Received",
      yardsReceived_per_game = "Yards Received Per Game",
      yardsReceived_per_possession = "Yards Received Per Possession",
      completions = "Total Completions",
      completions_per_game = "Completions Per Game",
      completions_per_possession = "Completions Per Possession",
      oOpportunities = "Possessions",
      completion_percentage = "Completion Percentage",
      cpoe = "Completion Percentage Over Expected",
      xcp = "Expected Completion Percentage",
      assists = "Total Assists",
      assists_per_possession = "Assists Per Possession",
      hockeyAssists = "Hockey Assists",
      hockeyAssists_per_possession = "Hockey Assists Per Possession",
      turnovers = "Total Turnovers",
      turnovers_per_possession = "Turnovers Per Possession",
      thrower_ec = "Thrower Expected Contribution",
      thrower_aec = "Thrower Adjusted Expected Contribution",
      thrower_ec_per_possession = "Thrower Expected Contribution Per Possession",
      thrower_aec_per_possession = "Thrower Adjusted Expected Contribution Per Possession",
      offensive_efficiency = "Offensive Efficiency"
    )
    
    # Mutate the dataframe to include the full_metric column
    data <- data %>%
      mutate(
        full_metric = recode(!!sym(column), !!!full_metric_map),
        !!column := recode(!!sym(column), !!!rename_map)
      )
    
    return(data)
  }
  rename_map <- c(
    goals = "Goals ",
    hockeyAssists = "Hockey Assists ",
    assists = "Assists ",
    yardsThrown = "Throwing Yards ",
    yardsReceived = "Receiving Yards ",
    cpoe = "CPOE ",
    completion_percentage = "Completion Percentage ",
    blocks = "Blocks ",
    completions = "Completions ",
    xcp = "xCP ",
    defensive_efficiency = "Defensive Efficiency",
    offensive_efficiency = "Offensive Efficiency",
    turnovers = "Turnovers"
  )
  data <- data %>%
    mutate(
      !!column := sub("_per_possession$", "", !!sym(column)),  
      !!column := sub("_per_game$", "", !!sym(column)),        
      !!column := recode(!!sym(column), !!!rename_map)
    )
  return(data)
}

adjust_for_role <- function(df, handler_value, offense_value, player_year, player_full_name) {
  if (handler_value) {
    player_handler <- df %>% filter(fullName == player_full_name, year == player_year) %>% pull(handler)
    df <- df %>% filter(handler == player_handler | fullName == player_full_name)
  }
  if (offense_value) {
    player_offense <- df %>% filter(fullName == player_full_name, year == player_year) %>% pull(offense)
    df <- df %>% filter(offense == player_offense | fullName == player_full_name)
  }
  
  return(df)
}

adjust_for_category <- function(df, category) {
  if (category == "Per Game") {
    df$goals_per_game <- df$goals / df$games
    df$assists_per_game <- df$assists / df$games
    df$blocks_per_game <- df$blocks / df$games
    df$completions_per_game <- df$completions / df$games
    df$hockeyAssists_per_game <- df$hockeyAssists / df$games
    df$yardsThrown_per_game <- df$yardsThrown / df$games
    df$yardsReceived_per_game <- df$yardsReceived / df$games
    
    # Add turnovers per game if the turnovers column exists
    if ("turnovers" %in% colnames(df)) {
      df$turnovers_per_game <- df$turnovers / df$games
    }
  } else if (category == "Per Possession") {
    df$goals_per_possession <- df$goals / df$oOpportunities
    df$assists_per_possession <- df$assists / df$oOpportunities
    df$blocks_per_possession <- df$blocks / df$dOpportunities
    df$completions_per_possession <- df$completions / df$oOpportunities
    df$hockeyAssists_per_possession <- df$hockeyAssists / df$oOpportunities
    df$yardsThrown_per_possession <- df$yardsThrown / df$oOpportunities
    df$yardsReceived_per_possession <- df$yardsReceived / df$oOpportunities
    
    # Add turnovers per possession if the turnovers column exists
    if ("turnovers" %in% colnames(df)) {
      df$turnovers_per_possession <- df$turnovers / df$oOpportunities
    }
  }
  return(df)
}


get_playerID_by_fullName <- function(data, fullName_filter) {
  # Filter the dataframe by fullName
  filtered_data <- data %>%
    filter(fullName == fullName_filter) %>%
    slice_head(n = 1)  # Get the first row
  
  # Return the playerID from the first row, or NA if no match found
  if (nrow(filtered_data) > 0) {
    return(filtered_data$playerID)
  } else {
    return(NA)  # Return NA if fullName does not exist
  }
}

prepare_metric_data <- function(df, player_full_name, thrower_handler_value, thrower_offense_value, all_metrics) {
  metric_data <- list()
  
  player_df <- df %>% filter(fullName == player_full_name, year != "Career") %>% arrange(year)
  
  for (i in 1:nrow(player_df)) {
    year <- player_df[i, "year"]
    df_year <- adjust_for_role(df, thrower_handler_value, thrower_offense_value, year, player_full_name)
    filtered_df <- df_year %>% filter(fullName == player_full_name, year != "Career") %>% arrange(year)
    player_value <- filtered_df[i, all_metrics]
    
    df_final_players <- df_year %>% filter(games >= 3)
    for (metric in all_metrics) {
      if (!is.na(player_value[[metric]])) {
        metric_values <- df_final_players[df_final_players$year == year, metric]
        percentile_value <- calc_percentile(player_value[[metric]], metric_values) %>% round(2)
        if (grepl("turnover", metric)) {
          percentile_value <- 100 - percentile_value
        }
        metric_data[[length(metric_data) + 1]] <- list(
          year = year,
          metric = metric,
          percentile = percentile_value,
          value = player_value[[metric]]
        )
      }
    }
  }
  
  return(metric_data)
}

format_metric_data <- function(metric_data) {
  metric_df <- bind_rows(lapply(metric_data, function(x) {
    data.frame(
      year = x$year,
      metric = x$metric,
      percentile = x$percentile,
      value = x$value
    )
  })) %>% arrange(year) %>% rename_metrics(keep_category = TRUE)
  
  return(metric_df)
}

#' @importFrom ggiraph geom_line_interactive geom_point_interactive girafe girafe_options opts_hover opts_hover_inv opts_toolbar opts_sizing opts_selection
#' @importFrom ggrepel geom_text_repel
generate_percentile_plot <- function(metric_df, title) {
  metric_df$year <- as.numeric(metric_df$year)
  last_points <- metric_df %>%
    group_by(metric) %>%
    filter(year == max(year))
  # Create the ggplot
  plot <- ggplot(
    metric_df, 
    aes(
      x = year, 
      y = percentile, 
      color = metric, 
      group=metric,
      data_id = metric
    )
  ) +
    geom_line_interactive(aes(tooltip = full_metric), size = 1.2) +
    geom_point_interactive(aes(tooltip = paste(metric, ": ", round(value,2), "\nPercentile: ", percentile)), size = 3) +
    geom_text_repel(
      data=last_points,
      aes(color = metric, label = metric),  
      family = "Lato",  # Adjust the font family
      size = 8,
      direction = "y",  # Direction of text (either 'x', 'y', or 'both')
      xlim = c(max(metric_df$year) + 0.1, NA),  # Limit for x-axis labels
      hjust = 0,
      vjust = 0,
      segment.size = 0.7,
      segment.alpha = 0.5,
      segment.linetype = "dotted",
      box.padding = 0.4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20
    ) +
    labs(title = title, x = "Year", y = "Percentile") +
    scale_y_continuous(limits = c(0, 102), breaks = seq(0, 100, 20)) +
    scale_x_continuous(limits = c(min(metric_df$year), max(metric_df$year) + 1), breaks = seq(min(metric_df$year), max(metric_df$year), 1)) +
    theme_minimal() +
    theme(
  legend.position = "none",
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
    ) + coord_cartesian(clip = "off")
  
  # Convert to interactive plot using ggiraph
  interactive_plot <- girafe(ggobj = plot)
  interactive_plot <- girafe_options(
    interactive_plot,
    opts_hover(
      css = "stroke:attr(color); stroke-width:4px; r:4px; transition: all 0.1s ease;"
    ),
    opts_hover_inv(css = "opacity:0.5; filter:saturate(10%);"),
    opts_toolbar(saveaspng = FALSE, hidden = c("selection")),
    opts_selection(type = "none")
  )

  return(interactive_plot)
}


get_letter_grade <- function(percentile) {
  if (percentile >= 90) {
    return("A+")
  } else if (percentile >= 85) {
    return("A")
  } else if (percentile >= 80) {
    return("A-")
  } else if (percentile >= 75) {
    return("B+")
  } else if (percentile >= 70) {
    return("B")
  } else if (percentile >= 65) {
    return("B-")
  } else if (percentile >= 60) {
    return("C+")
  } else if (percentile >= 55) {
    return("C")
  } else if (percentile >= 50) {
    return("C-")
  } else if (percentile >= 45) {
    return("D+")
  } else if (percentile >= 40) {
    return("D")
  } else if (percentile >= 35) {
    return("D-")
  } else {
    return("F")
  }
}

