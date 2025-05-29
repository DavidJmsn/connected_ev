
# APP FOR POSIT CONNECT CLOUD ---------------------------------------------

# The purpose of this script is to build a shiny app to display the results of 
# previous games and projections for upcoming games

# SETUP -------------------------------------------------------------------

# Clean environment 
rm(list = ls())

# Load libraries
library(shiny)
library(bslib)
library(bsicons)
library(bigrquery)
library(data.table)
library(dplyr)
library(plotly)
library(DT)
library(lubridate)
library(tibble)
library(ggimage)
library(png)
library(base64enc)

## VARIABLES ---------------------------------------------------------------

# Any hardcoded variables, options etc...
# Authentication setup
project_id <- Sys.getenv("project_id")
ds <- Sys.getenv("dataset")
dataset <- bq_dataset(project_id, ds)

# FUNCTIONS ---------------------------------------------------------------

## NEGATE IN ---------------------------------------------------------------

`%ni%` <- Negate(`%in%`)

## GET LINE FROM EXPECTED VALUE AND WIN PROBABILITY ------------------------

get_line <- function(EV, winProb){
  line = ((EV+1)/winProb)
  return(line)
}

## GET WIN PROBABILITY FROM LINE AND EV ------------------------------------

get_prob <- function(EV, line){
  prob <- ((EV+1)/line)
  return(prob)
}

## PLOT EV -----------------------------------------------------------------
# Enhanced plot_ev function using preloaded logos from RDS
plot_ev <- function(df = games_df,
                    line_col = "price",
                    win_prob_col = "win_percent",
                    winner_col = "winner",
                    team_col = "team",  # Column containing team names
                    title = 'Odds vs Win Probability',
                    use_logos = FALSE,  # Toggle for using logos
                    logo_dir = "01_data/team_color_codes/logos",  # Path to logos directory (for fallback)
                    nhl_logos = NULL,  # Preloaded logos object (optional)
                    logo_type = "default",  # Which logo variant to use
                    logo_size = 0.1,  # Size of logos
                    interactive = TRUE,  # Toggle for interactive plot
                    temp_logo_dir = NULL  # Temporary directory for logo files
){
  
  print(getwd())
  
  # Set axis limits
  ylims = c(min(df[[line_col]]-0.25),
            max(df[[line_col]]+0.25))
  xlims = c(min(df[[win_prob_col]]-0.05),
            max(df[[win_prob_col]]+0.05))
  
  # Add logo paths if using logos
  if (use_logos && !interactive) {
    # Check if preloaded logos are provided
    if (!is.null(nhl_logos)) {
      # Use preloaded logos from RDS
      message("Using preloaded logos from RDS file...")
      
      # Set up temporary directory for this session
      if (is.null(temp_logo_dir)) {
        temp_logo_dir <- file.path(tempdir(), "nhl_logos_temp", format(Sys.time(), "%Y%m%d_%H%M%S"))
      }
      if (!dir.exists(temp_logo_dir)) {
        dir.create(temp_logo_dir, recursive = TRUE)
      }
      
      # Create logo paths using preloaded data
      df$team_key <- gsub(" ", "_", df[[team_col]])
      df$logo_path <- NA
      
      # Process each unique team
      unique_teams <- unique(df$team_key)
      for (team in unique_teams) {
        if (team %in% names(nhl_logos$logos)) {
          logo_data <- nhl_logos$logos[[team]][[logo_type]]
          
          if (!is.null(logo_data) && logo_data$success) {
            # Create temp file path
            temp_file <- file.path(temp_logo_dir, paste0(team, "_", logo_type, ".png"))
            
            # Write PNG to temp file
            writePNG(logo_data$image, temp_file)
            
            # Update all rows with this team
            df$logo_path[df$team_key == team] <- temp_file
          }
        }
      }
      
      # Check for missing logos
      missing_logos <- df[[team_col]][is.na(df$logo_path)]
      if (length(missing_logos) > 0) {
        warning("Missing logos in RDS for teams: ", paste(unique(missing_logos), collapse = ", "))
      }
      
    } else {
      # Fallback to original file-based method
      message("No preloaded logos provided, using files from disk...")
      
      # Simple conversion: just replace spaces with underscores
      df$logo_filename <- paste0(gsub(" ", "_", df[[team_col]]), ".png")
      df$logo_path <- file.path(logo_dir, df$logo_filename)
      
      # Check if logo files exist
      missing_logos <- df[[team_col]][!file.exists(df$logo_path)]
      if (length(missing_logos) > 0) {
        warning("Missing logo files for teams: ", paste(unique(missing_logos), collapse = ", "))
        
        # Show which files it's looking for
        missing_files <- unique(df$logo_filename[!file.exists(df$logo_path)])
        message("Looking for files: ", paste(missing_files, collapse = ", "))
      }
    }
  }
  
  # Create base plot with ribbons
  z <- data.table(x=c(0,1.05))
  p <- ggplot(data = z, mapping = aes(x=x)) +
    stat_function(fun=function(x) get_line(EV = 0, winProb = x)) +
    stat_function(fun=function(x) get_line(EV = 0, winProb = x),
                  geom = 'ribbon',
                  mapping = aes(ymin=after_stat((get_line(EV = -5, winProb = x))),
                                ymax=after_stat((get_line(EV = 0, winProb = x)))),
                  fill = 'red',
                  alpha = 0.3) +
    stat_function(fun=function(x) get_line(EV = 0, winProb = x),
                  geom = 'ribbon',
                  mapping = aes(ymin=after_stat((get_line(EV = 0, winProb = x))),
                                ymax=after_stat((get_line(EV = 0.05, winProb = x)))),
                  fill = 'orange',
                  alpha = 0.2) +
    stat_function(fun=function(x) get_line(EV = 0, winProb = x),
                  geom = 'ribbon',
                  mapping = aes(ymin=after_stat((get_line(EV = 0.05, winProb = x))),
                                ymax=after_stat((get_line(EV = 0.10, winProb = x)))),
                  fill = 'yellow',
                  alpha = 0.2) +
    stat_function(fun=function(x) get_line(EV = 0, winProb = x),
                  geom = 'ribbon',
                  mapping = aes(ymin=after_stat((get_line(EV = 0.10, winProb = x))),
                                ymax=after_stat((get_line(EV = 100, winProb = x)))),
                  fill = 'green',
                  alpha = 0.2) +
    stat_function(fun=function(x) get_line(EV = -5, winProb = x),
                  geom = 'area',
                  fill = 'red',
                  alpha = 0.4)
  
  # Add data points - either as logos or regular points
  if (use_logos && !interactive) {
    # Split data into teams with and without logos
    df_with_logos <- df[!is.na(df$logo_path) & file.exists(df$logo_path), ]
    df_without_logos <- df[is.na(df$logo_path) | !file.exists(df$logo_path), ]
    
    # Add logos for teams that have them
    if (nrow(df_with_logos) > 0) {
      p <- p + 
        geom_image(data = df_with_logos,
                   mapping = aes(x = .data[[win_prob_col]],
                                 y = .data[[line_col]],
                                 image = logo_path),
                   size = logo_size)
    }
    
    # Add points for teams without logos
    if (nrow(df_without_logos) > 0) {
      p <- p + 
        geom_point(data = df_without_logos,
                   mapping = aes(x = .data[[win_prob_col]],
                                 y = .data[[line_col]],
                                 color = .data[[winner_col]]),
                   size = 3) +
        geom_text(data = df_without_logos,
                  mapping = aes(x = .data[[win_prob_col]],
                                y = .data[[line_col]],
                                label = .data[[team_col]]),
                  vjust = -1,
                  size = 2.5)
    }
  } else {
    # Use regular points (for interactive or non-logo version)
    p <- p + 
      geom_point(data = df,
                 mapping = aes(x = .data[[win_prob_col]],
                               y = .data[[line_col]],
                               color = .data[[winner_col]],
                               # Add team info to hover text if interactive
                               text = if(interactive) paste("Team:", .data[[team_col]],
                                                            "<br>Win Prob:", round(.data[[win_prob_col]], 3),
                                                            "<br>Line:", .data[[line_col]],
                                                            "<br>Winner:", .data[[winner_col]]) 
                               else NULL))
  }
  
  # Complete the plot styling
  p <- p +
    labs(x = 'Win Probability', 
         y = 'Line',
         title = title) +
    coord_cartesian(xlim = xlims,
                    ylim = ylims) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.10)) +
    scale_y_continuous(breaks = seq(floor(min(df[[line_col]])), 
                                    ceiling(max(df[[line_col]])), 
                                    by = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Add legend only if not using logos
  if (!use_logos || interactive) {
    p <- p + scale_color_discrete(name = winner_col)
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # Clean up temp directory if we created one (optional)
  # Uncomment if you want automatic cleanup after each plot
  # if (!is.null(nhl_logos) && exists("temp_logo_dir")) {
  #   unlink(temp_logo_dir, recursive = TRUE)
  # }
  
  # Return either interactive or static plot
  if (interactive) {
    return(ggplotly(p, tooltip = if(!use_logos) "text" else "all"))
  } else {
    return(p)
  }
}

# Helper function to clean up all temporary logo directories
cleanup_temp_logos <- function() {
  temp_dirs <- list.dirs(file.path(tempdir(), "nhl_logos_temp"), full.names = TRUE, recursive = FALSE)
  if (length(temp_dirs) > 0) {
    unlink(temp_dirs, recursive = TRUE)
    message("Cleaned up ", length(temp_dirs), " temporary logo directories")
  }
}


# CONVERT RDS LOGO INTO HTML IMG TAG --------------------------------------
# Function to convert logo from RDS to base64 HTML img tag
create_logo_img_tag <- function(team_name, nhl_logos, logo_type = "default", height = 30) {
  # Convert team name to key format
  team_key <- gsub(" ", "_", team_name)
  
  # Check if team exists in logos
  if (!team_key %in% names(nhl_logos$logos)) {
    return(team_name)  # Return text if no logo found
  }
  
  # Get logo data
  logo_data <- nhl_logos$logos[[team_key]][[logo_type]]
  
  if (is.null(logo_data) || !logo_data$success) {
    return(team_name)  # Return text if logo not available
  }
  
  # Create temporary file to write PNG
  temp_file <- tempfile(fileext = ".png")
  writePNG(logo_data$image, temp_file)
  
  # Convert to base64
  base64_string <- base64encode(temp_file)
  
  # Clean up temp file
  unlink(temp_file)
  
  # Create HTML img tag
  img_tag <- sprintf(
    '<img src="data:image/png;base64,%s" height="%d" alt="%s" title="%s" style="vertical-align: middle;">',
    base64_string, height, team_name, team_name
  )
  
  return(img_tag)
}


# FORMAT GAME DATE AND TIME FOR DATATABLE ---------------------------------
# Helper function to format game time
format_game_time <- function(game_time, date = NULL) {
  tryCatch({
    # Check if it's just a time string (e.g., "20:00" or "8:00 PM")
    if (grepl("^\\d{1,2}:\\d{2}(\\s*(AM|PM))?$", game_time)) {
      if (!is.null(date)) {
        # Combine date and time
        datetime_str <- paste(date, game_time)
        # Try different formats
        datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M")
        if (is.na(datetime)) {
          datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%d %I:%M %p")
        }
        return(format(datetime, "%I:%M %p"))
      } else {
        # Just return the time as is
        return(game_time)
      }
    } else {
      # Assume it's already a full datetime
      return(format(as.POSIXct(game_time), "%I:%M %p"))
    }
  }, error = function(e) {
    # If all else fails, return the original string
    return(game_time)
  })
}

# EXTRACT DATA ------------------------------------------------------------

bigrquery::bq_deauth()

service_json <- Sys.getenv("service_account")

token <- gargle::credentials_service_account(
  path = service_json,
  scopes = c(
    "https://www.googleapis.com/auth/userinfo.email",
    "https://www.googleapis.com/auth/bigquery"
  )
)

bigrquery::bq_auth(token = token)

# # Create connection
# bq_conn <- dbConnect(
#   bigrquery::bigquery(),
#   project = project_id,
#   dataset = ds
# )

tryCatch({
  # Query the table data from BigQuery with proper backticks for escaping identifiers
  colors_query <- sprintf("SELECT * FROM `%s.%s.%s`", project_id, ds, "team_colors")
  team_colors <- bq_project_query(project_id, colors_query) %>% bq_table_download() %>% arrange(team, priority)
  
  completed_query <- sprintf("SELECT * FROM `%s.%s.%s`", project_id, ds, "completed_games")
  completed_games_db <- bq_project_query(project_id, completed_query) %>% bq_table_download()
  
  upcoming_query <- sprintf("SELECT * FROM `%s.%s.%s`", project_id, ds, "upcoming_games")
  upcoming_games_db <- bq_project_query(project_id, upcoming_query) %>% bq_table_download()
  
  # Disconnect
  # dbDisconnect(conn = bq_conn)
}, error = function(e){
  message("Error querying data: ", e)
  # dbDisconnect(conn = bq_conn)
}) 

upcoming_games_db <- merge.data.frame(x = upcoming_games_db, 
                                      y = slice_head(filter(team_colors, color_hex != "#000000"), n = 1, by ="team"),
                                      by = "team") %>%
  arrange(game_id, current_time_odds, home_or_away)

nhl_logos <- readRDS("nhl_logos_preloaded.rds")

# UI ----------------------------------------------------------------------

ui <- page_navbar(
  title = "Willy Snipe?",
  theme = bs_theme(preset = "flatly", version = 5),
  nav_panel(
    title = "Expected Value",
    icon = bs_icon("graph-up-arrow"),
    navset_card_pill(
      title = "Plots",
      full_screen = TRUE,
      nav_panel(
        "Profit/Loss",
        plotlyOutput("profit_loss_plot")
      ),
      nav_panel(
        "E.V. Scatterplot",
        plotlyOutput("ev_plot")
      ),
      nav_panel(
        "Win Percentage",
        plotlyOutput("win_percentage_plot")
      ),
      nav_panel(
        "Implied Win Percentage",
        plotlyOutput("implied_win_percentage_plot")
      )
    )
  ),
  nav_panel(
    title = "Upcoming Games",
    navset_card_pill(
      full_screen = TRUE,
      nav_panel(
        "Line Movement",
        layout_column_wrap(
          width = NULL,
          style = htmltools::css(grid_template_columns =  "5fr 1fr"),
          plotlyOutput("line_movement_plot"),
          DTOutput("narrow_dt")
        )
      ),
      nav_panel(
        "Table",
        DTOutput("upcoming_games_table")
      ),
      nav_panel(
        "E.V. Scatterplot",
        # plotlyOutput("upcoming_ev_plot")
        plotOutput("upcoming_ev_plot")
      )
    )
  ),
  nav_spacer(),
  nav_item(
    actionButton("quit_btn", "Quit",
                 icon = icon("power-off"),
                 class = "btn-danger")
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  # Observe when the quit button is pressed
  observeEvent(input$quit_btn, {
    stopApp()  # Stop the app when the button is clicked
  })
  
  upcoming_df <- reactive({
    upcoming_games_db %>%
    filter(game_state_odds %ni% c("LIVE", "CRIT", "FINAL")) %>%
      filter(is.na(game_state_results)) %>%
      mutate(current_time_odds = format(
        as.POSIXct(current_time_odds, format = "%Y%m%d %H%M%S"),
        format = "%Y-%m-%d %H:%M")) %>%
      mutate(game_time = format(
        as.POSIXct(sub(" ET", "", game_time), format = "%I:%M %p"),
        format = "%H:%M")) %>%
      mutate(game_date_time  = format(
        paste0(date, " ", game_time),
        format = "%Y%m%d %H:%M")) %>%
      mutate(time_to_gametime = difftime(game_date_time, current_time_odds, units = "hours")) 
      # left_join(y = slice_head(team_colors_df, n = 1, by ="team"), by = "team")
  })
  
  value_df <- reactive({
    completed_games_db %>%
      filter(game_state_odds %in% c("FUT", "PRE")) %>% 
      slice_max(kelly_criterion, n = 1, with_ties = FALSE, by = c(game_id, team))
  })
  
  profit_by_date <- reactive({
    value_df() %>%
      filter(kelly_criterion > 0) %>%
      group_by(date) %>%
      mutate(N_BETS = length(winner)) %>%
      mutate(N_WINNERS = sum(winner)) %>%
      mutate(TOTAL_WAGERED = sum(kelly_criterion*100)) %>%
      mutate(TOTAL_RETURN = sum(ifelse(winner, (kelly_criterion*price*100), 0))) %>%
      mutate(PROFIT = sum(ifelse(winner, (kelly_criterion*price*100), 0)) - sum(kelly_criterion*100)) %>%
      select(c("date", "N_BETS", "N_WINNERS", "TOTAL_WAGERED", "TOTAL_RETURN", "PROFIT")) %>%
      slice_head(n = 1) %>%
      ungroup()
  })  
  
  output$profit_loss_plot <- renderPlotly({
    plot_ly(data = profit_by_date()) |>
      add_lines(x = ~date, y = ~cumsum(PROFIT), name = 'Profit/Loss', 
                line = list(shape = 'spline', smoothing = 0.5)) |> 
      add_lines(x = ~date, y = ~cumsum(TOTAL_WAGERED*0.0476*-1), name = 'Juice Tax',
                line = list(shape = 'spline', smoothing = 0.5)) |> 
      add_bars(x = ~date, y = ~PROFIT,
               color = ~ifelse(PROFIT > 0, 'Daily Profit', 'Daily Loss'),
               hoverinfo = "text",
               hovertext = ~paste0('<b>Profit:    </b>', round(PROFIT,2),"<br>",
                                   '<b>N Bets:    </b>', N_BETS,"<br>",
                                   '<b>N Winners: </b>', N_WINNERS,"<br>",
                                   '<b>Wagered: </b>', round(TOTAL_WAGERED, 2),"<br>",
                                   '<b>Returned:   </b>', round(TOTAL_RETURN, 2))
      ) |>
      layout(
        title = list(text = 'Cumulative Profit Loss Over Time',
                     y = 0.97),
        xaxis = list(title = 'Date'),
        yaxis = list(title = 'Dollars ($)')
      )
  })
  
  output$ev_plot <- renderPlotly({
    plot_ev(df = value_df(), line_col = "price", win_prob_col = "win_percent", winner_col = "winner")
  })
  
  upcoming_value_df <- reactive({
    upcoming_df() %>%
      slice_max(current_time_odds, n = 1, with_ties = FALSE, by = c("game_id", "team"))
  })
  
  # output$upcoming_ev_plot <- renderPlotly({
  output$upcoming_ev_plot <- renderPlot({
    plot_ev(df = upcoming_value_df(), line_col = "price", win_prob_col = "win_percent", winner_col = "winner", 
            team_col = "team",
            use_logos = TRUE,  # Toggle for using logos
            # logo_dir = "01_data/team_color_codes/logos",  # Path to logos directory (for fallback)
            nhl_logos = nhl_logos,  # Preloaded logos object (optional)
            logo_type = "default",  # Which logo variant to use
            logo_size = 0.1,  # Size of logos
            interactive = FALSE,  # Toggle for interactive plot
            temp_logo_dir = NULL)  # Temporary directory for logo files
  })
  
  win_prob_bin_df <- reactive({
    value_df() %>% 
      mutate(WIN_PROB_BIN = cut(win_percent, breaks = seq(0, 1, by = 0.10), include.lowest = TRUE, right = FALSE)) %>%
      mutate(Price_BIN = cut(rank(price), breaks = 10, labels = FALSE)) 
  })
  
  win_prob_vs_observed <- reactive({
    win_prob_bin_df() %>%
      group_by(WIN_PROB_BIN) %>%
      mutate(AVERAGE_WIN_PROB = mean(win_percent)) %>%
      mutate(WINNERS = sum(winner) ) %>%
      mutate(GAMES = length(winner)) %>%
      mutate(OBSERVED_WIN_PERCENTAGE = sum(winner)/length(winner)) %>%
      select(WIN_PROB_BIN, AVERAGE_WIN_PROB, WINNERS, GAMES, OBSERVED_WIN_PERCENTAGE) %>%
      ungroup()
  })  
  
  line_vs_observed <- reactive({
    win_prob_bin_df() %>%
      group_by(Price_BIN) %>%
      mutate(AVERAGE_LINE = mean(price)) %>%
      mutate(EXP_WIN_PROB = get_prob(EV = 0, line = mean(price))) %>% 
      mutate(WINNERS = sum(winner) ) %>%
      mutate(GAMES = length(winner)) %>%
      mutate(OBSERVED_WIN_PERCENTAGE = sum(winner)/length(winner)) %>%
      mutate(MIN_LINE = min(price)) %>%
      mutate(MAX_LINE = max(price)) %>%
      select(Price_BIN, AVERAGE_LINE, EXP_WIN_PROB, WINNERS, GAMES, OBSERVED_WIN_PERCENTAGE, MIN_LINE, MAX_LINE) %>%
      ungroup()
  })  
  
  output$win_percentage_plot <- renderPlotly({
    plot_ly(data = win_prob_vs_observed(), x = ~WIN_PROB_BIN) |>
      add_trace(y = ~AVERAGE_WIN_PROB*100, name = 'Expected', type = "bar",
                text = ~round(AVERAGE_WIN_PROB*100, 1), textposition = 'outside',
                opacity = 0.75) |>
      add_trace(y = ~OBSERVED_WIN_PERCENTAGE*100, name = 'Observed', type = "bar",
                text = ~round(OBSERVED_WIN_PERCENTAGE*100, 1), textposition = 'outside',
                opacity = 0.75) |>
      layout(barmode = 'overlay',
             xaxis = list(title = 'Win Probability'),
             yaxis = list(title = 'Win Percentage', range = c(0,100)),
             legend= list(title = list(text = 'Win Percentage')))
  })
  
  output$implied_win_percentage_plot <- renderPlotly({
    plot_ly(data = line_vs_observed(), y = ~as.factor(round(AVERAGE_LINE,2))) |>
      add_trace(x = ~EXP_WIN_PROB*100, name = 'Expected Win Percentage',
                type = 'bar', orientation = 'h',
                hovertext = ~paste0('<b>Exp. Win %: </b>', round(EXP_WIN_PROB*100, 1)), textposition = 'outside',
                opacity = 0.75) |>
      add_trace(x = ~OBSERVED_WIN_PERCENTAGE*100, name = 'Observed Win Percentage',
                type = 'bar', orientation = 'h',
                hovertext = ~paste0('<b>Obs. Win %: </b>', round(OBSERVED_WIN_PERCENTAGE*100, 1)),
                textposition = 'outside',
                opacity = 0.75) |>
      layout(barmode = 'overlay',
             yaxis = list(title = 'Line'),
             xaxis = list(title = 'Win Percentage', range = c(0,100)),
             legend= list(title = list(text = 'Win Percentage')))
  })
  
  output$narrow_dt <- renderDT({
    datatable(data.frame(unique(upcoming_df()$game)),
              rownames = FALSE, selection = "single",
              options = list(pageLength = 30,
                             dom = "t")
    )
  })
  
  output$upcoming_games_table <- renderDT({
    # datatable(upcoming_df()[, c("game", "team", "price", "kelly_criterion", "goalie", "date","game_time"
    #                           # "win_probability","expected_value",  "date",
    #                           # "game_time", "book", "game"
    # )],
    # rownames = FALSE, selection = "single",
    # options = list(pageLength = 30,
    #                dom = "t")
    # )
    games_data <- upcoming_df() %>%
      slice_max(current_time_odds, n = 1, by = team) %>%
      arrange(date, game_time, home_or_away)
    
    # Build the compact display exactly as before
    games_compact <- games_data %>%
      group_by(game, date, game_time) %>%
      summarise(
        display = paste0(
          '<div style="display: flex; align-items: center; justify-content: center;">',
          '<div style="text-align: center; width: 160px;">',
          create_logo_img_tag(first(team), nhl_logos, "default", 40),
          '<br><strong>', first(team), '</strong><br>',
          'Line: ', round(first(price), 2), '<br>',
          'KC: ', round(first(kelly_criterion) * 100, 2), '%<br>',
          'Goalie: ', first(goalie),
          '</div>',
          '<div style="text-align: center; width: 20px;"><strong>VS</strong></div>',
          '<div style="text-align: center; width: 160px;">',
          create_logo_img_tag(last(team), nhl_logos, "default", 40),
          '<br><strong>', last(team), '</strong><br>',
          'Line: ', round(last(price), 2), '<br>',
          'KC: ', round(last(kelly_criterion) * 100, 2), '%<br>',
          'Goalie: ', last(goalie),
          '</div></div>'
        ),
        .groups = "drop"
      )
    
    datatable(
      # include 'date' as first column for grouping
      games_compact[, c("date", "display")],
      rownames = FALSE,
      escape = FALSE,           
      extensions = "RowGroup",  # load the grouping extension
      options = list(
        pageLength = 30,
        dom = "t",
        rowGroup = list(dataSrc = 0),  # group by the first (0-indexed) column
        columnDefs = list(
          # hide the raw date column (we only want the header rows)
          list(visible = FALSE, targets = 0),
          # center the display column
          list(className = 'dt-center', targets = "_all")
        )
      ),
      colnames = c("Date" = "date", "Matchup" = "display")
    )
  })
    
  #   games_data <- upcoming_df() %>%
  #     slice_max(current_time_odds, n = 1, by = team) %>%
  #     arrange(game_time, home_or_away)
  # 
  #   # Create a more compact display with logos
  #   games_compact <- games_data %>%
  #     group_by(game, date, game_time) %>%
  #     summarise(
  #       display = paste0(
  #         '<div style="display: flex; align-items: center; justify-content: center;">',
  #         '<div style="text-align: center; width: 150px;">',
  #         # Team 1 logo
  #         '<div style="margin-bottom: 5px;">',
  #         create_logo_img_tag(first(team), nhl_logos, "default", 40),
  #         '</div>',
  #         '<strong>', first(team), '</strong><br>',
  #         'Line: ', round(first(price), 2), '<br>',
  #         'KC: ', round(first(kelly_criterion) * 100, 2), '%<br>',
  #         'Goalie: ', first(goalie),
  #         '</div>',
  #         '<div style="text-align: center; width: 20px;">',
  #         '<strong>VS</strong>', '<br>',
  #         # date[1],
  #         '</div>',
  #         '<div style="text-align: center; width: 150px;">',
  #         # Team 2 logo
  #         '<div style="margin-bottom: 5px;">',
  #         create_logo_img_tag(last(team), nhl_logos, "default", 40),
  #         '</div>',
  #         '<strong>', last(team), '</strong><br>',
  #         'Line: ', round(last(price), 2), '<br>',
  #         'KC: ', round(last(kelly_criterion) * 100, 2), '%<br>',
  #         'Goalie: ', last(goalie),
  #         '</div>',
  #         '</div>'
  #       ),
  #       .groups = "drop"
  #     )
  # 
  #   datatable(
  #     games_compact[, c("game", "display")],
  #     rownames = FALSE,
  #     selection = "single",
  #     escape = FALSE,  # Allow HTML rendering
  #     options = list(
  #       pageLength = 30,
  #       dom = "t",
  #       columnDefs = list(
  #         list(visible = FALSE, targets = 0),  # Hide the game column
  #         list(className = 'dt-center', targets = c("_all"))  # Center date and time
  #       )
  #     ),
  #     colnames = c(
  #       "Game" = "game",
  #       "Matchup" = "display"
  #     )
  #   )
  # })
  
  selected_game <- reactive({
    print(data.frame(unique(upcoming_df()$game))[input$narrow_dt_row_last_clicked,])
    data.frame(unique(upcoming_df()$game))[input$narrow_dt_row_last_clicked,]
  })
  
  game_time_df <- reactive({
    req(selected_game())
    return(upcoming_df() %>%
             filter(game == selected_game())
    )
  })
  
  output$line_movement_plot <- renderPlotly({
    game_time_df() |>
      group_by(as.factor(interaction(game_id, team))) |>
      plot_ly() |>
      add_trace(x = ~time_to_gametime, y = ~kelly_criterion, 
                type = "scatter", mode = "lines+markers",
                color = ~team,  # Use team as the grouping variable
                colors = game_time_df() %>% select(team, color_hex) %>% deframe(),  # Create a named vector of colors
                marker = list(symbol = 'circle', size = 10),
                line = list(shape = 'hv', width = 3),
                # symbols = c("circle", "x"),
                # color = ~color_hex, colors = ~color_hex),
                hoverinfo = "text",
                hovertext = ~paste0('<b>Team: </b>', team,"<br>",
                                    '<b>KC:   </b>', round(kelly_criterion, 2),"<br>",
                                    '<b>Line: </b>', round(price, 2),"<br>",
                                    '<b>Win Prob.:   </b>', round(win_percent, 2),"<br>",
                                    '<b>HRS TO PD: </b>', round(-1*time_to_gametime, 0)),
                textposition = 'outside', opacity = 0.75, name = ~paste(team, "kelly_criterion"), 
                visible = TRUE) |>
      add_trace(x = ~time_to_gametime, y = ~expected_value,
                type = "scatter", mode = "lines+markers",
                color = ~team,  # Use team as the grouping variable
                colors = game_time_df() %>% select(team, color_hex) %>% deframe(),  # Create a named vector of colors
                marker = list(symbol = 'x', size = 10),
                line = list(shape = 'hv', width = 3, dash = "dash"),
                # color = ~color_hex, colors = ~color_hex),
                hoverinfo = "text",
                hovertext = ~paste0('<b>Team: </b>', team,"<br>",
                                    '<b>EV:   </b>', round(expected_value, 2),"<br>",
                                    '<b>Line: </b>', round(price, 2),"<br>",
                                    '<b>Win Prob.:   </b>', round(win_percent, 2),"<br>",
                                    '<b>HRS TO PD: </b>', round(-1*time_to_gametime, 0)),
                textposition = 'outside', opacity = 0.75, yaxis = "y3", name = ~paste(team, "EV"), 
                visible = TRUE) |>
      add_trace(x = ~time_to_gametime, y = ~price, 
                type = "scatter", mode = "lines+markers",
                color = ~team,  # Use team as the grouping variable
                colors = game_time_df() %>% select(team, color_hex) %>% deframe(),  # Create a named vector of colors
                marker = list(symbol = "diamond", size = 10),
                line = list(shape = 'hv', width = 3, dash = "dot"),
                # color = ~color_hex, colors = ~color_hex),
                hoverinfo = "text",
                hovertext = ~paste0('<b>Team: </b>', team,"<br>",
                                    '<b>Line: </b>', round(price, 2),"<br>",
                                    '<b>HRS TO PD: </b>', round(-1*time_to_gametime, 0)),
                textposition = 'outside', opacity = 0.75, yaxis = "y2", name = ~paste(team, "Line"),
                visible = FALSE) |>
      add_trace(x = ~time_to_gametime, y = ~win_percent,
                type = "scatter", mode = "lines+markers",
                color = ~team,  # Use team as the grouping variable
                colors = game_time_df() %>% select(team, color_hex) %>% deframe(),  # Create a named vector of colors
                marker = list(symbol = "circle-open", size = 10),
                line = list(shape = 'hv', width = 3, dash = "dashdot"),
                # color = ~color_hex, colors = ~color_hex),
                hoverinfo = "text",
                hovertext = ~paste0('<b>Team: </b>', team,"<br>",
                                    '<b>Win Prob.:   </b>', round(win_percent, 2),"<br>",
                                    '<b>HRS TO PD: </b>', round(-1*time_to_gametime, 0)),
                textposition = 'outside', opacity = 0.75, yaxis = "y3", name = ~paste(team, "Win Prob."),
                visible = FALSE) |>
      # add_lines(line = list(shape = 'hv', colors = ~paste0("#", color_hex)),
      #           hovertext = ~paste0('<b>Team: </b>', team,"<br>",
      #                               '<b>KC:   </b>', round(kelly_criterion, 2),"<br>",
      #                               '<b>HRS TO PD: </b>', round(-1*time_to_gametime, 0)), textposition = 'outside',
      #           opacity = 0.75) |>
      # add_lines(line = list(shape = 'hv', colors = ~paste0("#", color_hex)),
      #           hovertext = ~paste0('<b>Team: </b>', team,"<br>",
      #                               '<b>KC:   </b>', round(kelly_criterion, 2),"<br>",
      #                               '<b>HRS TO PD: </b>', round(-1*time_to_gametime, 0)), textposition = 'outside',
      #           opacity = 0.75) |>
      layout(
        title = list(text = 'Line Movement Before Gametime',
                     y = 0.97),
        xaxis = list(title = 'Hours Before Game',
                     range = c(max(game_time_df()$time_to_gametime)+3, 0),
                     zerolinecolor = "#CCCCCC",
                     zerolinewidth = 3),
        yaxis = list(title = 'Kelly Criterion',
                     range = c(-0.3, 0.3),
                     zerolinecolor = "#CCCCCC",
                     zerolinewidth = 3),
        yaxis2 = list(title = '',
                      overlaying = "y",
                      side = "left",
                      showline=F,showticklabels=F,
                      zerolinecolor = "#CCCCCC",
                      zerolinewidth = 0,
                      showgrid = F),
        # range = c(-0.3, 0.3)),
        yaxis3 = list(title = 'Expected Value',
                      range = c(-20, 20),
                      overlaying = "y",
                      side = "right",
                      showline=T,showticklabels=T,
                      zerolinecolor = "#CCCCCC",
                      zerolinewidth = 3,
                      showgrid = T),
        legend = list(
          title = list(text = "Team"),
          x = 0,  # Adjust the x position of the legend
          y = 0   # Adjust the y position of the legend
        ),
        margin = list(
          # t = 50,
          # b = 50,
          # l = 50,
          r = 50
        ),
        updatemenus = list(
          list(
            active = 1,
            type = "buttons",
            buttons = list(
              list(
                label = "All",
                method = "update", 
                args = list(
                  list(visible = rep(TRUE, 8)),
                  list(yaxis = list(showline=T,showticklabels=T,
                                    title = 'Kelly Criterion',
                                    range = c(-0.3, 0.3),
                                    zerolinecolor = "#CCCCCC",
                                    zerolinewidth = 3),
                       yaxis2 = list(showline=T,showticklabels=T,
                                     title = 'Line',
                                     overlaying = "y",
                                     side = "left"),
                       yaxis3 = list(showline=T,showticklabels=T,
                                     title = 'Win Probability',
                                     range = c(-0.3, 100),
                                     overlaying = "y",
                                     side = "right")
                  )
                )
              ),
              list(
                label = "Kelly Crit.",
                method = "update",
                args = list(
                  list(visible = c(rep(TRUE, 4), rep(FALSE, 4))),
                  list(yaxis = list(title = 'Kelly Criterion',
                                    range = c(-0.3, 0.3),
                                    zerolinecolor = "#CCCCCC",
                                    zerolinewidth = 3),
                       yaxis2 = list(title = '',
                                     overlaying = "y",
                                     side = "left",
                                     showline=F,showticklabels=F,
                                     zerolinecolor = "#FFF",
                                     zerolinewidth = 0,
                                     showgrid = F),
                       # range = c(-0.3, 0.3)),
                       yaxis3 = list(title = 'Expected Value',
                                     range = c(-20, 20),
                                     overlaying = "y",
                                     side = "right",
                                     showline=T,showticklabels=T,
                                     zerolinecolor = "#CCCCCC",
                                     zerolinewidth = 3,
                                     showgrid = T)
                  )
                )
              ),
              list(label = "Line & Win %",
                   method = "update",
                   args = list(
                     list(visible = c(rep(FALSE, 4), rep(TRUE, 4))),
                     list(yaxis = list(showline=F,showticklabels=F,title = '',
                                       showgrid = F),
                          yaxis2 = list(showline=T,showticklabels=T,
                                        title = 'Line',
                                        overlaying = "y",
                                        side = "left"),
                          yaxis3 = list(showline=T,showticklabels=T,
                                        title = 'Win Probability',
                                        range = c(-0.3, 100),
                                        overlaying = "y",
                                        side = "right")
                     )
                   )
              )
            ),
            direction = "down",
            x = 0.1,
            y = 1
          )
        )
      )
  })
}

# RUN APP -----------------------------------------------------------------

shinyApp(ui, server)

# END ---------------------------------------------------------------------
