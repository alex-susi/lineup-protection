#devtools::install_github(repo = "saberpowers/sabRmetrics")

library(devtools)
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(Lahman)
library(sabRmetrics)
library(ggplot2)

players <- get_chadwick_lu() %>% as.data.frame()






## Formatting and Cleaning -----------------------------------------------------------
format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set

  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                            ifelse(type == "X" & events == "double", 2,
                            ifelse(type == "X" & events == "triple", 3,
                            ifelse(type == "X" & events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                       ifelse(type == "X" & events == "double", 1,
                       ifelse(type == "X" & events == "triple", 1, 
                       ifelse(type == "X" & events == "home_run", 1, NA)))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", 
                                        away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", 
                                       home_team, away_team))
    
    df <- df %>%
      mutate(
        pitch_result = case_when(
          # If description is "hit_into_play", base pitch_result on the events column
          description == "hit_into_play" & events == "force_out" ~ "field_out",
          description == "called_strike" & grepl("strikeout", events) ~ "strikeout",
          description == "swinging_strike" & grepl("strikeout", events) ~ "strikeout",
          description == "swinging_strike_blocked" & grepl("strikeout", events) ~ 
            "strikeout",
          description == "hit_into_play" & events == "field_out" ~ "field_out",
          description == "swinging_strike" & events == "field_out" ~ "strikeout",
          description == "hit_into_play" & events == "single" ~ "single",
          description == "ball" & events == "walk" ~ "walk",
          description == "blocked_ball" & events == "walk" ~ "walk",
          description == "hit_into_play" & events == "field_error" ~ "field_out",
          description == "hit_into_play" & events == "sac_fly" ~ "field_out",
          description == "hit_into_play" & events == "grounded_into_double_play" ~ 
            "field_out",
          description == "hit_into_play" & events == "hit_by_pitch" ~ "hit_by_pitch",
          description == "hit_into_play" & events == "fielders_choice" ~ "field_out",
          description == "hit_into_play" & events == "double_play" ~ "field_out",
          description == "hit_into_play" & events == "triple" ~ "triple",
          description == "hit_into_play" & events == "double" ~ "double",
          description == "hit_into_play" & events == "home_run" ~ "home_run",
          description == "hit_into_play" & events == "fielders_choice_out" ~ 
            "field_out",
          description == "hit_into_play" & events == "sac_bunt" ~ "field_out",
          description == "hit_into_play" & events == "sac_fly_double_play" ~ 
            "field_out",
          description == "hit_into_play" & events == "strikeout_double_play" ~ 
            "strikeout",
          description == "hit_into_play" & events == "triple_play" ~ "field_out",
          
          
          description == "ball" & is.na(events) ~ "ball",
          description == "ball" & events == "truncated_pa" ~ "ball",
          description == "ball" & events == "catcher_interf" ~ "ball",
          description == "blocked_ball" & events == "truncated_pa" ~ "ball",
          description == "called_strike" & events == "truncated_pa" ~ "called_strike",
          description == "called_strike" & events == "catcher_interf" ~ 
            "called_strike",
          description == "swinging_strike" & events == "truncated_pa" ~ 
            "swinging_strike",
          description == "swinging_strike" & events == "catcher_interf" ~ 
            "swinging_strike",
          description == "foul" ~ "foul",
          description == "called_strike" & is.na(events) ~ "called_strike",
          description == "foul_tip" & is.na(events) ~ "foul",
          description == "foul_tip" & grepl("strikeout", events) ~ "strikeout",
          description == "swinging_strike" & is.na(events) ~ "swinging_strike",
          description == "blocked_ball" & is.na(events) ~ "ball",
          description == "swinging_strike_blocked" & is.na(events) ~ "swinging_strike",
          description == "hit_by_pitch" ~ "hit_by_pitch",
          description == "foul_bunt" ~ "foul",
          description == "pitchout" ~ "ball",
          description == "missed_bunt" ~ "swinging_strike",
          description == "bunt_foul_tip" ~ "foul",
          
          # Default case for unmatched rows
          TRUE ~ NA_character_
        )
      )
    
    df <- df %>%
      mutate(barrel = ifelse(launch_angle <= 50 & 
                               launch_speed >= 98 & 
                               launch_speed * 1.5 - launch_angle >= 117 & 
                               launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      mutate(spray_angle = round(
        (atan((hit_coord_x - 125.42)/(198.27 - hit_coord_y)) * 180 / pi * .75)
        ,1)
      )
    
    df <- df %>%
      rename(release_speed_mph = release_speed) %>%
      mutate(
        #vyf = ay * plate_time + by,
        vyf = -sqrt(vy0**2 - (2 * ay * (50 - (17/12)))),
        #vxf = ax * plate_time + bx,
        vxf = vx0 + (ax * ((vyf - vy0) / ay)),
        vzf = vz0 + (az * ((vyf - vy0) / ay)),
        #vzf = az * plate_time + bz,
        #VAA = -atan((vz0 + (az * ((vyf - vy0) / ay))) / vyf) * (180 / pi),
        VAA = -atan(vzf / vyf) * (180 / pi),
        HAA = -atan(vxf / vyf) * (180 / pi)#,
        #count = paste0(balls, "-", strikes)
        
      ) %>%
      mutate(in_strike_zone = ifelse(plate_x >= -(17/(12*2)) &
                                          plate_x <= (17/(12*2)) &
                                          plate_z >= strike_zone_bottom &
                                          plate_z <= strike_zone_top, 1, 0)) %>%
      relocate(in_strike_zone, .after = plate_z) %>%
      #get_quadratic_coef(source = "baseballsavant") %>%
      #get_trackman_metrics() %>%
      as.data.frame()
    
    df <- df %>%
      filter(!is.na(year))
    
    return(df)
    
    }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    arrange(game_date, game_id, inning, at_bat_number, pitch_number)
  
  df <- df %>%
    filter(!is.na(game_date))
  
  df <- df %>%
    ungroup()
  
  df <- df %>%
    select(setdiff(names(.), c("error")))
  
  df <- df %>%
    select(-contains("_deprecated"), -c("spin_dir", "sv_id"), -c("inning_topbot"))
        #, -c(cx, cy, cz, x0, y0, z0, t0, bx, by, bz, release_x, release_y, release_z,
            #  plate_y, gravity, plate_x_line, plate_z_line, plate_z_gravity,
             # vxf, vyf, vzf, release_speed))
  
  df <- df %>%
    left_join(players %>%
                select(key_mlbam, name_last, name_first),
              by = c("pitcher_id" = "key_mlbam")) %>%
    mutate(pitcher_name = paste(name_last, name_first, sep = ", ")) %>%
    relocate(pitcher_name, .after = pitcher_id) %>%
    select(-name_last, -name_first)
  
  df <- df %>%
    relocate(game_id, year, game_date, game_type, hp_umpire, inning, half_inning, 
             outs, balls, strikes, .before = 1) %>%
    relocate(home_team, away_team, home_score, away_score, bat_score, fld_score,
             home_score_diff, bat_score_diff, batting_team, fielding_team,
             .after = game_type) %>%
    relocate(batter_id, batter_name, bat_side, 
             pitcher_id, pitcher_name, pitch_hand, 
             .after = away_team) %>%
    relocate(at_bat_number, pitch_number, pitch_type, pitch_name,
             release_speed_mph, effective_speed, release_spin_rate, 
             release_pos_x, release_pos_y, release_pos_z,
             extension, arm_angle, 
             .after = strikes) %>%
    relocate(ax, ay, az, vx0, vy0, vz0, spin_axis, pfx_x, pfx_z,
             #plate_time, 
             api_break_x_arm, api_break_x_batter_in,
             api_break_z_with_gravity, 
             #horz_break, vert_break, induced_vert_break,
             plate_x, plate_z, VAA, HAA,
             strike_zone_top, strike_zone_bottom, zone,
             type, description, events, pitch_result, des,
             .after = arm_angle) %>%
    relocate(bat_speed, swing_length, launch_speed, launch_angle, launch_speed_angle,
             hit, hit_type, bb_type, barrel, spray_angle,
             expected_woba, expected_babip, estimated_slg_using_speedangle, 
             hit_coord_x, hit_coord_y, hit_distance_sc,
             hit_location, woba_value, woba_denom, babip_value, iso_value, 
             .after = des) %>%
    relocate(if_fielding_alignment, of_fielding_alignment, 
             .after = fielder_9_id) %>%
    relocate(post_away_score, post_home_score, post_bat_score, post_fld_score,
             delta_run_exp, delta_pitcher_run_exp, 
             home_win_exp, bat_win_exp, delta_home_win_exp,
             .after = iso_value)
  
  return(df)
  
  }










## Umpire IDs ------------------------------------------------------------------------
umpire_match <- function(df) {
  
  # Create a list to store game_id and corresponding umpire
  game_id <- unique(df$game_id)
  umpire_list <- vector("list", length(game_id))
  
  # Iterate over unique game_id values and make API requests
  for (i in seq_along(game_id)) {
    
    # Pull API info via game_id
    url <- paste0('https://statsapi.mlb.com/api/v1/game/', 
                  game_id[i], 
                  '/boxscore')
    response <- GET(url)
    content <- content(response, as = "text", encoding = "UTF-8")
    boxscore <- fromJSON(content, flatten = TRUE)$info
    
    # Extract umpire name
    boxscore_str <- toString(boxscore)
    umpire <- str_match(boxscore_str, "HP: (.*?)1B")[, 2]
    umpire <- str_trim(umpire)
    umpire_list[[i]] <- umpire
    }
  
  umpire_df <- data.frame(game_id = game_id,
                          umpire = unlist(umpire_list))
  
  df <- df %>%
    left_join(umpire_df, by = "game_id") %>%
    
    mutate(umpire.y = str_remove(umpire.y, "\\.$")) %>%

    select(-contains("umpire.x")) %>%
    
    rename(hp_umpire = umpire.y)
  
  return(df)
  
  }










## NEW sabRmetrics Package -----------------------------------------------------------
statcast_scraper <- function(start_date, end_date) {
  
  cluster <- parallel::makeCluster(parallel::detectCores())
  
  df <- sabRmetrics::download_baseballsavant(
    start_date = start_date,
    end_date = end_date,
    cl = cluster)
  
  df <- umpire_match(df)
  df <- format_append_statcast(df)
  
  parallel::stopCluster(cluster)
  
  return(df)
  
  }




