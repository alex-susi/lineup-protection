library(dplyr)
library(tidyr)


on_deck <- function(df) {
  
  #
  # 1) One row per PA, assign an appearance_order within each game–team
  #
  at_bats <- df %>%
    arrange(game_id,
            inning,
            ifelse(half_inning == "top", 0, 1),
            at_bat_number,
            pitch_number) %>%
    group_by(game_id, batting_team, inning, half_inning, at_bat_number) %>%
    summarise(batter_id   = first(batter_id),
              batter_name = first(batter_name),
              outs        = first(outs),
              .groups     = "drop") %>%
    group_by(game_id, batting_team) %>%
    mutate(appearance_order = row_number()) %>%
    ungroup()
  
  #
  # 2) Pull out each team’s original nine‐man lineup
  #
  lineup_lookup <- at_bats %>%
    group_by(game_id, batting_team) %>%
    distinct(batter_id, .keep_all = TRUE) %>%  # first nine distinct batters
    slice_head(n = 9) %>%
    summarise(lineup_ids   = list(batter_id),
              lineup_names = list(batter_name),
              .groups      = "drop")
  
  #
  # 3) Simulate a rotating “batting‐order queue” for each team
  #
  sim <- at_bats %>%
    left_join(lineup_lookup, by = c("game_id","batting_team")) %>%
    group_by(game_id, batting_team) %>%
    arrange(appearance_order) %>%
    group_modify(~{
      d <- .x
      queue_ids   <- d$lineup_ids[[1]]
      queue_names <- d$lineup_names[[1]]
      N <- nrow(d)
      
      out <- tibble(appearance_order         =  d$appearance_order,
                    batter_id                =  d$batter_id,
                    batter_name              =  d$batter_name,
                    expected_id              =  integer(N),
                    expected_name            =  character(N),
                    natural_on_deck_id       =  integer(N),
                    natural_on_deck_name     =  character(N),
                    is_last_ab               =  logical(N),
                    is_pinch_hitter          =  logical(N),
                    replaced_starter_id      =  integer(N),
                    replaced_starter_name    =  character(N),
                    true_lineup_spot         =  integer(N),
                    override_on_deck_id      =  integer(N),
                    override_on_deck_name    =  character(N))
      
      for(i in seq_len(N)) {
        curr_id   <- d$batter_id[i]
        curr_name <- d$batter_name[i]
        
        # who _should_ have batted?
        expected_id   <- queue_ids[1]
        expected_name <- queue_names[1]
        
        # who is next if we just do lead(queue)?
        next_id   <- if(length(queue_ids)>1)   queue_ids[2]   else NA_integer_
        next_name <- if(length(queue_names)>1) queue_names[2] else NA_character_
        
        last_ab <- (i==N)
        ph      <- (curr_id != expected_id)   # pinch‐hitter if not the expected starter
        
        # if PH, record who they replaced
        rep_id   <- if(ph) expected_id   else NA_integer_
        rep_name <- if(ph) expected_name else NA_character_
        
        # determine that batter’s numeric spot in the original lineup
        lookup_id  <- if (ph) expected_id else curr_id
        true_spot  <- match(lookup_id, d$lineup_ids[[1]])  # returns 1–9 or NA
        
        # write into out
        out$expected_id[i]           <- expected_id
        out$expected_name[i]         <- expected_name
        out$natural_on_deck_id[i]    <- next_id
        out$natural_on_deck_name[i]  <- next_name
        out$is_last_ab[i]            <- last_ab
        out$is_pinch_hitter[i]       <- ph
        out$replaced_starter_id[i]   <- rep_id
        out$replaced_starter_name[i] <- rep_name
        out$true_lineup_spot[i]      <- true_spot
        
        # now update the queue:
        #  * if normal, pop head and push that same starter to tail
        #  * if PH, pop head (starter) and push PH to tail
        head_id   <- queue_ids[1]
        head_name <- queue_names[1]
        queue_ids   <- queue_ids[-1]
        queue_names <- queue_names[-1]
        tail_id   <- if(ph) curr_id   else head_id
        tail_name <- if(ph) curr_name else head_name
        queue_ids   <- c(queue_ids,   tail_id)
        queue_names <- c(queue_names, tail_name)
        
        # override on‐deck only if last AB was a PH
        if(last_ab && ph) {
          out$override_on_deck_id[i]   <- next_id
          out$override_on_deck_name[i] <- next_name
        }
      }
      
      out
    }) %>%
    ungroup()
  
  #
  # 4) Stitch everything back onto the original pitch‐level df
  #
  df %>%
    left_join(at_bats %>%
                select(game_id, batting_team, inning, half_inning, 
                       at_bat_number, appearance_order),
              by = c("game_id","batting_team","inning",
                     "half_inning","at_bat_number")) %>%
    left_join(sim %>%
                select(game_id, batting_team, appearance_order,
                       expected_id, expected_name,
                       natural_on_deck_id, natural_on_deck_name,
                       is_last_ab, is_pinch_hitter,
                       replaced_starter_id, replaced_starter_name,
                       true_lineup_spot,
                       override_on_deck_id, override_on_deck_name),
              by = c("game_id","batting_team","appearance_order")) %>%
    arrange(game_date, game_id, inning, at_bat_number, pitch_number) %>%
    mutate(on_deck_batter_id = if_else(override_on_deck_name != "" & 
                                         !is.na(override_on_deck_name),
                                       override_on_deck_id,
                                       natural_on_deck_id),
           on_deck_batter_name = if_else(override_on_deck_name != "" &
                                           !is.na(override_on_deck_name),
                                         override_on_deck_name,
                                         natural_on_deck_name)) %>%
    mutate(lookup = paste0(on_deck_batter_id, "_", year)) %>%
    left_join(woba_on_deck_lookup %>%
                select(lookup, stabilized_xwoba_on_deck),
              by = "lookup") %>%
    select(-lookup) %>%
    mutate(lookup = paste0(batter_id, "_", year)) %>%
    left_join(woba_lookup %>%
                select(lookup, stabilized_xwoba),
              by = "lookup") %>%
    select(-lookup) %>%
    mutate(in_strike_zone = ifelse(plate_x >= -(17/(12*2)) &
                                     plate_x <= (17/(12*2)) &
                                     plate_z >= strike_zone_bottom &
                                     plate_z <= strike_zone_top, 1, 0)) %>%
    relocate(in_strike_zone, .after = plate_z) %>%
    relocate(stabilized_xwoba, .after = batter_name) %>%
    relocate(stabilized_xwoba_on_deck, .after = on_deck_batter_name) %>%
    group_by(year) %>%
    mutate(stabilized_xwoba_on_deck = ifelse(is.na(stabilized_xwoba_on_deck), 
                                             mean(stabilized_xwoba_on_deck, 
                                                  na.rm = TRUE), 
                                             stabilized_xwoba_on_deck)) %>%
    mutate(stabilized_xwoba = ifelse(is.na(stabilized_xwoba),
                                     mean(stabilized_xwoba,
                                          na.rm = TRUE),
                                     stabilized_xwoba)) %>%
    ungroup() %>%
    as.data.frame()
}








data_t <- data_all %>% on_deck()


    

data_t %>%
  filter(is.na(stabilized_xwoba_on_deck)) %>%
  #filter(on_deck_batter_name == "Iglesias, Jose") %>%
  tail() %>%
  as.data.frame()

