source("Statcast Scraper.R")
library(rstan)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(splines)


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



## Scrape Statcast Data -------------------------------------------------------------
data_2024 <- statcast_scraper(start_date = "2024-01-01", end_date = "2024-12-31")
data_2023 <- statcast_scraper(start_date = "2023-01-01", end_date = "2023-12-31")
data_2022 <- statcast_scraper(start_date = "2022-01-01", end_date = "2022-12-31")
data_2021 <- statcast_scraper(start_date = "2021-01-01", end_date = "2021-12-31")
data_2020 <- statcast_scraper(start_date = "2020-01-01", end_date = "2020-12-31")
data_2019 <- statcast_scraper(start_date = "2019-01-01", end_date = "2019-12-31")
data_2018 <- statcast_scraper(start_date = "2018-01-01", end_date = "2018-12-31")
data_2017 <- statcast_scraper(start_date = "2017-01-01", end_date = "2017-12-31")
data_2016 <- statcast_scraper(start_date = "2016-01-01", end_date = "2016-12-31")
data_2015 <- statcast_scraper(start_date = "2015-01-01", end_date = "2015-12-31")


data_all <- rbind(data_2015, data_2016, data_2017, data_2018, data_2019, 
                  data_2020, data_2021, data_2022, data_2023, data_2024)

data_all <- read.csv("data_all.csv")





## Aggregates wOBA by Player-Season -------------------------------------------------
woba_summary_by_player_season <- function(data) {
  
  # 1) Summarize season-level stats by player-season.
  # Only include records where woba_denom == 1.
  season_summary <- data %>%
    filter(woba_denom == 1) %>%
    group_by(batter_id, batter_name, year, age_bat_legacy) %>%
    summarise(season_PA = sum(woba_denom),
              season_sum_expected = sum(expected_woba, na.rm = TRUE),
              season_xwoba = season_sum_expected / season_PA,
              .groups = "drop")
  
  
  # 2) Calculate historical values for each batter from seasons prior.
  season_summary <- season_summary %>%
    arrange(batter_id, year) %>%  
    group_by(batter_id) %>%
    mutate(hist_PA = lag(cumsum(season_PA), default = 0),
           hist_sum_expected = lag(cumsum(season_sum_expected), default = 0),
           hist_xwoba = if_else(hist_PA > 0, hist_sum_expected / hist_PA, 0)) %>%
    ungroup() %>%
    select(-c("season_sum_expected", "hist_sum_expected")) %>%
    rename(age = age_bat_legacy)
  
  as.data.frame(season_summary)
}


df_woba <- woba_summary_by_player_season(data_all) %>%
  filter(year >= 2016) %>%
  mutate(season_id = as.integer(factor(year, levels = sort(unique(year)))))





## Gets Primary position for each player in each season -----------------------------

get_positions <- function(data_all = data_all,
                          df_woba = df_woba,
                          min_fld = 4000,
                          min_pit = 10) {
  
  # 1) Pivot fielder columns long
  pos_long <- data_all %>%
    select(year, starts_with("fielder_")) %>%
    pivot_longer(cols            = starts_with("fielder_"),
                 names_to        = "field_col",
                 values_to       = "player_id",
                 values_drop_na  = TRUE) %>%
    mutate(position = case_when(field_col == "fielder_2_id" ~ "C",
                                field_col == "fielder_3_id" ~ "1B",
                                field_col == "fielder_4_id" ~ "2B",
                                field_col == "fielder_5_id" ~ "3B",
                                field_col == "fielder_6_id" ~ "SS",
                                field_col == "fielder_7_id" ~ "LF",
                                field_col == "fielder_8_id" ~ "CF",
                                field_col == "fielder_9_id" ~ "RF",
                                TRUE ~ NA_character_)) %>%
    filter(!is.na(position))
  
  
  # 2) Count fielder appearances and pick most frequent spot
  fielder_primary <- pos_long %>%
    count(year, player_id, position, name = "fld_count") %>%
    group_by(year, player_id) %>%
    slice_max(order_by = fld_count, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(total_fld = fld_count,
           fld_pos   = if_else(total_fld >= min_fld, position, NA_character_)) %>%
    select(year, player_id, fld_pos, total_fld)
  
  
  # 3) Count pitcher appearances
  pitcher_primary <- data_all %>%
    count(year, player_id = pitcher_id, name = "pitch_count") %>%
    filter(!is.na(player_id))
  
  
  # 4) Combine and apply rules
  position_map <- full_join(fielder_primary, pitcher_primary,
                            by = c("year","player_id")) %>%
    replace_na(list(total_fld = 0, pitch_count = 0)) %>%
    mutate(primary_position = case_when(!is.na(fld_pos) ~ fld_pos,
                                        total_fld == 0 & pitch_count > min_pit ~ "P",
                                        TRUE ~ "DH")) %>%
    select(year, player_id, primary_position)
  
  
  # 5) Join back onto batter‐level data
  df_woba <- df_woba %>%
    left_join(position_map,
              by = c("year", "batter_id" = "player_id")) %>%
    mutate(primary_position = ifelse(is.na(primary_position), "DH", 
                                     primary_position)) %>%
    mutate(primary_position = ifelse(batter_name == "Ohtani, Shohei", "DH", 
                                     primary_position)) %>%
    filter(primary_position != "P") # Remove pitchers
  
  return(df_woba)

}


df_woba <- get_positions(data_all, df_woba)






## Stabilized xwOBA with Age Curve and Positional Adjustments -----------------------

# 1) Stan Model
stan_code_wOBA <- "

data {
  int<lower=1>  J;          // number of player‐seasons
  vector[J]     y;          // historical xwOBA
  int<lower=0>  N[J];       // historical PA
  matrix[J,4]   A_basis;    // B‐spline basis evaluated at age
  int<lower=1>  pos[J];     // position index in 1..9
  int<lower=1>  K_pos;      // 9
  int<lower=1>  K_basis;    // 4
}


parameters {
  real<lower=0,upper=1>   mu;       // global intercept
  real<lower=0>           tau;      // between‐player SD
  real<lower=0>           sigma;    // measurement noise scale
  matrix[K_pos,K_basis]   gamma;    // age‐position spline coefs
  vector[J]               eta;      // player‐level deviations
}


transformed parameters {
  vector[J] theta;
  
  // construct theta_j = mu + f_{pos[j]}(age[j]) + eta[j]
  for (j in 1:J) {
    // dot product of the 4‐vector A_basis[j] with gamma[pos[j], ]
    theta[j] = mu + dot_product( A_basis[j], gamma[pos[j]] ) + eta[j];
  }
}


model {
  // Priors
  mu    ~ normal(0.320, 0.05);      // league-average prior (0.320)
  tau   ~ cauchy(0, 0.05);
  sigma ~ cauchy(0, 0.05);
  to_vector(gamma) ~ normal(0, 0.1);
  eta   ~ normal(0, tau);

  // Likelihood (if N[j]=0, no data term → pure prior)
  for (j in 1:J) {
    if (N[j] > 0)
      y[j] ~ normal(theta[j], sigma / sqrt(N[j]));
  }
}


generated quantities {
  vector[J] y_rep;
  
for (j in 1:J) {
    if (N[j] > 0) {
      // realistic posterior‐predictive draw
      y_rep[j] = normal_rng(theta[j], sigma / sqrt(N[j]));
    } else {
      // no plate appearances → just return the latent theta (or league mean)
      y_rep[j] = theta[j];
    }
  }
}

"


# 2) Build spline basis (4 basis functions, cubic) on age
#    The `intercept=TRUE` means B_1...B_4 sum to 1 at each age.
Bmat <- bs(df_woba$age, df = 4, degree = 3, intercept = TRUE)


# 3) Turn primary_position into an integer code 1..9
#    Mapping: 1 = "DH", 2 = "C", 3 = "1B", 4 = "2B", 5 = "3B", 
#             6 = "SS", 7 = "LF", 8 = "CF",  9= "RF"
pos_levels <- c("DH","C","1B","2B","3B","SS","LF","CF","RF")
df_woba <- df_woba %>%
  mutate(pos_idx = match(primary_position, pos_levels))


# 4) Prepare data
stan_data_wOBA <- list(J       = nrow(df_woba),
                       y       = df_woba$hist_xwoba,
                       N       = df_woba$hist_PA,
                       A_basis = as.data.frame(Bmat), # J×4 matrix
                       pos     = df_woba$pos_idx,     # length‑J integer vector (1..9)
                       K_pos   = length(pos_levels),  # 9
                       K_basis = ncol(Bmat))          # 4


# 4) Fit the model
fit_wOBA <- stan(model_code = stan_code_wOBA,
                 data       = stan_data_wOBA,
                 iter       = 2000,
                 warmup     = 1000, 
                 chains     = 4,
                 seed       = 123,
                 control    = list(adapt_delta = 0.95),
                 cores      = parallel::detectCores())


# 5) Extract posterior means (and CIs if you like)
df_woba <- df_woba %>%
  mutate(stabilized_xwoba = apply(rstan::extract(fit_wOBA, pars = "theta")$theta, 2, 
                                  mean),
         ci_low = apply(rstan::extract(fit_wOBA, pars = "theta")$theta, 2,
                        quantile, 
                        probs = 0.025),
         ci_high = apply(rstan::extract(fit_wOBA, pars = "theta")$theta, 2,
                         quantile,
                         probs = 0.975),
         resid = hist_xwoba - stabilized_xwoba)






# pull out all gamma samples: an array [iterations × K_pos × K_basis]
gamma_samps <- rstan::extract(fit_wOBA, pars="gamma")$gamma

# summarise each position k’s spline coefficients
# e.g. posterior mean ± 95% CI
gamma_summary <- apply(gamma_samps, c(2,3), function(x)
  c(mean = mean(x),
    hdi_lower = quantile(x, .025),
    hdi_upper = quantile(x, .975)))

print(gamma_summary)




ages <- seq(min(df_woba$age), 
            max(df_woba$age), 
            length = 100)
Bgrid <- bs(ages, 
            df = 4, 
            degree = 3, 
            intercept = TRUE)

# for each draw t and position k, compute f_k(ages)
# this gives an array [draws × positions × ages]
f_draws <- array(NA, dim = c(dim(gamma_samps)[1],   # draws
                             dim(gamma_samps)[2],   # positions
                             length(ages)))
for (t in 1:dim(gamma_samps)[1]) {
  f_draws[t,,] <- gamma_samps[t,,] %*% t(Bgrid)
}

# collapse to mean and 95% CI for each position at each age
# assemble a tidy data.frame for ggplot
plot_df <- expand.grid(Position = pos_levels,
                       age      = ages) %>%
  mutate(mean = as.vector(t(apply(f_draws, c(2,3), mean))),
         lo   = as.vector(t(apply(f_draws, c(2,3), quantile, probs = .025))),
         hi   = as.vector(t(apply(f_draws, c(2,3), quantile, probs = .975))))

ggplot(plot_df, aes(age, mean, color = Position)) +
  geom_line() +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = Position),
              alpha = 0.2, color = NA) +
  labs(title = "Estimated age trajectories by position",
       y = "f_k(age)", x = "Age")


posterior <- as.array(fit_wOBA)

# 3a) R̂ and n_eff
print(fit_wOBA, pars = c("mu", "tau", "sigma"), probs = c(.025, .5, .975))

# 3b) Traceplots for key parameters
mcmc_trace(posterior, pars = c("mu", "tau", "sigma"))

# 3c) Pair‐plots for potential correlations
mcmc_pairs(posterior, pars = c("mu", "tau", "sigma"))



# extract observed y and replicated y
# 4a) Density overlay
ppc_dens_overlay(y = stan_data_wOBA$y,
                 yrep = rstan::extract(fit_wOBA, "y_rep")$y_rep[1:1000, ]) +
  ggtitle("PPC: Observed (y) vs rep hist_xwOBA") + 
  scale_x_continuous("wOBA",
                     limits = c(0, 0.5))

# 4b) Scatter of mean & SD
ppc_stat(y = stan_data_wOBA$y,
         yrep = rstan::extract(fit_wOBA, "y_rep")$y_rep[1:200, ],
         stat = "mean")
ppc_stat(y = stan_data_wOBA$y,
         yrep = rstan::extract(fit_wOBA, "y_rep")$y_rep[1:200, ],
         stat = "sd")


plot(df_woba$hist_PA, df_woba$resid, 
     xlab = "PA", ylab = "Residual", main = "Residual vs Plate Appearances") + 
  abline(h = 0, lty = 2)












woba_on_deck_lookup <- df_woba %>%
  select(batter_id, year, stabilized_xwoba) %>%
  rename(on_deck_batter_id = batter_id,
         stabilized_xwoba_on_deck = stabilized_xwoba) %>%
  mutate(lookup = paste0(on_deck_batter_id, "_", year))

woba_lookup <- df_woba %>%
  select(batter_id, year, stabilized_xwoba) %>%
  rename(stabilized_xwoba = stabilized_xwoba) %>%
  mutate(lookup = paste0(batter_id, "_", year))

df_woba <- df_woba %>%
  mutate(lookup = paste0(batter_id, "_", year),
         stabilized_xwoba_on_deck = stabilized_xwoba)



woba_lookup %>%
  filter(batter_id == 808982)
