library(rstan)
library(bayesplot)
library(ggplot2)
library(dplyr)





data_t <- data_t %>%
  filter(year >= 2022) %>%
  filter(!is.na(in_strike_zone)) %>%
  mutate(runners_on_base = rowSums(!is.na(across(c(pre_runner_1b_id, 
                                                   pre_runner_2b_id, 
                                                   pre_runner_3b_id)))))




data_t %>%
  filter(inning == 9,
         is_pinch_hitter == FALSE,
         half_inning == "bottom",
         outs == 0,
         bat_score != away_score,
         events == "strikeout" | events == "field_out") %>%
  tail()


data_t_reduced %>%
  filter(is.na(stabilized_xwoba))



stan_code_1 <- "

data {
  int<lower=1> N;
  int<lower=0, upper=1> y[N];             // pitch in zone
  vector[N] woba;                         // wOBA of current batter
  vector[N] woba_on_deck;                 // wOBA of on-deck batter
  int<lower=0, upper=3> balls[N];         // number of balls in count
  int<lower=0, upper=2> strikes[N];       // number of strikes in count
  int<lower=0, upper=2> outs[N];          // number of outs in inning
  int<lower=0, upper=3> runners_on[N];    // number of runners on base
}


parameters {
  real alpha;
  real beta_woba;
  real beta_woba_on_deck;
  real beta_balls;
  real beta_strikes;
  real beta_outs;
  real beta_runners_on;
}


model {
  // Priors
  alpha ~ normal(0, 2);
  beta_woba ~ normal(0, 1);
  beta_woba_on_deck ~ normal(0, 1);
  beta_balls ~ normal(0, 1);
  beta_strikes ~ normal(0, 1);
  beta_outs ~ normal(0, 1);
  beta_runners_on ~ normal(0, 1);

  // Logistic regression likelihood
  for (i in 1:N)
    y[i] ~ bernoulli_logit(alpha + 
    beta_woba * woba[i] + beta_woba_on_deck * woba_on_deck[i] + 
    beta_balls * balls[i] + beta_strikes * strikes[i] + beta_outs * outs[i] +
    beta_runners_on * runners_on[i]);
}

"


data_t_reduced <- data_t %>%
  group_by(in_strike_zone) %>%   # preserves the class balance
  sample_frac(0.01) %>%          # adjust as needed (0.05 = 5%)
  ungroup() %>%
  filter(balls < 4) %>%
  as.data.frame()

stan_data <- list(N = nrow(data_t_reduced),
                  y = data_t_reduced$in_strike_zone,
                  woba = data_t_reduced$stabilized_xwoba,
                  woba_on_deck = data_t_reduced$stabilized_xwoba_on_deck,
                  balls = data_t_reduced$balls,
                  strikes = data_t_reduced$strikes,
                  outs = data_t_reduced$outs,
                  runners_on = data_t_reduced$runners_on_base)

fit_zone <- stan(model_code = stan_code_1,
                 data = stan_data,
                 iter = 2000,
                 warmup = 1000,
                 chains = 4,
                 seed = 42,
                 control = list(adapt_delta = 0.95),
                 cores = parallel::detectCores())


print(fit_zone, pars = c("alpha", "beta_woba", "beta_woba_on_deck", 
                         "beta_balls", "beta_strikes", "beta_outs", "beta_runners_on"), 
      probs = c(0.025, 0.5, 0.975))


