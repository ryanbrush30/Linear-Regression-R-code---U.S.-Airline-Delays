############################################################
# Bayesian Linear Regression for Airline Arrival Delays
############################################################

# 0. Setup --------------------------------------------------------------

# install.packages(c("tidyverse", "brms", "knitr"))  # if needed

library(tidyverse)
library(brms)
library(knitr)

# 1. Load data ----------------------------------------------------------

# Assumes the CSV is in your working directory
flights <- read_csv("flight_data_2018_2024.csv",
                    show_col_types = FALSE) %>%
  rename(
    ARR_DELAY = ArrDelayMinutes,
    DISTANCE  = Distance,
    CARRIER   = IATA_Code_Marketing_Airline,
    MONTH     = Month
  )

# 2. Clean & engineer variables -----------------------------------------

flights_clean <- flights %>%
  filter(
    !is.na(ARR_DELAY),
    ARR_DELAY > -60,   # trim extreme early arrivals
    ARR_DELAY < 360    # trim extreme long delays
  ) %>%
  mutate(
    delay       = ARR_DELAY,
    winter      = if_else(MONTH %in% c(12, 1, 2), 1, 0),
    dist_scaled = as.numeric(scale(DISTANCE)),
    # keep top 5 carriers; others go into "Other"
    CARRIER     = fct_lump_n(CARRIER, n = 5)
  )

# Quick sanity check
summary(flights_clean$delay)

# 3. Exploratory plots ---------------------------------------------------

# Histogram of delays
ggplot(flights_clean, aes(x = delay)) +
  geom_histogram(binwidth = 5) +
  labs(
    title = "Distribution of Arrival Delays",
    x     = "Delay (minutes)",
    y     = "Count"
  ) +
  theme_minimal()

# Boxplot of winter vs non-winter
ggplot(flights_clean, aes(x = factor(winter), y = delay)) +
  geom_boxplot() +
  labs(
    title = "Delays: Winter vs Non-Winter",
    x     = "Winter (1 = Dec–Feb, 0 = Other)",
    y     = "Delay (minutes)"
  ) +
  theme_minimal()

# 4. Sample data for modeling -------------------------------------------

set.seed(123)

flights_sample <- flights_clean %>%
  slice_sample(n = 8000) %>%  # subsample for speed
  drop_na(delay, winter, dist_scaled, CARRIER)

# Inspect carrier levels
levels(flights_sample$CARRIER)

# 5. Bayesian linear regression model -----------------------------------

# Model: delay ~ winter + dist_scaled + CARRIER
# Likelihood: Gaussian
# Priors:
#   Intercept ~ Normal(0, 20)
#   Coefficients ~ Normal(0, 5)
#   sigma ~ Student-t(3, 0, 20)

fit_lm <- brm(
  delay ~ winter + dist_scaled + CARRIER,
  data   = flights_sample,
  family = gaussian(),
  prior  = c(
    prior(normal(0, 20), class = "Intercept"),
    prior(normal(0, 5),  class = "b"),
    prior(student_t(3, 0, 20), class = "sigma")
  ),
  chains  = 2,
  iter    = 1500,
  warmup  = 750,
  cores   = 2,
  control = list(adapt_delta = 0.9),
  seed    = 123
)

summary(fit_lm)

# 6. Model summary table -------------------

# Basic model info
model_family  <- family(fit_lm)
n_obs         <- nobs(fit_lm)

cat("Family:", model_family$family, "\n")
cat("Links: mu = identity; sigma = identity\n")
cat("Formula: delay ~ winter + dist_scaled + CARRIER\n")
cat("Data: flights_sample (Number of observations:", n_obs, ")\n\n")

# Numeric parameter table
pars_of_interest <- c(
  "b_Intercept",
  "b_winter",
  "b_dist_scaled",
  "b_CARRIERAS",
  "b_CARRIERDL",
  "b_CARRIERUA",
  "b_CARRIERWN",
  "b_CARRIEROther",
  "sigma"
)

model_summary_tbl <- posterior_summary(fit_lm, variable = pars_of_interest) %>%
  as_tibble(rownames = "Parameter") %>%
  mutate(
    Parameter = str_replace(Parameter, "^b_", "")  # remove "b_"
  ) %>%
  transmute(
    Parameter,
    Estimate   = round(Estimate,   2),
    Est.Error  = round(Est.Error,  2),
    `l-95% CI` = round(Q2.5,       2),
    `u-95% CI` = round(Q97.5,      2)
  )

# Add plain-English explanations for non-technical readers
model_summary_explained <- model_summary_tbl %>%
  mutate(
    Explanation = case_when(
      Parameter == "Intercept"    ~ "Baseline average delay (American Airlines, non-winter, average distance).",
      Parameter == "winter"       ~ "Average winter delay compared to non-winter.",
      Parameter == "dist_scaled"  ~ "Effect of flight distance (one standard deviation increase).",
      Parameter == "CARRIERAS"    ~ "Difference in average delay: Alaska vs American.",
      Parameter == "CARRIERDL"    ~ "Difference in average delay: Delta vs American.",
      Parameter == "CARRIERUA"    ~ "Difference in average delay: United vs American.",
      Parameter == "CARRIERWN"    ~ "Difference in average delay: Southwest vs American.",
      Parameter == "CARRIEROther" ~ "Difference in average delay: 'Other' airlines vs American.",
      Parameter == "sigma"        ~ "Random variability in delays (how much individual flights bounce around).",
      TRUE                        ~ "Other parameter."
    )
  ) %>%
  # put Explanation right after Parameter
  select(Parameter, Explanation, Estimate, Est.Error, `l-95% CI`, `u-95% CI`)

# Print / knit table with explanations
model_summary_explained

kable(
  model_summary_explained,
  caption = "Bayesian Linear Regression: Parameter Estimates",
  align   = c("l","l","r","r","r","r")
)

# 7. Posterior predictive check -----------------------------------------

pp_check(fit_lm, ndraws = 100) +
  ggtitle("Posterior Predictive Check: Arrival Delays")

# 8. Prediction comparison: Winter vs Non-Winter ------------------------

# Use American Airlines as reference (level "AA")
carrier_aa <- "AA"

newdata_aa <- tibble(
  winter      = c(1, 0),  # 1 = Winter (Dec–Feb), 0 = Non-winter
  dist_scaled = mean(flights_sample$dist_scaled),
  CARRIER     = factor(c(carrier_aa, carrier_aa),
                       levels = levels(flights_sample$CARRIER))
)

# 8a. Posterior predictive draws
pred_draws <- posterior_predict(fit_lm, newdata = newdata_aa)

pred_tbl <- as_tibble(pred_draws) %>%
  set_names(c("Winter", "NonWinter")) %>%
  pivot_longer(everything(),
               names_to  = "Scenario",
               values_to = "delay_pred") %>%
  group_by(Scenario) %>%
  summarise(
    Mean              = mean(delay_pred),
    Median            = median(delay_pred),
    `95% CI Lower`    = quantile(delay_pred, 0.025),
    `95% CI Upper`    = quantile(delay_pred, 0.975),
    `P(Delay >= 30)`  = mean(delay_pred >= 30),
    .groups = "drop"
  )

# Difference in probability of a 30+ minute delay
diff_draws <- pred_draws[, 1] - pred_draws[, 2]

diff_row <- tibble(
  Scenario         = "Difference (Winter - NonWinter)",
  Mean             = mean(diff_draws),
  Median           = median(diff_draws),
  `95% CI Lower`   = quantile(diff_draws, 0.025),
  `95% CI Upper`   = quantile(diff_draws, 0.975),
  `P(Delay >= 30)` = mean(pred_draws[, 1] >= 30) - mean(pred_draws[, 2] >= 30)
)

pred_summary_tbl <- bind_rows(pred_tbl, diff_row) %>%
  mutate(
    Mean             = round(Mean, 1),
    Median           = round(Median, 1),
    `95% CI Lower`   = round(`95% CI Lower`, 1),
    `95% CI Upper`   = round(`95% CI Upper`, 1),
    `P(Delay >= 30)` = round(`P(Delay >= 30)` * 100, 1)  # percent
  )

pred_summary_tbl

kable(
  pred_summary_tbl,
  caption = "Posterior Predictive Delay: Winter vs Non-Winter (American Airlines)",
  align   = c("l","r","r","r","r","r")
)

# 8b. Mean-delay comparison using posterior

mu_draws <- posterior_linpred(fit_lm, newdata = newdata_aa)

mu_tbl <- as_tibble(mu_draws) %>%
  set_names(c("Winter", "NonWinter")) %>%
  pivot_longer(everything(),
               names_to  = "Scenario",
               values_to = "mu") %>%
  group_by(Scenario) %>%
  summarise(
    Mean_mu        = mean(mu),
    Median_mu      = median(mu),
    `95% CI Lower` = quantile(mu, 0.025),
    `95% CI Upper` = quantile(mu, 0.975),
    .groups = "drop"
  )

diff_mu <- mu_draws[, 1] - mu_draws[, 2]

diff_mu_row <- tibble(
  Scenario         = "Difference (Winter - NonWinter, mean delay)",
  Mean_mu          = mean(diff_mu),
  Median_mu        = median(diff_mu),
  `95% CI Lower`   = quantile(diff_mu, 0.025),
  `95% CI Upper`   = quantile(diff_mu, 0.975)
)

mean_diff_tbl <- bind_rows(mu_tbl, diff_mu_row) %>%
  mutate(
    Mean_mu        = round(Mean_mu, 1),
    Median_mu      = round(Median_mu, 1),
    `95% CI Lower` = round(`95% CI Lower`, 1),
    `95% CI Upper` = round(`95% CI Upper`, 1)
  )

mean_diff_tbl

kable(
  mean_diff_tbl,
  caption = "Posterior Mean Arrival Delay: Winter vs Non-Winter (American Airlines)",
  align   = c("l","r","r","r","r")
)

# 9. Posterior predictive distributions plot -----------------------------

pred_df <- as_tibble(pred_draws) %>%
  set_names(c("Winter", "NonWinter")) %>%
  pivot_longer(everything(),
               names_to  = "Scenario",
               values_to = "delay_pred")

ggplot(pred_df, aes(x = delay_pred, fill = Scenario)) +
  geom_density(alpha = 0.4) +
  labs(
    title    = "Posterior Predictive Delay Distributions",
    subtitle = "American Airlines flights: Winter (Dec–Feb) vs Non-Winter",
    x        = "Predicted Arrival Delay (minutes)",
    y        = "Density"
  ) +
  theme_minimal()

# 10. Coefficient plot with credible intervals ---------------------------

coef_plot_tbl <- posterior_summary(fit_lm) %>%   # no 'variable =' here
  as_tibble(rownames = "Parameter") %>%
  # keep only regression coefficients (those starting with "b_")
  filter(str_starts(Parameter, "b_")) %>%
  mutate(
    Parameter = str_replace(Parameter, "^b_", "")  # remove "b_" prefix
  ) %>%
  select(Parameter, Estimate, Q2.5, Q97.5)

coef_plot_tbl  # optional: inspect

ggplot(coef_plot_tbl,
       aes(y = fct_reorder(Parameter, Estimate),
           x = Estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5)) +
  labs(
    title = "Posterior Coefficients with 95% Credible Intervals",
    x     = "Effect on Arrival Delay (minutes)",
    y     = NULL
  ) +
  theme_minimal()

############################################################
# End of script
############################################################