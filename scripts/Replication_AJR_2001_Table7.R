# --- 1. Load Libraries ---
library(haven)
library(dplyr)
library(fixest)
library(modelsummary)
library(gt)
library(purrr)

# --- 2. Load and Prepare Data ---
ajr_dta <- read_dta("D:/replication-ajr/maketable7.dta")

# Create the base sample and the `other_cont` dummy variable
base_sample <- ajr_dta %>%
  filter(baseco == 1) %>%
  mutate(other_cont = if_else(shortnam %in% c("AUS", "MLT", "NZL"), 1, 0))

# Create subsamples for specific regressions
ols_sample_789 <- base_sample %>%
  filter(!is.na(logem4), !is.na(latabs), !is.na(lt100km), !is.na(meantemp))

ols_sample_1011 <- base_sample %>%
  filter(!is.na(yellow))

# --- 3. Run All Regression Models ---

# Run IV Models (for Panels A and B)
iv_models <- list(
  "(1)" = feols(logpgp95 ~ malfal94 | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst + malfal94 | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ leb95 | avexpr ~ logem4, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst + leb95 | avexpr ~ logem4, data = base_sample),
  "(5)" = feols(logpgp95 ~ imr95 | avexpr ~ logem4, data = base_sample),
  "(6)" = feols(logpgp95 ~ lat_abst + imr95 | avexpr ~ logem4, data = base_sample),
  "(7)" = feols(logpgp95 ~ 1 | avexpr + malfal94 ~ logem4 + latabs + lt100km + meantemp, data = base_sample),
  "(8)" = feols(logpgp95 ~ 1 | avexpr + leb95 ~ logem4 + latabs + lt100km + meantemp, data = base_sample),
  "(9)" = feols(logpgp95 ~ 1 | avexpr + imr95 ~ logem4 + latabs + lt100km + meantemp, data = base_sample),
  "(10)" = feols(logpgp95 ~ 1 | avexpr ~ yellow, data = base_sample),
  "(11)" = feols(logpgp95 ~ africa + asia + other_cont | avexpr ~ yellow, data = base_sample)
)

# Run OLS Models (for Panel C)
ols_models <- list(
  "(1)" = feols(logpgp95 ~ avexpr + malfal94, data = base_sample),
  "(2)" = feols(logpgp95 ~ avexpr + lat_abst + malfal94, data = base_sample),
  "(3)" = feols(logpgp95 ~ avexpr + leb95, data = base_sample),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst + leb95, data = base_sample),
  "(5)" = feols(logpgp95 ~ avexpr + imr95, data = base_sample),
  "(6)" = feols(logpgp95 ~ avexpr + lat_abst + imr95, data = base_sample),
  "(7)" = feols(logpgp95 ~ avexpr + malfal94, data = ols_sample_789),
  "(8)" = feols(logpgp95 ~ avexpr + leb95, data = ols_sample_789),
  "(9)" = feols(logpgp95 ~ avexpr + imr95, data = ols_sample_789),
  "(10)" = feols(logpgp95 ~ avexpr, data = ols_sample_1011),
  "(11)" = feols(logpgp95 ~ avexpr + africa + asia + other_cont, data = ols_sample_1011)
)

# --- 4. Generate Tables for Each Panel ---

# Panel A: 2SLS with Health Variables
print(modelsummary(
  iv_models,
  output = "gt",
  title = "Table 7, Panel A: 2SLS Regressions with Geography and Health Variables",
  coef_map = c("fit_avexpr" = "Average Expropriation Risk", "fit_malfal94" = "Malaria Index",
               "fit_leb95" = "Life Expectancy", "fit_imr95" = "Infant Mortality",
               "lat_abst" = "Latitude", "malfal94" = "Malaria Index",
               "leb95" = "Life Expectancy", "imr95" = "Infant Mortality",
               "africa" = "Africa Dummy", "asia" = "Asia Dummy", "other_cont" = "Other Continent Dummy"),
  gof_map = "nobs",
  stars = TRUE
))

# Panel B: First Stage
# --- THIS IS THE CORRECTED LINE ---
first_stage_models_avexpr <- purrr::map(iv_models, ~.$iv_first_stage$avexpr)
# ---------------------------------

print(modelsummary(
  first_stage_models_avexpr,
  output = "gt",
  title = "Table 7, Panel B: First-Stage Regressions for Expropriation Risk",
  coef_map = c("logem4" = "Log Settler Mortality", "yellow" = "Yellow Fever Dummy",
               "latabs" = "Latitude (abs)", "lt100km" = "Coastal Dummy",
               "meantemp" = "Mean Temperature", "lat_abst" = "Latitude",
               "malfal94" = "Malaria Index", "leb95" = "Life Expectancy", "imr95" = "Infant Mortality",
               "africa" = "Africa Dummy", "asia" = "Asia Dummy", "other_cont" = "Other Continent Dummy",
               "(Intercept)" = "Constant"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE
))

# Panel C: OLS Regressions
print(modelsummary(
  ols_models,
  output = "gt",
  title = "Table 7, Panel C: OLS Regressions with Geography and Health Variables",
  coef_map = c("avexpr" = "Average Expropriation Risk", "lat_abst" = "Latitude",
               "malfal94" = "Malaria Index", "leb95" = "Life Expectancy",
               "imr95" = "Infant Mortality", "africa" = "Africa Dummy",
               "asia" = "Asia Dummy", "other_cont" = "Other Continent Dummy"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE
))