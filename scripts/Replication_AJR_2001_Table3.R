# --- Load Libraries ---
library(haven)
library(dplyr)
library(fixest)
library(modelsummary)
library(gt) 

# --- Load and Prepare Data ---
ajr_dta <- read_dta("D:/replication-ajr/maketable3.dta")

# Create the main sample used in most regressions for Table 3
main_sample <- ajr_dta %>%
  filter(excolony == 1, !is.na(extmort4)) %>%
  mutate(euro1900 = euro1900 / 100)

# Create the smaller subsample for regressions that also require log GDP data
lpgp_sample <- main_sample %>%
  filter(!is.na(logpgp95))

# --- Run Regressions for Panel A ---
panel_A_models <- list(
  "(1)" = feols(avexpr ~ cons00a, data = main_sample),
  "(2)" = feols(avexpr ~ cons00a + lat_abst, data = main_sample),
  "(3)" = feols(avexpr ~ democ00a, data = main_sample),
  "(4)" = feols(avexpr ~ democ00a + lat_abst, data = main_sample),
  "(5)" = feols(avexpr ~ indtime + cons1, data = main_sample),
  "(6)" = feols(avexpr ~ indtime + cons1 + lat_abst, data = main_sample),
  "(7)" = feols(avexpr ~ euro1900, data = main_sample),
  "(8)" = feols(avexpr ~ euro1900 + lat_abst, data = main_sample),
  "(9)" = feols(avexpr ~ logem4, data = lpgp_sample), # Uses smaller sample
  "(10)" = feols(avexpr ~ logem4 + lat_abst, data = lpgp_sample) # Uses smaller sample
)

# --- Generate Table for Panel A ---
modelsummary(
  panel_A_models,
  output = "gt",
  title = "Table 3, Panel A: Determinants of Institutions",
  coef_map = c("cons00a" = "Constraint on Executive in 1900", "democ00a" = "Democracy in 1900",
               "cons1" = "Constraint on Executive at Independence", "indtime" = "Date of Independence",
               "euro1900" = "European Settlements in 1900", "logem4" = "Log Settler Mortality",
               "lat_abst" = "Distance from Equator"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE,
  notes = "Notes: Dependent Variable is Average Expropriation Risk, 1985-95. Standard errors in parentheses."
)

# --- Run Regressions for Panel B ---
panel_B_models <- list(
  # Dep Var: Constraint on Executive in 1900
  "(1)" = feols(cons00a ~ euro1900, data = lpgp_sample),
  "(2)" = feols(cons00a ~ euro1900 + lat_abst, data = lpgp_sample),
  "(3)" = feols(cons00a ~ logem4, data = main_sample),
  "(4)" = feols(cons00a ~ logem4 + lat_abst, data = main_sample),
  # Dep Var: Democracy in 1900
  "(5)" = feols(democ00a ~ euro1900, data = lpgp_sample),
  "(6)" = feols(democ00a ~ euro1900 + lat_abst, data = lpgp_sample),
  "(7)" = feols(democ00a ~ logem4, data = lpgp_sample),
  "(8)" = feols(democ00a ~ logem4 + lat_abst, data = lpgp_sample),
  # Dep Var: European Settlements in 1900
  "(9)" = feols(euro1900 ~ logem4, data = lpgp_sample),
  "(10)" = feols(euro1900 ~ logem4 + lat_abst, data = lpgp_sample)
)

# --- Generate Table for Panel B ---
modelsummary(
  panel_B_models,
  output = "gt",
  title = "Table 3, Panel B: Determinants of Early Institutions",
  coef_map = c("euro1900" = "European Settlements in 1900", "logem4" = "Log Settler Mortality",
               "lat_abst" = "Distance from Equator"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE,
  notes = "Notes: Standard errors in parentheses."
) %>%
  # Add spanners to clarify the dependent variable for each set of columns
  tab_spanner(label = "Dep. Var: Constraint on Executive in 1900", columns = 2:5) %>%
  tab_spanner(label = "Dep. Var: Democracy in 1900", columns = 6:9) %>%
  tab_spanner(label = "Dep. Var: European Settlements in 1900", columns = 10:11)