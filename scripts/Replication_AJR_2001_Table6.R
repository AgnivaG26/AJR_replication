# --- 1. Load Libraries ---
library(haven)
library(dplyr)
library(fixest)
library(modelsummary)
library(gt)
library(purrr)
library(broom)
library(tidyr)

# --- 2. Load and Prepare Data ---
ajr_dta <- read_dta("D:/replication-ajr/maketable6.dta")
base_sample <- ajr_dta %>% filter(baseco == 1)

# --- 3. Define Control Variable Sets ---
temp_humid_controls <- "temp1 + temp2 + humid1 + humid2"
resource_controls <- "steplow + deslow + stepmid + desmid + drystep + drywint + goldm + iron + silv + zinc + oilres + landlock"
all_controls <- paste("lat_abst", temp_humid_controls, "edes1975", "avelf", resource_controls, sep = " + ")

# --- 4. Run All Regression Models ---

# Run IV Models (for Panels A and B)
iv_models <- list(
  "(1)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(as.formula(paste("logpgp95 ~ lat_abst +", temp_humid_controls, "| avexpr ~ logem4")), data = base_sample),
  "(3)" = feols(logpgp95 ~ edes1975 | avexpr ~ logem4, data = base_sample),
  "(4)" = feols(logpgp95 ~ lat_abst + edes1975 | avexpr ~ logem4, data = base_sample),
  "(5)" = feols(as.formula(paste("logpgp95 ~", resource_controls, "| avexpr ~ logem4")), data = base_sample),
  "(6)" = feols(as.formula(paste("logpgp95 ~ lat_abst +", resource_controls, "| avexpr ~ logem4")), data = base_sample),
  "(7)" = feols(logpgp95 ~ avelf | avexpr ~ logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + avelf | avexpr ~ logem4, data = base_sample),
  "(9)" = feols(as.formula(paste("logpgp95 ~", all_controls, "| avexpr ~ logem4")), data = base_sample)
)

# Run OLS Models (for Panel C)
ols_models <- list(
  "(1)" = feols(as.formula(paste("logpgp95 ~ avexpr +", temp_humid_controls)), data = base_sample),
  "(2)" = feols(as.formula(paste("logpgp95 ~ avexpr + lat_abst +", temp_humid_controls)), data = base_sample),
  "(3)" = feols(logpgp95 ~ avexpr + edes1975, data = base_sample),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst + edes1975, data = base_sample),
  "(5)" = feols(as.formula(paste("logpgp95 ~ avexpr +", resource_controls)), data = base_sample),
  "(6)" = feols(as.formula(paste("logpgp95 ~ avexpr + lat_abst +", resource_controls)), data = base_sample),
  "(7)" = feols(logpgp95 ~ avexpr + avelf, data = base_sample),
  "(8)" = feols(logpgp95 ~ avexpr + lat_abst + avelf, data = base_sample),
  "(9)" = feols(as.formula(paste("logpgp95 ~ avexpr +", all_controls)), data = base_sample)
)


# --- 5. Generate Table for Panel A ---
# This section manually builds the complex layout for Panel A

# Extract coefficients and p-values
tidy_results <- map_dfr(iv_models, tidy, .id = "model") %>%
  mutate(estimate_str = sprintf("%.2f", estimate), se_str = sprintf("(%.2f)", std.error))

get_p_value <- function(model, vars) { sprintf("[%.2f]", wald(model, vars)[[4]]) }
temp_vars <- c("temp1", "temp2"); humid_vars <- c("humid1", "humid2")
soil_vars <- c("steplow", "deslow", "stepmid", "desmid", "drystep", "drywint")
resource_vars <- c("goldm", "iron", "silv", "zinc", "oilres")

p_values <- list(
  temp2 = get_p_value(iv_models[['(2)']], temp_vars), humid2 = get_p_value(iv_models[['(2)']], humid_vars),
  soil5 = get_p_value(iv_models[['(5)']], soil_vars), resource5 = get_p_value(iv_models[['(5)']], resource_vars),
  soil6 = get_p_value(iv_models[['(6)']], soil_vars), resource6 = get_p_value(iv_models[['(6)']], resource_vars),
  temp9 = get_p_value(iv_models[['(9)']], temp_vars), humid9 = get_p_value(iv_models[['(9)']], humid_vars),
  soil9 = get_p_value(iv_models[['(9)']], soil_vars), resource9 = get_p_value(iv_models[['(9)']], resource_vars)
)

nobs <- map_chr(iv_models, ~as.character(nobs(.)))
find_val <- function(m, t, type) { tidy_results %>% filter(model == m, term == t) %>% pull({{type}}) %>% first() }

# Manually construct the table data frame
final_table_A <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, ~`(7)`, ~`(8)`, ~`(9)`,
  "Average protection against expropriation risk, 1985-1995", find_val("(1)", "fit_avexpr", "estimate_str"), find_val("(2)", "fit_avexpr", "estimate_str"), find_val("(3)", "fit_avexpr", "estimate_str"), find_val("(4)", "fit_avexpr", "estimate_str"), find_val("(5)", "fit_avexpr", "estimate_str"), find_val("(6)", "fit_avexpr", "estimate_str"), find_val("(7)", "fit_avexpr", "estimate_str"), find_val("(8)", "fit_avexpr", "estimate_str"), find_val("(9)", "fit_avexpr", "estimate_str"),
  "", find_val("(1)", "fit_avexpr", "se_str"), find_val("(2)", "fit_avexpr", "se_str"), find_val("(3)", "fit_avexpr", "se_str"), find_val("(4)", "fit_avexpr", "se_str"), find_val("(5)", "fit_avexpr", "se_str"), find_val("(6)", "fit_avexpr", "se_str"), find_val("(7)", "fit_avexpr", "se_str"), find_val("(8)", "fit_avexpr", "se_str"), find_val("(9)", "fit_avexpr", "se_str"),
  "Latitude", NA, find_val("(2)", "lat_abst", "estimate_str"), NA, find_val("(4)", "lat_abst", "estimate_str"), NA, find_val("(6)", "lat_abst", "estimate_str"), NA, find_val("(8)", "lat_abst", "estimate_str"), find_val("(9)", "lat_abst", "estimate_str"),
  "", NA, find_val("(2)", "lat_abst", "se_str"), NA, find_val("(4)", "lat_abst", "se_str"), NA, find_val("(6)", "lat_abst", "se_str"), NA, find_val("(8)", "lat_abst", "se_str"), find_val("(9)", "lat_abst", "se_str"),
  "p-value for temperature variables", NA, p_values$temp2, NA, NA, NA, NA, NA, NA, p_values$temp9,
  "p-value for humidity variables", NA, p_values$humid2, NA, NA, NA, NA, NA, NA, p_values$humid9,
  "Percent of European descent in 1975", NA, NA, find_val("(3)", "edes1975", "estimate_str"), find_val("(4)", "edes1975", "estimate_str"), NA, NA, NA, NA, find_val("(9)", "edes1975", "estimate_str"),
  "", NA, NA, find_val("(3)", "edes1975", "se_str"), find_val("(4)", "edes1975", "se_str"), NA, NA, NA, NA, find_val("(9)", "edes1975", "se_str"),
  "p-value for soil quality", NA, NA, NA, NA, p_values$soil5, p_values$soil6, NA, NA, p_values$soil9,
  "p-value for natural resources", NA, NA, NA, NA, p_values$resource5, p_values$resource6, NA, NA, p_values$resource9,
  "Dummy for being landlocked", NA, NA, NA, NA, find_val("(5)", "landlock", "estimate_str"), find_val("(6)", "landlock", "estimate_str"), NA, NA, find_val("(9)", "landlock", "estimate_str"),
  "", NA, NA, NA, NA, find_val("(5)", "landlock", "se_str"), find_val("(6)", "landlock", "se_str"), NA, NA, find_val("(9)", "landlock", "se_str"),
  "Ethnolinguistic fragmentation", NA, NA, NA, NA, NA, NA, find_val("(7)", "avelf", "estimate_str"), find_val("(8)", "avelf", "estimate_str"), find_val("(9)", "avelf", "estimate_str"),
  "", NA, NA, NA, NA, NA, NA, find_val("(7)", "avelf", "se_str"), find_val("(8)", "avelf", "se_str"), find_val("(9)", "avelf", "se_str"),
  "Number of observations", nobs[1], nobs[2], nobs[3], nobs[4], nobs[5], nobs[6], nobs[7], nobs[8], nobs[9]
)

# Render Panel A with gt
print(
  gt(final_table_A, rowname_col = "term") %>%
    tab_header(title = "Table 6, Panel A: Two-Stage Least Squares") %>%
    sub_missing(missing_text = "") %>%
    cols_align(align = "center", columns = -term) %>%
    tab_options(table.border.top.style = "none",
                column_labels.border.bottom.style = "solid",
                column_labels.border.bottom.width = px(2),
                table_body.border.bottom.style = "none")
)

# --- 6. Generate Table for Panel B ---
first_stage_models <- purrr::map(iv_models, ~.$iv_first_stage$avexpr)

print(
  modelsummary(
    first_stage_models,
    output = "gt",
    title = "Table 6, Panel B: First-Stage Regressions",
    coef_map = c("logem4" = "Log Settler Mortality"),
    gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                   list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
    stars = TRUE,
    notes = "Notes: Only the coefficient on Log Settler Mortality is shown. All models include the full set of controls."
  )
)

# --- 7. Generate Table for Panel C ---
print(
  modelsummary(
    ols_models,
    output = "gt",
    title = "Table 6, Panel C: OLS Robustness Checks",
    coef_map = c("avexpr" = "Average Expropriation Risk"),
    gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                   list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
    stars = TRUE,
    notes = "Notes: Only the coefficient on Average Expropriation Risk is shown. All models include the full set of controls."
  )
)
