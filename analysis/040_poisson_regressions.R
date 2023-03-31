library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/poisson_regressions.R"))

# import data ------------------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))
time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet"))
time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet"))
time_data_frac <- arrow::read_parquet(here::here("output/timeupdate_dataset_fracture.gz.parquet"))

# add simple poisson regression results -----------------------------------
stratifiers_t <- c("vaccines","t_vacc_detail", "t_vacc_grouped", "t_vacc_twodose")
strat_plot_names_t <- c("No. vaccine doses", "Vaccine schedule (detail)", "Vaccine schedule (grouped)", "Vaccine schedule (first 2 doses)")
adjusted_rates_t_lc_all <- map(stratifiers_t, ~poisson_regressions(cohort_data = time_data_lc_all, .x))
adjusted_rates_t_lc_dx <- map(stratifiers_t, ~poisson_regressions(cohort_data = time_data_lc_dx, .x))
adjusted_rates_t_frac <- map(stratifiers_t, ~poisson_regressions(cohort_data = time_data_frac, .x))

stratifiers <- c("sex","age_cat", "practice_nuts", "ethnicity", "comorbidities")
strat_plot_names <- c("Sex","Age category", "Region", "Ethnicity", "Comorbidities")
  cleaned_data$out <- cleaned_data$lc_out
adjusted_rates_c_lc_all <- map(stratifiers, ~poisson_regressions(cohort_data = cleaned_data, .x))
  cleaned_data$out <- as.numeric(!is.na(cleaned_data$first_lc_dx))
adjusted_rates_c_lc_dx <- map(stratifiers, ~poisson_regressions(cohort_data = cleaned_data, .x))
  cleaned_data$out <- as.numeric(!is.na(cleaned_data$first_fracture_hosp))
adjusted_rates_c_frac <- map(stratifiers, ~poisson_regressions(cohort_data = cleaned_data, .x))

adjusted_rates_out <- NULL
adjusted_rates_list <- c("adjusted_rates_t_lc_all", "adjusted_rates_t_lc_dx", "adjusted_rates_t_frac")
adjusted_rates_c_list <- c("adjusted_rates_c_lc_all", "adjusted_rates_c_lc_dx", "adjusted_rates_c_frac")
outcome_list <- c("All Long COVID", "Long COVID diagnoses", "Fractures")
for(j in 1:3){
  adjusted_rates_temp <- bind_rows(get(adjusted_rates_list[j]), get(adjusted_rates_c_list[j]))
  outcome_name <- outcome_list[j]
  adjusted_rates_out <- bind_rows(
    adjusted_rates_out, 
    bind_cols(adjusted_rates_temp, outcome = outcome_name)
  ) 
}

adjusted_rates_out <- adjusted_rates_out %>% 
  mutate(strat_var = factor(
    var, 
    levels = c(stratifiers_t,  stratifiers), 
    labels = c(strat_plot_names_t, strat_plot_names)),
    term2 = stringr::str_remove_all(term, var))

adjusted_rates_out %>% 
  write_csv(here("output/tab023_poissonrates.csv"))
