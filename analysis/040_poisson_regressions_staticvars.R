library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/poisson_regressions.R"))

# import data ------------------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))

# add poisson regression results -----------------------------------
stratifiers <- c("sex","age_cat", "practice_nuts", "ethnicity", "comorbidities", "highrisk_shield")
strat_plot_names <- c("Sex","Age category", "Region", "Ethnicity", "Comorbidities", "Shielding (high risk group)")
  cleaned_data$out <- as.numeric(!is.na(cleaned_data$first_lc))
adjusted_rates_c_lc_all <- map(stratifiers, ~poisson_regressions(cohort_data = cleaned_data, .x))
  cleaned_data$out <- as.numeric(!is.na(cleaned_data$first_lc_dx))
adjusted_rates_c_lc_dx <- map(stratifiers, ~poisson_regressions(cohort_data = cleaned_data, .x))
  cleaned_data$out <- as.numeric(!is.na(cleaned_data$first_fracture_hosp))
adjusted_rates_c_frac <- map(stratifiers, ~poisson_regressions(cohort_data = cleaned_data, .x))

adjusted_rates_out <- NULL
adjusted_rates_c_list <- c("adjusted_rates_c_lc_all", "adjusted_rates_c_lc_dx", "adjusted_rates_c_frac")
outcome_list <- c("All Long COVID", "Long COVID diagnoses", "Fractures")
for(j in 1:3){
  print(j)
  adjusted_rates_temp <- bind_rows(get(adjusted_rates_c_list[j]))
  outcome_name <- outcome_list[j]
  adjusted_rates_out <- bind_rows(
    adjusted_rates_out, 
    bind_cols(adjusted_rates_temp, outcome = outcome_name)
  ) 
}

adjusted_rates_out <- adjusted_rates_out %>% 
  mutate(strat_var = factor(
    var, 
    levels = stratifiers, 
    labels = strat_plot_names),
    term2 = stringr::str_remove_all(term, var))

adjusted_rates_out %>% 
  write_csv(here("output/tab023_poissonrates_static.csv"))
