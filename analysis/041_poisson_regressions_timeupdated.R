library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/poisson_regressions.R"))


# add simple poisson regression results -----------------------------------
run_the_regressions <- function(data){
  map(stratifiers_t, ~poisson_regressions(cohort_data = data, .x))
}

stratifiers_t <-
  c(
    "sex",
    "age_cat",
    "practice_nuts",
    "ethnicity",
    "imd_q5",
    "comorbidities",
    "highrisk_shield",
    "vaccines",
    "t_vacc_primary",
    "t_vacc_mrna"
  )

strat_plot_names_t <-
  c(
    "Sex",
    "Age category",
    "Region",
    "Ethnicity",
    "IMD (quintile)",
    "Comorbidities",
    "Shielding (high risk group)",
    "No. vaccine doses",
    "First vaccine received",
    "mRNA vaccine received"
  )

# import data ------------------------------------------------------------
## All Long COVID codes as outcome
time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet"))
adjusted_rates_t_lc_all <- run_the_regressions(data = time_data_lc_all)
time_data_lc_all <- NULL

## Long COVID Dx codes only as outcome
time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet"))
adjusted_rates_t_lc_dx <- run_the_regressions(data = time_data_lc_dx)
time_data_lc_dx <- NULL

## Hospitalised with fracture codes as outcome
time_data_frac <- arrow::read_parquet(here::here("output/timeupdate_dataset_fracture.gz.parquet"))
adjusted_rates_t_frac <- run_the_regressions(data = time_data_frac)
time_data_frac <- NULL

## Hospitalised with COVID-19 as outcome
time_data_covidhosp <- arrow::read_parquet(here::here("output/timeupdate_dataset_covidhosp.gz.parquet"))
adjusted_rates_t_covidhosp <- run_the_regressions(data = time_data_covidhosp)
time_data_covidhosp <- NULL

# group the outcomes ------------------------------------------------------
adjusted_rates_out <- NULL
adjusted_rates_list <- c("adjusted_rates_t_lc_all",
                         "adjusted_rates_t_lc_dx",
                         "adjusted_rates_t_frac",
                         "adjusted_rates_t_covidhosp")
outcome_list <- c("All Long COVID", 
                  "Long COVID diagnoses", 
                  "Fractures", 
                  "COVID-19 hospitalisation")
for(j in 1:4){
  adjusted_rates_temp <- bind_rows(get(adjusted_rates_list[j]))
  outcome_name <- outcome_list[j]
  adjusted_rates_out <- bind_rows(
    adjusted_rates_out, 
    bind_cols(adjusted_rates_temp, outcome = outcome_name)
  ) 
}

adjusted_rates_out <- adjusted_rates_out %>% 
  mutate(strat_var = factor(
    var, 
    levels = stratifiers_t,
    labels = strat_plot_names_t),
    term2 = stringr::str_remove_all(term, var))

adjusted_rates_out %>%
  write_csv(here("output/tab023_poissonrates_timeupdated.csv"))
