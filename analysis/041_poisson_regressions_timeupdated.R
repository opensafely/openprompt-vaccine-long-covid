library(MASS)
library(broom)
library(lubridate)
library(here)
library(tidyverse)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/glm_regressions.R"))

fs::dir_create(here::here("output/regression_outputs/"))

# add simple poisson regression results -----------------------------------
run_the_regressions <- function(data){
  map(stratifiers_t, ~glm_regressions(cohort_data = data, .x))
}

# run the regressions collapsed over variant
run_the_regressions_collapsed <- function(data){
  map(stratifiers_t, ~glm_regressions(cohort_data = data, .x, collapse_variant = TRUE))
}

stratifiers_t <-
  c(
    "sex",
    "age_cat",
    "vaccines",
    "t_vacc_mrna"
  )

strat_plot_names_t <-
  c(
    "Sex",
    "Age category",
    "No. vaccine doses",
    "mRNA vaccine received"
  )

vars_for_regressions <- c("patient_id", 
                          "t", 
                          "sex", 
                          "age_cat", 
                          "age_centred", 
                          "vaccines", 
                          "t_vacc_mrna", 
                          "variant",
                          "out")
# import data ------------------------------------------------------------

## import clean_Data to get the region data
clean_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet")) %>% 
  dplyr::select(patient_id, region = practice_nuts)

## 1 - import All Long COVID codes as outcome - merge on region
time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet")) %>%
  dplyr::select(all_of(vars_for_regressions)) %>% 
  left_join(clean_data, by = "patient_id")



## run the regressions
adjusted_rates_t_lc_all <- run_the_regressions(data = time_data_lc_all)
# and collapsed over variant period
adjusted_rates_t_lc_all_collapsed <- run_the_regressions_collapsed(data = time_data_lc_all)

## remove the big time-data frame to save memory
time_data_lc_all <- NULL

## 2 - import Long COVID Dx codes only as outcome - merge on region
time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet")) %>%
  dplyr::select(all_of(vars_for_regressions)) %>% 
  left_join(clean_data, by = "patient_id")

## run the regressions
adjusted_rates_t_lc_dx <- run_the_regressions(data = time_data_lc_dx)
# and collapsed over variant period
adjusted_rates_t_lc_dx_collapsed <- run_the_regressions_collapsed(data = time_data_lc_dx)

## remove the big time-data frame to save memory
time_data_lc_dx <- NULL

## 3 - Hospitalised with COVID-19 as outcome - merge on region
time_data_covidhosp <- arrow::read_parquet(here::here("output/timeupdate_dataset_covidhosp.gz.parquet")) %>%
  dplyr::select(all_of(vars_for_regressions)) %>% 
  left_join(clean_data, by = "patient_id")

## run the regressions
adjusted_rates_t_covidhosp <- run_the_regressions(data = time_data_covidhosp)
# and collapsed over variant period
adjusted_rates_t_covidhosp_collapsed <- run_the_regressions_collapsed(data = time_data_covidhosp)

## remove the big time-data frame to save memory
time_data_covidhosp <- NULL

# group the outcomes ------------------------------------------------------
adjusted_rates_out <- NULL
adjusted_rates_list <- c("adjusted_rates_t_lc_all",
                         "adjusted_rates_t_lc_dx",
                         "adjusted_rates_t_covidhosp",
                         "adjusted_rates_t_lc_all_collapsed",
                         "adjusted_rates_t_lc_dx_collapsed",
                         "adjusted_rates_t_covidhosp_collapsed"
                         )
outcome_list <- c("All Long COVID", 
                  "Long COVID diagnoses", 
                  "COVID-19 hospitalisation",
                  "All Long COVID",
                  "Long COVID diagnoses",
                  "COVID-19 hospitalisation"
                  )
for(j in 1:length(adjusted_rates_list)){
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


adjusted_rates_out %>% 
  filter(str_detect(term, "vaccine"), model == "adjusted", !str_detect(term, "baseline"), outcome == "Long COVID diagnoses")
