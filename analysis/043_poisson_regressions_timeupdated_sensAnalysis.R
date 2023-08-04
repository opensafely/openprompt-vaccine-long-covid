library(MASS)
library(broom)
library(lubridate)
library(here)
library(tidyverse)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/glm_regressions.R"))

fs::dir_create(here::here("output/regression_outputs/"))

# add simple poisson regression results -----------------------------------
# run the standard 
run_the_regressions_collapsed <- function(data){
  map(stratifiers_t, ~glm_regressions(cohort_data = data, .x, collapse_variant = TRUE))
}


stratifiers_t <-
  c(
    "vaccines",
    "t_vacc_mrna"
  )

strat_plot_names_t <-
  c(
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


# import data -------------------------------------------------------------
## import clean_data to get the region data
clean_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet")) %>% 
  dplyr::select(patient_id, region = practice_nuts, comorbid = comorbidities, imd = imd_q5)

# ALL long covid as an outcome --------------------------------------------
## 1 - import All Long COVID codes as outcome - merge on region
time_data_lc_all_sensAnalysis_12wks <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all_sensAnalysis_12wks.gz.parquet")) %>%
  dplyr::select(all_of(vars_for_regressions)) %>% 
  left_join(clean_data, by = "patient_id")

## run the regressions
adjusted_rates_t_lc_all_12wks <- run_the_regressions_collapsed(data = time_data_lc_all_sensAnalysis_12wks)
## got to add on a new column to denote which sensAnalysis is being regressed
adjusted_rates_t_lc_all_12wks <- lapply(adjusted_rates_t_lc_all_12wks, function(x){bind_cols(x, sa = "12 weeks")})

## remove the big time-data frame to save memory
time_data_lc_all_sensAnalysis_12wks <- NULL

## repeat for 4 weeks
time_data_lc_all_sensAnalysis_4wks <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all_sensAnalysis_4wks.gz.parquet")) %>%
  dplyr::select(all_of(vars_for_regressions)) %>% 
  left_join(clean_data, by = "patient_id")
adjusted_rates_t_lc_all_4wks <- run_the_regressions_collapsed(data = time_data_lc_all_sensAnalysis_4wks)
adjusted_rates_t_lc_all_4wks <- lapply(adjusted_rates_t_lc_all_4wks, function(x){bind_cols(x, sa = "4 weeks")})
time_data_lc_all_sensAnalysis_4wks <- NULL


# Long COVID Diagnoses only -----------------------------------------------
## 12 week exclusion window
time_data_lc_dx_sensAnalysis_12wks <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx_sensAnalysis_12wks.gz.parquet")) %>%
  dplyr::select(all_of(vars_for_regressions)) %>% 
  left_join(clean_data, by = "patient_id")
adjusted_rates_t_lc_dx_12wks <- run_the_regressions_collapsed(data = time_data_lc_dx_sensAnalysis_12wks)
adjusted_rates_t_lc_dx_12wks <- lapply(adjusted_rates_t_lc_dx_12wks, function(x){bind_cols(x, sa = "12 weeks")})
time_data_lc_dx_sensAnalysis_12wks <- NULL

## 4 week exclusion window
time_data_lc_dx_sensAnalysis_4wks <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx_sensAnalysis_4wks.gz.parquet")) %>%
  dplyr::select(all_of(vars_for_regressions)) %>% 
  left_join(clean_data, by = "patient_id")
adjusted_rates_t_lc_dx_4wks <- run_the_regressions_collapsed(data = time_data_lc_dx_sensAnalysis_4wks)
adjusted_rates_t_lc_dx_4wks <- lapply(adjusted_rates_t_lc_dx_4wks, function(x){bind_cols(x, sa = "4 weeks")})
time_data_lc_dx_sensAnalysis_4wks <- NULL

# group the outcomes ------------------------------------------------------
adjusted_rates_out <- NULL
adjusted_rates_list <- c("adjusted_rates_t_lc_all_12wks",
                         "adjusted_rates_t_lc_all_4wks",
                         "adjusted_rates_t_lc_dx_12wks",
                         "adjusted_rates_t_lc_dx_4wks")
outcome_list <- c("All Long COVID",
                  "All Long COVID",
                  "Long COVID diagnoses", 
                  "Long COVID diagnoses")
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
  write_csv(here("output/tab023_poissonrates_timeupdated_sensAnalysis.csv"))
