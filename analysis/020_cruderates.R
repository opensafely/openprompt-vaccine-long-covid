#library(targets)
library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/import_data.R"))
source(here::here("analysis/functions/clean_vaccine_data.R"))
source(here::here("analysis/functions/calculate_rates.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

cases_data_path <- here("output/dataset_cases.csv.gz")
controls_data_path <- here("output/dataset_controls.csv.gz")

# import, combine, clean data -------------------------------------------------
imported_data <-
  import_and_combine(cases_path = cases_data_path, controls_path = controls_data_path)
imported_data %>% select(contains("age"))

cleaned_data <- tidy_vaccine_data(imported_data)

# calculate crude rates ---------------------------------------------------
crude_rates <- apply_rates_over_stratifiers(cleaned_data)
crude_rates %>% 
  write_csv(here("output/tab021_crude_lc_rates.csv"))

# time update on vaccine date ---------------------------------------------
time_data <- time_update_vaccinedoses(cleaned_data)
time_updated_rates <- bind_rows(
  calculate_rates(time_data, vaccines),
  calculate_rates(time_data, t_mixmatch)
)

time_updated_rates %>% 
  write_csv(here("output/tab022_tuv_lc_rates.csv"))