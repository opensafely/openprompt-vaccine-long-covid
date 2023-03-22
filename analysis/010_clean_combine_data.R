#library(targets)
library(tidyverse)
library(broom)
library(lubridate)
library(survival)
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

cleaned_data <- tidy_vaccine_data(imported_data)

