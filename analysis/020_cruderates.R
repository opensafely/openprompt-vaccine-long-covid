library(tidyverse)
library(broom)
library(lubridate)
library(arrow)
library(here)

source(here::here("analysis/functions/calculate_rates.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

# import, combine, clean data -------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))
time_data <- arrow::read_parquet(here::here("output/timeupdate_dataset.gz.parquet"))

# calculate crude rates ---------------------------------------------------
crude_rates <- apply_rates_over_stratifiers(cleaned_data)
crude_rates %>% 
  write_csv(here("output/tab021_crude_lc_rates.csv"))

# time update on vaccine date ---------------------------------------------
time_updated_rates <- bind_rows(
  calculate_rates(time_data, vaccines),
  calculate_rates(time_data, t_mixmatch)
)

time_updated_rates %>% 
  write_csv(here("output/tab022_tuv_lc_rates.csv"))
