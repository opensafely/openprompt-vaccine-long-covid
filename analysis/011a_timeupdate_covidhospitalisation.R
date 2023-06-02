library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))

# time update vaccine data ------------------------------------------------ 
# hospitalised with COVID-19
time_data_covidhosp <- time_update_vaccinedoses(cleaned_data, first_covid_hosp)
arrow::write_parquet(time_data_covidhosp, 
                     sink = here::here("output/timeupdate_dataset_covidhosp.gz.parquet"),
                     compression = "gzip", compression_level = 5)
# delete the big dataset to save memory 
time_data_covidhosp <- NULL


