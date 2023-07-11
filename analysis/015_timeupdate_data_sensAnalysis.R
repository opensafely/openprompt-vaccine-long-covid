library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))


# sensitivity analysis - exclude recent vaccinations -------------------------
## exclude vaccine records that are within12 weeks of the end of f-up
time_data_lc_all_sensAnalysis <- time_update_vaccinedoses(cleaned_data, first_lc, exclude_recent_vacc = TRUE, exclusion_window = 12*7)
arrow::write_parquet(time_data_lc_all_sensAnalysis, 
                     sink = here::here("output/timeupdate_dataset_lc_all_sensAnalysis.gz.parquet"),
                     compression = "gzip", compression_level = 5)
# delete the big dataset to save memory 
time_data_lc_all_sensAnalysis <- NULL

# Long COVID Dx only
time_data_lc_dx_sensAnalysis <- time_update_vaccinedoses(cleaned_data, first_lc_dx, exclude_recent_vacc = TRUE, exclusion_window = 12*7)
arrow::write_parquet(time_data_lc_dx_sensAnalysis, 
                     sink = here::here("output/timeupdate_dataset_lc_dx_sensAnalysis.gz.parquet"),
                     compression = "gzip", compression_level = 5)
# delete the big dataset to save memory 
time_data_lc_dx_sensAnalysis <- NULL

# hospitalised with COVID-19 - exclude <= 2 weeks before event
time_data_covidhosp_sensAnalysis <- time_update_vaccinedoses(cleaned_data, first_covid_hosp, exclude_recent_vacc = TRUE, exclusion_window = 2*7)
arrow::write_parquet(time_data_covidhosp_sensAnalysis, 
                     sink = here::here("output/timeupdate_dataset_covidhosp_sensAnalysis.gz.parquet"),
                     compression = "gzip", compression_level = 5)
# delete the big dataset to save memory 
time_data_covidhosp_sensAnalysis <- NULL