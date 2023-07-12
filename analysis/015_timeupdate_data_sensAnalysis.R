library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))


# sensitivity analysis - exclude recent vaccinations -------------------------


# long covid - ALL --------------------------------------------------------
## exclude vaccine records that are within12 weeks of the end of f-up
time_data_lc_all_sensAnalysis <- time_update_vaccinedoses(cleaned_data, first_lc, exclude_recent_vacc = TRUE, exclusion_window = 12*7)

## save the file
arrow::write_parquet(time_data_lc_all_sensAnalysis, 
                     sink = here::here("output/timeupdate_dataset_lc_all_sensAnalysis_12wks.gz.parquet"),
                     compression = "gzip", compression_level = 5)

# delete the big dataset to save memory 
time_data_lc_all_sensAnalysis <- NULL


## repeat for 4 weeks
time_data_lc_all_sensAnalysis <- time_update_vaccinedoses(cleaned_data, first_lc, exclude_recent_vacc = TRUE, exclusion_window = 4*7)
arrow::write_parquet(time_data_lc_all_sensAnalysis, 
                     sink = here::here("output/timeupdate_dataset_lc_all_sensAnalysis_4wks.gz.parquet"),
                     compression = "gzip", compression_level = 5)
time_data_lc_all_sensAnalysis <- NULL

# Long COVID Dx only --------------------------------------------------------

## 12 weeks
time_data_lc_dx_sensAnalysis <- time_update_vaccinedoses(cleaned_data, first_lc_dx, exclude_recent_vacc = TRUE, exclusion_window = 12*7)

## save file 
arrow::write_parquet(time_data_lc_dx_sensAnalysis, 
                     sink = here::here("output/timeupdate_dataset_lc_dx_sensAnalysis_12wks.gz.parquet"),
                     compression = "gzip", compression_level = 5)

# delete the big dataset to save memory 
time_data_lc_dx_sensAnalysis <- NULL

## repeat for 4 weeks
time_data_lc_dx_sensAnalysis <- time_update_vaccinedoses(cleaned_data, first_lc_dx, exclude_recent_vacc = TRUE, exclusion_window = 4*7)
arrow::write_parquet(time_data_lc_dx_sensAnalysis, 
                     sink = here::here("output/timeupdate_dataset_lc_dx_sensAnalysis_4wks.gz.parquet"),
                     compression = "gzip", compression_level = 5)

time_data_lc_dx_sensAnalysis <- NULL

# COVID hospitalisations --------------------------------------------------
# hospitalised with COVID-19 - exclude <= 2 weeks before event ONLY
time_data_covidhosp_sensAnalysis <- time_update_vaccinedoses(cleaned_data, first_covid_hosp, exclude_recent_vacc = TRUE, exclusion_window = 2*7)
arrow::write_parquet(time_data_covidhosp_sensAnalysis, 
                     sink = here::here("output/timeupdate_dataset_covidhosp_sensAnalysis_2wks.gz.parquet"),
                     compression = "gzip", compression_level = 5)
# delete the big dataset to save memory 
time_data_covidhosp_sensAnalysis <- NULL
