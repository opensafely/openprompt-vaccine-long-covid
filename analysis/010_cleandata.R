library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/summarise_data.R"))
source(here::here("analysis/functions/import_raw_data.R"))
source(here::here("analysis/functions/clean_raw_data.R"))
source(here::here("analysis/functions/clean_vaccine_data.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

cases_data_path <- here("output/dataset_cases.csv.gz")
controls_data_path <- here("output/dataset_controls.csv.gz")

# import, combine, clean data -------------------------------------------------
imported_data <-
  import_raw_data(cases_path = cases_data_path, controls_path = controls_data_path)

# need these manual overrides with the dummy data
#data_size <- dim(imported_data)[1]
#source("analysis/debugging_codes/make_fake_vaccine_codes.R")
#imported_data$vaccine_dose_1_manufacturer[!is.na(imported_data$vaccine_dose_1_date)] <- test_mrna_code$vaccine_dose_1_manufacturer[!is.na(imported_data$vaccine_dose_1_date)]
#imported_data$vaccine_dose_2_manufacturer[!is.na(imported_data$vaccine_dose_2_date)] <- test_mrna_code$vaccine_dose_2_manufacturer[!is.na(imported_data$vaccine_dose_2_date)]
#imported_data$vaccine_dose_3_manufacturer[!is.na(imported_data$vaccine_dose_3_date)] <- test_mrna_code$vaccine_dose_3_manufacturer[!is.na(imported_data$vaccine_dose_3_date)]


# cleaning the data -------------------------------------------------------
# Step 1 - clean and format the raw data
cleaned_data <- clean_raw_data(imported_data)

# Step 2 - tidy the vaccination data
cleaned_data <- tidy_vaccine_data(cleaned_data)
arrow::write_parquet(cleaned_data, 
                     sink = here::here("output/clean_dataset.gz.parquet"),
                     compression = "gzip", compression_level = 5)


# summarise the raw and the cleaned data  ---------------------------------
summarise_data(data_in = imported_data, filenamebase = "raw_dataset")
summarise_data(data_in = cleaned_data, filenamebase = "clean_dataset")
