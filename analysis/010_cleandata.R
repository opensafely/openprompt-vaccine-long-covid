library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/import_data.R"))
source(here::here("analysis/functions/clean_vaccine_data.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

cases_data_path <- here("output/dataset_cases.csv.gz")
controls_data_path <- here("output/dataset_controls.csv.gz")

# import, combine, clean data -------------------------------------------------
imported_data <-
  import_and_combine(cases_path = cases_data_path, controls_path = controls_data_path)

# need these manual overrides with the dummy data
#data_size <- dim(imported_data)[1]
#source("analysis/debugging_codes/make_fake_vaccine_codes.R")
#imported_data$vaccine_dose_1_manufacturer[!is.na(imported_data$vaccine_dose_1_date)] <- test_mrna_code$vaccine_dose_1_manufacturer[!is.na(imported_data$vaccine_dose_1_date)]
#imported_data$vaccine_dose_2_manufacturer[!is.na(imported_data$vaccine_dose_2_date)] <- test_mrna_code$vaccine_dose_2_manufacturer[!is.na(imported_data$vaccine_dose_2_date)]
#imported_data$vaccine_dose_3_manufacturer[!is.na(imported_data$vaccine_dose_3_date)] <- test_mrna_code$vaccine_dose_3_manufacturer[!is.na(imported_data$vaccine_dose_3_date)]
combined_codelist <- bind_rows(
  readr::read_csv(here("codelists/opensafely-nice-managing-the-long-term-effects-of-covid-19.csv")),
  readr::read_csv(here("codelists/opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv")),
  readr::read_csv(here("codelists/opensafely-referral-and-signposting-for-long-covid.csv"))
)
cleaned_data <- tidy_vaccine_data(imported_data)
arrow::write_parquet(cleaned_data, 
                     sink = here::here("output/clean_dataset.gz.parquet"),
                     compression = "gzip", compression_level = 5)

# skimr to print a summary of the data ------------------------------------
options(width=200) # set output width for capture.output

output_dir <- "output/data_properties"
filenamebase <- "clean_dataset"

dir.create(here(output_dir), showWarnings = FALSE, recursive=TRUE)

## high-level variable overview ----
capture.output(
  skimr::skim_without_charts(cleaned_data),
  file = here(output_dir, paste0(filenamebase, "_skim", ".txt")),
  split = FALSE
)

## tabulated data ----

# delete file if it exists
if(file.exists(here(output_dir, paste0(filenamebase, "_tabulate", ".txt")))){
  file.remove(here(output_dir, paste0(filenamebase, "_tabulate", ".txt")))
}


### categorical and logical ----
sumtabs_cat <-
  cleaned_data %>%
  select(-ends_with("_id")) %>%
  select(where(is.character), where(is.logical), where(is.factor)) %>%
  map(redacted_summary_cat) %>%
  enframe()

capture.output(
  walk2(sumtabs_cat$value, sumtabs_cat$name, print_cat),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=FALSE
)


### numeric ----
sumtabs_num <-
  cleaned_data %>%
  select(-ends_with("_id")) %>%
  select(where(~ {!is.logical(.x) & is.numeric(.x) & !is.Date(.x)})) %>%
  map(redacted_summary_num) %>%
  enframe()

capture.output(
  walk2(sumtabs_num$value, sumtabs_num$name, print_num),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=TRUE
)

### dates ----

sumtabs_date <-
  cleaned_data %>%
  select(-ends_with("_id")) %>%
  select(where(is.Date)) %>%
  map(redacted_summary_date) %>%
  enframe()

capture.output(
  walk2(sumtabs_date$value, sumtabs_date$name, print_num),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=TRUE
)
  