#library(targets)
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

cleaned_data <- tidy_vaccine_data(imported_data)

arrow::write_parquet(cleaned_data, 
                     sink = here::here("output/clean_dataset.gz.parquet"),
                     compression = "gzip", compression_level = 5)


# time update vaccine data ------------------------------------------------
time_data <- time_update_vaccinedoses(cleaned_data)

arrow::write_parquet(time_data, 
                     sink = here::here("output/timeupdate_dataset.gz.parquet"),
                     compression = "gzip", compression_level = 5)

# skimr to print a summary of the data ------------------------------------
options(width=200) # set output width for capture.output

output_dir <- "output/data_properties"
filenamebase <- "clean_dataset"

dir.create(here(output_dir), showWarnings = FALSE, recursive=TRUE)

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
  