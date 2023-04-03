library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))

# time update vaccine data ------------------------------------------------
# main outcome (any long covid code)
time_data_lc_all <- time_update_vaccinedoses(cleaned_data, first_lc)
arrow::write_parquet(time_data_lc_all, 
                     sink = here::here("output/timeupdate_dataset_lc_all.gz.parquet"),
                     compression = "gzip", compression_level = 5)

# Long COVID Dx only
time_data_lc_dx <- time_update_vaccinedoses(cleaned_data, first_lc_dx)
arrow::write_parquet(time_data_lc_dx, 
                     sink = here::here("output/timeupdate_dataset_lc_dx.gz.parquet"),
                     compression = "gzip", compression_level = 5)

time_data_fracture <- time_update_vaccinedoses(cleaned_data, first_fracture_hosp)
arrow::write_parquet(time_data_fracture, 
                     sink = here::here("output/timeupdate_dataset_fracture.gz.parquet"),
                     compression = "gzip", compression_level = 5)


options(width=200) # set output width for capture.output

output_dir <- "output/data_properties"
filenamebase <- "timeupdated_dataset"

dir.create(here(output_dir), showWarnings = FALSE, recursive=TRUE)

## high-level variable overview ----
capture.output(
  skimr::skim_without_charts(time_data_lc_all),
  file = here(output_dir, paste0(filenamebase, "_skim", ".txt")),
  split = FALSE
)

