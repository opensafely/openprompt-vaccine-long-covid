library(tidyverse)
library(broom)
library(lubridate)
library(arrow)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))
source(here::here("analysis/functions/calculate_rates.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

# create folder to put plot in
dir.create(here("output/supplementary/"), showWarnings = FALSE, recursive=TRUE)

# import, combine, clean data -------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))

# calculate crude rates ---------------------------------------------------
crude_rates <- apply_rates_over_stratifiers(cleaned_data)
crude_rates %>% 
  write_csv(here("output/tab021_crude_lc_rates.csv"))