library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/summarise_data.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet")) %>% 
  rename(lc_out = out)
print(table(time_data_lc_all$vaccines, time_data_lc_all$lc_out))

time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet")) %>% 
  rename(lc_dx = out)
print(table(time_data_lc_dx$vaccines, time_data_lc_dx$lc_dx))

# summarise data ----------------------------------------------------------
summarise_data(data_in = time_data_lc_all, filenamebase = "timeupdated_lc_all")
