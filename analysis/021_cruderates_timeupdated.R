library(tidyverse)
library(broom)
library(lubridate)
library(arrow)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/calculate_rates.R"))

# import data ------------------------------------------------------------
time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet"))
time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet"))

# rewrite function --------------------------------------------------------
calculate_timeupdated_rates <- function(data, grouping_var){
  data %>% 
    group_by({{grouping_var}}) %>% 
    summarise(
      n = n(), 
      lc = sum(out),
      fup = sum(t)
    ) %>% 
    mutate(rate_per1e5 = (lc/fup)*1e5,
           se_rate = (sqrt(lc)/fup)*1e5,
           errorfactor = exp(1.96/sqrt(lc)),
           lci = rate_per1e5/errorfactor,
           uci = rate_per1e5*errorfactor) %>% 
    arrange({{grouping_var}}) %>% 
    mutate(stratifier = stringr::str_c(enexpr(grouping_var)),
           level = as.character({{grouping_var}})) %>% 
    dplyr::select(stratifier, level, everything()) %>% 
    dplyr::select(-{{grouping_var}})
}

# calculate crude rates ---------------------------------------------------
time_updated_rates_all <- bind_rows(
  calculate_timeupdated_rates(time_data_lc_all, vaccines),
  calculate_timeupdated_rates(time_data_lc_all, t_vacc_detail),
  calculate_timeupdated_rates(time_data_lc_all, t_vacc_twodose_grouped)
)
time_updated_rates_all %>% 
  write_csv(here("output/tab022_tuv_rates_lc_all.csv"))

time_updated_rates_dx <- bind_rows(
  calculate_timeupdated_rates(time_data_lc_dx, vaccines),
  calculate_timeupdated_rates(time_data_lc_dx, t_vacc_detail),
  calculate_timeupdated_rates(time_data_lc_dx, t_vacc_twodose_grouped)
)
time_updated_rates_dx %>% 
  write_csv(here("output/tab023_tuv_rates_lc_dx.csv"))
