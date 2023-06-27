library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(here)
library(data.table)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here("output/tables"), showWarnings = FALSE, recursive=TRUE)
dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

# import data ------------------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))

# convert to time series --------------------------------------------------
datevars <- select_if(cleaned_data, is.Date) %>% 
  names()
date_data <- cleaned_data %>% 
  dplyr::select(patient_id, lc_dx_flag, all_of(datevars)) %>% 
  mutate(first_lc_dx = as.Date(ifelse(lc_dx_flag == "Dx", first_lc, NA), origin = "1970-01-01"))

# Convert the data frame to a data.table
setDT(date_data)

# Create a sequence of dates from the minimum eligible start date to the maximum eligible end date
dates <- seq(min(date_data$pt_start_date), max(date_data$pt_end_date), by = "1 day")

# Create a data table with the first day of each day as a column
dt_daily <- data.table(date = as.Date(format(dates, "%Y-%m-%d")))

# Define a modified version of the count_eligible_patients function that takes only one argument
count_date_events <- function(day, date_var) {
  date_data[get(date_var) == day, .N]
}

# Use mapply to count the number of eligible patients in each day for each date variable
count_eligible_patients_var <- function(var) {
  mapply(function(day) {
    sum(date_data$pt_start_date <= day & date_data$pt_end_date >= day & !is.na(date_data[[var]]))
  }, day = dt_daily$date)
}

dt_daily[, n_eligible_pt := count_eligible_patients_var("pt_start_date")]
dt_daily[, n_first_lc := mapply(count_date_events, day = date, date_var = "first_lc")]
dt_daily[, n_first_lc_dx := mapply(count_date_events, day = date, date_var = "first_lc_dx")]
dt_daily[, n_hospitalised := mapply(count_date_events, day = date, date_var = "first_covid_hosp")]
dt_daily[, n_tested := mapply(count_date_events, day = date, date_var = "latest_test_before_diagnosis")]
dt_daily[, n_vacc := mapply(count_date_events, day = date, date_var = "vaccine_dose_1_date")]

# Calculate monthly incidence and cumulative monthly incidence
cols_to_process <- c("first_lc", "first_lc_dx", "hospitalised", "tested", "vacc")

for (col in cols_to_process) {
  # Calculate monthly incidence
  dt_daily[, paste0("inc_", col) := ifelse(n_eligible_pt == 0, 0, get(paste0("n_", col)) / n_eligible_pt)]
  
  # Calculate cumulative monthly incidence
  dt_daily[, paste0("cum_inc_", col) := cumsum(get(paste0("inc_", col)))]
}


readr::write_csv(dt_daily, here::here("output/data_daily_dynamics.csv"))
