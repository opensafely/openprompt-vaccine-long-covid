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
dates <- seq(min(date_data$pt_start_date), max(date_data$pt_end_date), by = "1 month")

# Create a data table with the first day of each month as a column
dt_monthly <- data.table(month_start_date = as.Date(format(dates, "%Y-%m-01")))

# Define a modified version of the count_eligible_patients function that takes only one argument
count_date_events <- function(month, date_var) {
  date_data[get(date_var) >= month & get(date_var) < month %m+% months(1), .N]
}

# Use mapply to count the number of eligible patients in each month for each date variable
count_eligible_patients_var <- function(var) {
  mapply(function(month) {
    sum(date_data$pt_start_date <= month & date_data$pt_end_date >= month & !is.na(date_data[[var]]))
  }, month = dt_monthly$month_start_date)
}

dt_monthly[, n_eligible_pt := count_eligible_patients_var("pt_start_date")]
dt_monthly[, n_first_lc := mapply(count_date_events, month = month_start_date, date_var = "first_lc")]
dt_monthly[, n_first_lc_dx := mapply(count_date_events, month = month_start_date, date_var = "first_lc_dx")]
dt_monthly[, n_hospitalised := mapply(count_date_events, month = month_start_date, date_var = "first_covid_hosp")]
dt_monthly[, n_tested := mapply(count_date_events, month = month_start_date, date_var = "latest_test_before_diagnosis")]
dt_monthly[, n_vacc := mapply(count_date_events, month = month_start_date, date_var = "vaccine_dose_1_date")]

# Calculate monthly incidence and cumulative monthly incidence
cols_to_process <- c("first_lc", "first_lc_dx", "hospitalised", "tested", "vacc")

for (col in cols_to_process) {
  # Calculate monthly incidence
  dt_monthly[, paste0("inc_", col) := ifelse(n_eligible_pt == 0, 0, get(paste0("n_", col)) / n_eligible_pt)]
  
  # Calculate cumulative monthly incidence
  dt_monthly[, paste0("cum_inc_", col) := cumsum(get(paste0("inc_", col)))]
}


readr::write_csv(dt_monthly, here::here("output/data_monthly_dynamics.csv"))


# redact output table for summary -----------------------------------------
redact_threshold <- 7

redacted_monthly <- dt_monthly %>% 
  mutate(
    n_eligible_pt = redact_and_round(n_eligible_pt, redact_threshold),
    n_first_lc = redact_and_round(n_first_lc, redact_threshold),
    n_first_lc_dx = redact_and_round(n_first_lc_dx, redact_threshold),
    n_hospitalised = redact_and_round(n_hospitalised, redact_threshold),
    inc_first_lc = (n_first_lc/n_eligible_pt)*100,
    inc_first_lc_dx = (n_first_lc_dx/n_eligible_pt)*100,
    inc_hospitalised = (n_hospitalised/n_eligible_pt)*100
  ) %>%
  mutate_if(is.numeric, ~as.character(prettyNum(formatC(., digits = 3, format = "f"), 
                                                big.mark = ",", preserve.width = "none",
                                                drop0trailing = TRUE))) %>% 
  dplyr::select(month_start_date, 
                n_eligible_pt,
                n_first_lc, 
                inc_first_lc, 
                n_first_lc_dx, 
                inc_first_lc_dx,
                n_hospitalised,
                inc_hospitalised
  )

## output nice .csv table
redacted_monthly %>% 
  write_csv(here("output/tables/supptab01_monthly_dynamics.csv"))
redacted_monthly