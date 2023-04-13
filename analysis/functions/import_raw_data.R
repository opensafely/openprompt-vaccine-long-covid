#' Import cases and controls for primary cohort construction
#' @description Imports the cases and controls data from Data builder, combines and tidies a couple of variables
#' @param cases_path File path for the file containing case data
#' @param controls_path File path for the file containing controls data
#' @return A dataframe containing data on both cases and controls

import_raw_data <- function(cases_path = "str", controls_path = "str"){
  cases <- readr::read_csv(cases_path) %>% 
    janitor::clean_names()
  spec(cases) %>% print()
#browser()
#cases %>% select(patient_id, first_lc, lc_dx_flag, first_lc_dx, longcovid_categorical) %>% View()
#cases %>% select(patient_id, pt_start_date, pt_end_date,first_lc_dx, first_covid_critical, first_covid_hosp, latest_primarycare_covid, latest_test_before_diagnosis, longcovid_categorical) %>% filter(!is.na(longcovid_categorical)) %>% View()
  controls <- readr::read_csv(controls_path) %>% 
    janitor::clean_names()
  spec(controls) %>% print()
  
  bind_rows(cases, controls)
}
