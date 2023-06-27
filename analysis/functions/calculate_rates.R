#' Calculate crude rates per 100,000 p-years by a single stratifier variable
#' @description Calculate the rate of Long COVID by levels of a stratifying variable
#' 95% CI calculated as normal approx. to Poisson distribution 
#' @param data Data to summarise
#' @param grouping_var name of the variable with the stratifying variable
#' @return A dataframe containing summary data with crude rates/100,000 person

calculate_rates <- function(data, grouping_var){
  data %>% 
    filter(!is.na({{grouping_var}})) %>% 
    group_by({{grouping_var}}) %>% 
    summarise(
      n = n(), 
      lc = sum(lc_out),
      lc_dx = sum(lc_dx_only),
      n_rx = sum(lc_dx_flag == "Rx", na.rm = TRUE),
      n_fratures = sum(fracture),
      fup = sum(t),
      total_tests = sum(all_test_positive),
      avg_last_vacc = mean(last_vacc_gap, na.rm = TRUE)
    ) %>% 
    mutate(rate_per1e5 = (lc/fup)*1e5,
           se_rate = (sqrt(lc)/fup)*1e5,
           errorfactor = exp(1.96/sqrt(lc)),
           lci = rate_per1e5/errorfactor,
           uci = rate_per1e5*errorfactor) %>% 
    # calculate rates using only Long Covid diagnoses 
    mutate(rate_per1e5_dx = (lc_dx/fup)*1e5,
           se_rate_dx = (sqrt(lc_dx)/fup)*1e5,
           errorfactor_dx = exp(1.96/sqrt(lc_dx)),
           lci_dx = rate_per1e5_dx/errorfactor_dx,
           uci_dx = rate_per1e5_dx*errorfactor_dx) %>% 
    arrange({{grouping_var}}) %>% 
    mutate(stratifier = stringr::str_c(enexpr(grouping_var)),
           level = as.character({{grouping_var}})) %>% 
    dplyr::select(stratifier, level, everything()) %>% 
    dplyr::select(-{{grouping_var}})
}

#' Apply crude rate calculation to a data set over a common set of stratifying vars
#' @description Apply crude rate calculation to a data set over a common set of stratifying vars
#' 95% CI calculated as normal approx. to Poisson distribution 
#' @param data Data to summarise
#' @return A dataframe containing summary data over all stratifiers with crude rates/100,000 person

apply_rates_over_stratifiers <- function(data){
  bind_rows(
    calculate_rates(data, grouping_var = sex),
    calculate_rates(data, grouping_var = age_cat),
    calculate_rates(data, grouping_var = no_prev_vacc),
    calculate_rates(data, grouping_var = vaccine_schedule_detail),
    calculate_rates(data, grouping_var = practice_nuts),
    calculate_rates(data, grouping_var = imd_q5),
    calculate_rates(data, grouping_var = comorbidities),
    calculate_rates(data, grouping_var = highrisk_shield),
    calculate_rates(data, grouping_var = ethnicity)
  )
}
