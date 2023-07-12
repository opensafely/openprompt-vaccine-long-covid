#' Time update on date of vaccine dose
#' @description Takes a flat 1-row-per-patient file and time updates on the date of vaccine dosage 
#' @param data data to be time updated
#' @param outcome_var A date variable with the date of the Outcome event
#' @return A dataframe containing time updated data 

time_update_vaccinedoses <- function(data, outcome_var, exclude_recent_vacc = FALSE, exclusion_window = 0){
  # remove anyone who starts and ends on the same date 
  # need to redefine the participant end date because the outcome date may be different to Any Long COVID date (used in data builder) 
  #e.g., may have a fracture before/after Long COVID so need to update end date
  
  # Ran into strange error with dates when doing all-in-one (end dates in the year 4,562,122 etc.) 
  # so split it out into cases and non-cases
  small_base <- data %>% 
    dplyr::select(patient_id, 
                  starts_with("pt_"), 
                  contains("age"), 
                  sex,
                  age_cat,
                  no_prev_vacc,
                  vaccine_schedule_detail,
                  practice_nuts,
                  imd_q5,
                  comorbidities,
                  highrisk_shield,
                  ethnicity,
                  {{outcome_var}}, 
                  contains("vaccine_dose"),
                  contains("vaccine_schedule"),
                  first_mrna_vaccine_date,
                  first_non_mrna_vaccine_date) %>% 
    rename(outcome = {{outcome_var}}) %>% 
    mutate(outcome_binary = as.numeric(!is.na(outcome)))
  
  # save some memory space
  data <- NULL 
  
  small_base_cases <- small_base %>% 
    filter(outcome_binary == 1) %>%
    # create new end date = outcome date (this may be the same as the current end date but it may be earlier or later depending on the outcome, so need to override)
    mutate(new_end_date = outcome,
           t = pt_start_date %--% new_end_date / dyears(1))  %>% 
    # again, need to filter out anyone who has the event before/on the same day as study entry
    filter(t>0)
  
  # get the controls 
  small_base_controls <- small_base %>% 
    filter(outcome_binary == 0) %>% 
    mutate(new_end_date = pt_end_date,
           t = pt_start_date %--% new_end_date / dyears(1)) %>% 
    # again, need to filter out anyone who has the event before/on the same day as study entry
    filter(t>0)
  
  small_base <- bind_rows(small_base_cases, small_base_controls)
  
  # save memory
  small_base_cases <- NULL
  small_base_controls <- NULL
  
  # amend the vaccine data if it's < 12 weeks before the end of the study
  if(exclude_recent_vacc) {
    small_base$vaccine_dose_1_date[small_base$vaccine_dose_1_date > (small_base$new_end_date - exclusion_window)] <- NA
    small_base$vaccine_dose_2_date[small_base$vaccine_dose_2_date > (small_base$new_end_date - exclusion_window)] <- NA
    small_base$vaccine_dose_3_date[small_base$vaccine_dose_3_date > (small_base$new_end_date - exclusion_window)] <- NA
    ## repeat for first mrna and first non-mrna vaccine dates
    small_base$first_non_mrna_vaccine_date[small_base$first_non_mrna_vaccine_date > (small_base$new_end_date - exclusion_window)] <- NA
    small_base$first_mrna_vaccine_date[small_base$first_mrna_vaccine_date > (small_base$new_end_date - exclusion_window)] <- NA
  }
  
  # calculate time difference for each vaccine dose
  df_vacc_base_t <- small_base %>%
    # again, need to filter out anyone who has the event before/on the same day as study entry (should be redundant but just in case)
    filter(t>0) %>% 
    dplyr::select(patient_id, pt_start_date, contains("vaccine_dose_")) %>%
    mutate(vacc_time_1 = pt_start_date %--% vaccine_dose_1_date / dyears(1),
           vacc_time_2 = pt_start_date %--% vaccine_dose_2_date / dyears(1),
           vacc_time_3 = pt_start_date %--% vaccine_dose_3_date / dyears(1)
           ) %>% 
    dplyr::select(-contains("vaccine_dose")) %>% 
    pivot_longer(cols = starts_with("vacc_time_"), names_to = "vacc_no", names_pattern = "vacc_time_(.*)", values_to = "years")
  
  ## repeat that process for mrna 
  df_vacc_base_mrna <- small_base %>%
    filter(t>0) %>% 
    dplyr::select(patient_id, pt_start_date, first_non_mrna_vaccine_date, first_mrna_vaccine_date) %>% 
    mutate(
      vacc_time_non_mRNA = pt_start_date %--% first_non_mrna_vaccine_date / dyears(1),
      vacc_time_mRNA = pt_start_date %--% first_mrna_vaccine_date / dyears(1)
      ) %>% 
    dplyr::select(-contains("vaccine_dose")) %>% 
    pivot_longer(cols = starts_with("vacc_time_"), names_to = "mrna", names_pattern = "vacc_time_(.*)", values_to = "years")
  
  # time update on dominant variant 
  # from UK covid dashboard https://coronavirus.data.gov.uk/details/cases?areaType=nation%26areaName=England#card-variant_percentage_of_available_sequenced_episodes_by_week
  date_delta <- as.Date("2021-05-16") 
  date_omicron <- as.Date("2021-12-01")
  
  variant_timeupdate <- small_base %>% 
    dplyr::select(patient_id, starts_with("pt_"), t) %>%
    mutate(time_delta = as.numeric((date_delta - pt_start_date) / dyears(1)),
           time_omicron  = as.numeric((date_omicron - pt_start_date) / dyears(1))) %>% 
    pivot_longer(cols = starts_with("time_"), names_to = "variant", names_pattern = "time_(.*)", values_to = "years") %>% 
    filter(years <= t)
  
  # create the survival dataset 
  survivaldata <- survival::tmerge(small_base, small_base, id = patient_id, out = event(t, outcome_binary))
  
  newsurvival <- survival::tmerge(survivaldata, variant_timeupdate, id = patient_id, variant = tdc(years, variant, "wild/alpha")) 
  
  newsurvival <- survival::tmerge(newsurvival, df_vacc_base_t, id = patient_id, vaccines = tdc(years, vacc_no, 0)) 
  
  newsurvival <- survival::tmerge(newsurvival, df_vacc_base_mrna, id = patient_id, t_vacc_mrna = tdc(years, mrna, "No vaccine")) 
  
  newsurvival %>%
    dplyr::select(-outcome_binary,-pt_end_date) %>%
    rename(pt_end_date = new_end_date) %>%
    mutate(vaccines = factor(
      vaccines,
      levels = 0:3,
      labels = c("0", "1", "2", "3+")
    )) %>%
    mutate(t_vacc_mrna = factor(t_vacc_mrna,
                                  levels = c("No vaccine", "non_mRNA", "mRNA"))) %>%
    mutate(variant = factor(variant, 
                            levels = c("wild/alpha", "delta", "omicron"),
                            labels = c("0: wild/alpha", "1: delta", "2: omicron"))) %>% 
    mutate(t = tstop - tstart)
}
