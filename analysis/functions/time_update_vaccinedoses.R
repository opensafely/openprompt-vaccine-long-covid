#' Time update on date of vaccine dose
#' @description Takes a flat 1-row-per-patient file and time updates on the date of vaccine dosage 
#' @param data data to be time updated
#' @param outcome_var A date variable with the date of the Outcome event
#' @return A dataframe containing time updated data 

time_update_vaccinedoses <- function(data, outcome_var){
  # remove anyone who starts and ends on the same date 
  small_base <- data %>% 
    dplyr::select(patient_id, starts_with("pt_"), contains("age"), sex, {{outcome_var}}) %>% 
    filter(pt_start_date >= as.Date("2020-11-01")) %>% 
    rename(outcome = {{outcome_var}}) %>%
    mutate(
      new_end_date =  lubridate::as_date(
        ifelse(!is.na(outcome) & outcome < pt_end_date,
               outcome, pt_end_date),
        origin = lubridate::origin
      ),
      t = pt_start_date %--% new_end_date / dyears(1),
      outcome_binary = as.numeric(!is.na(outcome))
    ) %>% 
    filter(t>0)
  
  # calculate time difference for each vaccine dose
  df_vacc_base_t <- data %>% 
    filter(t>0) %>% 
    select(patient_id, pt_start_date, contains("vaccine_dose_"), contains("vaccine_schedule_")) %>% 
    mutate(vacc_time_1 = pt_start_date %--% vaccine_dose_1_date / dyears(1),
           vacc_time_2 = pt_start_date %--% vaccine_dose_2_date / dyears(1),
           vacc_time_3 = pt_start_date %--% vaccine_dose_3_date / dyears(1)
           ) %>% 
    dplyr::select(-contains("vaccine_dose")) %>% 
    pivot_longer(cols = starts_with("vacc_time_"), names_to = "vacc_no", names_pattern = "vacc_time_(.)", values_to = "years")
  data <- NULL
  
  # create new time-updated version of homologous/heterologous vaccine dose - need two versions of a time-updated variable (one with detail, one grouped by homo/hetero)
  vaccines_schedule_timeupdate <- df_vacc_base_t %>% 
    filter(!is.na(years)) 
  vaccines_schedule_timeupdate$vaccine_schedule_detail[vaccines_schedule_timeupdate$vacc_no == "1"] <- "1 dose"
  vaccines_schedule_timeupdate$vaccine_schedule_grouped[vaccines_schedule_timeupdate$vacc_no == "1"] <- "1 dose"
  vaccines_schedule_timeupdate$vaccine_schedule_twodose_detail[vaccines_schedule_timeupdate$vacc_no == "1"] <- "1 dose"
  vaccines_schedule_timeupdate$vaccine_schedule_twodose_grouped[vaccines_schedule_timeupdate$vacc_no == "1"] <- "1 dose"
  
  vaccines_schedule_timeupdate$vaccine_schedule_detail[vaccines_schedule_timeupdate$vacc_no == "2"] <- as.character(vaccines_schedule_timeupdate$vaccine_schedule_twodose_detail[vaccines_schedule_timeupdate$vacc_no == "2"])
  vaccines_schedule_timeupdate$vaccine_schedule_grouped[vaccines_schedule_timeupdate$vacc_no == "2"] <- as.character(vaccines_schedule_timeupdate$vaccine_schedule_twodose_grouped[vaccines_schedule_timeupdate$vacc_no == "2"])
  
  # create the survival dataset 
  survivaldata <- survival::tmerge(small_base, small_base, id = patient_id, out = event(t, outcome_binary)) 
  newsurvival <- survival::tmerge(survivaldata, df_vacc_base_t, id = patient_id, vaccines = tdc(years, vacc_no, 0)) 
  
  newsurvival <- survival::tmerge(newsurvival, vaccines_schedule_timeupdate, id = patient_id, t_vacc_detail = tdc(years, vaccine_schedule_detail, "No vaccine")) 
  newsurvival <- survival::tmerge(newsurvival, vaccines_schedule_timeupdate, id = patient_id, t_vacc_grouped = tdc(years, vaccine_schedule_grouped, "No vaccine")) 
  newsurvival <- survival::tmerge(newsurvival, vaccines_schedule_timeupdate, id = patient_id, t_vacc_twodose_detail = tdc(years, vaccine_schedule_twodose_detail, "No vaccine")) 
  newsurvival <- survival::tmerge(newsurvival, vaccines_schedule_timeupdate, id = patient_id, t_vacc_twodose_grouped = tdc(years, vaccine_schedule_twodose_grouped, "No vaccine")) 
  
  newsurvival %>% 
    select(-outcome_binary, -pt_end_date) %>% 
    rename(pt_end_date = new_end_date) %>% 
    mutate(vaccines = factor(vaccines,
                             levels = 0:3, 
                             labels = c("0","1","2","3+"))) %>% 
    mutate(t_vacc_detail = factor(t_vacc_detail, 
                               levels = c("No vaccine", levels(vaccines_schedule_timeupdate$vaccine_schedule_detail))),
           t_vacc_grouped = factor(t_vacc_grouped, 
                                  levels = c("No vaccine", levels(vaccines_schedule_timeupdate$vaccine_schedule_grouped))),
           t_vacc_twodose_detail = factor(t_vacc_twodose_detail, 
                                   levels = c("No vaccine", levels(vaccines_schedule_timeupdate$vaccine_schedule_twodose_detail))),
           t_vacc_twodose_grouped = factor(t_vacc_twodose_grouped, 
                                   levels = c("No vaccine", levels(vaccines_schedule_timeupdate$vaccine_schedule_grouped))),
           t = tstop - tstart
    ) %>% 
    # make sure time variable is sensible, if greater than 
    filter(t < 2.25)
}
