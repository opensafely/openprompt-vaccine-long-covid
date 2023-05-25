#' Time update on date of vaccine dose
#' @description Takes a flat 1-row-per-patient file and time updates on the date of vaccine dosage 
#' @param data data to be time updated
#' @param outcome_var A date variable with the date of the Outcome event
#' @return A dataframe containing time updated data 

time_update_vaccinedoses <- function(data, outcome_var){
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
  
  # calculate time difference for each vaccine dose
  df_vacc_base_t <- small_base %>%
    # again, need to filter out anyone who has the event before/on the same day as study entry (should be redundant but just in case)
    filter(t>0) %>% 
    select(patient_id, pt_start_date, contains("vaccine_dose_"), contains("vaccine_schedule_"), first_mrna_vaccine_date) %>% 
    mutate(vacc_time_1 = pt_start_date %--% vaccine_dose_1_date / dyears(1),
           vacc_time_2 = pt_start_date %--% vaccine_dose_2_date / dyears(1),
           vacc_time_3 = pt_start_date %--% vaccine_dose_3_date / dyears(1)
           ) %>% 
    dplyr::select(-contains("vaccine_dose")) %>% 
    pivot_longer(cols = starts_with("vacc_time_"), names_to = "vacc_no", names_pattern = "vacc_time_(.*)", values_to = "years")
  
  ## check the right number of patids are involved before doing the computationally intensive tmerge stuff
  isEmpty <- function(x) {
    return(length(x)==0)
  }
  l1 <- unique(small_base$patient_id)
  l2 <- unique(df_vacc_base_t$patient_id)
  if(!isEmpty(setdiff(l1, l2))){stop("id values in time updated vaccine data that are not in base data")}
  
  # create new time-updated version of homologous/heterologous vaccine dose - need two versions of a time-updated variable (one with detail, one grouped by homo/hetero)
  vaccines_schedule_timeupdate <- df_vacc_base_t %>% 
    filter(!is.na(years))
  vaccines_schedule_timeupdate$vaccine_schedule_detail[vaccines_schedule_timeupdate$vacc_no == "1"] <- "1 dose"
  # 2nd dose if a three doser
  vaccines_schedule_timeupdate$vaccine_schedule_detail[vaccines_schedule_timeupdate$vacc_no == "2"] <- as.character(vaccines_schedule_timeupdate$vaccine_schedule_twodose_detail[vaccines_schedule_timeupdate$vacc_no == "2"])
  
  ## repeat that process for mrna 
  df_vacc_base_mrna <- small_base %>%
    filter(t>0) %>% 
    select(patient_id, pt_start_date, first_non_mrna_vaccine_date, first_mrna_vaccine_date) %>% 
    mutate(
      vacc_time_non_mRNA = pt_start_date %--% first_non_mrna_vaccine_date / dyears(1),
      vacc_time_mRNA = pt_start_date %--% first_mrna_vaccine_date / dyears(1)
      ) %>% 
    dplyr::select(-contains("vaccine_dose")) %>% 
    pivot_longer(cols = starts_with("vacc_time_"), names_to = "mrna", names_pattern = "vacc_time_(.*)", values_to = "years")
  
  ## repeat that process for first vaccine manufacturer 
  df_vacc_base_primarydose <- small_base %>%
    # again, need to filter out anyone who has the event before/on the same day as study entry (should be redundant but just in case)
    filter(t>0) %>% 
    select(patient_id, pt_start_date, vaccine_dose_1_date, vaccine_dose_1_manufacturer) %>% 
    mutate(
      vacc_time_dose1 = pt_start_date %--% vaccine_dose_1_date / dyears(1)
      ) %>% 
    #dplyr::select(-contains("vaccine_dose")) %>% 
    pivot_longer(cols = starts_with("vacc_time_"), names_to = "vacc_no", values_to = "years")
  
  df_vacc_base_primarydose$primary_course = df_vacc_base_primarydose$vaccine_dose_1_manufacturer
  
  # create the survival dataset 
  survivaldata <- survival::tmerge(small_base, small_base, id = patient_id, out = event(t, outcome_binary)) 
  newsurvival <- survival::tmerge(survivaldata, vaccines_schedule_timeupdate, id = patient_id, vaccines = tdc(years, vacc_no, 0)) 
  
  newsurvival <- survival::tmerge(newsurvival, df_vacc_base_mrna, id = patient_id, t_vacc_mrna = tdc(years, mrna, "No vaccine")) 
  
  newsurvival <- survival::tmerge(newsurvival, df_vacc_base_primarydose, id = patient_id, t_vacc_primary = tdc(years, primary_course, "No vaccine")) 
  
  newsurvival <- survival::tmerge(newsurvival, vaccines_schedule_timeupdate, id = patient_id, t_vacc_detail = tdc(years, vaccine_schedule_detail, "No vaccine")) 
  
  newsurvival %>%
    select(-outcome_binary,-pt_end_date) %>%
    rename(pt_end_date = new_end_date) %>%
    mutate(vaccines = factor(
      vaccines,
      levels = 0:3,
      labels = c("0", "1", "2", "3+")
    )) %>%
    mutate(t_vacc_detail = factor(t_vacc_detail,
                                  levels = c(
                                    "No vaccine",
                                    levels(vaccines_schedule_timeupdate$vaccine_schedule_detail)
                                  ))) %>%
    mutate(t_vacc_primary = factor(t_vacc_primary,
                                  levels = c("No vaccine", "AstraZeneca", "Pfizer", "Other"))) %>%
    mutate(t_vacc_mrna = factor(t_vacc_mrna,
                                  levels = c("No vaccine", "non_mRNA", "mRNA"))) %>%
    mutate(t = tstop - tstart)
}
