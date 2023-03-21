#' Time update on date of vaccine dose
#' @description Takes a flat 1-row-per-patient file and time updates on the date of vaccine dosage 
#' @param data data to be time updated
#' @param controls_path File path for the file containing controls data
#' @return A dataframe containing time updated data 

time_update_vaccinedoses <- function(data){
  # remove anyone who starts and ends on the same date 
  df_vacc_base <- data %>% 
    filter(t>0) 
  
  # ignoring vaccine records from admin records and only using vaccination table results for now (no diagnosis records)
  # only includes first 6 vaccinations (check `datasets.py` for the `create_sequential_variables` functino that generates these data)
  df_vacc_long <- data %>% 
    filter(t>0) %>% 
    dplyr::select(patient_id, pt_start_date, starts_with("covid_vacc_"), mix_and_match) %>% 
    dplyr::select(!contains("adm")) %>%  
    pivot_longer(cols = contains("vacc_tab"), names_to = "vacc_no", names_pattern = "covid_vacc_(.)_vacc_tab", values_to = "date") %>% 
    mutate(t = pt_start_date %--% date / dyears(1)) %>% 
    # get the date of the second vaccine for everyone (for time updating the mixmatch variable)
    mutate_at("mix_and_match", ~ifelse(vacc_no < 2 & . != "No vaccine", 2, .)) 
  
  # if this gap is negative then the vaccine was delivered pre-study enrol so set t = 0
  df_vacc_long$t[df_vacc_long$t < 0] <- 0 
  
  df_vacc_timeupdated <- df_vacc_base %>% 
    survival::tmerge(df_vacc_base, id = patient_id, lc_out = event(t, lc_out)) %>% 
    survival::tmerge(df_vacc_long, id = patient_id, vaccines = tdc(t, vacc_no, 0)) %>% 
    survival::tmerge(df_vacc_long, id = patient_id, t_mixmatch = tdc(t, mix_and_match, 1)) %>% 
    mutate(vaccines = factor(vaccines)) %>% 
    mutate(t_mixmatch = factor(t_mixmatch, 
                         levels = 1:6,
                         labels = c(
                           "No vaccine",
                           "One dose only",
                           "Homologous",
                           "Heterologeous",
                           "Missing manufacturer info",
                           "Fewer than 2 doses"
                         )))
  df_vacc_timeupdated
}
