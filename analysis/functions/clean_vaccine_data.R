#' Process vaccination data in full combined dataset
#' @description Take raw vaccine data from data builder ad create grouping variables for analysis
#' including vaccines categorised by no. of doses (0-6+) and hetero-/homo-logous doses in first two vaccines
#' @param dataset File path for the file containing case data
#' @return A dataframe containing new variables on number of vaccine doses and heterogeneous protection from first 2 doses

tidy_vaccine_data <- function(dataset){
  # convert no. vaccines to factor (0-6+)
  df_full <- dataset %>%
    mutate(no_prev_vacc = cut(
      no_prev_vacc,
      breaks = c(-Inf, 0:5, Inf),
      labels = c(as.character(0:5), "6+")
    )) 
  
  # create variable indicating whether all vaccines have been of same type
  mix_and_match_vacc <- df_full %>% 
    dplyr::select(patient_id, contains("manufacturer_tab")) %>% 
    pivot_longer(cols = -patient_id, names_to = "vacc_episode", names_pattern = "covid_vacc_(.)_manufacturer_tab", values_to = "manufacturer") %>%
    drop_na() %>% 
    mutate(mRNA = stringr::str_detect(manufacturer, pattern = "mRNA|mrna|comirnarty|moderna|pfizer")) %>% 
    group_by(patient_id) %>% 
    mutate(mix_and_match = n_distinct(mRNA)!=1) %>% 
    slice(1)
  
  # merge these back on
  df_full <- df_full %>% 
    left_join(
      dplyr::select(mix_and_match_vacc, patient_id, mRNA, mix_and_match)
    ) %>% 
    mutate("mix_and_match" = factor(
      case_when(
        no_prev_vacc == 0 ~ 0,
        no_prev_vacc == 1 ~ 1,
        mix_and_match == FALSE ~ 2,
        mix_and_match == TRUE ~ 3,
        is.na(mix_and_match) ~ 4
      ),
      levels = c(0:4), 
      labels = c("No vaccine", "One dose only", "Homologous", "Heterologeous", "Missing manufacturer info")
    ))
  
  df_full %>% ungroup()
}
