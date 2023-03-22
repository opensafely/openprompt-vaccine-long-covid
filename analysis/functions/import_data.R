#' Import cases and controls for primary cohort construction
#' @description Imports the cases and controls data from Data builder, combines and tidies a couple of variables
#' @param cases_path File path for the file containing case data
#' @param controls_path File path for the file containing controls data
#' @return A dataframe containing data on both cases and controls

import_and_combine <- function(cases_path = "str", controls_path = "str"){
  cases <- readr::read_csv(cases_path) %>% 
    janitor::clean_names()
  spec(cases) %>% print()
  
  controls <- readr::read_csv(controls_path) %>% 
    janitor::clean_names()
  spec(controls) %>% print()
  
  df_full <- bind_rows(cases, 
                       controls) %>% 
    arrange(patient_id) %>% 
    mutate(
      # create time variable for follow up (years)
      t =  pt_start_date %--% pt_end_date / dyears(1),
      # convert first Long COVID record date into numeric 1,0 variable
      lc_out = as.numeric(!is.na(first_lc_dx)),
      # convert gap between most recent vaccine and LC record into years
      last_vacc_gap = last_vacc_gap / 365.25,
      # convert IMD to quintiles
      imd_q5 = cut(imd,
                   breaks = c(32844 * seq(0, 1, 0.2)),
                   labels = c("1 (most deprived)",
                              "2",
                              "3",
                              "4",
                              "5 (least deprived)")
      ),
      # label ethnicity variable 
      ethnicity = factor(
        ethnicity,
        levels = 1:6, 
        labels = c(
          "White",
          "Mixed", 
          "South Asian", 
          "Black",
          "Other",
          "Not stated"
        )),
      # create an age category variable for easy stratification
      age_cat = cut(
        age, 
        breaks = c(0, 31, 41, 51, 61, 71, Inf),
        labels = c(
          "18-29",
          "30-39",
          "40-49",
          "50-59",
          "60-69",
          "70+"
        )),
      # age centred (for modelling purposes)
      age_centred = age - mean(age, na.rm = TRUE),
      # make ethnicity a factor
      ethnicity = factor(ethnicity)
    ) %>% 
    filter(
      sex %in% c("male", "female", "intersex")
    ) %>% 
    mutate(sex = factor(sex, levels = c("male", "female", "intersex")))
  
  df_full
}
