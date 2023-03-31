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
      # Outcome variables
      # convert first Long COVID record date into numeric 1,0 variable
      lc_out = as.numeric(!is.na(first_lc)),
      lc_dx_only = as.numeric(!is.na(first_lc_dx)),
      fracture = as.numeric(!is.na(first_fracture_hosp)),
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
    # only keep people with recorded sex
    filter(
      sex %in% c("male", "female", "intersex")
    ) %>% 
    mutate(sex = factor(sex, levels = c("male", "female", "intersex"))) %>% 
    # treat region as a factor
    mutate(practice_nuts = factor(practice_nuts)) %>% 
    # treat Long covid diagnosis (Dx) or referral (Rx) as a factor
    mutate(lc_dx_flag = factor(lc_dx_flag, levels = c("Dx", "Rx"))) %>% 
    # convert number of comorbidities to factor (0,1,2+)
    mutate(comorbidities = cut(
      comorbid_count, 
      breaks = c(0,1,2, Inf),
      labels = c("0", "1", "2+"))
    )
  
  df_full %>% 
    dplyr::select(patient_id, pt_start_date, pt_end_date, 
                  sex, age, age_centred, age_cat, 
                  practice_nuts, ethnicity, 
                  imd_q5, comorbidities,
                  care_home, care_home_nursing, care_home_code,
                  highrisk_shield, lowrisk_shield,
                  ons_death_date, death_date,
                  all_test_positive, no_prev_vacc, date_last_vacc, last_vacc_gap,
                  first_covid_hosp, all_covid_hosp,
                  latest_primarycare_covid, total_primarycare_covid,
                  starts_with("vaccine_dose_"),
                  first_lc, first_lc_dx, lc_dx_flag, first_fracture_hosp,
                  t, lc_out, lc_dx_only, fracture
                  )
}
