#' Process vaccination data in full combined dataset
#' @description Take raw vaccine data from data builder ad create grouping variables for analysis
#' including vaccines categorised by no. of doses (0-6+) and hetero-/homo-logous doses in first two vaccines
#' @param dataset File path for the file containing case data
#' @return A dataframe containing new variables on number of vaccine doses and heterogeneous protection from first 2 doses

tidy_vaccine_data <- function(dataset){
  # convert no. vaccines to factor (0-3+)
  df_full <- dataset %>%
    mutate(no_prev_vacc = cut(
      no_prev_vacc,
      breaks = c(-Inf, 0:2, Inf),
      labels = c(as.character(0:2), "3+")
    )) 
  #df_full$no_prev_vacc <- factor(is.na(df_full$vaccine_dose_1_date) +
  #                       is.na(df_full$vaccine_dose_2_date) +
  #                       is.na(df_full$vaccine_dose_3_date),
  #                       labels = c("3+", "2", "1", "0"))
  
  # create variable indicating whether all vaccines have been of same type
  mix_and_match_vacc_interim <- df_full %>% 
    dplyr::select(patient_id, no_prev_vacc, no_prev_vacc, contains("vaccine_dose")) %>% 
    mutate(mRNA1 = stringr::str_detect(vaccine_dose_1_manufacturer, pattern = "mRNA|mrna|comirnarty|moderna|pfizer"),
           mRNA2 = stringr::str_detect(vaccine_dose_2_manufacturer, pattern = "mRNA|mrna|comirnarty|moderna|pfizer"),
           mRNA3 = stringr::str_detect(vaccine_dose_3_manufacturer, pattern = "mRNA|mrna|comirnarty|moderna|pfizer")) %>% 
    # calculate different combinations of first 3 doses 
    mutate(
      no_vaccine = case_when(
        no_prev_vacc == "0" ~ TRUE
      ),
      one_dose = case_when(
        no_prev_vacc == "1" ~ TRUE
      ),
      m_m_twodose = case_when(
        mRNA1 & mRNA2   ~ TRUE,
        mRNA1 & !mRNA2  ~ FALSE,
        !mRNA1 & mRNA2  ~ FALSE,
        !mRNA1 & !mRNA2 ~ FALSE
      ),
      non_non_twodose = case_when(
        mRNA1 & mRNA2   ~ FALSE,
        mRNA1 & !mRNA2  ~ FALSE,
        !mRNA1 & mRNA2  ~ FALSE,
        !mRNA1 & !mRNA2 ~ TRUE
      ),
      mix_twodose = case_when(
        mRNA1 & mRNA2 ~ FALSE,
        mRNA1 & !mRNA2   ~ TRUE,
        !mRNA1 & mRNA2  ~ TRUE,
        !mRNA1 & !mRNA2  ~ FALSE
      ),
      m_boost_nonprior = case_when(
        mRNA3  & non_non_twodose ~ TRUE,
        !mRNA3 ~ FALSE
      ),
      nonm_boost_mprior = case_when(
        !mRNA3  & m_m_twodose ~ TRUE,
        mRNA3 ~ FALSE
      ),
      homologous_m_boost = case_when(
        mRNA3 & m_m_twodose ~ TRUE
      ),
      homologous_n_boost = case_when(
        !mRNA3 & non_non_twodose ~ TRUE
      ),
      vaccine_schedule = case_when(
        no_vaccine ~ "0 dose", 
        one_dose ~ "1 dose", 
        m_m_twodose & no_prev_vacc == "2" ~ "2 dose: mRNA", 
        non_non_twodose & no_prev_vacc == "2" ~ "2 dose: non-mRNA",
        mix_twodose & no_prev_vacc == "2" ~ "2 dose: heterologous",
        homologous_m_boost ~ "3 dose: homologous (mRNA) booster",
        homologous_n_boost ~ "3 dose: homologous (non-mRNA) booster",
        m_boost_nonprior ~ "3 dose: heterologous (mRNA) booster",
        nonm_boost_mprior ~ "3 dose: heterologous (non-mRNA) booster",
        mix_twodose & no_prev_vacc == "3+" ~ "3 dose: booster on mixed initial",
      )
    )
  
  mix_and_match_vacc_interim <- mix_and_match_vacc_interim %>% 
    mutate(
      vaccine_schedule_detail = factor(
        vaccine_schedule,
        levels = c("0 dose", 
                   "1 dose",
                   "2 dose: mRNA",
                   "2 dose: non-mRNA",
                   "2 dose: heterologous",
                   "3 dose: homologous (mRNA) booster",
                   "3 dose: homologous (non-mRNA) booster",
                   "3 dose: heterologous (mRNA) booster",
                   "3 dose: heterologous (non-mRNA) booster",
                   "3 dose: booster on mixed initial"
        )),
      vaccine_schedule_grouped = factor(
        vaccine_schedule,
        levels = c("0 dose", 
                   "1 dose",
                   "2 dose: mRNA",
                   "2 dose: non-mRNA",
                   "2 dose: heterologous",
                   "3 dose: homologous (mRNA) booster",
                   "3 dose: homologous (non-mRNA) booster",
                   "3 dose: heterologous (mRNA) booster",
                   "3 dose: heterologous (non-mRNA) booster",
                   "3 dose: booster on mixed initial"
        ),
        labels = c("0 dose", 
                   "1 dose",
                   "2 dose: homologous",
                   "2 dose: homologous",
                   "2 dose: heterologous",
                   "3 dose: homologous",
                   "3 dose: homologous",
                   "3 dose: heterologous",
                   "3 dose: heterologous",
                   "3 dose: heterologous"
        )),
      vaccine_schedule_twodose_detail = factor(
        vaccine_schedule,
        levels = c("0 dose", 
                   "1 dose",
                   "2 dose: mRNA",
                   "2 dose: non-mRNA",
                   "2 dose: heterologous",
                   "3 dose: homologous (mRNA) booster",
                   "3 dose: homologous (non-mRNA) booster",
                   "3 dose: heterologous (mRNA) booster",
                   "3 dose: heterologous (non-mRNA) booster",
                   "3 dose: booster on mixed initial"
        ),
        labels = c("0 dose", 
                   "1 dose",
                   "2 dose: mRNA",
                   "2 dose: non-mRNA",
                   "2 dose: heterologous",
                   # at 2 doses, this group had mrna
                   "2 dose: mRNA",
                   # at 2 doses, this group had non-mrna
                   "2 dose: non-mRNA",
                   # at 2 doses, this group had non-mrna (then got a mRNA booster)
                   "2 dose: non-mRNA",
                   # at 2 doses, this group had mrna (then got a non-mRNA booster)
                   "2 dose: mRNA",
                   "2 dose: heterologous"
        )),
      vaccine_schedule_twodose_grouped = factor(
        vaccine_schedule,
        levels = c("0 dose", 
                   "1 dose",
                   "2 dose: mRNA",
                   "2 dose: non-mRNA",
                   "2 dose: heterologous",
                   "3 dose: homologous (mRNA) booster",
                   "3 dose: homologous (non-mRNA) booster",
                   "3 dose: heterologous (mRNA) booster",
                   "3 dose: heterologous (non-mRNA) booster",
                   "3 dose: booster on mixed initial"
        ),
        labels = c("0 dose", 
                   "1 dose",
                   "2 dose: homologous",
                   "2 dose: homologous",
                   "2 dose: heterologous",
                   # at 2 doses, this group had mrna
                   "2 dose: homologous",
                   # at 2 doses, this group had non-mrna
                   "2 dose: homologous",
                   # at 2 doses, this group had non-mrna (then got a mRNA booster)
                   "2 dose: homologous",
                   # at 2 doses, this group had mrna (then got a non-mRNA booster)
                   "2 dose: homologous",
                   "2 dose: heterologous"
        ))
    ) %>%
    dplyr::select(-vaccine_schedule)
  
  df_full %>% 
    left_join(
      dplyr::select(mix_and_match_vacc_interim, patient_id, starts_with("vaccine_schedule"))
    ) %>% 
    ungroup()
}
