library(tidyverse)
library(lubridate)

source("analysis/debugging_codes/make_fake_vaccine_codes.R")

mix_and_match_vacc_interim <- test_mrna_code %>% 
  dplyr::select(patient_id, contains("manufacturer")) %>% 
  mutate(mRNA1 = stringr::str_detect(vaccine_dose_1_manufacturer, pattern = "mRNA|mrna|comirnarty|moderna|pfizer"),
         mRNA2 = stringr::str_detect(vaccine_dose_2_manufacturer, pattern = "mRNA|mrna|comirnarty|moderna|pfizer"),
         mRNA3 = stringr::str_detect(vaccine_dose_3_manufacturer, pattern = "mRNA|mrna|comirnarty|moderna|pfizer")) %>% 
  # calculate different combinations of first 3 doses 
  mutate(
    no_vaccine = case_when(
      is.na(mRNA1) ~ TRUE
    ),
    one_dose = case_when(
      !is.na(mRNA1) & is.na(mRNA2) ~ TRUE, 
      !is.na(mRNA2) ~ FALSE
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
      mRNA1 & !mRNA2   ~ TRUE,
      !mRNA1 & mRNA2  ~ TRUE,
      !mRNA1 & !mRNA2  ~ FALSE,
      mRNA1 & mRNA2 ~ FALSE
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
      m_m_twodose & is.na(mRNA3) ~ "2 dose: mRNA", 
      non_non_twodose & is.na(mRNA3) ~ "2 dose: non-mRNA",
      mix_twodose & is.na(mRNA3) ~ "2 dose: heterologous",
      homologous_m_boost ~ "3 dose: homologous (mRNA) booster",
      homologous_n_boost ~ "3 dose: homologous (non-mRNA) booster",
      m_boost_nonprior ~ "3 dose: heterologous (mRNA) booster",
      nonm_boost_mprior ~ "3 dose: heterologous (non-mRNA) booster",
      mix_twodose & !is.na(mRNA3) ~ "3 dose: booster on mixed initial"
    )
    )
table(mix_and_match_vacc_interim$vaccine_schedule) %>% sort()
  
  
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
                 # at 2 doses, this gorup had mrna
                 "2 dose: mRNA",
                 # at 2 doses, this gorup had non-mrna
                 "2 dose: non-mRNA",
                 # at 2 doses, this gorup had non-mrna (then got a mRNA booster)
                 "2 dose: non-mRNA",
                 # at 2 doses, this gorup had mrna (then got a non-mRNA booster)
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
                 # at 2 doses, this gorup had mrna
                 "2 dose: homologous",
                 # at 2 doses, this gorup had non-mrna
                 "2 dose: homologous",
                 # at 2 doses, this gorup had non-mrna (then got a mRNA booster)
                 "2 dose: homologous",
                 # at 2 doses, this gorup had mrna (then got a non-mRNA booster)
                 "2 dose: homologous",
                 "2 dose: heterologous"
      ))
    ) %>% 
  dplyr::select(-vaccine_schedule)

df_full <- test_mrna_code %>% 
  left_join(
    dplyr::select(mix_and_match_vacc_interim, patient_id, starts_with("vaccine_schedule"))
  ) 
df_full %>% filter(patient_id==1)



# run the time updating code on these data --------------------------------

# remove anyone who starts and ends on the same date 
set.seed(1571)
df_full$sex <- sample(c("male", "female"), size = data_size, replace = TRUE)
df_full$pt_start_date = as.Date("2020-11-01")
df_full$outcome <- as.Date("2020-11-01") + sample(0:600, size = data_size, replace = TRUE)
df_full$outcome[sample(1:data_size, size = 1200)] <- NA
df_full$pt_end_date = as.Date("2023-01-31") - sample(0:365, size = data_size, replace = TRUE)
df_full$age_centred <- sample(-40:40, size = data_size, replace=TRUE)

small_base <- df_full %>% 
  #dplyr::select(patient_id, starts_with("pt_"), contains("age"), sex, outcome) %>% 
  mutate(
    new_end_date =  as.Date(
      ifelse(!is.na(outcome) & outcome < pt_end_date,
             outcome, pt_end_date),
      origin = "1970-01-01"
    ),
    t = pt_start_date %--% new_end_date / dyears(1),
    outcome_binary = as.numeric(!is.na(outcome))
  ) %>% 
  filter(t>0)
small_base$outcome_binary %>% sum()

# calculate time difference for each vaccine dose
df_vacc_base_t <- df_full %>% 
  select(patient_id, pt_start_date, contains("vaccine_dose_"), contains("vaccine_schedule_")) %>% 
  mutate(vacc_time_1 = pt_start_date %--% vaccine_dose_1_date / dyears(1),
         vacc_time_2 = pt_start_date %--% vaccine_dose_2_date / dyears(1),
         vacc_time_3 = pt_start_date %--% vaccine_dose_3_date / dyears(1),
         vaccine_schedule_2d  = ifelse(!is.na(vaccine_dose_2_date), as.character(vaccine_schedule_twodose_detail), NA),
         vaccine_schedule_2g  = ifelse(!is.na(vaccine_dose_2_date), as.character(vaccine_schedule_twodose_grouped), NA),
         vaccine_schedule_3d = ifelse(!is.na(vaccine_dose_3_date), as.character(vaccine_schedule_detail), NA),
         vaccine_schedule_3g = ifelse(!is.na(vaccine_dose_3_date), as.character(vaccine_schedule_grouped), NA)
  ) %>% 
  dplyr::select(-contains("vaccine_dose")) %>% 
  pivot_longer(cols = starts_with("vacc_time_"), names_to = "vacc_no", names_pattern = "vacc_time_(.)", values_to = "years")

# create new time-updated version of homologous/heterologous vaccine dose - need two versions of a time-updated variable (one with detail, one grouped by homo/hetero)
vaccines_schedule_timeupdate <- df_vacc_base_t %>% 
  filter(!is.na(years)) 
vaccines_schedule_timeupdate$vaccine_schedule_detail[vaccines_schedule_timeupdate$vacc_no == "1"] <- "1 dose"
vaccines_schedule_timeupdate$vaccine_schedule_grouped[vaccines_schedule_timeupdate$vacc_no == "1"] <- "1 dose"
vaccines_schedule_timeupdate$vaccine_schedule_twodose_grouped[vaccines_schedule_timeupdate$vacc_no == "1"] <- "1 dose"

vaccines_schedule_timeupdate$vaccine_schedule_detail[vaccines_schedule_timeupdate$vacc_no == "2"] <- as.character(vaccines_schedule_timeupdate$vaccine_schedule_2d[vaccines_schedule_timeupdate$vacc_no == "2"])
vaccines_schedule_timeupdate$vaccine_schedule_grouped[vaccines_schedule_timeupdate$vacc_no == "2"] <- as.character(vaccines_schedule_timeupdate$vaccine_schedule_2g[vaccines_schedule_timeupdate$vacc_no == "2"])
vaccines_schedule_timeupdate$vaccine_schedule_twodose_grouped[vaccines_schedule_timeupdate$vacc_no == "2"] <- as.character(vaccines_schedule_timeupdate$vaccine_schedule_2g[vaccines_schedule_timeupdate$vacc_no == "2"])

# create the survival dataset 
survivaldata <- survival::tmerge(small_base, small_base, id = patient_id, out = event(t, outcome_binary)) 
newsurvival <- survival::tmerge(survivaldata, df_vacc_base_t, id = patient_id, vaccines = tdc(years, vacc_no, 0)) 

newsurvival <- survival::tmerge(newsurvival, vaccines_schedule_timeupdate, id = patient_id, t_vacc_detail = tdc(years, vaccine_schedule_detail, "No vaccine")) 
newsurvival <- survival::tmerge(newsurvival, vaccines_schedule_timeupdate, id = patient_id, t_vacc_grouped = tdc(years, vaccine_schedule_grouped, "No vaccine")) 
newsurvival <- survival::tmerge(newsurvival, vaccines_schedule_timeupdate, id = patient_id, t_vacc_twodose = tdc(years, vaccine_schedule_twodose_grouped, "No vaccine")) 

final <- newsurvival %>% 
  select(-outcome_binary, -pt_end_date) %>% 
  rename(pt_end_date = new_end_date) %>% 
  mutate(vaccines = factor(vaccines,
                           levels = 0:3, 
                           labels = c("0","1","2","3+"))) %>% 
  mutate(t_vacc_detail = factor(t_vacc_detail, 
                                levels = c("No vaccine", levels(vaccines_schedule_timeupdate$vaccine_schedule_detail))),
         t_vacc_grouped = factor(t_vacc_grouped, 
                                 levels = c("No vaccine", levels(vaccines_schedule_timeupdate$vaccine_schedule_grouped))),
         t_vacc_twodose = factor(t_vacc_twodose, 
                                 levels = c("No vaccine", levels(vaccines_schedule_timeupdate$vaccine_schedule_grouped))),
         t = tstop - tstart
  )
final %>% 
  saveRDS("output/testdata/time_updated_test.RData")
