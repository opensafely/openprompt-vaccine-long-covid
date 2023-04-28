# Import Long COVID cohort

library(tidyverse)
library(lubridate)
library(here)
library(arrow)
library(gtsummary)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/summarise_data.R"))
source(here::here("analysis/functions/import_raw_data.R"))
source(here::here("analysis/functions/clean_raw_data.R"))
source(here::here("analysis/functions/clean_vaccine_data.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

lc_data_path <- here("output/dataset_lc_pre_vacc.csv.gz")

# import, combine, clean data -------------------------------------------------
imported_data <- readr::read_csv(lc_data_path) %>% 
  janitor::clean_names()
spec(imported_data) %>% print()

imported_data_unvacced <- imported_data %>% 
  filter(already_vacced == 0)

summarise_data(data_in = import_data, filenamebase = "lcfirst_cohort")
summarise_data(data_in = imported_data_unvacced, filenamebase = "lcfirst_cohort_unvacced")

imported_data_unvacced %>% glimpse()
imported_data_unvacced <- imported_data_unvacced %>% 
  mutate(
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
      breaks = c(0, seq(30, 70, 10), Inf),
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
  ) %>% 
  mutate(sex = factor(sex, levels = c("male", "female"))) %>% 
  # treat region as a factor
  mutate(practice_nuts = factor(practice_nuts,
                                levels = c("London", 
                                           "East Midlands",
                                           "East",
                                           "North East",
                                           "North West",
                                           "South East",
                                           "South West",
                                           "West Midlands", 
                                           "Yorkshire and The Humber"
                                ))) %>% 
  # treat Long covid diagnosis (Dx) or referral (Rx) as a factor
  mutate(lc_dx_flag = factor(lc_dx_flag, levels = c("Dx", "Rx"))) %>% 
  # get number of vaccines as a factor
  mutate(post_lc_vaccines = cut(no_post_lc_vacc, 
                                breaks = c(-Inf, 0, 1, 2, 3, Inf),
                                labels = c("0", "1", "2", ">2", ">2"))) %>% 
  # categorise tests post Long COVID 
  mutate(test_positive_cat = cut(
    all_test_positive, 
    breaks = c(-Inf, 0:5, Inf),
    labels = c(as.character(0:4), "5+", "5+"))
  ) %>% 
  # create number of covid Tests as factor (0-20+)
  mutate(test_total_cat = cut(
    all_tests, 
    breaks = c(-Inf, 0:5, Inf),
    labels = c(as.character(0:4), "5+", "5+"))
  ) 

var_labels <- list(
  N  ~ "Total N",
  post_lc_vaccines ~ "Number of vaccines post COVID",
  sex ~ "Sex",
  age ~ "Age",
  age_cat ~ "Age (categorised)",
  ethnicity ~ "Ethnicity",
  practice_nuts ~ "NHS region",
  imd_q5 ~ "Index of multiple deprivation (quintile)",
  test_positive_cat ~ "COVID-19 positive tests (n)",
  test_total_cat ~ "COVID-19 tests (n)"
)

var_labels <- var_labels %>%
  set_names(., map_chr(., all.vars))

tab1 <- imported_data_unvacced %>% 
  dplyr::select(lc_dx_flag, any_of(names(var_labels))) %>% 
  tbl_summary(
    by = lc_dx_flag, 
    label = unname(var_labels[-1]),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  bold_labels() %>%
  add_overall()

tab1
