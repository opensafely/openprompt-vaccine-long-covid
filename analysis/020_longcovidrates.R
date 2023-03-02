#' 020 - table 1 - rate of Long COVID by vaccine group
#'

library(tidyverse)
library(survival)
library(here)

cases <- readr::read_csv(here("output/dataset_cases.csv.gz")) %>% janitor::clean_names()
spec(cases) %>% print()
controls <- readr::read_csv(here("output/dataset_controls.csv.gz")) %>% janitor::clean_names()
spec(controls) %>% print()

df_full <- bind_rows(cases, 
                    controls) %>% 
  arrange(patient_id)

#glimpse(df_full)
#skimr::skim(df_full)


# calculate the raw rate as a sense check ---------------------------------
# using vaccine status at endpoint so obviously inaccurate as someone with 3 vaccines just before their LC record
# did not spend the whole follow up with 3 vaccine doses. But we will deal with that in the next step...
df_full <- df_full %>% 
  mutate(t = as.numeric((pt_end_date - pt_start_date)/365.25),
         lc_out = as.numeric(!is.na(first_lc_dx)),
         last_vacc_gap = last_vacc_gap/365.25) 
df_full %>% 
  group_by(no_prev_vacc) %>% 
  summarise(
    lc = sum(lc_out),
    fup = sum(t),
    total_tests = sum(all_test_positive),
    avg_last_vacc = mean(last_vacc_gap),
    n_dx = sum(lc_dx_flag == "Dx", na.rm = T),
    n_rx = sum(lc_dx_flag == "Rx", na.rm = T)
  ) %>% 
  mutate(rate_per1e5 = (lc/fup)*100000) %>% 
  write_csv(here("output/tab020_raw_lc_rates.csv"))

# time update vaccine status ----------------------------------------------
df_vacc_base <- df_full %>% 
  filter(t>0)

df_vacc_long <- df_full %>% 
  filter(t>0) %>% 
  dplyr::select(patient_id, pt_start_date, starts_with("covid_vacc_")) %>% 
  dplyr::select(!contains("adm")) %>%  ## ignoring vaccine records from admin records and only using vacc table results for now
  pivot_longer(cols = starts_with("covid_vacc_"), names_to = "vacc_no", names_patter = "covid_vacc_(.)_vacc_tab", values_to = "date") %>% 
  mutate(t = as.numeric((date- pt_start_date)/365.25))
df_vacc_long$t[df_vacc_long$t < 0] <- 0

df_vacc_timeupdated <- df_vacc_base %>% 
  survival::tmerge(df_vacc_base, id = patient_id, lc_out = event(t, lc_out)) %>% 
  survival::tmerge(df_vacc_long, id = patient_id, vaccines = tdc(t, vacc_no, 0))  

df_vacc_timeupdated %>% 
  group_by(vaccines) %>% 
  summarise(
    lc = sum(lc_out),
    fup = sum(t),
    total_tests = sum(all_test_positive),
    avg_last_vacc = mean(last_vacc_gap),
    n_dx = sum(lc_dx_flag == "Dx", na.rm = T),
    n_rx = sum(lc_dx_flag == "Rx", na.rm = T)
  ) %>% 
  mutate(rate_per1e5 = (lc/fup)*100000) %>% 
  write_csv(here("output/tab021_tdc_lc_rates.csv"))

