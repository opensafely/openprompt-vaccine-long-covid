#' 020 - table 1 - rate of Long COVID by vaccine group
#'

library(tidyverse)
library(survival)
library(cmprsk)
library(ggsurvfit)
library(lubridate)
library(here)

cases <- readr::read_csv(here("output/dataset_cases.csv.gz")) %>% 
  janitor::clean_names()
spec(cases) %>% print()

controls <- readr::read_csv(here("output/dataset_controls.csv.gz")) %>% 
  janitor::clean_names()
spec(controls) %>% print()

df_full <- bind_rows(cases, 
                    controls) %>% 
  arrange(patient_id)

# convert no. vaccines to factor (0-6+)
df_full <- df_full %>%
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
  group_by(patient_id) %>% 
  mutate(mix_and_match = n_distinct(manufacturer)!=1) %>% 
  slice(1)

# merge these back on
df_full <- df_full %>% 
  left_join(
    dplyr::select(mix_and_match_vacc, patient_id, mix_and_match)
  ) %>% 
  mutate("mix_and_match" = factor(
    case_when(
      mix_and_match == TRUE ~ 2,
      mix_and_match == FALSE ~ 1,
      no_prev_vacc == 0 ~ 0,
      is.na(mix_and_match) ~ 3
    ),
    levels = c(0:3), 
    labels = c("No vaccine", "Homologous", "Heterologeous", "Missing manufacturer info")
  ))

# calculate the raw rate as a sense check ---------------------------------
# using vaccine status at endpoint so obviously inaccurate as someone with 3 vaccines just before their LC record
# did not spend the whole follow up with 3 vaccine doses. But we will deal with that in the next step...
df_full <- df_full %>% 
  mutate(t =  pt_start_date %--% pt_end_date / dyears(1),
         lc_out = as.numeric(!is.na(first_lc_dx)),
         last_vacc_gap = last_vacc_gap / 365.25) 

# function to calculate rates by a grouping variable 
# 95% CI calced as normal approx. to Poisson distribution 
calculate_rates <- function(data, grouping_var){
  data %>% 
    group_by({{grouping_var}}) %>% 
    summarise(
      n = n(), 
      lc = sum(lc_out),
      fup = sum(t),
      total_tests = sum(all_test_positive),
      avg_last_vacc = mean(last_vacc_gap),
      n_dx = sum(lc_dx_flag == "Dx", na.rm = T),
      n_rx = sum(lc_dx_flag == "Rx", na.rm = T)
    ) %>% 
    mutate(rate_per1e5 = (lc/fup)*1e5,
           se_rate = (sqrt(lc)/fup)*1e5,
           errorfactor = exp(1.96/sqrt(lc)),
           lci = rate_per1e5/errorfactor,
           uci = rate_per1e5*errorfactor)
}
table1_rawrates <- calculate_rates(df_full, no_prev_vacc)
table1_rawrates %>% 
  write_csv(here("output/tab020_raw_lc_rates.csv"))

tab2_rates_mixmatch <- calculate_rates(df_full, mix_and_match)
tab2_rates_mixmatch %>% 
  write_csv(here("output/tab021_raw_lc_rates_mixmatch.csv"))

# time update vaccine status ----------------------------------------------
# remove anyone who starts and ends on the same date 
df_vacc_base <- df_full %>% 
  filter(t>0) 

# ignoring vaccine records from admin records and only using vacc table results for now 
df_vacc_long <- df_full %>% 
  filter(t>0) %>% 
  dplyr::select(patient_id, pt_start_date, starts_with("covid_vacc_")) %>% 
  dplyr::select(!contains("adm")) %>%  
  pivot_longer(cols = contains("vacc_tab"), names_to = "vacc_no", names_pattern = "covid_vacc_(.)_vacc_tab", values_to = "date") %>% 
  mutate(t = pt_start_date %--% date / dyears(1))

# if this gap is negative then the vaccine was delivered pre-study enrol so set t = 0
df_vacc_long$t[df_vacc_long$t < 0] <- 0 

df_vacc_timeupdated <- df_vacc_base %>% 
  survival::tmerge(df_vacc_base, id = patient_id, lc_out = event(t, lc_out)) %>% 
  survival::tmerge(df_vacc_long, id = patient_id, vaccines = tdc(t, vacc_no, 0))  

tab3_tdcrates <- calculate_rates(df_vacc_timeupdated, vaccines)
tab3_tdcrates %>% 
  write_csv(here("output/tab022_tdc_lc_rates.csv"))


# plot cumulative incidence curves ----------------------------------------
pdf(here("output/fig1_cumulative_incidence.pdf"), width = 8, height = 6)
df_vacc_timeupdated$lc_out_fct <- factor(df_vacc_timeupdated$lc_out, levels = 0:1)
tidycmprsk::cuminc(Surv(t, lc_out_fct) ~ vaccines, data = df_vacc_timeupdated) %>% 
  ggcuminc() + 
  labs(
    x = "Years"
  ) + 
  add_confidence_interval()
dev.off()


# plot time of Long COVID diagnosis ---------------------------------------
timeplot <- df_full %>% 
  ungroup() %>% 
  dplyr::select(patient_id, pt_start_date, pt_end_date, sex, lc_out, first_lc_dx) %>% 
  mutate(month = month(first_lc_dx),
         year = year(first_lc_dx)) %>% 
  group_by(year, month, sex) %>% 
  summarise(total_lc = sum(lc_out, na.rm = TRUE)) %>% 
  mutate(day = 1,
         date = make_date(year, month, day))

pdf(here("output/fig2_raw_counts_bysex.pdf"), width = 8, height = 6)
ggplot(timeplot, aes(x = date, y = total_lc, colour = sex, fill = sex)) +
  geom_col() +
  labs(x = "Date", y = "Count of Long COVID codes") +
  theme_bw()
dev.off()
