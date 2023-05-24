library(tidyverse)
library(here)
library(glue)
library(arrow)
library(gtsummary)
library(lubridate)

source(here::here("analysis/functions/ggplot_theme.R"))
source(here::here("analysis/functions/redaction.R"))

#dir.create(here::here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

# create output directories 
output_dir_tab <- here("output/tables")
fs::dir_create(output_dir_tab)

output_dir_plots <- here("output/supplementary")
fs::dir_create(output_dir_plots)


# redaction functions for data --------------------------------------------
roundmid_any <- function(x, to=1){
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

# will update this when releasing outputs
threshold <- 7

# import data -------------------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))

baseline_data <- cleaned_data %>%
  # select only baseline variables 
  dplyr::select(pt_start_date, 
                pt_end_date,
                t,
                sex,
                age,
                age_cat,
                practice_nuts, 
                imd_q5,
                ethnicity,
                comorbidities, 
                care_home, 
                highrisk_shield, 
                lowrisk_shield) %>% 
  # calculate year of study enrolment
  dplyr::mutate(pt_start_year = factor(year(pt_start_date))) %>% 
  dplyr::select(-pt_start_date,
                -pt_end_date) %>% 
  dplyr::select(pt_start_year, everything())


# var_labels --------------------------------------------------------------
var_labels <- list(
  N  ~ "Total N",
  pt_start_year ~ "Follow-up start (year)",
  sex ~ "Sex",
  t ~ "Follow-up time (years)",
  age ~ "Age",
  age_cat ~ "Age category",
  ethnicity ~ "Ethnicity",
  practice_nuts ~ "Region",
  imd_q5 ~ "IMD (quintile)",
  comorbidities ~ "Comorbidities",
  care_home ~ "Resident in care home",
  highrisk_shield ~ "Shielding (high risk group)",
  lowrisk_shield ~ " Shielding (Low/moderate risk group)"
)

var_labels_full <- splice(
  var_labels,
  lc_out ~ "Any Long COVID record", 
  lc_dx_only ~ "Long COVID diagnosis",
  lc_cat ~ "Long COVID record by prior status",
  fracture ~ "Hospital record of fracture", 
  covid_hosp_cat ~ "COVID-19 hospitalisations (n)",
  covid_primary_cat ~ "COVID-19 primary care record (n)", 
  test_positive_cat ~ "COVID-19 positive tests (n)",
  test_total_cat ~ "COVID-19 tests (n)",
  vaccine_schedule_detail ~ "Vaccination schedule received"
)

var_labels <- var_labels %>%
  set_names(., map_chr(., all.vars))
var_labels_full <- var_labels_full %>%
  set_names(., map_chr(., all.vars))

# make table 1 ------------------------------------------------------------
table1 <- baseline_data %>% 
  dplyr::select(any_of(names(var_labels))) %>% 
  tbl_summary(
    label = unname(var_labels[names(.)]),
    statistic = list(
      all_continuous() ~ "{mean} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  )

table1 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "tab1_baseline_description.html",
    path = fs::path(output_dir_tab)
  )

raw_stats <- table1$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats)

raw_stats_redacted_catgorical <- raw_stats %>%
  filter(!is.na(n)) %>% 
  mutate(
    n=redact_and_round(n, threshold),
    N=redact_and_round(N, threshold),
    p=round(100*n/N,1),
    N_miss = redact_and_round(N_miss, threshold),
    N_obs = redact_and_round(N_obs, threshold),
    p_miss = round(100*N_miss/N_obs,1),
    N_nonmiss = redact_and_round(N_nonmiss, threshold),
    p_nonmiss = round(100*N_nonmiss/N_obs,1),
    var_label = factor(var_label, levels=map_chr(var_labels[-c(1)], ~last(as.character(.)))),
    variable_levels = replace_na(as.character(variable_levels), "")
  ) %>% 
  mutate(prettyN = as.character(prettyNum(formatC(n, digits = 1, format = "f"), 
                                          big.mark = ",", preserve.width = "none",
                                          drop0trailing = TRUE)),
         stat = paste0(prettyN, " (",p,"%)")) %>% 
  dplyr::select(variable = var_label, 
                level = variable_levels, 
                stat) 

raw_stats_redacted_numeric <- raw_stats %>% 
  filter(is.na(n)) %>% 
  mutate_if(is.numeric, ~prettyNum(formatC(., digits = 1, format = "f"), 
                                   big.mark = ",", preserve.width = "none",
                                   drop0trailing = TRUE)) %>% 
  mutate(stat = paste0(mean, " (", p25, "-", p75,")")) %>% 
  dplyr::select(variable = var_label, 
                level = variable_levels, 
                stat) 


raw_stats_output <- raw_stats_redacted_catgorical %>% 
  bind_rows(raw_stats_redacted_numeric) %>% 
  mutate(variable = factor(variable,
                           levels = c(
                             "Follow-up start (year)",
                             "Follow-up time (years)",
                             "Age",
                             "Age category",
                             "Sex",
                             "Region",
                             "IMD (quintile)",
                             "Ethnicity",
                             "Comorbidities",
                             "Resident in care home",
                             "Shielding (high risk group)",
                             "Shielding (Low/moderate risk group)"
                           ))) %>%
    arrange(variable, level)

write_csv(raw_stats_output, here::here("output/table1_data.csv"))

# vaccine-to-long COVID gaps by dose number --------------------------------
vaccine_gaps_tab <- cleaned_data %>%
  group_by(no_prev_vacc, lc_dx_flag) %>% 
  summarise(avg_gap = mean(last_vacc_gap), sd_gap = sd(last_vacc_gap), .groups = "keep")
write_csv(vaccine_gaps_tab, here("output/supplementary/vaccines_longcovid_gap.csv"))

pdf(here("output/supplementary/fig_vaccines_longcovid_gap.pdf"), width = 8, height = 6)
cleaned_data %>% 
  dplyr::select(no_prev_vacc, last_vacc_gap, lc_dx_flag) %>% 
  dplyr::filter(!is.na(lc_dx_flag)) %>% 
  ggplot(aes(x = last_vacc_gap, fill = no_prev_vacc)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~lc_dx_flag, ncol = 1) +
  labs(fill = "Number of vaccine doses", x = "Gap between vaccine and long COVID record (years)") +
  theme_ali()
dev.off()

pdf(here("output/supplementary/fig_vaccines_longcovid_gap_zoomed.pdf"), width = 8, height = 6)
cleaned_data %>% 
  dplyr::select(no_prev_vacc, last_vacc_gap, lc_dx_flag) %>% 
  dplyr::filter(!is.na(lc_dx_flag)) %>% 
  ggplot(aes(x = last_vacc_gap, fill = no_prev_vacc)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~lc_dx_flag, ncol = 1) +
  xlim(c(0, 2)) +
  labs(fill = "Number of vaccine doses", x = "Gap between vaccine and long COVID record (years)") +
  theme_ali()
dev.off()
