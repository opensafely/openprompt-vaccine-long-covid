library(tidyverse)
library(here)
library(glue)
library(arrow)
library(gtsummary)
library(lubridate)

source(here::here("analysis/functions/ggplot_theme.R"))

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
threshold <- 1

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
  age_cat ~ "Age (categorised)",
  ethnicity ~ "Ethnicity",
  practice_nuts ~ "NHS region",
  imd_q5 ~ "Index of multiple deprivation (quintile)",
  comorbidities ~ "Comorbidities",
  care_home ~ "Resident in care home",
  highrisk_shield ~ "High risk shielding category",
  lowrisk_shield ~ "Low/moderate risk shielding category"
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
      all_continuous() ~ "{p50} ({p25}-{p75})",
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

raw_stats_redacted <- raw_stats %>%
  mutate(
    n=roundmid_any(n, threshold),
    N=roundmid_any(N, threshold),
    p=round(100*n/N,1),
    N_miss = roundmid_any(N_miss, threshold),
    N_obs = roundmid_any(N_obs, threshold),
    p_miss = round(100*N_miss/N_obs,1),
    N_nonmiss = roundmid_any(N_nonmiss, threshold),
    p_nonmiss = round(100*N_nonmiss/N_obs,1),
    var_label = factor(var_label, levels=map_chr(var_labels[-c(1)], ~last(as.character(.)))),
    variable_levels = replace_na(as.character(variable_levels), "")
  )
write_csv(raw_stats_redacted, here::here("output/table1_data.csv"))

table1_data <- raw_stats_redacted %>%
  rowwise() %>%
  transmute(
    var_label,
    # gt creates a column called `label` when run locally, `variable_labels` 
    # when run in opensafely (probs different versions)
    # label,
    variable_levels,
    value = glue(stat_display)
  ) 

table1_review <- table1_data %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_paper() %>%
  kableExtra::kable_styling(
    full_width = FALSE
  )

# table to help reviewing
kableExtra::save_kable(table1_review, file = fs::path(output_dir_tab, glue("tab1_baseline_description_redacted.html")))

# make table 2 - description of outcomes by vaccines -----------------------
fup_table <- cleaned_data %>%
  # select only baseline variables 
  dplyr::select(no_prev_vacc, 
                pt_start_date, 
                pt_end_date,
                t,
                lc_out,
                lc_dx_only,
                lc_cat,
                fracture,
                vaccine_schedule_detail,
                covid_hosp_cat, 
                covid_primary_cat, 
                test_positive_cat,
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

table2 <- fup_table %>% 
  tbl_summary(
    by = no_prev_vacc,
    statistic = list(
      all_continuous() ~ "{p50} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    label = list(
      pt_start_year = "Follow-up start (year)",
      sex = "Sex",
      t = "Follow-up time (years)",
      age = "Age",
      age_cat = "Age (categorised)",
      ethnicity = "Ethnicity",
      practice_nuts = "NHS region",
      imd_q5 = "Index of multiple deprivation (quintile)",
      comorbidities = "Comorbidities",
      care_home = "Resident in care home",
      highrisk_shield = "High risk shielding category",
      lowrisk_shield = "Low/moderate risk shielding category",
      lc_out = "Any Long COVID record", 
      lc_dx_only = "Long COVID diagnosis",
      lc_cat = "Long COVID record by prior status",
      fracture = "Hospital record of fracture", 
      covid_hosp_cat = "COVID-19 hospitalisations (n)",
      covid_primary_cat = "COVID-19 primary care record (n)", 
      test_positive_cat = "COVID-19 positive tests (n)",
      vaccine_schedule_detail = "Vaccination schedule received"
    )
  ) %>%
  bold_labels() %>%
  add_overall()

table2 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "tab2_full_description.html",
    path = fs::path(output_dir_tab)
  )

raw_stats <- table2$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats)

raw_stats_redacted <- raw_stats %>%
  mutate(
    n=roundmid_any(n, threshold),
    N=roundmid_any(N, threshold),
    p=round(100*n/N,1),
    N_miss = roundmid_any(N_miss, threshold),
    N_obs = roundmid_any(N_obs, threshold),
    p_miss = round(100*N_miss/N_obs,1),
    N_nonmiss = roundmid_any(N_nonmiss, threshold),
    p_nonmiss = round(100*N_nonmiss/N_obs,1),
    var_label = factor(var_label, levels=map_chr(var_labels_full[-c(1)], ~last(as.character(.)))),
    variable_levels = replace_na(as.character(variable_levels), ""),
    by = replace_na(as.character(by), "Overall")
  )
write_csv(raw_stats_redacted, here::here("output/table2_data.csv"))

table2_data <- raw_stats_redacted %>%
  rowwise() %>%
  transmute(
    var_label,
    # gt creates a column called `label` when run locally, `variable_labels` 
    # when run in opensafely (probs different versions)
    # label,
    variable_levels,
    by,
    value = glue(stat_display)
  ) %>% 
  pivot_wider(
    names_from = by,
    values_from = value
  )

table2_review <- table2_data %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_paper() %>%
  kableExtra::kable_styling(
    full_width = FALSE
  )

# table to help reviewing
kableExtra::save_kable(table2_review, file = fs::path(output_dir_tab, glue("tab2_full_descripion_redacted.html")))


# density plot of start and end date? -------------------------------------
cols <- hcl.colors(2, palette = "viridis")
colsmap <- c("Start date"=cols[1],
             "End date"=cols[2])

pdf(here("output/supplementary/histogram_start_end_dates.pdf"), width = 6, height = 4)
cleaned_data %>% 
  dplyr::select(pt_start_date, pt_end_date) %>% 
  ggplot() +
  geom_histogram(aes(x = pt_start_date, col = "Start date", fill = "Start date"), alpha = 0.4, bins = 50) +
  geom_histogram(aes(x = pt_end_date, col = "End date", fill = "End date"), alpha = 0.4, bins = 50) +
  scale_color_manual("Follow up", values = colsmap) +
  scale_fill_manual("Follow up", values = colsmap) +
  scale_x_date(limits = c(as.Date("2020-10-01"), as.Date("2023-04-30")), date_breaks = "1 year", date_labels = "%Y") + 
  labs(x = "Date", y = "Count") +
  theme_ali()
dev.off()


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
