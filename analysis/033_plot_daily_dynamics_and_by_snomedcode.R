library(tidyverse)
library(here)
library(ggpubr)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

# create folders to put plots in
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here("output/tables"), showWarnings = FALSE, recursive=TRUE)
dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

redact_threshold <- 10

dt_daily <- readr::read_csv(here::here("output/data_daily_dynamics.csv"))

clean_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet")) %>% 
  dplyr::select(patient_id, first_lc, first_lc_dx, pt_start_date, pt_end_date, sex, age, region = practice_nuts, first_lc_code, lc_dx_flag)

longcovid_combined_codelist <- bind_rows(
  readr::read_csv(here("codelists/opensafely-nice-managing-the-long-term-effects-of-covid-19.csv")),
  readr::read_csv(here("codelists/opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv")),
  readr::read_csv(here("codelists/opensafely-referral-and-signposting-for-long-covid.csv"))
)

# define colours ----------------------------------------------------------
cols <- c(
  "long COVID" = "red1",
  "long COVID Dx" = "dodgerblue1",
  "Hospitalised" = "orange",
  "Last test positive" = "black",
  "1st vaccine dose" = "blueviolet",
  "long COVID hospital" = "forestgreen")

lc_dx_cols <- c("coral", "aquamarine2")


# censor daily_dynamics ---------------------------------------------------
dt_daily$n_first_lc <- redact_and_round(dt_daily$n_first_lc, redact_threshold)
dt_daily$n_first_lc_dx <- redact_and_round(dt_daily$n_first_lc_dx, redact_threshold)

# daily dynamics  ---------------------------------------------------------
pd <- position_dodge(width = 0.5)
p4f1 <- ggplot(dt_daily, aes(x = date)) + 
  geom_col(aes(y = n_first_lc, fill = "long COVID"), lwd = 1, position = pd) +
  geom_col(aes(y = n_first_lc_dx, fill = "long COVID Dx"), lwd = 1, position = pd) +
  labs(x = "Month", y = "# cases", 
       fill = "COVID-19 outcome") +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_manual(values = cols[1:2]) +
  theme_ali() +
  theme(legend.position = "bottom",
        legend.box="vertical", 
        legend.margin=margin())

pdf(here("output/figures/fig2g_daily_cases.pdf"), width = 8, height = 6)
  p4f1
dev.off()

write.csv(dt_daily, here::here("output/data_daily_dynamics_plot.csv"))

# plot snomed codes over time ---------------------------------------------
## Got too many codes so the counts will be small. Refactor the 
# codes based on the counts in the above table
clean_data_snomedcode <- clean_data %>% 
  left_join(longcovid_combined_codelist, by = c("first_lc_code" = "code"))

snomedcode_table <- clean_data_snomedcode %>% 
  filter(!is.na(term)) %>% 
  group_by(first_lc_code, term, lc_dx_flag) %>% 
  tally() %>% 
  arrange(-n) %>% 
  mutate(n = redact_and_round(n, redact_threshold))

write.csv(snomedcode_table, here::here("output/tables/tab5_snomedcode_count.csv"))

# keep the top 3 codes
x <- snomedcode_table$term
keep_codes <- x[1:3]

# repeat "Other" for the remaining categories
other_codes <- rep("Other", length(x)-length(keep_codes))

# create factor variable with new grouping
clean_data_snomedcode$term_fct <- factor(clean_data_snomedcode$term, 
                                         levels = snomedcode_table$term, 
                                         labels = str_wrap(c(keep_codes, other_codes), width = 30))

summarise_records_by_month <- function(timedata_in, outcome_date){
  timedata_in %>% 
    # create a binary and date variable for the outcome
    rename(outcome_date = {{outcome_date}}) %>% 
    mutate(outcome_binary = 1) %>% 
    ungroup() %>% 
    # select the important variables
    dplyr::select(patient_id, first_lc_code, term_fct, outcome_date, outcome_binary) %>% 
    # get the week number and year from the date
    mutate(week = lubridate::week(outcome_date),
           year = lubridate::year(outcome_date)) %>% 
    # group and summarise weekly counts
    group_by(year, week, term_fct) %>% 
    summarise(sum_out = sum(outcome_binary, na.rm = TRUE), .groups = "keep") %>% 
    # make a fake date variable for plotting purposes
    mutate(date = lubridate::make_date(year, 1, 1) + (week*7))
}
snomed_over_time <- summarise_records_by_month(filter(clean_data_snomedcode, !is.na(first_lc)), "first_lc")

snomed_over_time$sum_out <- redact_and_round(snomed_over_time$sum_out, redact_threshold)

write.csv(snomed_over_time, here::here("output/raw_counts_by_code_over_time.csv"))

if(max(snomed_over_time$sum_out, na.rm = T) > 0) {
  p1 <- ggplot(snomed_over_time, aes(x = date, y = sum_out, col = term_fct, fill = term_fct)) + 
    geom_col() +
    guides(
      colour=guide_legend(nrow=3,byrow=TRUE),
      fill=guide_legend(nrow=3,byrow=TRUE)
    ) +
    facet_wrap(~term_fct, ncol = 2) +
    labs(y = "Weekly Long COVID code count",
         x = "Date", 
         colour = "SNOMED code",
         fill = "SNOMED code") +
    theme_ali() +
    theme(legend.position = "bottom",
          legend.box="vertical", 
          legend.text = element_text(size = 8),
          legend.margin=margin(),
          strip.text = element_blank())
}else{
  p1 <- ggplot() + theme_void()
}

pdf(here::here("output/figures/fig2f_raw_counts_by_code.pdf"), width = 8, height = 6)
  p1
dev.off()

# master plot!!! ----------------------------------------------------------
p_master <- ggarrange(
  p4f1,
  p1,
  ncol = 1,
  nrow = 2,
  common.legend = F,
  legend = "bottom",
  heights = c(0.3, 0.7),
  labels = "AUTO"
)

pdf(here::here("output/figures/fig2h_daily_dynamics_and_by_snomed.pdf"), width = 10, height = 8)
  p_master
dev.off()

# "Your COVID recovery" by region -----------------------------------------
three_spike_dates <- c("2022-01-27", "2021-11-29", "2021-07-21") %>% as.Date()
clean_data_snomedcode %>% 
  filter(first_lc %in% three_spike_dates & str_detect(term, "Your COVID Recovery")) %>% 
  group_by(first_lc, region) %>% 
  summarise(n = n()) %>% 
  mutate(n = redact_and_round(n, redact_threshold)) %>%
  arrange(first_lc, -n, region) %>% 
  write_csv(here::here("output/supplementary/yourcovidrecovery.csv"))
