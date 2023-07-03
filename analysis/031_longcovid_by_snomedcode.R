library(tidyverse)
library(lubridate)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

# create folders to put plots in
dir.create(here("output/tables"), showWarnings = FALSE, recursive=TRUE)
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)

redact_threshold <- 10

# import data ------------------------------------------------------------
clean_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet")) %>% 
  dplyr::select(patient_id, first_lc, first_lc_dx, pt_start_date, pt_end_date, sex, age, first_lc_code, lc_dx_flag)

longcovid_combined_codelist <- bind_rows(
  readr::read_csv(here("codelists/opensafely-nice-managing-the-long-term-effects-of-covid-19.csv")),
  readr::read_csv(here("codelists/opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv")),
  readr::read_csv(here("codelists/opensafely-referral-and-signposting-for-long-covid.csv"))
)

clean_data_snomedcode <- clean_data %>% 
  left_join(longcovid_combined_codelist, by = c("first_lc_code" = "code"))

snomedcode_table <- clean_data_snomedcode %>% 
  filter(!is.na(term)) %>% 
  group_by(first_lc_code, term, lc_dx_flag) %>% 
  tally() %>% 
  arrange(-n)

write.csv(snomedcode_table, here::here("output/tables/tab5_snomedcode_count.csv"))


# plot snomed codes over time ---------------------------------------------

## Got too many codes so the counts will be small. Refactor the 
# codes beased on the counts in the above table

# keep the top 5 codes
x <- snomedcode_table$term
keep_codes <- x[1:5]

# repeat "Other" for the remaining categories
other_codes <- rep("Other", length(x)-length(keep_codes))

# create factor variable with new grouping
clean_data_snomedcode$term_fct <- factor(clean_data_snomedcode$term, 
                                         levels = snomedcode_table$term, 
                                         labels = c(keep_codes, other_codes))

summarise_records_by_month <- function(timedata_in, outcome_date){
  timedata_in %>% 
    # create a binayr and date variable for the outcome
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

write.csv(snomed_over_time, here::here("output/raw_counts_by_code_over_time.csv"))
# TODO: add in redaction here

p1 <- ggplot(snomed_over_time, aes(x = date, y = sum_out, col = term_fct)) + 
  geom_point(pch = 1) +
  geom_line(alpha = 0.5) +
  guides(colour=guide_legend(nrow=3,byrow=TRUE)) +
  labs(y = "Weekly Long COVID code count",
       x = "Date", 
       colour = "Code") +
  theme_ali() +
  theme(legend.position = "bottom",
        legend.box="vertical", 
        legend.text = element_text(size = 8),
        legend.margin=margin())

pdf(here::here("output/figures/fig2f_raw_counts_by_code.pdf"), width = 12, height = 6)
  p1
dev.off()
