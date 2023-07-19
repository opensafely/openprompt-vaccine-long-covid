# Import Long COVID-first and summarise vaccine coverage

library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

# create folders to put plots in
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)
output_dir_tab <- here("output/tables")
fs::dir_create(output_dir_tab)


# import data -------------------------------------------------------------
lc_first <- arrow::read_parquet(here("output/clean_dataset_lc_first.gz.parquet"))
clean_data_non_long_covid <- arrow::read_parquet(here("output/clean_dataset.gz.parquet")) %>% 
  filter(lc_out == 0)

# combine lc --> vaccine, and no_lc --> vaccine ---------------------------
vaccine_data <- lc_first %>% 
  dplyr::select(patient_id, no_total_vacc, first_lc, starts_with("pt_"), sex, age_cat) %>% 
  mutate(
    # make the lc date the start of obs. period
    pt_start_date = first_lc,
    no_prev_vacc = cut(
      no_total_vacc,
      breaks = c(-Inf, 0:2, Inf),
      labels = c(as.character(0:2), "3+")
    ), 
    lc_out = 1
  ) %>%  
  bind_rows(
    dplyr::select(clean_data_non_long_covid, patient_id, starts_with("pt_"), no_prev_vacc, sex, age_cat, lc_out)
  ) %>% 
  mutate(
    pyrs = (pt_start_date %--% pt_end_date) / dyears(1)
  )

# regroup vaccines --------------------------------------------------------
lc_first_vacced <- vaccine_data %>% 
  mutate(lc_out = factor(lc_out)) %>% 
  group_by(no_prev_vacc, lc_out, age_cat) %>% 
  summarise(n = n(), pyrs = sum(pyrs), .groups = "keep") %>% 
  ungroup() %>% 
  mutate(
    n = redact_and_round(n, redact_threshold = 10),
    vacc_rate = round((n*1e5)/pyrs, 2)
    )

write.csv(lc_first_vacced, here::here("output/lc_first_vaccination.csv"))

pdf(here("output/figures/fig6_vaccine_after_lc.pdf"), width = 6, height = 6)
ggplot(lc_first_vacced, aes(x = no_prev_vacc, y = vacc_rate, fill = lc_out, colour = lc_out)) + 
  geom_col(position = position_dodge(width = 0.2), alpha = 0.6) +
  coord_flip() +
  labs(x = "# vaccines received",
       y = "Vaccination rate (per 100,000 person-years",
       fill = "long COVID",
       colour = "long COVID"
       ) +
  facet_grid(age_cat ~ " ") +
  theme_ali()
dev.off()
