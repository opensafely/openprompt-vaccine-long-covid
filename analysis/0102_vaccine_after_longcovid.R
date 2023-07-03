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
  dplyr::select(patient_id, no_total_vacc, sex, age_cat, ethnicity) %>% 
  mutate(no_prev_vacc = cut(
    no_total_vacc,
    breaks = c(-Inf, 0:2, Inf),
    labels = c(as.character(0:2), "3+")
    ),
    lc_out = 1
  ) %>%  
  bind_rows(
    dplyr::select(clean_data_non_long_covid, patient_id, no_prev_vacc, sex, age_cat, ethnicity, lc_out)
  )

# regroup vaccines --------------------------------------------------------
lc_first_vacced <- vaccine_data %>% 
  mutate(lc_out = factor(lc_out)) %>% 
  group_by(no_prev_vacc, lc_out, age_cat, ethnicity) %>% 
  summarise(n = n(), .groups = "keep") %>% 
  group_by(lc_out, age_cat, ethnicity) %>% 
  mutate(group_n = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct_vacc = round((n/group_n)*100, 2))

# TODO: redact these data 
write.csv(lc_first_vacced, here::here("output/lc_first_vaccination.csv"))

pdf(here("output/figures/fig6_vaccine_after_lc.pdf"), width = 8, height = 6)
ggplot(lc_first_vacced, aes(x = no_prev_vacc, y = pct_vacc, fill = lc_out, colour = lc_out)) + 
  geom_col(position = position_dodge(width = 0.2), alpha = 0.6) +
  coord_flip() +
  labs(y = "# vaccines received",
       x = "%",
       fill = "long COVID",
       colour = "long COVID"
       ) +
  facet_grid(age_cat~ethnicity) +
  theme_ali()
dev.off()
