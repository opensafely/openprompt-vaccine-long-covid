# Import Long COVID-first and summarise vaccine coverage

library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))

lc_first <- arrow::read_parquet(here("output/clean_dataset_lc_first.gz.parquet"))

output_dir_tab <- here("output/tables")
fs::dir_create(output_dir_tab)

vacc_range <- range(lc_first$no_total_vacc)
lc_first$over2_vacc = factor(lc_first$no_total_vacc, labels = c("0", "1", rep("2+", vacc_range[2] - vacc_range[1] - 2)))
lc_first_vacced <- lc_first %>% 
  group_by(over2_vacc, sex, age_cat, ethnicity) %>% 
  summarise(n = n(), .groups = "keep") %>% 
  group_by(sex, age_cat, ethnicity) %>% 
  mutate(group_n = sum(n)) %>% 
  ungroup() %>% 
  mutate(pct_vacc = round((n/group_n)*100, 2))

# TODO: redact these data 
write.csv(lc_first_vacced, here::here("output/lc_first_vaccination.csv"))

pdf(here("output/figures/fig6_vaccine_after_lc.pdf"), width = 8, height = 6)
ggplot(lc_first_vacced, aes(x = over2_vacc, y = pct_vacc, fill = sex, colour = sex)) + 
  geom_col(position = position_dodge(width = 0.2), alpha = 0.6) +
  coord_flip() +
  labs(y = "# vaccines received",
       x = "%",
       fill = "Sex",
       colour = "Sex"
       ) +
  facet_grid(age_cat~ethnicity) +
  theme_ali()
dev.off()
