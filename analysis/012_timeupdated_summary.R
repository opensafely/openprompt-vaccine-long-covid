library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/summarise_data.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet")) %>% 
  rename(lc_out = out)
print(table(time_data_lc_all$vaccines, time_data_lc_all$lc_out))

time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet")) %>% 
  rename(lc_dx = out)
print(table(time_data_lc_dx$vaccines, time_data_lc_dx$lc_dx))

# summarise data ----------------------------------------------------------
summarise_data(data_in = time_data_lc_all, filenamebase = "timeupdated_lc_all")

# describe time updated data - lc dx only outcome  ------------------------
pdf(here("output/supplementary/time_updated_t_byvaccines.pdf"), width = 8, height = 6)
ggplot(time_data_lc_all, aes(x = t, group = vaccines, colour = vaccines)) +
  geom_density() + 
  facet_wrap(~lc_out) + 
  theme_ali()
dev.off()

pdf(here("output/supplementary/time_updated_t_byvaccines_lc_dx.pdf"), width = 8, height = 6)
ggplot(time_data_lc_dx, aes(x = t, group = vaccines, colour = vaccines)) +
  geom_density() + 
  facet_wrap(~lc_dx) + 
  theme_ali()
dev.off()
