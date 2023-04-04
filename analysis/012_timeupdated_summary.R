library(tidyverse)
library(lubridate)
library(here)
library(arrow)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet")) %>% 
  rename(lc_out = out)
print(table(time_data_lc_all$vaccines, time_data_lc_all$lc_out))

time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet")) %>% 
  rename(lc_dx = out)
print(table(time_data_lc_dx$vaccines, time_data_lc_dx$lc_dx))

## tabulated data ----
options(width=200) # set output width for capture.output

filenamebase <- "timeupdated_lc_all"
output_dir <- "output/data_properties"

dir.create(here(output_dir), showWarnings = FALSE, recursive=TRUE)

# delete file if it exists
if(file.exists(here(output_dir, paste0(filenamebase, "_tabulate", ".txt")))){
  file.remove(here(output_dir, paste0(filenamebase, "_tabulate", ".txt")))
}


### categorical and logical ----
sumtabs_cat <-
  time_data_lc_all %>%
  select(-ends_with("_id")) %>%
  select(where(is.character), where(is.logical), where(is.factor)) %>%
  map(redacted_summary_cat) %>%
  enframe()

capture.output(
  walk2(sumtabs_cat$value, sumtabs_cat$name, print_cat),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=FALSE
)


### numeric ----
sumtabs_num <-
  time_data_lc_all %>%
  select(-ends_with("_id")) %>%
  select(where(~ {!is.logical(.x) & is.numeric(.x) & !is.Date(.x)})) %>%
  map(redacted_summary_num) %>%
  enframe()

capture.output(
  walk2(sumtabs_num$value, sumtabs_num$name, print_num),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=TRUE
)

### dates ----

sumtabs_date <-
  time_data_lc_all %>%
  select(-ends_with("_id")) %>%
  select(where(is.Date)) %>%
  map(redacted_summary_date) %>%
  enframe()

capture.output(
  walk2(sumtabs_date$value, sumtabs_date$name, print_num),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=TRUE
)


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