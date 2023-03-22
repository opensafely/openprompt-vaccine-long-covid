#library(targets)
library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(here)

source(here::here("analysis/functions/import_data.R"))
source(here::here("analysis/functions/clean_vaccine_data.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))

cases_data_path <- here("output/dataset_cases.csv.gz")
controls_data_path <- here("output/dataset_controls.csv.gz")

# import, combine, clean data -------------------------------------------------
imported_data <-
  import_and_combine(cases_path = cases_data_path, controls_path = controls_data_path)

cleaned_data <- tidy_vaccine_data(imported_data)

# time update on vaccine date ---------------------------------------------
time_data <- time_update_vaccinedoses(cleaned_data)

# plot cumulative incidence curves ----------------------------------------
vaccgroups <- levels(time_data$vaccines)
n_groups <- length(vaccgroups)
colours <- topo.colors(n_groups)
fit <- survival::survfit(Surv(t, lc_out) ~ as.factor(vaccines), data=time_data)

pdf(here("output/fig1_cumulative_incidence.pdf"), width = 8, height = 6)
  plot(fit, conf.int = FALSE, col = colours, lwd = 2, fun = "cumhaz", xlab = "Years", ylab = "Cumulative Hazard")
  legend("topleft", legend = vaccgroups, col = colours, lty = 1, lwd = 1.5)
dev.off()

# plot time of Long COVID diagnosis ---------------------------------------
timeplot <- time_data %>% 
  ungroup() %>% 
  filter(sex %in% c("male", "female")) %>% 
  dplyr::select(patient_id, pt_start_date, pt_end_date, sex, vaccines, lc_out, first_lc_dx) %>% 
  mutate(month = month(first_lc_dx),
         year = year(first_lc_dx)) %>% 
  group_by(year, month, sex, vaccines) %>% 
  summarise(total_lc = sum(lc_out, na.rm = TRUE)) %>% 
  mutate(day = 1,
         date = make_date(year, month, day))

pdf(here("output/fig2_raw_counts_bysex.pdf"), width = 8, height = 6)
  ggplot(timeplot, aes(x = date, y = total_lc, fill = vaccines)) +
    geom_col(lwd = 0.2, lty = 1, col = "gray20") +
    scale_fill_manual(values = colours) +
    facet_wrap(~sex, ncol = 1) + 
    labs(x = "Date", y = "Count of Long COVID codes", caption = "Intersex individuals omitted due to small numbers") +
    theme_bw() + 
    theme(strip.background = element_blank())
dev.off()

