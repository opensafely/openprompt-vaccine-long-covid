library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(here)

cases_data_path <- here("output/dataset_cases.csv.gz")
controls_data_path <- here("output/dataset_controls.csv.gz")

# import data ------------------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))
time_data <- arrow::read_parquet(here::here("output/timeupdate_dataset.gz.parquet"))

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

