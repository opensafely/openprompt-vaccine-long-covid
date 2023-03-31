library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(here)
library(data.table)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

# import data ------------------------------------------------------------
time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet")) %>% 
  rename(lc_out = out)
time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet")) %>% 
  rename(lc_dx = out)

# plot cumulative incidence curves ----------------------------------------
vaccgroups <- levels(time_data_lc_all$vaccines)
n_groups <- length(vaccgroups)
colours <- hcl.colors(n_groups, palette = "viridis")
colours_dx <- colours #hcl.colors(n_groups, palette = "zissou1")
fit <- survival::survfit(Surv(t, lc_out) ~ as.factor(vaccines), data=time_data_lc_all)
fit_dx <- survival::survfit(Surv(t, lc_dx) ~ as.factor(vaccines), data=time_data_lc_dx)

pdf(here("output/figures/fig1_cumulative_incidence.pdf"), width = 8, height = 6)
par(mfrow = c(1,2))
  plot(fit, conf.int = FALSE, col = colours, lwd = 2, fun = "cumhaz", xlab = "Years", ylab = "Cumulative Hazard", main = "All Long COVID records")
  legend("topleft", legend = vaccgroups, col = colours, lty = 1, lwd = 1.5)
  plot(fit_dx, conf.int = FALSE, col = colours_dx, lwd = 2, fun = "cumhaz", xlab = "Years", ylab = "Cumulative Hazard", main = "Long COVID diagnoses only")
  legend("topleft", legend = vaccgroups, col = colours_dx, lty = 1, lwd = 1.5)
dev.off()

# plot time of Long COVID diagnosis ---------------------------------------
summarise_records_by_month <- function(timedata_in, outcome_var){
  timedata_in %>% 
    rename(outcome_binary = {{outcome_var}},
           outcome_date = outcome) %>% 
    ungroup() %>% 
    filter(sex %in% c("male", "female")) %>% 
    dplyr::select(patient_id, age_cat, sex, vaccines, outcome_date, outcome_binary) %>% 
    mutate(month = month(outcome_date),
           year = year(outcome_date)) %>% 
    group_by(year, month, sex, vaccines) %>% 
    summarise(sum_out = sum(outcome_binary, na.rm = TRUE), .groups = "keep") %>% 
    mutate(day = 1,
           date = make_date(year, month, day))
}
timeseries_lc_all <- summarise_records_by_month(time_data_lc_all, lc_out) %>% 
  mutate(outcome = "All Long COVID records")
timeseries_lc_dx <- summarise_records_by_month(time_data_lc_dx, lc_dx) %>% 
  mutate(outcome = "Long COVID diagnoses only")
timeseries_plot <- bind_rows(timeseries_lc_all,
                             timeseries_lc_dx)
timeseries_lc_all <- NULL
timeseries_lc_dx <- NULL

timeseries_plot$redacted_out <- redactor2(timeseries_plot$sum_out, threshold = 10)

pdf(here("output/figures/fig2_raw_counts_line.pdf"), width = 8, height = 6)
timeseries_plot %>% 
  group_by(date, sex, outcome) %>% 
  summarise(redacted_out = sum(redacted_out), .groups = "keep") %>% 
  ggplot(aes(x = date, y = redacted_out)) +
    geom_line(lwd = 0.2, lty = 1, col = "gray20") +
    facet_grid(sex~outcome) +
    labs(x = "Date", y = "Count of Long COVID codes", caption = "Intersex individuals omitted due to small numbers") +
    theme_ali() + 
    theme(strip.background = element_blank())
dev.off()

pdf(here("output/figures/fig2b_raw_counts_column.pdf"), width = 8, height = 6)
timeseries_plot %>% 
  ggplot(aes(x = date, y = redacted_out, fill = vaccines)) +
    geom_col(lwd = 0.2, lty = 1, col = "gray20") +
    scale_fill_manual(values = colours) +
    facet_grid(sex~outcome) + 
    labs(x = "Date", y = "Count of Long COVID codes", caption = "Intersex individuals omitted due to small numbers") +
    theme_ali() + 
    theme(strip.background = element_blank())
dev.off()

# time gaps between vaccine doses  ----------------------------------------
vaccine_gaps <- time_data_lc_all %>% 
  mutate(t_gap = tstop-tstart) %>% 
  filter(sex %in% c("male", "female"), vaccines != "0")

avg_vaccine_gaps <- vaccine_gaps %>% 
  drop_na() %>% 
  group_by(vaccines) %>% 
  summarise(avg_gap = mean(t_gap), n = n(), .groups = "keep")

n_groups <- length(unique(vaccine_gaps$sex))
colours <- hcl.colors(n_groups)

pdf(here("output/supplementary/fig_agegap_vaccines.pdf"), width = 8, height = 6)
ggplot(drop_na(vaccine_gaps), aes(x = t_gap, colour = sex, fill = sex)) +
  geom_density(alpha = 0.7) +
  geom_vline(data = avg_vaccine_gaps, aes(xintercept = avg_gap), lty = 2, col = "gray50") +
  facet_wrap(~vaccines, ncol = 1) + 
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  theme_ali() +
  labs(x = "Avg. difference between doses") +
  theme(strip.background = element_blank())
dev.off()

