library(tidyverse)
library(here)
library(data.table)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)
#dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

# import data ------------------------------------------------------------
dt_monthly <- readr::read_csv(here::here("output/data_monthly_dynamics.csv"))

cols <- c(
    "long COVID" = "red1",
    "long COVID Dx" = "red4",
    "Hospitalised" = "orange",
    "Last test positive" = "black")

pdf(here("output/figures/fig4a_outbreak_dynamics.pdf"), width = 8, height = 6)
ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = (inc_first_lc*100), color = "long COVID"), lwd = 1) +
  geom_line(aes(y = (inc_first_lc_dx*100), color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = (inc_hospitalised*100), color = "Hospitalised"), lwd = 1) +
  geom_line(aes(y = (inc_tested*100), color = "Last test positive"), lwd = 1) +
  labs(x = "Month", y = "Incidence (%)", 
       title = "Monthly Incidence of Events") +
  scale_color_manual(values = cols) +
  theme_ali()
dev.off()


pdf(here("output/figures/fig4b_outbreak_dynamics_cumulative.pdf"), width = 8, height = 6)
ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = cum_inc_first_lc*100, color = "long COVID"), lwd = 1) +
  geom_line(aes(y = cum_inc_first_lc_dx*100, color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = cum_inc_hospitalised*100, color = "Hospitalised"), lwd = 1) +
  geom_line(aes(y = cum_inc_tested*100, color = "Last test positive"), lwd = 1) +
  labs(x = "Month", y = "Cumulative incidence", 
       title = "Monthly Cumulative Incidence of Events") +
  scale_color_manual(values = cols) +
  theme_ali()
dev.off()

pdf(here("output/figures/fig4c_outbreak_dynamics_experimental.pdf"), width = 8, height = 6)
ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = cum_inc_first_lc*100, color = "long COVID"), lwd = 1) +
  geom_line(aes(y = cum_inc_first_lc_dx*100, color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = inc_tested*100, color = "Last test positive"), lwd = 1) +
  labs(x = "Month", y = "Cumulative incidence", 
       title = "Monthly Cumulative Incidence of Events") +
  scale_color_manual(values = cols) +
  theme_ali()
dev.off()
