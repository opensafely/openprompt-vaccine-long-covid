library(tidyverse)
library(here)
library(data.table)
library(zoo)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)
#dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

# import data ------------------------------------------------------------
dt_monthly <- readr::read_csv(here::here("output/data_monthly_dynamics.csv"))
uk_gov_cases <- readr::read_csv(here::here("data/data_2023-Apr-27.csv"))

cols <- c(
    "long COVID" = "red1",
    "long COVID Dx" = "red4",
    "Hospitalised" = "orange",
    "Last test positive" = "black",
    "Vaccinated" = "darkblue")

pdf(here("output/figures/fig4a_outbreak_dynamics.pdf"), width = 8, height = 6)
ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = (inc_first_lc*100), color = "long COVID"), lwd = 1) +
  geom_line(aes(y = (inc_first_lc_dx*100), color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = (inc_hospitalised*100), color = "Hospitalised"), lwd = 1) +
  #geom_line(aes(y = (inc_tested*100), color = "Last test positive"), lwd = 1) +
  geom_line(aes(y = (inc_vacc*100), color = "Vaccinated"), lwd = 1) +
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
  #geom_line(aes(y = cum_inc_tested*100, color = "Last test positive"), lwd = 1) +
  geom_line(aes(y = cum_inc_vacc*100, color = "Vaccinated"), lwd = 1) +
  labs(x = "Month", y = "Cumulative incidence", 
       title = "Monthly Cumulative Incidence of Events") +
  scale_color_manual(values = cols) +
  theme_ali()
dev.off()

pdf(here("output/figures/fig4c_outbreak_dynamics_experimental.pdf"), width = 8, height = 6)
ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = cum_inc_first_lc*100, color = "long COVID"), lwd = 1) +
  geom_line(aes(y = cum_inc_first_lc_dx*100, color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = cum_inc_vacc*100, color = "Vaccinated"), lwd = 1) +
  geom_line(aes(y = inc_tested*100, color = "Last test positive"), lwd = 1) +
  labs(x = "Month", y = "Cumulative incidence", 
       title = "Monthly Cumulative Incidence of Events") +
  scale_color_manual(values = cols) +
  theme_ali()
dev.off()

pdf(here("output/figures/fig4d_outbreak_dynamics_log.pdf"), width = 8, height = 6)
ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = log2(inc_first_lc), color = "long COVID"), lwd = 1) +
  geom_line(aes(y = log2(inc_first_lc_dx), color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = log2(inc_hospitalised), color = "Hospitalised"), lwd = 1) +
  geom_line(aes(y = log2(inc_tested), color = "Last test positive"), lwd = 1) +
  geom_line(aes(y = log2(inc_vacc), color = "Vaccinated"), lwd = 1) +
  labs(x = "Month", y = "log(events)") +
  scale_color_manual(values = cols) +
  theme_ali()
dev.off()

dt_monthly_long <- dt_monthly %>% 
  pivot_longer(cols = c(inc_first_lc, inc_first_lc_dx, inc_tested))

# compare long COVID dynamics to England data -----------------------------
plot_uk_gov_cases <- uk_gov_cases %>% 
  filter(date >= min(dt_monthly$month_start_date) & date <= max(dt_monthly$month_start_date)) %>% 
  mutate(roll_mean = rollmean(newCasesBySpecimenDate, 7, na.pad = T))

pdf(here("output/figures/fig4e_longcovid_and_national_cases.pdf"), width = 8, height = 6)
par(mar=c(4,4,1,4))
plot(
  dt_monthly$month_start_date,
  dt_monthly$cum_inc_first_lc*100,
  type = "l",
  col = cols[1],
  ylim = c(0, 100),
  ylab = "Cumulative incidence (%)",
  xlab = "Date")
lines(dt_monthly$month_start_date,
      dt_monthly$cum_inc_first_lc_dx*100,
      type = "l",
      col = cols[2])
par(new=T)
plot(plot_uk_gov_cases$date, plot_uk_gov_cases$roll_mean, type = "l", col = 1, lwd = 2,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(side = 4)
mtext("Positive cases in England (7-day average)", side = 4, padj = 4)
dev.off()
