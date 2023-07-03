library(tidyverse)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

# create folders to put plots in
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)

redact_threshold <- 10

dt_monthly <- readr::read_csv(here::here("output/data_monthly_dynamics.csv"))
uk_gov_cases <- readr::read_csv(here::here("data/data_2023-Apr-27.csv"))

# define colours ----------------------------------------------------------
cols <- c(
  "long COVID" = "red1",
  "long COVID Dx" = "dodgerblue1",
  "Hospitalised" = "orange",
  "Last test positive" = "black",
  "1st vaccine dose" = "blueviolet",
  "long COVID hospital" = "forestgreen")

lc_dx_cols <- c("coral", "aquamarine2")

# Monthly dynamics plots --------------------------------------------------
p4a <- ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = (inc_first_lc*100), color = "long COVID"), lwd = 1) +
  geom_line(aes(y = (inc_first_lc_dx*100), color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = (inc_hospitalised*100), color = "Hospitalised"), lwd = 1) +
  labs(x = "Month", y = "Incidence (%)", 
       colour = "COVID-19 outcome") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE)) +
  scale_color_manual(values = cols) +
  theme_ali() +
  theme(legend.position = "bottom",
        legend.box="vertical", 
        legend.margin=margin())

pdf(here("output/figures/fig4a_outbreak_dynamics.pdf"), width = 8, height = 6)
p4a
dev.off()

# cumulative incidence curves
p4b <- ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = cum_inc_first_lc*100, color = "long COVID"), lwd = 1) +
  geom_line(aes(y = cum_inc_first_lc_dx*100, color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = cum_inc_hospitalised*100, color = "Hospitalised"), lwd = 1) +
  labs(x = "Month", y = "Cumulative incidence (%)", colour = "COVID-19 outcome") +
  scale_color_manual(values = cols) +
  theme_ali() + 
  theme(legend.position = "bottom")

pdf(here("output/figures/fig4b_outbreak_dynamics_cumulative.pdf"), width = 8, height = 6)
p4b
dev.off()

# mix of cumulative and monthly incidence curves
p4c <- ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = cum_inc_first_lc*100, color = "long COVID"), lwd = 1) +
  geom_line(aes(y = cum_inc_first_lc_dx*100, color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = inc_vacc*100, color = "1st vaccine dose"), lwd = 1) +
  geom_line(aes(y = inc_tested*100, color = "Last test positive"), lwd = 1) +
  labs(x = "Month", y = "Incidence (%)", 
       colour = "COVID-19 outcome") +
  scale_color_manual(values = cols) +
  theme_ali() +
  theme(legend.position = "bottom")

pdf(here("output/figures/fig4c_outbreak_dynamics_experimental.pdf"), width = 8, height = 6)
p4c
dev.off()

# on log scale
p4d <- ggplot(dt_monthly, aes(x = month_start_date)) +
  geom_line(aes(y = log2(inc_first_lc), color = "long COVID"), lwd = 1) +
  geom_line(aes(y = log2(inc_first_lc_dx), color = "long COVID Dx"), lwd = 1) +
  geom_line(aes(y = log2(inc_tested), color = "Last test positive"), lwd = 1) +
  labs(x = "Month", y = "log2(events)", colour = "COVID-19 outcome") +
  scale_color_manual(values = cols) +
  theme_ali() + 
  theme(legend.position = "bottom")

pdf(here("output/figures/fig4d_outbreak_dynamics_log.pdf"), width = 8, height = 6)
p4d +
  geom_line(aes(y = log2(inc_hospitalised), color = "Hospitalised"), lwd = 1) +
  geom_line(aes(y = log2(inc_vacc), color = "1st vaccine dose"), lwd = 1)
dev.off()

# compare long COVID dynamics to England data -----------------------------
plot_uk_gov_cases <- uk_gov_cases %>% 
  filter(date >= min(dt_monthly$month_start_date) & date <= max(dt_monthly$month_start_date)) %>% 
  mutate(roll_mean = zoo::rollmean(newCasesBySpecimenDate, 7, na.pad = T))

## make the plot using base R for dual-axes
pdf(here("output/figures/fig4e_longcovid_and_national_cases.pdf"), width = 8, height = 6)
par(mar=c(4,4,1,4))
plot(
  dt_monthly$month_start_date,
  dt_monthly$n_first_lc,
  type = "l",
  col = cols[1],
  ylim = c(0, max(dt_monthly$n_first_lc)*1.1),
  ylab = "# cases",
  xlab = "Date")
lines(dt_monthly$month_start_date,
      dt_monthly$n_first_lc_dx,
      type = "l",
      col = cols[2])
par(new=T)
plot(plot_uk_gov_cases$date, plot_uk_gov_cases$roll_mean, type = "l", col = 1, lwd = 2,
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(side = 4)
mtext("Positive cases in England (7-day average)", side = 4, padj = 4)
legend("topleft", legend = c("long COVID", "long COVID Dx only", "Positive COVID-19 tests (Eng)"), lty = 1, col = c(cols[c(1:2)], 1))
dev.off()