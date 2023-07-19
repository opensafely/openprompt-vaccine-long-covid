library(tidyverse)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

# create folders to put plots in
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)

redact_threshold <- 10

dt_daily <- readr::read_csv(here::here("output/data_daily_dynamics.csv"))

# define colours ----------------------------------------------------------
cols <- c(
  "long COVID" = "red1",
  "long COVID Dx" = "dodgerblue1",
  "Hospitalised" = "orange",
  "Last test positive" = "black",
  "1st vaccine dose" = "blueviolet",
  "long COVID hospital" = "forestgreen")

lc_dx_cols <- c("coral", "aquamarine2")


# censor daily_dynamics ---------------------------------------------------
dt_daily$n_first_lc <- redact_and_round(dt_daily$n_first_lc, redact_threshold = 10)
dt_daily$n_first_lc_dx <- redact_and_round(dt_daily$n_first_lc_dx, redact_threshold = 10)

# daily dynamics  ---------------------------------------------------------
pd <- position_dodge(width = 0.5)
p4f1 <- ggplot(dt_daily, aes(x = date)) + 
  geom_col(aes(y = n_first_lc, fill = "long COVID"), lwd = 1, position = pd) +
  geom_col(aes(y = n_first_lc_dx, fill = "long COVID Dx"), lwd = 1, position = pd) +
  labs(x = "Month", y = "# cases", 
       fill = "COVID-19 outcome") +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_manual(values = cols[1:2]) +
  theme_ali() +
  theme(legend.position = "bottom",
        legend.box="vertical", 
        legend.margin=margin())

pdf(here("output/figures/fig2g_daily_cases.pdf"), width = 8, height = 6)
  p4f1
dev.off()

write.csv(dt_daily, here::here("output/data_daily_dynamics_plot.csv"))
