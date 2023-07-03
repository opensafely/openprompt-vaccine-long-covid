library(tidyverse)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

# create folders to put plots in
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)

redact_threshold <- 10

dt_daily <- readr::read_csv(here::here("output/data_daily_dynamics.csv"))

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

pdf(here("output/figures/fig4f_daily_cases.pdf"), width = 8, height = 6)
  p4f1
dev.off()

write.csv(dt_daily, here::here("output/data_daily_dynamics_plot.csv"))
