library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/poisson_regressions.R"))

# import data ------------------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))
time_data <- arrow::read_parquet(here::here("output/timeupdate_dataset.gz.parquet"))

# add simple poisson regression results -----------------------------------
stratifiers <- c("vaccines","t_mixmatch")
strat_plot_names <- c("No. vaccine doses", "Vaccines received (first 2)")
adjusted_rates <- map(stratifiers, ~poisson_regressions(cohort_data = time_data, .x))

adjusted_rates_out <- NULL
for(i in 1:length(stratifiers)){
  adjusted_rates_out <- bind_rows(
    adjusted_rates_out, 
    adjusted_rates[[i]]
  )
}
adjusted_rates_out <- adjusted_rates_out %>% 
  mutate(strat_var = factor(
    var, 
    levels = stratifiers, 
    labels = strat_plot_names))

adjusted_rates_out %>% 
  write_csv(here("output/tab023_poissonrates.csv"))

# plot adjusted rates -----------------------------------------------------
pdf(here("output/fig3_rate_ratios.pdf"), width = 8, height = 6)
pd = position_dodge(1)
adjusted_rates_out %>% 
  filter(plot_marker) %>% 
  ggplot(aes(x=term, y = rate, ymin = conf.low, ymax = conf.high, colour = strat_var, lty = model)) +
    geom_point(size = 1.5, pch = 16, position = pd) +
    geom_linerange(lwd = 1, alpha = 0.8, position = pd) +
    geom_hline(yintercept = 1, lty = 2) +  # add a dotted line at x=1 after flip
    coord_flip() +
    facet_wrap(~strat_var, scales = "free", ncol = 2) + 
    labs(y = "Incidence Rate Ratio (95% CI)", x = "") +
    guides(colour = "none") + 
    theme_bw() +
    theme(legend.position = "top",
          strip.background = element_blank())
dev.off()
