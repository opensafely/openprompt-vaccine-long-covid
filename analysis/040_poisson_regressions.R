#library(targets)
library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/import_data.R"))
source(here::here("analysis/functions/clean_vaccine_data.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))
source(here::here("analysis/functions/poisson_regressions.R"))

cases_data_path <- here("output/dataset_cases.csv.gz")
controls_data_path <- here("output/dataset_controls.csv.gz")

# import, combine, clean data -------------------------------------------------
imported_data <-
  import_and_combine(cases_path = cases_data_path, controls_path = controls_data_path)

cleaned_data <- tidy_vaccine_data(imported_data)

# time update on vaccine date ---------------------------------------------
time_data <- time_update_vaccinedoses(cleaned_data)

# add simple poisson regression results -----------------------------------
stratifiers <- c("vaccines","t_mixmatch","practice_nuts","imd_q5","ethnicity")
strat_plot_names <- c("No. vaccine doses", "Vaccines received (first 2)", "Region", "IMD", "Ethnicity")
adjusted_rates <- map(stratifiers, ~poisson_regressions(cohort_data = time_data, .x))

adjusted_rates_out <- NULL
for(i in 1:length(stratifiers)){
  adjusted_rates_out <- bind_rows(
    adjusted_rates_out, 
    adjusted_rates[[i]]
  )
}
adjusted_rates_out <- adjusted_rates_out %>% 
  mutate(stratifiers = factor(
    var, 
    levels = stratifiers, 
    labels = strat_plot_names))

adjusted_rates_out %>% 
  write_csv(here("output/tab023_poissonrates.csv"))

# plot adjusted rates -----------------------------------------------------
pdf(here("output/fig3_rate_ratios.pdf"), width = 8, height = 6)
pd = position_dodge(1)
ggplot(adjusted_rates_out, aes(x=level, y = estimate, ymin = conf.low, ymax = conf.high, colour = stratifiers, lty = model)) +
  geom_point(size = 1.5, pch = 16, position = pd) +
  geom_linerange(lwd = 1, alpha = 0.8, position = pd) +
  geom_hline(yintercept = 1, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +
  facet_wrap(~stratifiers, scales = "free", ncol = 2) + 
  labs(y = "Incidence Rate Ratio (95% CI)", x = "") +
  guides(colour = "none") + 
  theme_bw() +
  theme(legend.position = "top",
        strip.background = element_blank())
dev.off()
