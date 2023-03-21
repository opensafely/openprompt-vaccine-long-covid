#library(targets)
library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(here)

source(here::here("analysis/functions/import_data.R"))
source(here::here("analysis/functions/clean_vaccine_data.R"))
source(here::here("analysis/functions/calculate_rates.R"))
source(here::here("analysis/functions/time_update_vaccinedoses.R"))
source(here::here("analysis/functions/poisson_regressions.R"))

cases_data_path <- here("output/dataset_cases.csv.gz")
controls_data_path <- here("output/dataset_controls.csv.gz")

# import, combine, clean data -------------------------------------------------
imported_data <-
  import_and_combine(cases_path = cases_data_path, controls_path = controls_data_path)

cleaned_data <- tidy_vaccine_data(imported_data)

# calculate crude rates ---------------------------------------------------
crude_rates <- apply_rates_over_stratifiers(cleaned_data)
crude_rates %>% 
  write_csv(here("output/tab021_crude_lc_rates.csv"))

# time update on vaccine date ---------------------------------------------
time_data <- time_update_vaccinedoses(cleaned_data)
time_updated_rates <- bind_rows(
  calculate_rates(time_data, vaccines),
  calculate_rates(time_data, t_mixmatch)
)
  
time_updated_rates %>% 
  write_csv(here("output/tab022_tuv_lc_rates.csv"))

# add simple poisson regression results -----------------------------------
stratifiers <- c("vaccines","t_mixmatch","practice_nuts","imd","ethnicity")
adjusted_rates <- map(stratifiers, ~poisson_regressions(cohort_data = time_data, .x))

adjusted_rates_out <- NULL
for(i in 1:length(stratifiers)){
  adjusted_rates_out <- bind_rows(
    adjusted_rates_out, 
    adjusted_rates[[i]]
  )
}
adjusted_rates_out %>% 
  write_csv(here("output/tab023_poissonrates.csv"))


# plot adjusted rates -----------------------------------------------------
pdf(here("output/fig3_rate_ratios.pdf"), width = 8, height = 6)
pd = position_dodge(0.5)
ggplot(adjusted_rates_out, aes(x=level, y = estimate, ymin = conf.low, ymax = conf.high, colour = var, lty = model)) +
  geom_point(size = 1.5, pch = 16, position = pd) +
  geom_linerange(lwd = 1, alpha = 0.8, position = pd) +
  geom_hline(yintercept = 1, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +
  facet_wrap(~var, scales = "free", ncol = 2) + 
  guides(colour = "none") + 
  theme_bw() +
  theme(legend.position = "top")
dev.off()

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
  dplyr::select(patient_id, pt_start_date, pt_end_date, sex, vaccines, lc_out, first_lc_dx) %>% 
  mutate(month = month(first_lc_dx),
         year = year(first_lc_dx)) %>% 
  group_by(year, month, sex, vaccines) %>% 
  summarise(total_lc = sum(lc_out, na.rm = TRUE)) %>% 
  mutate(day = 1,
         date = make_date(year, month, day))


pdf(here("output/fig2_raw_counts_bysex.pdf"), width = 8, height = 6)
ggplot(timeplot, aes(x = date, y = total_lc, fill = vaccines)) +
  geom_col(lwd = 0.5, lty = 1, col = "gray80") +
  scale_fill_manual(values = colours) +
  facet_wrap(~sex, ncol = 1) + 
  labs(x = "Date", y = "Count of Long COVID codes") +
  theme_bw()
dev.off()
