library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(ggpubr)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

adjusted_rates_out <- read_csv("output/tab023_poissonrates_timeupdated.csv") %>% 
  bind_rows(
    read_csv("output/tab023_poissonrates_static.csv")
  )
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here("output/tables"), showWarnings = FALSE, recursive=TRUE)

if(max(adjusted_rates_out$conf.high, na.rm = T) > 2e200){
  print("max confidence limit is strangely high for:")
  glimpse(filter(adjusted_rates_out, conf.high > 2e200))
  print("removing this from the plots")
  adjusted_rates_out <- adjusted_rates_out %>% 
    filter(conf.high<2e200)
}
if(max(adjusted_rates_out$std_error, na.rm = T) > 1){
  print("max std. error is strangely high for:")
  glimpse(filter(adjusted_rates_out, std_error > 1))
  print("removing this from the plots")
  adjusted_rates_out <- adjusted_rates_out %>% 
    filter(is.na(std_error) | std_error<1)
}

outcome_list <- c("All Long COVID", "Long COVID diagnoses", "Fractures", "COVID-19 hospitalisation")
cols <- hcl.colors(20, palette = "Viridis")[c(1,10,16,19)]
names(cols) <- outcome_list

# plot rate ratios with table -----------------------------------------------------
# bit of formatting so it all looks pretty in the plots
full_rates <- adjusted_rates_out %>% 
  # just keep the useful data
  dplyr::select(strat_var, term2, rate, conf.low, conf.high, outcome, model, plot_marker, baseline, model) %>% 
  # convert the lovely estimates to lovely strings
  mutate(textrate = sprintf("%0.1f", round(rate, digits = 1)),
         textci = sprintf(paste0("(", round(conf.low, 1), "-", round(conf.high, 1), ")"))) %>% 
  # get rid of the variable names as part of the term2 label
  mutate(term2 = str_remove_all(term2, "sex|age_cat|vaccines|ethnicity|comorbidities|practice_nuts|highrisk_shield|t_vacc_mrna|t_vacc_primary")) %>% 
  mutate_at(c("term2"), ~ifelse(str_detect(term2, "baseline"), paste0(".", term2), .)) %>% 
  mutate(term2 = str_replace_all(term2, "\\(", " \\(")) %>% 
  mutate(term2 = str_replace_all(term2, "  \\(", " \\(")) %>% 
  # change ordering of outcome variable 
  mutate(outcome = factor(outcome, levels = c("All Long COVID", "Long COVID diagnoses", "Fractures", "COVID-19 hospitalisation"))) %>% 
  # change ordering of stratification variables
  mutate(strat_var = factor(
    strat_var,
    levels = c(
      "Age category",
      "Sex",
      "No. vaccine doses",
      "First vaccine received",
      "mRNA vaccine received",
      "Region",
      "IMD (quintile)",
      "Ethnicity",
      "Comorbidities",
      "Shielding (high risk group)"
    )
  )) %>% 
  # filter so just the stratifier results are shown (not the age, and sex coefficients)
  filter(plot_marker) 

suppress_labs <- function(string) {
  rep("", length(string))
}

create_forest_plot <- function(data_in, y_col_var, plot_rel_widths = c(7, 3), legend_position = "right") {
  if (y_col_var == "outcome"){
    outcome_list <- c("All Long COVID", "Long COVID diagnoses", "Fractures", "COVID-19 hospitalisation")
    cols <- hcl.colors(20, palette = "Viridis")[c(1,10,16,19)]
    names(cols) <- outcome_list
  } else {
    n_cols <- dplyr::select(data_in, {{y_col_var}}) %>% unique() %>% pull()
    cols <- hcl.colors(length(n_cols), palette = "Viridis")
    names(cols) <- n_cols
  }
  
  data_for_table <- data_in %>%
    # keep the texty stuff for a nice table
    dplyr::select(strat_var, term2, outcome, model, textrate, textci) %>%
    # convert to longer for plotting
    tidyr::pivot_longer(c(textrate, textci), names_to = "stat") %>%
    mutate(stat = factor(stat, levels = c("textrate", "textci")))
  
  rr_tab <- data_for_table %>%
    ggplot(aes(x = stat, y = term2, label = value)) +
    geom_text(size = 5, hjust = 1) +
    scale_x_discrete(position = "bottom", labels = c("RR", "95% CI")) +
    facet_grid(
      strat_var ~ get(y_col_var),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    labs(x = NULL, y = NULL) +
    theme_classic() +
    theme(
      strip.background = element_blank(),
      strip.text.y.left = element_text(size = 12, angle = 0, hjust = 0, vjust = 0, face = "bold"),
      strip.text.x = element_text(face = "bold"),
      strip.placement = "outside",
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text = element_text(size = 12, hjust = 1, colour = "black"),
      axis.ticks = element_blank(),
      axis.title = element_text(face = "bold")
      )
  
  pd <- position_dodge(2)
  
  rr_forest <- ggplot(data = data_in,
                      aes(
                        x = term2,
                        y = rate,
                        ymin = conf.low,
                        ymax = conf.high,
                        group = get(y_col_var),
                        colour = get(y_col_var)
                      )) +
    geom_hline(yintercept = 1, colour = "gray60") +
    geom_pointrange(pch = 1, position = position_dodge(width = 0.5), width = 0.5) + 
    #geom_point(position = position_dodge(width = 0.5)) +
    labs(x = "", 
         y = "Rate ratio (95% Confidence Interval)",
         colour = "") +
    facet_grid(
      strat_var ~ " ",
      scales = "free",
      space = "free",
      labeller = labeller(strat_var = suppress_labs,
                          term2 = suppress_labs)
    ) +
    scale_color_manual(values = cols) +
    scale_y_log10(breaks = c(0.2, 0.33, 0.5, 1.0, 2.0, 3.0, 5),
                  limits = c(0.2, 5),
                  minor_breaks = NULL) +
    theme_classic() +
    theme(
      panel.background = element_blank(),
      strip.background = element_rect(colour = NA, fill = NA),
      strip.text.x = element_text(face = "bold", size = 9),
      strip.placement = "outside",
      panel.border = element_rect(fill = NA, color = "black"),
      legend.position = legend_position,
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_blank(),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 13
      )
    ) +
    coord_flip()
  
  ggarrange(rr_tab, rr_forest, ncol = 2, nrow=1, common.legend = T, legend = "bottom", widths = plot_rel_widths)
}

# Crude RRs
pdf(here("output/figures/fig3a_crude_RRs.pdf"), width = 20, height = 20, onefile=FALSE)
  create_forest_plot(data_in = filter(full_rates, model == "crude"), y_col_var = "outcome")
dev.off()

# Adjusted RRs
pdf(here("output/figures/fig3b_adjusted_RRs.pdf"), width = 20, height = 20, onefile=FALSE)
  create_forest_plot(filter(full_rates, model == "adjusted"), y_col_var = "outcome")
dev.off()

# Long COVID adjusted outcomes only
pdf(here("output/figures/fig3c_longcovid_RRs.pdf"), width = 20, height = 20, onefile=FALSE)
  create_forest_plot(filter(full_rates, str_detect(outcome, "Long") & model == "adjusted"), y_col_var = "outcome")
dev.off()

# All Long COVID only: adjusted versus crude
pdf(here("output/figures/fig3d_longcovid_models.pdf"), width = 20, height = 20, onefile=FALSE)
  create_forest_plot(filter(full_rates, outcome == "All Long COVID"), 
                     legend_position = "none", 
                     y_col_var = "model")
dev.off()

# Focus on vaccines
pdf(here("output/figures/fig3e_vaccines.pdf"), width = 20, height = 8, onefile=FALSE)
  create_forest_plot(filter(full_rates, str_detect(strat_var, "accine") & model == "adjusted"), y_col_var = "outcome",
                     plot_rel_widths = c(7, 3))
dev.off()

# vaccines and long covid focuse
pdf(here("output/figures/fig3f_longcovid_vaccine_models.pdf"), width = 20, height = 8, onefile=FALSE)
  create_forest_plot(filter(full_rates, str_detect(outcome, "Long") & str_detect(strat_var, "accine") & model == "adjusted"), y_col_var = "outcome",
                     plot_rel_widths = c(7, 3))
dev.off()

# non Vaccine stratifiers
pdf(here("output/figures/fig3g_demographics.pdf"), width = 20, height = 12, onefile=FALSE)
  create_forest_plot(filter(full_rates, !str_detect(strat_var, "accine") & model == "adjusted"), y_col_var = "outcome",
                   plot_rel_widths = c(7, 3))
dev.off()

# put them all on one plot  -----------------------------------------------
pdf(here("output/figures/fig3h_rate_ratios_facet.pdf"), width = 12, height = 14, onefile=FALSE)
pd = position_dodge(1)
adjusted_rates_out %>% 
  filter(plot_marker) %>% 
  ggplot(aes(x=term2, y = rate, ymin = conf.low, ymax = conf.high, colour = outcome, lty = model, alpha = model)) +
  geom_point(size = 1.5, pch = 16, position = pd) +
  geom_linerange(lwd = 1, position = pd) +
  geom_hline(yintercept = 1, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +
  facet_wrap(~strat_var, scales = "free", ncol = 3) + 
  labs(y = "Incidence Rate Ratio (95% CI)", x = "") +
  scale_color_manual(outcome_list, values = cols) +
  scale_alpha_manual(c("crude", "adjusted"), values = c(0.5, 1)) +
  guides(alpha = "none", colour = guide_legend("Outcome"), lty = guide_legend("Model")) +
  theme_ali() +
  theme(legend.position = "top",
        strip.background = element_blank())
dev.off()


# Make a nice table of the results ----------------------------------------
output_poisson_rates <- full_rates %>% 
  # tidy up outcome names because they're about to become a variable name
  mutate(outcome = str_to_lower(str_replace_all(outcome, " |-", "_"))) %>%
  dplyr::select(
    variable = strat_var, 
    level = term2, 
    model, 
    outcome,
    rr = textrate, 
    ci = textci
  ) %>% 
  pivot_wider(names_from = "outcome",
              values_from = c("rr", "ci")) %>% 
  dplyr::select(variable, level, model, 
                rr_all_long_covid, 
                ci_all_long_covid,
                rr_long_covid_diagnoses, 
                ci_long_covid_diagnoses,
                rr_covid_19_hospitalisation, 
                ci_covid_19_hospitalisation,
                rr_fractures, 
                ci_fractures) %>% 
  mutate(variable = factor(variable,
                             levels = c(
                               "Age category",
                               "Sex",
                               "Region",
                               "No. vaccine doses",
                               "First vaccine received",
                               "mRNA vaccine received",
                               "IMD (quintile)",
                               "Ethnicity",
                               "Comorbidities",
                               "Shielding (high risk group)"
                             ))) %>% 
  arrange(variable, model, level) 

## output the neat csv
output_poisson_rates %>% 
  write_csv(here::here("output/tables/tab4_poisson_rateratios.csv"))
