library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(ggpubr)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

adjusted_rates_out <- read_csv("output/tab023_poissonrates_timeupdated.csv") 
adjusted_rates_out_SA <- read_csv("output/tab023_poissonrates_timeupdated_sensAnalysis.csv") 

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

outcome_list <- c("All Long COVID", "Long COVID diagnoses", "COVID-19 hospitalisation")
cols <- hcl.colors(20, palette = "Viridis")[c(1,10,16)]
names(cols) <- outcome_list

# plot rate ratios with table -----------------------------------------------------
sigdig <- 2
# bit of formatting so it all looks pretty in the plots
full_rates <- adjusted_rates_out %>% 
  mutate(sa = "0 weeks") %>% 
  bind_rows(adjusted_rates_out_SA) %>% 
  # just keep the useful data
  dplyr::select(strat_var, term2, rate, conf.low, conf.high, outcome, model, plot_marker, baseline, model, covid_variant, sa) %>% 
  # convert the lovely estimates to lovely strings
  mutate(textrate = sprintf("%0.1f", signif(rate, sigdig)),
         textci = sprintf(paste0("(", signif(conf.low, sigdig), "-", signif(conf.high, sigdig), ")"))) %>% 
  # get rid of the variable names as part of the term2 label
  mutate(term2 = str_remove_all(term2, "sex|age_cat|vaccines|ethnicity|comorbidities|practice_nuts|highrisk_shield|t_vacc_mrna|t_vacc_primary")) %>% 
  mutate_at(c("term2"), ~ifelse(str_detect(term2, "baseline"), paste0(".", term2), .)) %>% 
  mutate(term2 = str_replace_all(term2, "\\(", " \\(")) %>% 
  mutate(term2 = str_replace_all(term2, "  \\(", " \\(")) %>% 
  # change ordering of outcome variable 
  mutate(outcome = factor(outcome, levels = c("All Long COVID", "Long COVID diagnoses", "COVID-19 hospitalisation"))) %>% 
  # change ordering of stratification variables
  mutate(strat_var = factor(
    strat_var,
    levels = c(
      "Age category",
      "Sex",
      "No. vaccine doses",
      "mRNA vaccine received"
    )
  )) %>% 
  # filter so just the stratifier results are shown (not the age, and sex coefficients)
  filter(plot_marker) %>% 
  # create structure of SA variable with factor
  mutate(sa = factor(sa,
         levels = c("0 weeks",
                    "2 weeks",
                    "4 weeks",
                    "12 weeks"
                    ))
  )

## Only keep the vaccine stratifiedmodels and the ALL variant models for this plot
full_rates <- full_rates %>% 
  filter(str_detect(strat_var, "vaccine"),
         covid_variant == "All"
         )

## little function to suppress the facet_grid labels so they are not repeated
suppress_labs <- function(string) {
  rep("", length(string))
}

create_forest_plot <- function(data_in, variant, plot_rel_widths = c(7, 3), legend_position = "right") {
  data_for_table <- data_in %>%
    # keep the texty stuff for a nice table
    dplyr::select(strat_var, term2, sa, outcome, model, textrate, textci) %>%
    # convert to longer for plotting
    tidyr::pivot_longer(c(textrate, textci), names_to = "stat") %>%
    mutate(stat = factor(stat, levels = c("textrate", "textci")))
  
  rr_tab <- data_for_table %>%
    ggplot(aes(x = stat, y = term2, label = value)) +
    geom_text(size = 5, hjust = 1) +
    scale_x_discrete(position = "top", labels = c("RR", "95% CI")) +
    facet_grid(
      rows = vars(strat_var),
      cols = vars(outcome, sa),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    labs(x = NULL, y = NULL, title = "") +
    theme_classic() +
    theme(
      strip.background = element_blank(),
      strip.text.y.left = element_text(size = 12, angle = 0, hjust = 0, vjust = 0.1, face = "bold"),
      strip.text.x = element_text(face = "bold"),
      strip.placement = "outside",
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text = element_text(size = 12, hjust = 1, colour = "black"),
      axis.ticks = element_blank(),
      axis.title = element_text(face = "bold")
    )
  
  rr_forest <- data_in %>% 
    ggplot(
      aes(x = term2,
          y = rate,
          ymin = conf.low,
          ymax = conf.high,
          group = outcome,
          colour = outcome)) +
    geom_hline(yintercept = 1, colour = "gray60") +
    geom_pointrange(size = 1.3, pch = 1, position = position_dodge(width = 0.5)) + 
    labs(x = "", 
         y = "Rate ratio (95% Confidence Interval)",
         colour = "Outcome",
         title = "") +
    facet_grid(
      rows = vars(strat_var),
      cols = vars(sa),
      scales = "free",
      space = "free",
      labeller = labeller(strat_var = suppress_labs,
                          #sa = suppress_labs,
                          term2 = suppress_labs)
    ) +
    scale_color_manual(values = cols) +
    scale_y_log10(breaks = c(0.1, 0.2, 0.33, 0.5, 1.0, 2.0, 3.0, 5, 10),
                  limits = c(0.1, 10),
                  minor_breaks = NULL) +
    theme_classic() +
    theme(
      panel.background = element_blank(), 
      title = element_text(size = 9),
      strip.background = element_rect(colour = NA, fill = NA),
      strip.text.x = element_text(face = "bold", size = 9),
      strip.placement = "outside",
      panel.border = element_rect(fill = NA, color = "black"),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 10),
      axis.text.x = element_text(face = "bold"),
      #axis.text.y = element_blank(),
      axis.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 13
      )
    ) +
    coord_flip()
  
  ggarrange(rr_forest, rr_tab, ncol = 1, nrow=2, common.legend = T, legend = "right", heights = c(0.4, 0.6))
}

## Crude RRs
pdf(here("output/figures/fig3a_crude_RRs_sensAnalysis.pdf"), width = 25, height = 10, onefile=FALSE)
  create_forest_plot(filter(full_rates, model == "crude", outcome != "COVID-19 hospitalisation"))
dev.off()

# Adjusted RRs
adjusted_plot <- create_forest_plot(filter(full_rates, model == "adjusted", outcome != "COVID-19 hospitalisation"))

pdf(here("output/figures/fig3b_adjusted_RRs_sensAnalysis.pdf"), width = 25, height = 10, onefile=FALSE)
  adjusted_plot
dev.off()

covid_hosp_plot <- create_forest_plot(filter(full_rates, model == "adjusted"))

pdf(here("output/figures/fig3b_adjusted_RRs_all_outcomes_sensAnalysis.pdf"), width = 25, height = 10, onefile=FALSE)
  covid_hosp_plot
dev.off()

# Make a nice table of the results ----------------------------------------
output_poisson_rates <- full_rates %>% 
  # tidy up outcome names because they're about to become a variable name
  mutate(outcome = str_to_lower(str_replace_all(outcome, " |-", "_"))) %>%
  dplyr::select(
    variant = covid_variant, 
    variable = strat_var, 
    level = term2, 
    model, 
    outcome,
    sa,
    rr = textrate, 
    ci = textci
  ) %>% 
  pivot_wider(names_from = "outcome",
              values_from = c("rr", "ci")) %>% 
  dplyr::select(variant, variable, level, model, sa,
                rr_all_long_covid, 
                ci_all_long_covid,
                rr_long_covid_diagnoses, 
                ci_long_covid_diagnoses,
                rr_covid_19_hospitalisation, 
                ci_covid_19_hospitalisation
  ) %>% 
  mutate(variable = factor(variable,
                           levels = c(
                             "Age category",
                             "Sex",
                             "No. vaccine doses",
                             "First vaccine received",
                             "mRNA vaccine received"
                           ))) %>% 
  arrange(variable, model, level, sa) 

## output the neat csv
output_poisson_rates %>% 
  write_csv(here::here("output/tables/tab4_poisson_rateratios_sensAnalysis.csv"))
