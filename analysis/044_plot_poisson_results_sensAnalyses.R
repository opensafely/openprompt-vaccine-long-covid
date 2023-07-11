library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(ggpubr)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

adjusted_rates_out <- read_csv("output/tab023_poissonrates_timeupdated_sensAnalysis.csv") 

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
cols <- hcl.colors(20, palette = "Viridis")[c(1,10,16,19)]
names(cols) <- outcome_list

# plot rate ratios with table -----------------------------------------------------
sigdig <- 2
# bit of formatting so it all looks pretty in the plots
full_rates <- adjusted_rates_out %>% 
  # just keep the useful data
  dplyr::select(strat_var, term2, rate, conf.low, conf.high, outcome, model, plot_marker, baseline, model, covid_variant) %>% 
  # convert the lovely estimates to lovely strings
  mutate(textrate = sprintf("%0.1f", signif(rate, sigdig)),
         textci = sprintf(paste0("(", signif(conf.low, sigdig), "-", signif(conf.high, sigdig), ")"))) %>% 
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
      "mRNA vaccine received"
    )
  )) %>% 
  # filter so just the stratifier results are shown (not the age, and sex coefficients)
  filter(plot_marker)

suppress_labs <- function(string) {
  rep("", length(string))
}

create_forest_plot <- function(data_in, y_col_var, variant, plot_rel_widths = c(7, 3), legend_position = "right") {
  if (y_col_var == "outcome"){
    outcome_list <- c("All Long COVID", "Long COVID diagnoses", "COVID-19 hospitalisation")
    cols <- hcl.colors(20, palette = "Viridis")[c(1,10,16,19)]
    names(cols) <- outcome_list
  } else {
    n_cols <- dplyr::select(data_in, {{y_col_var}}) %>% unique() %>% pull()
    cols <- hcl.colors(length(n_cols), palette = "Viridis")
    names(cols) <- n_cols
  }
  
  data_for_table <- data_in %>%
    # filter to the variant 
    filter(covid_variant == eval(variant)) %>% 
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
    labs(x = NULL, y = NULL, title = "") +
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
  
  rr_forest <- data_in %>% 
    filter(covid_variant == eval(variant)) %>% 
    ggplot(
      aes(x = term2,
          y = rate,
          ymin = conf.low,
          ymax = conf.high,
          group = get(y_col_var),
          colour = get(y_col_var))) +
    geom_hline(yintercept = 1, colour = "gray60") +
    geom_pointrange(size = 1.3, pch = 1, position = position_dodge(width = 0.5), width = 0.5) + 
    #geom_point(position = position_dodge(width = 0.5)) +
    labs(x = "", 
         y = "Rate ratio (95% Confidence Interval)",
         colour = eval(variant)) +
    facet_grid(
      strat_var ~ " ",
      scales = "free",
      space = "free",
      labeller = labeller(strat_var = suppress_labs,
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
      legend.position = legend_position,
      legend.title = element_text(face = "bold", size = 10),
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
  
  ggarrange(rr_tab, rr_forest, ncol = 2, nrow=1, common.legend = T, legend = "right", widths = plot_rel_widths)
}

# # Crude RRs
p_all <- create_forest_plot(filter(full_rates, model == "crude"), y_col_var = "outcome", variant = "All")

pdf(here("output/figures/fig3a_crude_RRs_sensAnalysis.pdf"), width = 16, height = 12, onefile=FALSE)
cowplot::plot_grid(p_all, ncol = 1)
dev.off()

# Adjusted RRs
p_all <- create_forest_plot(filter(full_rates, model == "adjusted"), y_col_var = "outcome", variant = "All")

pdf(here("output/figures/fig3b_adjusted_RRs_sensAnalysis.pdf"), width = 16, height = 12, onefile=FALSE)
cowplot::plot_grid(p_all, ncol = 1)
dev.off()

# Focus on vaccines
p_all <- create_forest_plot(filter(full_rates, str_detect(strat_var, "accine"), model == "adjusted"), y_col_var = "outcome", variant = "All")

pdf(here("output/figures/fig3e_vaccines_sensAnalysis.pdf"), width = 16, height = 12, onefile=FALSE)
cowplot::plot_grid(p_all, ncol = 1)
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
    rr = textrate, 
    ci = textci
  ) %>% 
  pivot_wider(names_from = "outcome",
              values_from = c("rr", "ci")) %>% 
  dplyr::select(variant, variable, level, model, 
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
  arrange(variable, model, level) 

## output the neat csv
output_poisson_rates %>% 
  write_csv(here::here("output/tables/tab4_poisson_rateratios_sensAnalysis.csv"))
