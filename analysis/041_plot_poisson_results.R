library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

adjusted_rates_out <- read_csv("output/tab023_poissonrates.csv")
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)

outcome_list <- c("All Long COVID", "Long COVID diagnoses", "Fractures")
cols <- hcl.colors(20, palette = "Viridis")[c(1,10,18)]
names(cols) <- outcome_list

# plot adjusted rates -----------------------------------------------------
pdf(here("output/figures/fig3a_rate_ratios.pdf"), width = 12, height = 12)
pd = position_dodge(1)
adjusted_rates_out %>% 
  filter(plot_marker, std_error < 1) %>% 
  ggplot(aes(x=term2, y = rate, ymin = conf.low, ymax = conf.high, colour = outcome, lty = model)) +
  geom_point(size = 1.5, pch = 16, position = pd) +
  geom_linerange(lwd = 1, alpha = 0.8, position = pd) +
  geom_hline(yintercept = 1, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +
  facet_wrap(~strat_var, scales = "free") + 
  labs(y = "Incidence Rate Ratio (95% CI)", x = "") +
  scale_color_manual(outcome_list, values = cols) +
  theme_ali() +
  theme(legend.position = "top",
        strip.background = element_blank())
dev.off()

pdf(here("output/figures/fig3b_rate_ratios_longcovid.pdf"), width = 12, height = 12)
pd = position_dodge(1)
adjusted_rates_out %>% 
  filter(plot_marker, std_error < 1) %>% 
  filter(outcome != "Fractures") %>% 
  ggplot(aes(x=term2, y = rate, ymin = conf.low, ymax = conf.high, colour = outcome, lty = model)) +
  geom_point(size = 1.5, pch = 16, position = pd) +
  geom_linerange(lwd = 0.75, position = pd) +
  geom_hline(yintercept = 1, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +
  facet_wrap(~strat_var, scales = "free") + 
  labs(y = "Incidence Rate Ratio (95% CI)", x = "") +
  scale_color_manual(outcome_list, values = cols) +
  theme_ali() +
  theme(legend.position = "top",
        strip.background = element_blank())
dev.off()

pdf(here("output/figures/fig3c_rate_ratios_controloutcome.pdf"), width = 12, height = 12)
pd = position_dodge(1)
adjusted_rates_out %>% 
  filter(plot_marker, std_error < 1) %>% 
  filter(outcome == "Fractures") %>% 
  ggplot(aes(x=term2, y = rate, ymin = conf.low, ymax = conf.high, colour = outcome, lty = model)) +
  geom_point(size = 1.5, pch = 16, position = pd) +
  geom_linerange(lwd = 1, alpha = 0.8, position = pd) +
  geom_hline(yintercept = 1, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +
  facet_wrap(~strat_var, scales = "free") + 
  labs(y = "Incidence Rate Ratio (95% CI)", x = "") +
  scale_color_manual(outcome_list, values = cols) +
  theme_ali() +
  theme(legend.position = "top",
        strip.background = element_blank())
dev.off()
