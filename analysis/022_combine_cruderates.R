library(tidyverse)
library(broom)
library(lubridate)
library(arrow)
library(here)

source(here::here("analysis/functions/redaction.R"))
output_dir_tab <- here("output/tables")
fs::dir_create(output_dir_tab)
fs::dir_create(here("output/figures"))

redact_threshold = 7

# tidy crude rates .csv for review ----------------------------------------
crude_rates_static <- read_csv(here("output/tab021_crude_lc_rates.csv"))
crude_rates_tuv_lc_all <- read_csv(here("output/tab022_tuv_rates_lc_all.csv"))
crude_rates_tuv_lc_dx <- read_csv(here("output/tab023_tuv_rates_lc_dx.csv"))

# combine the lc_all and lc_dx time updated results
crude_rates_tuv <- crude_rates_tuv_lc_all %>%
  left_join(
    dplyr::select(crude_rates_tuv_lc_dx, -n, -fup),
    by = c("stratifier", "level"),
    suffix = c("", "_dx")
  ) %>% 
  dplyr::select(-contains("errorfactor"), -contains("se_rate"))

# tidy the crude_rates_static to match the tuv format
drop_vars <- c("n_fratures", 
               "n_rx",
               "total_tests",
               "avg_last_vacc")
crude_rates_static <- crude_rates_static %>%
  dplyr::select(-all_of(drop_vars), -contains("errorfactor"), -contains("se_rate")) %>% 
  filter(!str_detect(stratifier, "vacc"))

#combine the two sets of results
crude_rates <- crude_rates_static %>% 
  bind_rows(crude_rates_tuv) %>% 
  mutate(fup = fup/1e5)

tidy_crude_rates <- crude_rates %>% 
  mutate(stratifier = factor(stratifier,
                    levels = c(
                      "sex",
                      "age_cat",
                      "practice_nuts",
                      "vaccines",
                      "t_vacc_primary",
                      "t_vacc_mrna",
                      "imd_q5",
                      "ethnicity",
                      "comorbidities",
                      "highrisk_shield"
                    ), 
                    labels = c(
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
  arrange(stratifier) 

# redact output 
redacted_crude_rates <- tidy_crude_rates %>% 
  mutate(
    n = redact_and_round(n, redact_threshold),
    fup = redact_and_round_rates(fup, redact_threshold),
    lc = redact_and_round(lc, redact_threshold),
    lc_dx = redact_and_round(lc_dx, redact_threshold),
    rate_per1e5 = redact_and_round_rates(rate_per1e5, redact_threshold),
    lci = redact_and_round_rates(lci, redact_threshold),
    uci = redact_and_round_rates(uci, redact_threshold),
    rate_per1e5_dx = redact_and_round_rates(rate_per1e5_dx, redact_threshold),
    lci_dx = redact_and_round_rates(lci_dx, redact_threshold),
    uci_dx = redact_and_round_rates(uci_dx, redact_threshold)
  ) %>% 
  mutate_if(is.numeric, ~as.character(prettyNum(formatC(., digits = 1, format = "f"), 
                                                big.mark = ",", preserve.width = "none",
                                                drop0trailing = TRUE))) %>% 
  mutate(ci = paste0("(", lci, "-", uci, ")"),
         ci_dx = paste0("(", lci_dx, "-", uci_dx, ")")) %>% 
  dplyr::select(-lci, -lci_dx, -uci, -uci_dx) %>% 
  dplyr::select(variable = stratifier,
                level, 
                n_total = n, 
                n_with_longcovid = lc, 
                n_with_longcovid_diagnosis = lc_dx, 
                follow_up_years_1e5 = fup, 
                longcovid_rate_per1e5 = rate_per1e5,
                ci_longcovid_rate_per1e5 = ci,
                longcovid_rate_per1e5_diagnosis = rate_per1e5_dx,
                ci_longcovid_rate_per1e5_diagnosis = ci_dx
                )

## output the neat csv
redacted_crude_rates %>% 
  write_csv(here("output/tables/tab3_crude_rates_redacted.csv"))


# make a nice plot of the crude rates  ------------------------------------
plot_crude_rates <- tidy_crude_rates %>% 
  dplyr::select(stratifier,
                level, 
                n, 
                fup,
                lc_all = lc, 
                lc_dx,
                rateper1e5_all = rate_per1e5,
                rateper1e5_dx = rate_per1e5_dx,
                lci_all = lci,
                uci_all = uci,
                lci_dx,
                uci_dx
                ) %>% 
  pivot_longer(cols = c(lc_all, lc_dx, rateper1e5_all, rateper1e5_dx, lci_all, uci_all, lci_dx, uci_dx), 
               names_to = c(".value", "outcome"), names_pattern = "(.*)_(.*)") %>% 
  mutate(outcome = factor(outcome, levels = c("all", "dx"), labels = c("All Long COVID", "Long COVID diagnoses")))

outcome_list <- c("All Long COVID", "Long COVID diagnoses", "Fractures")
cols <- hcl.colors(20, palette = "Viridis")[c(1,10,18)]
names(cols) <- outcome_list


pdf(here("output/figures/fig3_crude_rates.pdf"), width = 8, height = 8, onefile=FALSE)
ggplot(data = plot_crude_rates,
                    aes(
                      x = level,
                      y = rateper1e5,
                      ymin = lci,
                      ymax = uci,
                      colour = outcome)) +
  #geom_hline(yintercept = 1, colour = "gray60") +
  geom_pointrange(pch = 1, position = position_dodge(width = 0.5)) + 
  labs(x = "", 
       y = "Crude rate per 100,000 person-years (95% CI)",
       colour = "") +
  facet_grid(
    stratifier ~ " ",
    scales = "free",
    space = "free",
    switch = "y"
  ) +
  scale_color_manual(values = cols) +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    strip.background = element_rect(colour = NA, fill = NA),
    strip.text.y.left = element_text(size = 12, angle = 0, hjust = 0, vjust = 1, face = "bold"),
    strip.text.x = element_text(face = "bold"),
    strip.placement = "outside",
    panel.border = element_rect(fill = NA, color = "black"),
    legend.position = "top",
    panel.grid.major.x = element_line(colour = "gray80"),
    panel.grid.minor.x = element_line(colour = "gray90"),
    axis.text.x = element_text(face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 13
    )
  ) +
  coord_flip()
dev.off()
