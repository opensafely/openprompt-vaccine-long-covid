library(tidyverse)
library(ggalluvial)
library(here)
library(gt)
library(gtsummary)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)
output_dir_tab <- here("output/tables")
fs::dir_create(output_dir_tab)

# import data ------------------------------------------------------------
cleaned_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet"))

# relationship between test positive and long COVID -----------------------
summ_data <- cleaned_data %>% 
  dplyr::select(patient_id,
                all_test_positive,
                all_tests,
                test_positive_cat, 
                test_total_cat,
                lc_dx_flag,
                lc_out,
                lc_dx_only,
                last_vacc_gap) %>% 
  mutate(test_pos_binary = factor(
    as.numeric(all_test_positive > 0),
    labels = c("No positive tests", 
               ">=1 positive test")))  
                             
summ_data %>% 
  dplyr::select(test_pos_binary, lc_out, lc_dx_only) %>% 
  gtsummary::tbl_summary(
    by = test_pos_binary,
    label = list(
      "lc_out" ~ "Any long COVID code",
      "lc_dx_only" ~ "Long COVID Dx only"
    )
  ) %>% 
  gtsummary::add_overall() %>% 
  as_gt() %>%
  gt::gtsave(
    filename = "tab_tests_and_longcovid.html",
    path = fs::path(output_dir_tab)
  )

# plot of time between test and LC ----------------------------------------
gap_plot <- cleaned_data %>% 
  dplyr::select(first_lc, lc_dx_flag, testdate = latest_test_before_diagnosis) %>% 
  mutate(testgap = testdate %--% first_lc / dweeks(1))
summ_gap_plot <- gap_plot %>% 
  group_by(lc_dx_flag) %>% 
  summarise(avggap = mean(testgap, na.rm = T)) %>% 
  drop_na()

pdf(here("output/supplementary/test_to_longcovid_density.pdf"), width = 8, height = 6)
ggplot(gap_plot, aes(x = testgap, fill = lc_dx_flag)) +
  geom_density(alpha = 0.76) +
  geom_vline(data = summ_gap_plot, aes(xintercept = avggap, colour = lc_dx_flag)) +
  labs(x = "Weeks between test and code",
       y = "Density",
       colour = "Any or Dx code",
       fill = "Any or Dx code") +
  theme_ali()
dev.off()

# Sankey diagram of COVID states ------------------------------------------
sankey_data <- cleaned_data %>% 
  dplyr::select(
    patient_id,
    lc_dx_flag,
    all_test_positive,
    all_tests,
    all_covid_hosp,
    first_covid_critical,
    first_covid_hosp_primary_dx
  ) %>% 
  mutate(
    lc_dx_flag = replace_na(as.character(lc_dx_flag), "None"),
    test = all_test_positive > 0,
    hosp = case_when(
      all_covid_hosp > 0 & first_covid_critical ~ "ICU admission", 
      all_covid_hosp > 0 ~ "Hospital admission", 
      all_covid_hosp == 0 ~ "None"
    )
  ) %>% 
  dplyr::select(
    patient_id, 
    lc_dx_flag,
    test,
    hosp
  ) 
# sankey_data$hosp <- sample(c("None", 
#                              "Hospital admission",
#                              "Primary admission",
#                              "ICU admission"),
#                            size = nrow(sankey_data),
#                            replace = TRUE)
sankey_plot <- sankey_data %>% 
  group_by(lc_dx_flag, test, hosp) %>% 
  summarise(freq = n()) %>% 
  ungroup()
is_alluvia_form(as.data.frame(sankey_plot), axes = 1:4, silent = TRUE)


# reorder vars as factors
sankey_plot$lc_dx_flag <- factor(sankey_plot$lc_dx_flag, 
                                 levels = c("None", "Rx", "Dx"))
sankey_plot$hosp <- factor(sankey_plot$hosp, 
                           levels = rev(c("None", 
                                      "Hospital admission",
                                      "ICU admission")))
sankey_plot$hosp[sankey_plot$hosp=="None"] <- NA

pdf(here("output/figures/fig5_longcovid_flows.pdf"), width = 8, height = 6)
ggplot(sankey_plot,
       aes(y = freq, axis1 = test, axis2 = hosp, axis3 = lc_dx_flag)) +
  geom_alluvium(aes(fill = lc_dx_flag), width = 1/6) +
  geom_stratum(width = 1/6, fill = "gray70", color = "grey40") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Test positive", "Hospitalised", "Long COVID record"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(fill = "Long COVID record",
       y = "Frequency", 
       x = "COVID-19 status") + 
  theme_ali() + 
  theme(legend.position = "top")
dev.off()

sankey_plotv2 <- sankey_plot
sankey_plotv2$lc_dx_flag[sankey_plotv2$lc_dx_flag=="None"] <- NA
pdf(here("output/figures/fig5_longcovid_flows_v2.pdf"), width = 8, height = 6)
ggplot(sankey_plotv2,
       aes(y = freq, axis1 = test, axis2 = hosp, axis3 = lc_dx_flag)) +
  geom_alluvium(aes(fill = lc_dx_flag), width = 1/6) +
  geom_stratum(width = 1/6, fill = "gray70", color = "grey40") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Test positive", "Hospitalised", "Long COVID record"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  labs(fill = "Long COVID record",
       y = "Frequency", 
       x = "COVID-19 status") + 
  theme_ali() + 
  theme(legend.position = "top")
dev.off()