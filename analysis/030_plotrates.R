library(tidyverse)
library(broom)
library(lubridate)
library(survival)
library(here)
library(data.table)
## also uses zoo and cowplot, referred to directly

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/ggplot_theme.R"))

# create folders to put plots in
dir.create(here("output/figures"), showWarnings = FALSE, recursive=TRUE)
dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

redact_threshold <- 10

# import data ------------------------------------------------------------
time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet")) %>%
  rename(lc_out = out)
time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet")) %>%
  rename(lc_dx = out)

# define colours ----------------------------------------------------------
cols <- c(
  "long COVID" = "red1",
  "long COVID Dx" = "dodgerblue1",
  "Hospitalised" = "orange",
  "Last test positive" = "black",
  "1st vaccine dose" = "blueviolet",
  "long COVID hospital" = "forestgreen")

lc_dx_cols <- c("coral", "aquamarine2")

vaccgroups <- levels(time_data_lc_all$vaccines)
n_groups <- length(vaccgroups)
colours <- hcl.colors(n_groups, palette = "viridis")
colours_dx <- colours #hcl.colors(n_groups, palette = "zissou1")

# plot time of Long COVID diagnosis ---------------------------------------
summarise_records_by_month <- function(timedata_in, outcome_var){
  timedata_in %>% 
    # remove all the people with NA outcome
    filter(!is.na(outcome)) %>% 
    # create a binayr and date variable for the outcome
    rename(outcome_binary = {{outcome_var}},
           outcome_date = outcome) %>% 
    ungroup() %>% 
    # redundant filter but keep it in for safety
    filter(sex %in% c("male", "female")) %>% 
    # select the important variables
    dplyr::select(patient_id, age_cat, sex, vaccines, outcome_date, outcome_binary) %>% 
    # get the week number and year from the date
    mutate(week = lubridate::week(outcome_date),
           year = lubridate::year(outcome_date)) %>% 
    # group and summarise weekly counts
    group_by(year, week, sex, vaccines) %>% 
    summarise(sum_out = sum(outcome_binary, na.rm = TRUE), .groups = "keep") %>% 
    # make a fake date variable for plotting purposes
    mutate(date = make_date(year, 1, 1) + (week*7))
}

timeseries_lc_all <- summarise_records_by_month(time_data_lc_all, lc_out) %>% 
  mutate(outcome = "long COVID")
timeseries_lc_dx <- summarise_records_by_month(time_data_lc_dx, lc_dx) %>% 
  mutate(outcome = "long COVID Dx")
timeseries_plot <- bind_rows(timeseries_lc_all,
                             timeseries_lc_dx)
timeseries_lc_all <- NULL
timeseries_lc_dx <- NULL

timeseries_plot$redacted_out <- redactor2(timeseries_plot$sum_out, threshold = redact_threshold)

# for p2 need to agregate by sex to get a single panel plot
p2_data <- timeseries_plot %>% 
  group_by(date, outcome) %>% 
  summarise(redacted_out = redactor2(sum(sum_out, na.rm = T), threshold = redact_threshold), .groups = "keep")

if(max(p2_data$redacted_out, na.rm = T) > 0){
  p_base <- ggplot(p2_data, aes(x = date, y = redacted_out, colour = outcome, fill = outcome)) +
      labs(x = "Date", y = "Count of Long COVID codes") +
      scale_color_manual(values = cols[1:2]) +
      scale_fill_manual(values = cols[1:2]) +
      theme_ali() + 
      theme(strip.background = element_blank())
  
  p2 <- p_base + 
    geom_line(lwd = 0.2, lty = 1) +
    labs(colour = "Long COVID record") +
    theme(legend.position = "top")
  
  # group by sex as well redact data for 2x2 plot
  p2b_data <- timeseries_plot %>% 
    group_by(date, sex, outcome) %>% 
    summarise(redacted_out = redactor2(sum(sum_out, na.rm = T), threshold = redact_threshold), .groups = "keep")

  if(max(p2b_data$redacted_out, na.rm = T) > 0){
    p2a <- p_base +
      geom_col(data = p2b_data, col = "gray20", lwd = 0.2) +
      facet_grid(sex~outcome) +
      theme(legend.position = "none")
    
  }else{
    p2a <- ggplot() + theme_void()
  } # end 
}else{
  p2 <- ggplot() + theme_void()
} # end 

pdf(here("output/figures/fig2_raw_counts_line.pdf"), width = 8, height = 6)
  p2
dev.off()

pdf(here("output/figures/fig2a_raw_counts_line_bysex.pdf"), width = 8, height = 6)
  p2a
dev.off()
# column chart by vaccine status ------------------------------------------
# get long covid (ANY)  summarised by sex for a single panel plot
p2b_data <- timeseries_plot %>% 
  filter(outcome == "long COVID") %>% 
  group_by(date, vaccines) %>% 
  summarise(sum_out = sum(sum_out), .groups = "keep") %>% 
  mutate(redacted_out = redactor2(sum_out, threshold = redact_threshold)) 

# redact data by outcome and sex for 2x2 panel plot
p2c_data <- timeseries_plot %>% 
  mutate(redacted_out = redactor2(sum_out, threshold = redact_threshold)) 

if(max(p2c_data$redacted_out, na.rm = T) > 0){
  # make base plot for 2b and 2c  
  p2base <- ggplot(p2c_data, aes(x = date, y = redacted_out, fill = vaccines)) +
    scale_fill_manual(values = colours) +
    labs(x = "Date", y = "Count of Long COVID codes", fill = "Vaccines received") +
    theme_ali() + 
    theme(strip.background = element_blank())
  
  # plot column in a single panel
  p2b <- p2base +
    geom_col(data = p2b_data, lwd = 0, lty = 0, width = 6) +
    theme(legend.position = "top")
  
  # plot 2x2 panel version of long covid by vaccine status over time
  p2c <- p2base +
    geom_col(lwd = 0.2, lty = 0, width = 6) +
    facet_grid(sex~outcome)
}else{
  p2b <- ggplot() + theme_void()
  p2c <- ggplot() + theme_void()
} # end 

pdf(here("output/figures/fig2b_raw_counts_column.pdf"), width = 8, height = 6)
  p2b
dev.off()

pdf(here("output/figures/fig2c_raw_counts_column_bysex.pdf"), width = 8, height = 6)
  p2c
dev.off()

# stacked bar chart for proportion Rx vs Dx -------------------------------
stacked_bar <- timeseries_plot %>% 
  group_by(date, outcome) %>% 
  summarise(redacted_out = redactor2(sum(sum_out, na.rm = T), threshold = redact_threshold), .groups = "keep")
stacked_bar$lc_dx <- ifelse(stacked_bar$outcome == "long COVID Dx", "Dx", "Rx")

p2e <- ggplot(stacked_bar, aes(fill=lc_dx, y=redacted_out, x=date)) + 
  geom_bar(position="fill", stat="identity", width = 6) +
  scale_fill_manual(values = lc_dx_cols) +
  labs(x = "Date", y = "Type of long COVID code", fill = "Rx or Dx code") +
  theme_ali() + 
  theme(strip.background = element_blank(),
        legend.position = "bottom")

pdf(here("output/figures/fig2e_longcovid_stacked_dx_rx.pdf"), width = 8, height = 6)
  p2e
dev.off()

# put together the nice figure for the paper ------------------------------
p2_full <- cowplot::plot_grid(
  p2, p2b, p2e,
  ncol = 1,
  labels = "AUTO"
  )

pdf(here("output/figures/fig2_longcovid_dynamics.pdf"), width = 6, height = 10)
  p2_full
dev.off()

# supplement - time gaps between vaccine doses  ---------------------------
vaccine_gaps <- time_data_lc_all %>%
  dplyr::select(t, vaccines, sex) %>% 
  mutate(t_gap = t * (365.25/52)) %>%
  filter(sex %in% c("male", "female"), vaccines != "0")

avg_vaccine_gaps <- vaccine_gaps %>% 
  drop_na() %>% 
  group_by(vaccines) %>% 
  summarise(avg_gap = mean(t_gap), n = n(), .groups = "keep")

n_groups <- length(unique(vaccine_gaps$sex))
colours <- hcl.colors(n_groups)

pdf(here("output/supplementary/fig_agegap_vaccines.pdf"), width = 8, height = 6)
ggplot(drop_na(vaccine_gaps), aes(x = t_gap, colour = sex, fill = sex)) +
  geom_density(alpha = 0.7) +
  geom_vline(data = avg_vaccine_gaps, aes(xintercept = avg_gap), lty = 2, col = "gray50") +
  facet_wrap(~vaccines, ncol = 1) + 
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  theme_ali() +
  labs(x = "Avg. difference between doses (weeks)") +
  theme(strip.background = element_blank())
dev.off()

