library(tidyverse)
library(forcats)
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
snomed_code <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet")) %>% 
  dplyr::select(patient_id, first_lc_code)

time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet")) %>%
  rename(lc_out = out) %>% 
  left_join(snomed_code, by = "patient_id")

time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet")) %>%
  rename(lc_dx = out) %>%
  left_join(snomed_code, by = "patient_id")


longcovid_combined_codelist <- bind_rows(
  readr::read_csv(here("codelists/opensafely-nice-managing-the-long-term-effects-of-covid-19.csv")),
  readr::read_csv(here("codelists/opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv")),
  readr::read_csv(here("codelists/opensafely-referral-and-signposting-for-long-covid.csv"))
)


# define colours ----------------------------------------------------------
cols <- c(
  "long COVID" = "red1",
  "long COVID Dx" = "dodgerblue1",
  "Hospitalised" = "orange",
  "Last test positive" = "black",
  "1st vaccine dose" = "blueviolet",
  "long COVID hospital" = "forestgreen")

lc_dx_cols <- c("red1", "dodgerblue1")

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
    dplyr::select(patient_id, age_cat, sex, vaccines, first_lc_code, outcome_date, outcome_binary) %>% 
    # get the week number and year from the date
    mutate(week = lubridate::week(outcome_date),
           year = lubridate::year(outcome_date)) %>% 
    # group and summarise weekly counts
    group_by(year, week, first_lc_code, sex, vaccines) %>% 
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

# for p2 need to agregate by sex to get a single panel plot
p2_data <- timeseries_plot %>% 
  group_by(date, outcome) %>% 
  summarise(redacted_out = redact_and_round(sum(sum_out, na.rm = T), redact_threshold = redact_threshold), .groups = "keep")

if(max(p2_data$redacted_out, na.rm = T) > 0){
  p_base <- ggplot(p2_data, aes(x = date, y = redacted_out, colour = outcome, fill = outcome)) +
      labs(x = "Date", y = "Count of new Long COVID codes") +
      scale_color_manual(values = cols[1:2]) +
      scale_fill_manual(values = cols[1:2]) +
      theme_ali() + 
      theme(strip.background = element_blank())
  
  p2 <- p_base + 
    geom_line(lwd = 0.2, lty = 1) +
    labs(colour = "Long COVID record") +
    theme(legend.position = "bottom") +
    ylim(c(0, NA))
  
  # group by sex as well redact data for 2x2 plot
  p2b_data <- timeseries_plot %>% 
    group_by(date, sex, outcome) %>% 
    summarise(redacted_out = redact_and_round(sum(sum_out, na.rm = T), redact_threshold = redact_threshold), .groups = "keep")

  if(max(p2b_data$redacted_out, na.rm = T) > 0){
    p2a <- p_base +
      geom_col(data = p2b_data, col = "gray20", lwd = 0.2) +
      facet_grid(sex~outcome) +
      theme(legend.position = "none") +
      ylim(c(0, NA))
    
    
  }else{
    p2a <- ggplot() + theme_void()
  } # end 
}else{
  p2 <- ggplot() + theme_void()
  p2a <- ggplot() + theme_void()
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
  mutate(redacted_out = redact_and_round(sum_out, redact_threshold = redact_threshold)) 

# redact data by outcome and sex for 2x2 panel plot
p2c_data <- timeseries_plot %>% 
  mutate(redacted_out = redact_and_round(sum_out, redact_threshold = redact_threshold)) 

if(max(p2c_data$redacted_out, na.rm = T) > 0){
  # make base plot for 2b and 2c  
  p2base <- ggplot(p2c_data, aes(x = date, y = redacted_out, fill = vaccines)) +
    scale_fill_manual(values = colours) +
    labs(x = "Date", y = "Count of new Long COVID codes", fill = "Vaccines received") +
    theme_ali() + 
    theme(strip.background = element_blank())
  
  # plot column in a single panel
  p2b <- p2base +
    geom_col(data = p2b_data, lwd = 0, lty = 0, width = 6) +
    theme(legend.position = "bottom")
  
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
  left_join(longcovid_combined_codelist, by = c("first_lc_code" = "code")) %>% 
  group_by(date, outcome, term) %>% 
  summarise(count = sum(sum_out, na.rm = T), .groups = "keep")
stacked_bar$lc_dx <- ifelse(stacked_bar$outcome == "long COVID Dx", "Dx", "Rx")

## get the 5 most common terms
top5 <- sort(table(stacked_bar$term), decreasing = TRUE)[1:5] %>% names() 

## group everything else in "Other" and reorder according to appearance in the data
stacked_bar$term2 <- ifelse(stacked_bar$term %in% top5, stacked_bar$term, "Other") %>% forcats::fct_inorder()

## relevel the factor variable so "Other" comes at the end
stacked_bar$term2 <- fct_relevel(stacked_bar$term2, "Other", after = Inf)

## set colours for the variables depending on if they're Dx codes or not
# set up a palette of 6 colours of reds (Referral codes)
colours <- hcl.colors(6, palette = "Reds")

# name the vector based on the 5 selected terms (plus "Other") 
names(colours) <- levels(stacked_bar$term2)
print(colours)

# Now find out which terms are associated with Dx (not Rx) and keep the first 30 characters to search for next
dx_terms <- pull(longcovid_combined_codelist[1:2, "term"])
print(dx_terms)

# in the colour palette, find the position of the Dx terms if any exist
dx_col_terms <- str_detect(levels(stacked_bar$term2), paste0(dx_terms, collapse = "|"))
print(dx_col_terms)

# how many Dx terms do we have in this dataset? 
n_cols <- length(dx_col_terms[dx_col_terms == TRUE])

# set up some blue colours for the number of Dx terms in existence
dx_cols <- hcl.colors(n_cols, palette = "Blues")

# assign the blues to the dx terms
colours[dx_col_terms] <- dx_cols

# make Other gray 
colours["Other"] <- "gray60"

## str_wrap the levels labels so they spread over multiple rows in the plot
levels(stacked_bar$term2) <- str_wrap(levels(stacked_bar$term2), width = 40)

# re-assign the names of the colour vector with the new wrapped version
names(colours) <- levels(stacked_bar$term2)
print(colours)

test <- stacked_bar %>% 
  group_by(date) %>% 
  mutate(pct = prop.table(count) * 100) %>% 
  ungroup() 

## need to censor where one code makes up 100% of cases
test$pct[test$pct == 100] <- NA

# make the plot
p2e <- ggplot(test, aes(fill=term2, y=pct, x=date)) + 
  geom_bar(position="fill", stat="identity", width = 6) +
  scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,100,25)) +
  scale_fill_manual(values = colours) +
  guides(
    fill=guide_legend(nrow=3,byrow=TRUE)
  ) +
  labs(x = "Date", y = "% of all new records", fill = "") +
  theme_ali() + 
  theme(strip.background = element_blank(), 
        panel.grid.major.y = element_blank(), 
        legend.position = "bottom")

pdf(here("output/figures/fig2e_longcovid_stacked_dx_rx.pdf"), width = 8, height = 6)
  p2e
dev.off()

# put together the nice figure for the paper ------------------------------
p2_full <- cowplot::plot_grid(
  p2, p2e, p2b, 
  ncol = 1,
  labels = "AUTO"
  )

pdf(here("output/figures/fig2_longcovid_dynamics.pdf"), width = 7, height = 12)
  p2_full
dev.off()

# output the data used for figure 2 ---------------------------------------
write_csv(p2_data, here::here("output/fig2A_data_for_plot.csv"))
write_csv(dplyr::select(p2c_data, -sum_out), here::here("output/fig2B_data_for_plot.csv"))
write_csv(stacked_bar, here::here("output/fig2C_data_for_plot.csv"))

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

