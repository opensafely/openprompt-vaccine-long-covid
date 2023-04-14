library(tidyverse)
library(here)
library(lubridate)

source(here::here("analysis/functions/ggplot_theme.R"))
dir.create(here("output/supplementary"), showWarnings = FALSE, recursive=TRUE)

# figure with random selection of 20 follow-ups ---------------------------
set.seed(5)
n <- 1e5
plot_size <- 10
date_seq <- seq.Date(from = as.Date("2020-11-02"), 
                       to = as.Date("2023-03-31"),
                       by = "day")
start_probs <- dnorm(1:length(date_seq), mean = 47, sd = 167)
end_probs <- dnorm(1:length(date_seq), mean = 836, sd = 154)

fup_data <- tibble(
    newid = 1:n,
    pt_start_date = sample(x = date_seq, size = n, prob = start_probs, replace = TRUE),
    pt_end_date = sample(x = date_seq, size = n, prob = end_probs, replace = TRUE)
    ) %>% 
  mutate(#pt_end_date = pt_start_date + sample(1:(2.2*365), size = n, replace = TRUE),
         t =  pt_start_date %--% pt_end_date / dyears(1),
         vaccine_dose_1_date = as.Date("2021-12-01") + sample(0:365, size = n, replace = TRUE),
         vaccine_dose_2_date = vaccine_dose_1_date + sample(19:180, size = n, replace = TRUE),
         vaccine_dose_3_date = vaccine_dose_2_date + sample(58:365, size = n, replace = TRUE)) %>% 
  filter(t>0)

fup_figure <- fup_data %>% 
  #filter(vaccine_dose_3_date < pt_end_date) %>% 
  sample_n(size = plot_size) %>% 
  mutate(first_lc = pt_end_date) %>% 
  arrange(pt_end_date) %>% 
  mutate(newid = factor(1:plot_size))
fup_figure$first_lc[(0.4*plot_size):plot_size] <- NA

vacc_1_blank <- sample(1:plot_size, size = 0.3*plot_size)
vacc_2_blank <- sample(1:plot_size, size = 0.05*plot_size)
vacc_3_blank <- sample(1:plot_size, size = 0.1*plot_size)

fup_figure$vaccine_dose_1_date[vacc_1_blank] <- NA
fup_figure$vaccine_dose_2_date[c(vacc_1_blank,vacc_2_blank)] <- NA
fup_figure$vaccine_dose_3_date[c(vacc_1_blank, vacc_2_blank, vacc_3_blank)] <- NA

# balnk if later than end date
fup_figure$vaccine_dose_1_date[fup_figure$vaccine_dose_1_date>fup_figure$pt_end_date] <- NA
fup_figure$vaccine_dose_2_date[fup_figure$vaccine_dose_2_date>fup_figure$pt_end_date] <- NA
fup_figure$vaccine_dose_3_date[fup_figure$vaccine_dose_3_date>fup_figure$pt_end_date] <- NA

cols <- c("Start of follow up"="grey",
          "End of follow up"="black",
          "Long COVID record"="red", 
          "Vaccination"="blue")
shapes <- c("Start of follow up"=1,
          "End of follow up"=15,
          "Long COVID record"=3, 
          "Vaccination"=2)

pdf(here("output/supplementary/followup_diagram.pdf"), width = 6, height = 6)
ggplot(fup_figure, aes(xmin = pt_start_date, xmax = pt_end_date, y = newid)) +
  geom_linerange() +
  geom_point(aes(x = pt_start_date, shape = "Start of follow up", col = "Start of follow up"), size = 1.5) +
  geom_point(aes(x = pt_end_date, shape = "End of follow up", col = "End of follow up"), size = 1.5) +
  geom_point(aes(x = first_lc, shape = "Long COVID record", col = "Long COVID record"), size = 1.5) +
  geom_point(aes(x = vaccine_dose_1_date, shape = "Vaccination", col = "Vaccination"), size = 1.5) +
  geom_point(aes(x = vaccine_dose_2_date, shape = "Vaccination", col = "Vaccination"), size = 1.5) +
  geom_point(aes(x = vaccine_dose_3_date, shape = "Vaccination", col = "Vaccination"), size = 1.5) +
  scale_x_date(limits = c(as.Date("2020-11-02"), as.Date("2023-03-31")), date_breaks = "1 year", date_labels = "%Y") + 
  labs(x = "Date", y = "Participants") +
  scale_color_manual(name = "Timepoint", values = cols) +
  scale_shape_manual(name = "Timepoint", values = shapes) +
  theme_ali()
dev.off()  
