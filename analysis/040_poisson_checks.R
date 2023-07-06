library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/poisson_regressions.R"))

dir.create(here::here("output/data_properties"), showWarnings = FALSE, recursive=TRUE)

## import clean flat data
clean_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet")) %>% 
  dplyr::select(patient_id, starts_with("pt"), age, age_cat, sex, lc_out, lc_dx_only, no_prev_vacc)

y <- clean_data$lc_out
z <- clean_data$lc_dx_only

out1 <- c( mean(y), var(y) )
out2 <- c( mean(z), var(z) )

## but the assumption in the Poisson model is that the mean and variance are equal 
## conditional on the covariates in th model 
## So compare mean and var for lc_dx_only within sex strata

yf <- y[clean_data$sex == "female"]
ym <- y[clean_data$sex == "male"]
outf <- c( mean(yf), var(yf) )
outm <- c( mean(ym), var(ym) )

## and by vaccine
y0 <- y[clean_data$no_prev_vacc == "0"]
y1 <- y[clean_data$no_prev_vacc == "1"]
y2 <- y[clean_data$no_prev_vacc == "2"]
y3 <- y[clean_data$no_prev_vacc == "3+"]

out0 <- c( mean(y0), var(y0) )
out1 <- c( mean(y1), var(y1) )
out2 <- c( mean(y2), var(y2) )
out3 <- c( mean(y3), var(y3) )

capture.output(
  cat(
    c("mean", "variance"),"\n",
    "Any long COVID","\n",
    out1,"\n",
    "Long COVID dx","\n",
    out2,"\n",
    "Females","\n",
    outf,"\n",
    "Males","\n",
    outm, "\n",
    "0 vaccine", "\n",
    out0,"\n",
    "1 vaccine", "\n",
    out1,"\n",
    "2 vaccine", "\n",
    out2,"\n",
    "3+ vaccine", "\n",
    out3
  ),
  file = here::here("output/data_properties/poisson_checks.txt")
)

# do the mean and var by all covariates -----------------------------------
conditional_poisson_check <- clean_data %>% 
  group_by(sex, age_cat, no_prev_vacc) %>% 
  summarise(
    mu1 = mean(lc_out),
    var1 = var(lc_out),
    mu2 = mean(lc_dx_only),
    var2 = var(lc_dx_only),
    .groups = "keep"
    ) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~signif(., digits = 3))
write.csv(conditional_poisson_check, here::here("output/data_properties/conditional_poisson_check.csv"))
