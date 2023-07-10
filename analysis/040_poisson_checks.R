library(tidyverse)
library(broom)
library(lubridate)
library(here)

source(here::here("analysis/functions/redaction.R"))
source(here::here("analysis/functions/poisson_regressions.R"))

dir.create(here::here("output/data_properties/poisson_checks/"), showWarnings = FALSE, recursive=TRUE)

## import clean flat data
clean_data <- arrow::read_parquet(here::here("output/clean_dataset.gz.parquet")) %>% 
  dplyr::select(patient_id, starts_with("pt"), age, age_cat, sex, region = practice_nuts, lc_out, lc_dx_only, no_prev_vacc)

## import time_updated_data - all long COVID
time_data_lc_all <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_all.gz.parquet")) %>% 
  dplyr::select(patient_id, starts_with("pt"), t, age, age_cat, sex, lc_out = out, no_prev_vacc = vaccines, t_vacc_mrna) %>% 
  mutate(lc_dx_only = lc_out) %>% 
  left_join(dplyr::select(clean_data, patient_id, region), by = "patient_id")

## import time_updated_data - long COVID diagnoses
time_data_lc_dx <- arrow::read_parquet(here::here("output/timeupdate_dataset_lc_dx.gz.parquet")) %>% 
  dplyr::select(patient_id, starts_with("pt"), t, age, age_cat, sex, lc_dx_only = out, no_prev_vacc = vaccines, t_vacc_mrna) %>% 
  mutate(lc_out = lc_dx_only) %>% 
  left_join(dplyr::select(clean_data, patient_id, region), clean_data, by = "patient_id")

##
check_poisson <- function(data_in, stub_name){
  y <- data_in$lc_out
  z <- data_in$lc_dx_only
  
  out1 <- c( mean(y), var(y) )
  out2 <- c( mean(z), var(z) )
  
  ## but the assumption in the Poisson model is that the mean and variance are equal 
  ## conditional on the covariates in th model 
  ## So compare mean and var for lc_dx_only within sex strata
  
  yf <- y[data_in$sex == "female"]
  ym <- y[data_in$sex == "male"]
  outf <- c( mean(yf), var(yf) )
  outm <- c( mean(ym), var(ym) )
  
  ## and by vaccine
  y0 <- y[data_in$no_prev_vacc == "0"]
  y1 <- y[data_in$no_prev_vacc == "1"]
  y2 <- y[data_in$no_prev_vacc == "2"]
  y3 <- y[data_in$no_prev_vacc == "3+"]
  
  out0 <- c( mean(y0), var(y0) )
  out1 <- c( mean(y1), var(y1) )
  out2 <- c( mean(y2), var(y2) )
  out3 <- c( mean(y3), var(y3) )
  
  capture.output(
    cat(
      "\n",
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
    file = paste0(here("output/data_properties/poisson_checks/"), stub_name, ".txt")
  )
  
  # do the mean and var by all covariates -----------------------------------
  conditional_poisson_check <- data_in %>% 
    group_by(sex, age_cat, region, no_prev_vacc) %>% 
    summarise(
      mu1 = mean(lc_out),
      var1 = var(lc_out),
      mu2 = mean(lc_dx_only),
      var2 = var(lc_dx_only),
      .groups = "keep"
      ) %>% 
    ungroup() %>% 
    mutate_if(is.numeric, ~signif(., digits = 3))
  write.csv(conditional_poisson_check, paste0(here("output/data_properties/poisson_checks/"), stub_name, ".csv"))
}

check_poisson(data_in = clean_data, stub_name = "clean_data_poisson_checks")
check_poisson(data_in = time_data_lc_all, stub_name = "time_data_lc_first_poisson_checks")
check_poisson(data_in = time_data_lc_dx, stub_name = "time_data_lc_dx_poisson_checks")


# plot distribution of counts ---------------------------------------------
## convert to data.table and rename the outcome var as `out`
dt_poisson_lc_all <- data.table::setDT(time_data_lc_all)
colnames(dt_poisson_lc_all)[colnames(dt_poisson_lc_all)=="lc_out"] <- "out"

dt_poisson_lc_dx <- data.table::setDT(time_data_lc_dx)
colnames(dt_poisson_lc_dx)[colnames(dt_poisson_lc_dx)=="lc_dx_only"] <- "out"

# little function to draw a histogram
poisson_hist <- function(DT, stratifier){
  dt_name <- deparse(substitute(DT))
  
  col <- ifelse(stringr::str_detect(dt_name, "lc_dx"), 
                rgb(0,0.2,0.5, alpha = 0.4),
                rgb(0.8,0.5,0.2, alpha = 0.4))
  
  dt_summ <- DT[, .(out=sum(out), t=sum(t)),
     by = c(stratifier, "age_cat", "sex", "region")]
  
  hist(dt_summ$out, breaks = 20,
       main = eval(stratifier),
       xlab = "No. with outcome",
       col = col)
}

pdf(here::here("output/data_properties/poisson_checks/poisson_hist.pdf"), 12, 5)
par(mfrow=c(2,4))
  # any long covid code
  poisson_hist(dt_poisson_lc_all, "age_cat")
  legend("topright", c("any long COVID"), col = rgb(0.8,0.5,0.2, alpha = 1), lty = 1, lwd = 2, bty = "n")
  poisson_hist(dt_poisson_lc_all, "sex")
  poisson_hist(dt_poisson_lc_all, "no_prev_vacc")
  poisson_hist(dt_poisson_lc_all, "t_vacc_mrna")
  # lc diagnoses only
  poisson_hist(dt_poisson_lc_dx, "age_cat")
  legend("topright", c("long COVID diagnoses"), col = rgb(0,0.2,0.5, alpha = 1), lty = 1, lwd = 2, bty = "n")
  poisson_hist(dt_poisson_lc_dx, "sex")
  poisson_hist(dt_poisson_lc_dx, "no_prev_vacc")
  poisson_hist(dt_poisson_lc_dx, "t_vacc_mrna")
dev.off()
