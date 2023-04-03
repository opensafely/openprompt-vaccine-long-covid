library(tidyverse)
library(lubridate)

vacc_names <-
  c(
    "Comirnaty COVID-19 mRNA Vacc ready to use 0.3ml in md vials",
    "Comirnaty Original/Omicron BA.1 COVID-19 Vacc md vials",
    "COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)",
    "COVID-19 mRNA Vaccine Comirnaty Children 5-11yrs 10mcg/0.2ml dose con for disp for inj MDV (Pfizer)",
    "COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)",
    "COVID-19 Vac AZD2816 (ChAdOx1 nCOV-19) 3.5x10*9 viral part/0.5ml dose sol for inj MDV (AstraZeneca)",
    "COVID-19 Vac CoronaVac (adjuvanted) 600U/0.5ml dose susp for inj vials",
    "COVID-19 Vac Covaxin (NIV-2020-770 inactivated) micrograms/0.5ml dose susp for inj MDV",
    "COVID-19 Vac Covishield (ChAdOx1 S recombinant) 5x10*9 viral particles/0.5ml dose sol for in MDV",
    "COVID-19 Vac Covovax (adjuvanted) micrograms/0.5ml dose susp for inj MDV (Serum Institute of India)",
    "COVID-19 Vac Nuvaxovid (recombinant, adj) micrograms/0.5ml dose susp for inj MDV (Novavax CZ a.s.)",
    "COVID-19 Vac Sanofi (Cov2 preS dM monovalent D614 (recombinant)) 5mcg/0.5ml dose susp for inj MDV",
    "COVID-19 Vac Sinopharm BIBP (inactivated adjuvanted) 6.5U/0.5ml dose susp for inj vials",
    "COVID-19 Vac Spikevax (Zero) /(Omicron) in md vials",
    "COVID-19 Vac Sputnik V Component I 0.5ml multidose vials",
    "COVID-19 Vacc Sputnik V Component II 0.5ml multidose vials",
    "COVID-19 Vaccine Convidecia 0.5ml in vials",
    "COVID-19 Vaccine Jansen (Ad26.COV2-S (recomb)) 0.5ml dose solution for injection multidose vials",
    "COVID-19 Vaccine Medicago (CoVLP) 3.75micrograms/0.5ml dose emulsion for injection multidose vials",
    "COVID-19 Vaccine Moderna (mRNA-1273.529) 50micrograms/0.25ml dose sol for in MOV",
    "COVID-19 Vaccine Sputnik V Component I 0.5ml inj vials",
    "COVID-19 Vaccine Sputnik V Component II 0.5ml inj vials",
    "COVID-19 Vaccine Valneva (inactivated adj whole virus) 40antigen units/0.5ml dose susp for inj MDV",
    "COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)")
    #NA
#  )
vacc_weights <- rep(0.2/21, 24)
vacc_weights[vacc_names == "COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)"] <- 0.35
vacc_weights[vacc_names == "COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)"] <- 0.4
vacc_weights[vacc_names == "COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)"] <- 0.05
#vacc_weights[is.na(vacc_names)] <- 0.1
sum(vacc_weights)

if(!exists("data_size")){data_size = 2000}

set.seed(4214)
test_mrna_code <- data.frame(
  patient_id = 1:data_size, 
  vaccine_dose_1_manufacturer = sample(vacc_names, size = data_size, replace = TRUE, prob = vacc_weights),
  vaccine_dose_2_manufacturer = sample(vacc_names, size = data_size, replace = TRUE, prob = vacc_weights),
  vaccine_dose_3_manufacturer = sample(vacc_names, size = data_size, replace = TRUE, prob = vacc_weights)
) %>% mutate(
  vaccine_dose_2_manufacturer = ifelse(is.na(vaccine_dose_1_manufacturer), NA, vaccine_dose_2_manufacturer),
  vaccine_dose_3_manufacturer = ifelse(is.na(vaccine_dose_2_manufacturer), NA, vaccine_dose_3_manufacturer),
  no_prev_vacc_interim = as.numeric(!is.na(vaccine_dose_1_manufacturer)) +
    as.numeric(!is.na(vaccine_dose_2_manufacturer)) +
    as.numeric(!is.na(vaccine_dose_3_manufacturer)),
  no_prev_vacc = no_prev_vacc_interim + sample(0:2, size = data_size, replace = TRUE),
  no_prev_vacc = ifelse(no_prev_vacc_interim==0,0,no_prev_vacc),
  vaccine_dose_1_date = as.Date("2020-11-01") + sample(0:365, size = data_size, replace = TRUE),
  vaccine_dose_2_date = vaccine_dose_1_date + sample(19:180, size = data_size, replace = TRUE),
  vaccine_dose_3_date = vaccine_dose_2_date + sample(58:365, size = data_size, replace = TRUE)
)