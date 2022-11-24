from datetime import date

# where EHRQl is defined. Only really need Dastaset, tehe others are specific
from databuilder.ehrql import Dataset, case, codelist_from_csv, when, days
# this is where we import the schema to run the study with
from databuilder.tables.beta.tpp import patients, practice_registrations, clinical_events, vaccinations

from variable_lib import (
  age_as_of, 
  has_died,
  address_as_of,
  create_sequential_variables
)
import codelists

# Set index date
index_date = "2022-01-01" # can just hardcode as string format instead of date() from datetime. But need that package for operations on dates 
start_of_omicron_epoch = "2022-01-01"
covid_to_longcovid_lag = 84 # 12 weeks
vaccine_effective_lag = 14 # 2 weeks for vaccine to become effective
vaccine_to_longcovid_lag = covid_to_longcovid_lag + vaccine_effective_lag

dataset = Dataset()

# long covid code
first_lc_dx = clinical_events.take(clinical_events.snomedct_code.is_in(codelists.long_covid_combine)) \
  .take(clinical_events.date >= start_of_omicron_epoch + days(covid_to_longcovid_lag)) \
  .sort_by(clinical_events.date).first_for_patient()

# Demographic variables
dataset.sex = patients.sex
dataset.age = age_as_of(first_lc_dx.date)
dataset.has_died = has_died(first_lc_dx.date)
dataset.msoa = address_as_of(first_lc_dx.date).msoa_code
dataset.imd = address_as_of(first_lc_dx.date).imd_rounded

# Ethnicity in 6 categories
dataset.ethnicity = (
    clinical_events.take(clinical_events.ctv3_code.is_in(codelists.ethnicity))
    .sort_by(clinical_events.date)
    .last_for_patient()
    .ctv3_code.to_category(codelists.ethnicity.Grouping_6)
)

# vaccine code
create_sequential_variables(
  dataset, 
  "covid_vax_{n}_adm", 
  num_variables=5,
  events=clinical_events.take(clinical_events.snomedct_code.is_in(codelists.vac_adm_combine)),
  column="date"
)

# Vaccines from the vaccines schema
all_vacc = vaccinations.take(vaccinations.date < first_lc_dx.date - days(vaccine_to_longcovid_lag)) 
dataset.no_prev_vacc = all_vacc.count_for_patient()
dataset.date_last_vacc = all_vacc.sort_by(all_vacc.date).last_for_patient().date

## set pop and define vars
dataset.set_population(practice_registrations.exists_for_patient()) # when run this then 50% don't have registration as default? Dataset has 500 rows from expectation = 1000
dataset.first_lc_dx = first_lc_dx.date
