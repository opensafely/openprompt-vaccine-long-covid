from datetime import date

from databuilder.ehrql import Dataset, case, codelist_from_csv, when
from databuilder.tables.beta.tpp import patients, practice_registrations, clinical_events

from variable_lib import (
  age_as_of, 
  has_died
)
import codelists

# Set index date
index_date = date(2022, 6, 1)

dataset = Dataset()

# Demographic variables
dataset.sex = patients.sex
dataset.age = age_as_of(index_date)
dataset.has_died = has_died(index_date)
# Ethnicity in 6 categories
dataset.ethnicity = (
    clinical_events.take(clinical_events.ctv3_code.is_in(codelists.ethnicity))
    .sort_by(clinical_events.date)
    .last_for_patient()
    .ctv3_code.to_category(codelists.ethnicity.Grouping_6)
)

# long covid code
lc_dxs = clinical_events.take(clinical_events.snomedct_code.is_in(codelists.long_covid_combine))
first_lc_dx = lc_dxs.sort_by(clinical_events.date).first_for_patient()

# vaccine code
# create_sequential_variables(
#   dataset, 
#   "covid_vax_{n}_adm", 
#   num_variables=5,
#   events=clinical_events.take(clinical_events.snomedct_code.is_in(vac_adm_combine)) 
# )

# has had one vaccine code
vac_adm1 = clinical_events.take(clinical_events.snomedct_code.is_in(codelists.vac_adm_1)) 
first_vac_adm1 = vac_adm1.sort_by(clinical_events.date).first_for_patient()

dataset.set_population(practice_registrations.exists_for_patient()) # when run this then 50% don't have registration as default? Dataset has 500 rows from expectation = 1000
dataset.first_vac_adm1 = first_vac_adm1.date
dataset.first_lc_dx = first_lc_dx.date
