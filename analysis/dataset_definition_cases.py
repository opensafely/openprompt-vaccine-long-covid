from databuilder.ehrql import Dataset
from databuilder.tables.beta.tpp import (
  clinical_events
)

from analysis.variable_lib import long_covid_events_during
from datasets import add_common_variables, study_start_date, study_end_date

dataset = Dataset()

lc_dx = long_covid_events_during(study_start_date, study_end_date)

# long covid code
first_lc_dx = lc_dx.sort_by(clinical_events.date).first_for_patient()

add_common_variables(dataset, study_start_date, first_lc_dx.date, population=lc_dx.exists_for_patient())

# add specfic variables

dataset.first_lc_dx = first_lc_dx.date
