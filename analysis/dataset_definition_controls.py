from databuilder.ehrql import Dataset

from analysis.variable_lib import long_covid_events_during
from datasets import add_common_variables, study_start_date, study_end_date

dataset = Dataset()

lc_dx = long_covid_events_during(study_start_date, study_end_date)

# work out the end date
add_common_variables(dataset, study_start_date, study_end_date,
                     population=~lc_dx.exists_for_patient())

# add specfic variables ???
