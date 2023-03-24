from databuilder.ehrql import Dataset, case, when
import codelists

from variable_lib import long_covid_events_during, long_covid_dx_during
from datasets import add_common_variables, study_start_date, study_end_date

dataset = Dataset()

lc_code_any = long_covid_events_during(study_start_date, study_end_date)
lc_dx_code = long_covid_dx_during(study_start_date, study_end_date)

# long covid code
first_lc_code_any = lc_code_any.sort_by(lc_code_any.date).first_for_patient()
first_lc_dx_code = lc_dx_code.sort_by(lc_dx_code.date).first_for_patient()

# use the record of a patient's long covid on Dx (even if they had a Rx earlier)
first_lc_code = first_lc_code_any
first_lc_code.best_date = case(
    when((first_lc_code_any.date - first_lc_dx_code.date).days > 0).then(first_lc_dx_code.date),
    default=first_lc_code_any.date
)
first_lc_code.best_code = case(
    when((first_lc_code_any.date - first_lc_dx_code.date).days > 0).then(first_lc_dx_code.snomedct_code),
    default=first_lc_code_any.snomedct_code
)

# create flag about whether this is a diagnosis or a referral code
# default = NULL
lc_dx_flag = case(
  when(first_lc_code.best_code.is_in(codelists.long_covid_nice_dx)).then("Dx"),
  when(first_lc_code.best_code.is_in(codelists.long_covid_assessment_codes)).then("Rx"),
  when(first_lc_code.best_code.is_in(codelists.long_covid_referral_codes)).then("Rx")
)

add_common_variables(dataset, study_start_date, first_lc_code.best_date, population=first_lc_code.exists_for_patient())

# add specfic variables
dataset.first_lc_dx = first_lc_code.best_date
dataset.lc_dx_flag = lc_dx_flag
dataset.test_to_lc_dx_gap = (first_lc_code.best_date - dataset.latest_test_before_diagnosis).days
dataset.vacc_to_lc_dx_gap = (first_lc_code.best_date - dataset.date_last_vacc).days
