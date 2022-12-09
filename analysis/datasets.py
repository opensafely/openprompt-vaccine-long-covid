# where EHRQl is defined. Only really need Dastaset, tehe others are specific
from databuilder.ehrql import Dataset, days, years, case, when
# this is where we import the schema to run the study with
from databuilder.tables.beta.tpp import (
  patients,
  practice_registrations,
  clinical_events,
  vaccinations,
  sgss_covid_all_tests
)

from variable_lib import (
  age_as_of,
  has_died,
  address_as_of,
  create_sequential_variables
)
import codelists

study_start_date = "2020-11-01"
study_end_date = "2022-11-01"

minimum_registration = 90  # ~3 months of previous registration

def add_common_variables(dataset, study_start_date, end_date, population):
    # practice registration selection
    registrations = practice_registrations \
        .drop(practice_registrations.start_date > study_start_date - days(minimum_registration)) \
        .drop(practice_registrations.end_date <= study_start_date)
    # get the number of registrations in this period to exclude anyone with >1 in the `set_population` later
    registrations_number = registrations.count_for_patient()

    # need to get the start and end date of last registration only
    registration = registrations \
        .sort_by(practice_registrations.start_date).last_for_patient()
    dataset.uts = case(
        when(registration.end_date.is_null()).then(end_date),
        when(registration.end_date > end_date).then(end_date),
        default=registration.end_date,
    )

    population = population & (registrations_number == 1)
    dataset.set_population(population)  # when run this then 50% don't have registration as default? Dataset has 500 rows from expectation = 1000
