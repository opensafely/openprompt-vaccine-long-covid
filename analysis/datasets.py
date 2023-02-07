# where EHRQl is defined. Only really need Dastaset, tehe others are specific
from databuilder.ehrql import days, case, when
# this is where we import the schema to run the study with
from databuilder.tables.beta.tpp import (
  patients,
  practice_registrations,
  clinical_events,
  vaccinations,
  sgss_covid_all_tests,
  hospital_admissions
)
import datetime

from variable_lib import (
  age_as_of,
  has_died,
  address_as_of,
  create_sequential_variables
)
import codelists

study_start_date = datetime.date(2020, 11, 1)
study_end_date = datetime.date(2022, 11, 1)

minimum_registration = 90  # ~3 months of previous registration
covid_to_longcovid_lag = 84  # 12 weeks
vaccine_effective_lag = 14  # 2 weeks for vaccine to become effective
vaccine_to_longcovid_lag = covid_to_longcovid_lag + vaccine_effective_lag


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

    # need to find out if they had a hospitalisation for covid and censor then
    # note (18/01): needs looking at before I can use it. 
    #hospitalised = hospital_admissions.take(hospital_admissions.all_diagnoses.contains(codelists.covidhosp))
    # then add into the end_date definition below (or as a secondary end date for sensitivity analysis?)     

    dataset.pt_end_date = case(
        when(registration.end_date.is_null()).then(end_date),
        when(registration.end_date > end_date).then(end_date),
        default=registration.end_date,
    )

    # Demographic variables
    dataset.sex = patients.sex
    dataset.age = age_as_of(study_start_date)
    dataset.msoa = address_as_of(study_start_date).msoa_code
    dataset.imd = address_as_of(study_start_date).imd_rounded

    # Ethnicity in 6 categories
    dataset.ethnicity = clinical_events.take(clinical_events.ctv3_code.is_in(codelists.ethnicity)) \
        .sort_by(clinical_events.date) \
        .last_for_patient() \
        .ctv3_code.to_category(codelists.ethnicity.Grouping_6)

    # covid tests
    dataset.latest_test_before_diagnosis = sgss_covid_all_tests \
        .take(sgss_covid_all_tests.is_positive) \
        .drop(sgss_covid_all_tests.specimen_taken_date >= end_date - days(covid_to_longcovid_lag)) \
        .sort_by(sgss_covid_all_tests.specimen_taken_date).last_for_patient().specimen_taken_date

    dataset.all_test_positive = sgss_covid_all_tests \
        .take(sgss_covid_all_tests.is_positive) \
        .drop(sgss_covid_all_tests.specimen_taken_date <= study_start_date) \
        .drop(sgss_covid_all_tests.specimen_taken_date >= end_date - days(covid_to_longcovid_lag)) \
        .count_for_patient()

    # vaccine code
    create_sequential_variables(
      dataset,
      "covid_vax_{n}_adm",
      num_variables=5,
      events=clinical_events.take(clinical_events.snomedct_code.is_in(codelists.vac_adm_combine)),
      column="date"
    )

    # Vaccines from the vaccines schema
    all_vacc = vaccinations \
        .take(vaccinations.date < end_date - days(vaccine_to_longcovid_lag)) \
        .take(vaccinations.target_disease == "SARS-2 CORONAVIRUS")

    dataset.no_prev_vacc = all_vacc.count_for_patient()
    dataset.date_last_vacc = all_vacc.sort_by(all_vacc.date).last_for_patient().date

    population = population & (registrations_number == 1)
    dataset.set_population(population)
