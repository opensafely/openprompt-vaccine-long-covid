# where EHRQl is defined. Only really need Dastaset, tehe others are specific
from databuilder.ehrql import days, case, when
# this is where we import the schema to run the study with
from databuilder.tables.beta.tpp import (
  patients,
  practice_registrations,
  clinical_events,
  vaccinations,
  ons_deaths,
  sgss_covid_all_tests,
  hospital_admissions
)
import datetime

from variable_lib import (
  age_as_of,
  has_died,
  has_prior_event,
  address_as_of,
  has_prior_event_ctv3_numeric,
  create_sequential_variables,
  hospitalisation_diagnosis_matches
)
import codelists

study_start_date = datetime.date(2020, 11, 1)
study_end_date = datetime.date(2023, 3, 31)

minimum_registration = 90  # ~3 months of previous registration
covid_to_longcovid_lag = 84  # 12 weeks
vaccine_effective_lag = 14  # 2 weeks for vaccine to become effective
vaccine_to_longcovid_lag = covid_to_longcovid_lag + vaccine_effective_lag


def add_common_variables(dataset, study_start_date, end_date, population):
    # practice registration selection
    registrations = practice_registrations \
        .where(practice_registrations.start_date <= study_start_date - days(minimum_registration)) \
        .except_where(practice_registrations.end_date <= study_start_date)

    # get the number of registrations in this period to exclude anyone with >1 in the `set_population` later
    registrations_number = registrations.count_for_patient()

    # need to get the start and end date of last registration only
    registration = registrations \
        .sort_by(practice_registrations.start_date).last_for_patient()

    dataset.pt_start_date = case(
        when(registration.start_date + days(minimum_registration) > study_start_date).then(registration.start_date + days(minimum_registration)),
        default=study_start_date,
    )

    dataset.pt_end_date = case(
        when(registration.end_date.is_null()).then(end_date),
        when(registration.end_date > end_date).then(end_date),
        default=registration.end_date,
    )

    # Demographic variables ------------------------------------------------------------
    dataset.sex = patients.sex
    dataset.age = age_as_of(study_start_date)
    dataset.has_died = has_died(study_start_date)
    dataset.msoa = address_as_of(study_start_date).msoa_code
    dataset.practice_nuts = registration.practice_nuts1_region_name
    dataset.imd = address_as_of(study_start_date).imd_rounded

    # death ------------------------------------------------------------
    dataset.death_date = patients.date_of_death
    ons_deathdata = ons_deaths \
        .sort_by(ons_deaths.date).last_for_patient()
    dataset.ons_death_date = ons_deathdata.date

    # Ethnicity in 6 categories ------------------------------------------------------------
    dataset.ethnicity = clinical_events.where(clinical_events.ctv3_code.is_in(codelists.ethnicity)) \
        .sort_by(clinical_events.date) \
        .last_for_patient() \
        .ctv3_code.to_category(codelists.ethnicity)

    # covid tests ------------------------------------------------------------
    dataset.latest_test_before_diagnosis = sgss_covid_all_tests \
        .where(sgss_covid_all_tests.is_positive) \
        .except_where(sgss_covid_all_tests.specimen_taken_date >= dataset.pt_end_date - days(covid_to_longcovid_lag)) \
        .sort_by(sgss_covid_all_tests.specimen_taken_date).last_for_patient().specimen_taken_date

    dataset.all_test_positive = sgss_covid_all_tests \
        .where(sgss_covid_all_tests.is_positive) \
        .except_where(sgss_covid_all_tests.specimen_taken_date <= study_start_date) \
        .except_where(sgss_covid_all_tests.specimen_taken_date >= dataset.pt_end_date - days(covid_to_longcovid_lag)) \
        .count_for_patient()

    # covid hospitalisation ------------------------------------------------------------
    covid_hospitalisations = hospitalisation_diagnosis_matches(hospital_admissions, codelists.hosp_covid)
    all_covid_hosp = covid_hospitalisations \
        .except_where(covid_hospitalisations.admission_date >= dataset.pt_end_date - days(covid_to_longcovid_lag))

    dataset.all_covid_hosp = all_covid_hosp \
        .count_for_patient()

    first_covid_hosp = all_covid_hosp \
        .sort_by(all_covid_hosp.admission_date) \
        .first_for_patient()

    dataset.first_covid_hosp = first_covid_hosp.admission_date
    dataset.first_covid_discharge = first_covid_hosp.discharge_date
    dataset.first_covid_critical = first_covid_hosp.days_in_critical_care > 0
    dataset.first_covid_hosp_primary_dx = first_covid_hosp.primary_diagnoses.is_in(codelists.hosp_covid)

    # Any covid identification ------------------------------------------------------------
    primarycare_covid = clinical_events \
        .where(clinical_events.ctv3_code.is_in(codelists.any_primary_care_code)) \
        .except_where(clinical_events.date >= dataset.pt_end_date - days(covid_to_longcovid_lag))

    dataset.latest_primarycare_covid = primarycare_covid \
        .sort_by(primarycare_covid.date) \
        .last_for_patient().date

    dataset.total_primarycare_covid = primarycare_covid \
        .count_for_patient()

    # vaccine code - get date of first 5 doses ------------------------------------------------------------
    create_sequential_variables(
      dataset,
      "covid_vacc_{n}_adm",
      num_variables=5,
      events=clinical_events.where(clinical_events.snomedct_code.is_in(codelists.vac_adm_combine)),
      column="date"
    )

    # Vaccines from the vaccines schema
    # only take one record per day to remove duplication
    all_vacc = vaccinations \
        .where(vaccinations.date < dataset.pt_end_date - days(vaccine_to_longcovid_lag)) \
        .where(vaccinations.target_disease == "SARS-2 CORONAVIRUS")

    # this will be replaced with distinct_count_for_patient() once it is developed
    dataset.no_prev_vacc = all_vacc \
        .sort_by(vaccinations.date) \
        .count_for_patient()
    dataset.date_last_vacc = all_vacc.sort_by(all_vacc.date).last_for_patient().date
    dataset.last_vacc_gap = (dataset.pt_end_date - dataset.date_last_vacc).days

    # FIRST VACCINE DOSE ------------------------------------------------------------
    # first vaccine dose was 8th December 2020
    vaccine_dose_1 = all_vacc \
        .where(all_vacc.date.is_after(datetime.date(2020, 12, 7))) \
        .sort_by(all_vacc.date) \
        .first_for_patient()
    dataset.vaccine_dose_1_date = vaccine_dose_1.date
    dataset.vaccine_dose_1_manufacturer = vaccine_dose_1.product_name

    # SECOND VACCINE DOSE ------------------------------------------------------------
    # first recorded 2nd dose was 29th December 2020
    # need a 19 day gap from first dose
    vaccine_dose_2 = all_vacc \
        .where(all_vacc.date.is_after(datetime.date(2020, 12, 28))) \
        .where(all_vacc.date.is_after(dataset.vaccine_dose_1_date + days(19))) \
        .sort_by(all_vacc.date) \
        .first_for_patient()
    dataset.vaccine_dose_2_date = vaccine_dose_2.date
    dataset.vaccine_dose_2_manufacturer = vaccine_dose_2.product_name

    # BOOSTER VACCINE DOSE -----------------------------------------------------------
    # first recorded booster dose was 2021-09-24
    # need a 56 day gap from first dose (conservative)
    vaccine_dose_3 = all_vacc \
        .where(all_vacc.date.is_after(datetime.date(2021, 9, 23))) \
        .where(all_vacc.date.is_after(dataset.vaccine_dose_2_date + days(56))) \
        .sort_by(all_vacc.date) \
        .first_for_patient()
    dataset.vaccine_dose_3_date = vaccine_dose_3.date
    dataset.vaccine_dose_3_manufacturer = vaccine_dose_3.product_name

    # comorbidities ------------------------------------------------------------------
    diabetes_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.diabetes_codes)
    haem_cancer_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.haem_cancer_codes)
    lung_cancer_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.lung_cancer_codes)
    other_cancer_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.other_cancer_codes)
    asthma_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.asthma_codes)
    chronic_cardiac_disease_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.chronic_cardiac_disease_codes)
    chronic_liver_disease_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.chronic_liver_disease_codes)
    chronic_respiratory_disease_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.chronic_respiratory_disease_codes)
    other_neuro_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.other_neuro_codes)
    stroke_gp_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.stroke_gp_codes)
    dementia_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.dementia_codes)
    ra_sle_psoriasis_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.ra_sle_psoriasis_codes)
    psychosis_schizophrenia_bipolar_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.psychosis_schizophrenia_bipolar_codes)
    permanent_immune_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.permanent_immune_codes)
    temp_immune_codes = has_prior_event_ctv3_numeric(dataset.pt_start_date, codelists.temp_immune_codes)

    dataset.comorbid_count = diabetes_codes + \
        haem_cancer_codes + \
        lung_cancer_codes + \
        other_cancer_codes + \
        asthma_codes + \
        chronic_cardiac_disease_codes + \
        chronic_liver_disease_codes + \
        chronic_respiratory_disease_codes + \
        other_neuro_codes + \
        stroke_gp_codes + \
        dementia_codes + \
        ra_sle_psoriasis_codes + \
        psychosis_schizophrenia_bipolar_codes + \
        permanent_immune_codes + \
        temp_immune_codes

    # negative control - hospital fractures ------------------------------------------------------------
    fracture_hospitalisations = hospitalisation_diagnosis_matches(hospital_admissions, codelists.hosp_fractures)

    dataset.first_fracture_hosp = fracture_hospitalisations \
        .where(fracture_hospitalisations.admission_date.is_between(study_start_date, end_date)) \
        .sort_by(fracture_hospitalisations.admission_date) \
        .first_for_patient().admission_date

    # care home flag ------------------------------------------------------------
    dataset.care_home = address_as_of(dataset.pt_start_date) \
        .care_home_is_potential_match.if_null_then(False)

    dataset.care_home_nursing = address_as_of(dataset.pt_start_date) \
        .care_home_requires_nursing.if_null_then(False)

    dataset.care_home_code = has_prior_event(dataset.pt_start_date, codelists.care_home_flag)

    # shielding data ------------------------------------------------------------
    dataset.highrisk_shield = has_prior_event(dataset.pt_start_date, codelists.high_risk_shield)
    dataset.lowrisk_shield = has_prior_event(dataset.pt_start_date, codelists.low_risk_shield)

    # EXCLUSION criteria - gather these all here to remain consistent with the protocol
    # will remove missing age and anyone not male/female
    population = population & (registrations_number == 1) & (dataset.age <= 100) & (dataset.age >= 18) & (dataset.sex.contains("male"))

    dataset.define_population(population)
