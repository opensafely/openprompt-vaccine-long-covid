from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA


study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },
    population=patients.registered_with_one_practice_between(
        "2019-02-01", "2022-08-01"
    ),
    prefup_4mo=patients.with_complete_gp_consultation_history_between(
      start_date = "2018-11-01", 
      end_date = "2019-01-31",
      return_expectations={"incidence": 0.96}
    ),
    age=patients.age_as_of(
        "2019-09-01",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"female": 0.51, "male": 0.49}},
        },
    ),
)   
