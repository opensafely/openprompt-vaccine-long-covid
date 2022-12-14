from databuilder.codes import REGISTRY, Codelist, codelist_from_csv


def codelist(codes, system):
    code_class = REGISTRY[system]
    return Codelist(
        codes={code_class(code) for code in codes},
        category_maps={},
    )


def combine_codelists(*codelists):
    codes = set()
    for codelist in codelists:
        codes.update(codelist.codes)
    return Codelist(codes=codes, category_maps={})


# A variety of plausible long covid codelists:
# https://www.opencodelists.org/codelist/opensafely/nice-managing-the-long-term-effects-of-covid-19/64f1ae69/ 
long_covid_nice_dx = codelist_from_csv(
    "codelists/opensafely-nice-managing-the-long-term-effects-of-covid-19.csv",
    system="snomedct",
    column="code"
)


# https://www.opencodelists.org/codelist/opensafely/referral-and-signposting-for-long-covid/12d06dc0/
long_covid_referral_codes = codelist_from_csv(
    "codelists/opensafely-referral-and-signposting-for-long-covid.csv",
    system="snomedct",
    column="code"
) 


# https://www.opencodelists.org/codelist/opensafely/assessment-instruments-and-outcome-measures-for-long-covid/79c0fa8a/
long_covid_assessment_codes = codelist_from_csv(
    "codelists/opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv",
    system="snomedct",
    column="code"
)

long_covid_combine = combine_codelists(
  long_covid_nice_dx,
  long_covid_referral_codes,
  long_covid_assessment_codes
)

# https://www.opencodelists.org/codelist/opensafely/covid-identification-in-primary-care-probable-covid-sequelae/2020-07-16/
covid_primary_care_sequelae = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
    system="ctv3",
    column="CTV3ID",
)

# some demographic codelists: 
ethnicity = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
)

# adminstered vaccine codes
vac_adm_1 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm1.csv",
  system="snomedct",
  column="code"
)
vac_adm_2 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm2.csv",
  system="snomedct",
  column="code"
)
vac_adm_3 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm3_cod.csv",
  system="snomedct",
  column="code"
)
vac_adm_4 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm4_cod.csv",
  system="snomedct",
  column="code"
)
vac_adm_5 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm5_cod.csv",
  system="snomedct",
  column="code"
)

vac_adm_combine = combine_codelists(
  vac_adm_1,
  vac_adm_2,
  vac_adm_3,
  vac_adm_4,
  vac_adm_5
)
