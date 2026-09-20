/********************************************************************
 COMPETING-RISK ANALYSIS

 Example:
 First migration from an urban area

 Competing destinations:
     Urban → Rural
     Urban → Urban
********************************************************************/

*--------------------------------------------------
* Define competing events
*--------------------------------------------------

gen migration_destination = internal_migration

bysort id (date episode): ///
    replace migration_destination = 2 ///
    if next_residence == 4 & internal_migration == 1

label define destination ///
    0 "Censored" ///
    1 "Urban" ///
    2 "Rural"

label values migration_destination destination


*--------------------------------------------------
* Population at risk
*--------------------------------------------------

stset date [pw=weight], ///
    id(id) ///
    failure(internal_migration == 1) ///
    time0(start_date) ///
    origin(time birth_year) ///
    entry(time birth_year + 15)

gen at_risk_first_migration = _st


*--------------------------------------------------
* Competing-risk framework
*--------------------------------------------------

stset date [pw=weight], ///
    id(id) ///
    failure(migration_destination == 2) ///
    if at_risk_first_migration == 1 & origin_urban == 1 ///
    time0(start_date) ///
    origin(time birth_year) ///
    entry(time birth_year + 15)


* Cumulative incidence function
stcompet CIF = ci, compet1(1)


* Fine–Gray competing-risk regression
stcrreg ///
    i.education_tv ///
    i.residence_type ///
    i.cohort, ///
    compete(migration_destination == 1)