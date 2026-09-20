/********************************************************************
 EVENT-HISTORY DATA PREPARATION

 Purpose:
 Prepare residential biographies for survival analysis of
 internal migration.

 Data are not distributed with this repository.
********************************************************************/

*--------------------------------------------------
* Birth year and episode starting date
*--------------------------------------------------

gen birth_year = 1993 - age_1993

bysort id (episode date): ///
    replace date = date[_n-1] + 0.5 if date[_n-1] == date

bysort id (episode date): ///
    gen start_date = cond(_n == 1, birth_year, date[_n-1])


*--------------------------------------------------
* Birth cohorts
*--------------------------------------------------

recode birth_year ///
    (min/1949 = 0 "Before 1950") ///
    (1950/1959 = 1 "1950–1959") ///
    (1960/1969 = 2 "1960–1969") ///
    (1970/1979 = 3 "1970–1979") ///
    (1980/max = 4 "1980 or later"), ///
    gen(cohort)


*--------------------------------------------------
* Time-varying education
*--------------------------------------------------

recode education ///
    (0   = 1 "No schooling – illiterate") ///
    (1/4 = 2 "No schooling – literate") ///
    (5   = 3 "Primary") ///
    (6/8 = 4 "Secondary or higher"), ///
    gen(education_tv)