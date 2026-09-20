/********************************************************************
  DATA PREPARATION — PORTFOLIO VERSION

  Purpose:
  Construct the number of births occurring during the five years
  preceding the survey and prepare selected individual-level variables.

  Note:
  DHS microdata are not distributed with this repository.
********************************************************************/

*--------------------------------------------------
* 1. Construct births in the previous five years
*--------------------------------------------------

use "birth_recode.dta", clear

gen birth_5y = (b19 < 60) if !missing(b19)

collapse (sum) births_5y = birth_5y, by(caseid)

tempfile births
save `births'


*--------------------------------------------------
* 2. Merge with women's individual recode
*--------------------------------------------------

use "individual_recode.dta", clear

merge 1:1 caseid using `births'

* Women without a birth record had no birth
* during the observation window
replace births_5y = 0 if _merge == 1

drop _merge


*--------------------------------------------------
* 3. Survey weights
*--------------------------------------------------

gen weight = v005 / 1000000

svyset v021 [pweight=weight], strata(v022)


*--------------------------------------------------
* 4. Selected analytical variables
*--------------------------------------------------

gen secondary_plus = inlist(v106, 2, 3)

label define educ ///
    0 "Primary or less" ///
    1 "Secondary or higher"

label values secondary_plus educ