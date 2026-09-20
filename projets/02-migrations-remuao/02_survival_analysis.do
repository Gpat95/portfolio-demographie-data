/********************************************************************
 DESCRIPTIVE SURVIVAL ANALYSIS

 Event:
 First internal migration

 Time scale:
 Age

 Entry:
 Age 15
********************************************************************/

* Identify periods spent within the country
gen internal_residence = residence_code < 5 ///
    if !missing(residence_code)


* Identify internal migration events
bysort id (episode date): ///
    gen internal_migration = ///
    (internal_residence == 1 & internal_residence[_n+1] == 1) ///
    if internal_residence == 1


*--------------------------------------------------
* Declare survival-time data
*--------------------------------------------------

stset date [pw=weight], ///
    id(id) ///
    failure(internal_migration == 1) ///
    if internal_residence == 1 ///
    time0(start_date) ///
    origin(time birth_year) ///
    entry(time birth_year + 15)
	
	* Overall survival function
sts graph, ///
    ci ///
    xlabel(15(10)80) ///
    xtitle("Age") ///
    ytitle("Probability of not yet experiencing internal migration") ///
    title("Timing of first internal migration")


* Survival curves by education
sts graph, ///
    by(education_tv) ///
    ci ///
    xlabel(15(10)80) ///
    xtitle("Age") ///
    ytitle("Survival probability") ///
    title("First internal migration by education")