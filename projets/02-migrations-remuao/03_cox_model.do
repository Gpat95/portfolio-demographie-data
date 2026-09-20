/********************************************************************
 COX PROPORTIONAL HAZARDS MODEL
********************************************************************/

stcox ///
    ib1.education_tv ///
    ib1.residence_type ///
    ib0.cohort


*--------------------------------------------------
* Visualize hazard ratios
*--------------------------------------------------

coefplot, ///
    eform ///
    levels(95) ///
    xscale(log) ///
    xline(1) ///
    xtitle("Hazard Ratio (log scale)") ///
    title("Determinants of internal migration") ///
    graphregion(color(white))


*--------------------------------------------------
* Proportional-hazards assumption
*--------------------------------------------------

capture drop sch*
capture drop sca*

stcox ///
    i.education_tv ///
    i.residence_type ///
    i.cohort, ///
    schoenfeld(sch*) ///
    scaledsch(sca*)

estat phtest, detail