/********************************************************************
  MULTILEVEL POISSON MODELS

  Outcome:
      Number of births in the previous five years

  Level 1:
      Women

  Level 2:
      DHS clusters

  Main question:
      Does the association between education and fertility vary
      across communities and religious contexts?
********************************************************************/


*--------------------------------------------------
* Age specification
*--------------------------------------------------

gen age_c  = age - 26
gen age_c2 = age_c^2


*--------------------------------------------------
* MODEL 0 — Random-intercept model
*--------------------------------------------------

mepoisson births_5y ///
    c.age_c c.age_c2 ///
    [pweight=weight] ///
    || cluster:, ///
    vce(robust) irr


*--------------------------------------------------
* MODEL 1 — Individual-level education
*--------------------------------------------------

mepoisson births_5y ///
    i.secondary_plus ///
    c.age_c c.age_c2 ///
    i.religion ///
    i.urban ///
    [pweight=weight] ///
    || cluster:, ///
    vce(robust) irr


*--------------------------------------------------
* MODEL 2 — Random slope for education
*--------------------------------------------------

mepoisson births_5y ///
    i.secondary_plus ///
    c.age_c c.age_c2 ///
    i.religion ///
    i.urban ///
    [pweight=weight] ///
    || cluster: secondary_plus, ///
    covariance(unstructured) ///
    vce(robust) irr


*--------------------------------------------------
* MODEL 3 — Community religious context
*--------------------------------------------------

mepoisson births_5y ///
    i.secondary_plus ///
    c.age_c c.age_c2 ///
    i.religion ///
    i.urban ///
    i.dominant_religion ///
    [pweight=weight] ///
    || cluster:, ///
    vce(robust) irr


*--------------------------------------------------
* MODEL 4 — Cross-level interaction
*--------------------------------------------------

mepoisson births_5y ///
    i.secondary_plus##i.dominant_religion ///
    c.age_c c.age_c2 ///
    i.religion ///
    i.urban ///
    [pweight=weight] ///
    || cluster:, ///
    vce(robust) irr


* Predicted values
margins secondary_plus#dominant_religion

marginsplot, ///
    xdimension(dominant_religion) ///
    ytitle("Predicted number of births") ///
    xtitle("Dominant religion of the cluster") ///
    title("Education and community religious context")