/********************************************************************
  COMMUNITY-LEVEL CONTEXT

  Purpose:
  Construct cluster-level measures of religious composition.

  Unit level:
      Level 1 = individuals
      Level 2 = DHS clusters
********************************************************************/

* Generate indicators for religious affiliation
tab religion, generate(relig_)

* Cluster-level religious composition
bysort cluster: egen p_catholic   = mean(relig_1)
bysort cluster: egen p_protestant = mean(relig_2)
bysort cluster: egen p_evangelical = mean(relig_3)
bysort cluster: egen p_muslim     = mean(relig_4)
bysort cluster: egen p_other      = mean(relig_5)


*--------------------------------------------------
* Identify dominant religious context
*--------------------------------------------------

egen max_share = rowmax( ///
    p_catholic ///
    p_protestant ///
    p_evangelical ///
    p_muslim ///
    p_other)


gen dominant_religion = .

replace dominant_religion = 1 if p_catholic == max_share
replace dominant_religion = 2 if p_protestant == max_share ///
    & missing(dominant_religion)

replace dominant_religion = 3 if p_evangelical == max_share ///
    & missing(dominant_religion)

replace dominant_religion = 4 if p_muslim == max_share ///
    & missing(dominant_religion)

replace dominant_religion = 5 if p_other == max_share ///
    & missing(dominant_religion)