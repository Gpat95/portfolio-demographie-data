
global art1 "."

use "$art1/Base complète 02_07_2025.dta", clear


//Création de la variable de pondération
tab v005
cap drop poids
gen poids = v005 / 1000000 
tab poids 
lab var poids "Pondération"
**************************************************************
*1. PREPARARTION DES VARIABLES
**************************************************************

// 1. Variable dépendante : Nombre des visites prénatales 
tab m14_1
fre m14_1
* Supprimer les observations où m14_1 est égal à 98 (don't know)
// Supprimer les lignes après 8 dans la variable m14_1 ==> Pourquoi??
drop if m14_1 > 8

* Recode de m14_1 en ANC_access
gen ANC_access = .
replace ANC_access = 0 if m14_1 == 0
replace ANC_access = 1 if inrange(m14_1, 1, 3)
replace ANC_access = 2 if inrange(m14_1, 4, 8)


* Étiquettes de variable et de valeur
label variable ANC_access "Accès aux soins prénatals"
label define ANC_access_labels 0 "No" 1 "1 à 3 CPN" 2 "4 CPN et plus"
label values ANC_access ANC_access_labels

* Sélection des observations avec ANC_access 0, 1 ou 2
keep if inlist(ANC_access, 0, 1, 2)

tab ANC_access, miss


** 2. Variables indépendantes

// 2.1. Facteurs prédisposants ou prédéterminants

**2.1.1.Âge de la mère**
tab v013
rename v013 age_Fem
tab age_Fem
** 2.1.2. Parité***
tab v201
tab v201 m14_1
list v201 m14_1

gen Par_Fem = .
replace Par_Fem = 0 if v201 == 0  // Nullipare (Sans enfant)
replace Par_Fem = 1 if v201 == 1  // Primipare (Un enfant)
replace Par_Fem = 2 if inrange(v201, 2, 5)  // Multipare (2 à 5 enfants)
replace Par_Fem = 3 if v201 >= 6  // Grande multipare (6 enfants ou plus)

label define par_fem_labels 0 "NULLIPARE" 1 "PRIMIPARE" 2 "MULTIPARE" 3 "GRANDE MULTIPARE"
label values Par_Fem par_fem_labels

tab Par_Fem

**Statut matrimonial 
tab v501
// Créer une nouvelle variable Msatus
gen Mstatus = .

// Attribuer des valeurs correspondant aux groupes
replace Mstatus = 1 if v501 == 0 
replace Mstatus = 2 if v501 == 1 | v501 == 2 
replace Mstatus = 3 if v501 == 3 | v501 == 4 | v501 == 5 

// Créer des étiquettes pour les nouvelles modalités
label define marital_labels 1 "Célibataires" 2 "Mariées ou en union" 3 "Divorcés/séparées/veuves"
label values Mstatus marital_labels
tab Mstatus


**Région de résidence**
fre v101
* Créer une nouvelle variable pour le recodage des provinces
gen province = .

replace province = 1 if inlist(v101, 10, 11, 12, 13, 14) 
replace province = 2 if inlist(v101, 21, 22, 23, 24)
replace province = 3 if inlist(v101, 31, 32, 33) 
replace province = 4 if inlist(v101, 41, 42, 43, 44) 
replace province = 5 if inlist(v101, 51, 52, 53, 54)   
replace province = 6 if inlist(v101, 61, 62)    

label define province_labels 1 "Province d'Antananarivo" 2 "Province de Fianarantsoa" 3 "Province de Toamasina" 4 "Province de Mahajanga" 5 "Province de Toliara" 6 "Province d'Antsiranana"

label values province province_labels
tab province


**Niveau d'instruction parents**
**Mère**
tab v106
// Créer une nouvelle variable Niv_Instr
gen Niv_Instr = .

// Attribuer des valeurs correspondant aux groupes
replace Niv_Instr = 1 if v106 == 0 
replace Niv_Instr = 2 if v106 == 1 
replace Niv_Instr = 3 if v106 == 2 | v106 == 3 
tab Niv_Instr


// Créer des étiquettes pour les nouvelles modalités
label define education_labels 1 "Sans instruction" 2 "Primaire" 3 "Secondaire ou plus"
label values Niv_Instr education_labels
tab Niv_Instr


** Autonomie décisionnelle **
tab v743b
tab v743d
tab v739
tab v743f
tab v743a


* Créer la variable composite pour le niveau d'autonomie décisionnelle
* Initialiser les compteurs
gen woman_alone_count = 0
gen woman_partner_count = 0
gen husband_partner_count = 0

foreach var in v743b v743d v739 v743f v743a {
    replace woman_alone_count = woman_alone_count + 1 if `var' == 1
    replace woman_partner_count = woman_partner_count + 1 if `var' == 2
    replace husband_partner_count = husband_partner_count + 1 if `var' == 4
}

* Créer la variable composite
gen indep_decision = .
replace indep_decision = 1 if woman_alone_count >= 3
replace indep_decision = 2 if woman_partner_count >= 3 & indep_decision == .
replace indep_decision = 3 if husband_partner_count >= 3 & indep_decision == .

* Ajouter les labels
label define indep_label 1 "Niveau élevé" 2 "Niveau moyen" 3 "Niveau faible"
label values indep_decision indep_label

label drop  indep_label

* Vérification
tab indep_decision

egen score_test = concat(v743b v743d v739 v743f v743a), punct("-")
tab score_test



** Religion
tab v130
* Créer une nouvelle variable "religion_recoded"
gen religion = .

* Recoder la variable selon les catégories
replace religion = 1 if v130 == 1 
replace religion = 2 if v130 == 2 
replace religion = 3 if v130 == 3 
replace religion = 4 if v130 == 4 
replace religion = 5 if v130 == 5 
replace religion = 2 if v130 == 6 
replace religion= 6 if v130 == 96

* Ajouter des labels à la nouvelle variable
label define religion_labels 1 "Catholic" 2 "Protestant" 3 "Muslim" 4 "Traditional/animist" 5 "No religion" 6 "Other"
label values religion religion_labels
tab religion

recode religion ///
    (1 = 1 "Catholic") /// 
    (2 = 2 "Protestant") /// 
	(3/4 = 3 "Musulman/Tradi") ///
    (5/6 = 4 "Sans religion/Non declare"), generate(religion1)

fre religion1


// 2.2. Facteurs d'accès
**Niveau de vie du ménage (Tercile du niveau de vie)**
tab v190
// Créer une nouvelle variable Niv_Vie
gen Niv_Vie = .

// Attribuer des valeurs correspondant aux groupes
replace Niv_Vie = 1 if v190 == 1 | v190 == 2
replace Niv_Vie = 2 if v190 == 3 
replace Niv_Vie = 3 if v190 == 4 | v190 == 5 

// Créer des étiquettes pour les nouvelles modalités
label define wealth_labels 1 "Pauvre" 2 "Moyen" 3 "Riche"
label values Niv_Vie wealth_labels
tab Niv_Vie
**Assurance maladie**
tab v481
rename v481 Ass_Mal
tab Ass_Mal

**Milieu de résidence**
tab v102
rename v102 Mil_Res
tab Mil_Res

**Disponibilité des services de santé

**Distance géographique aux établissements de santé**
tab v467d
rename v467d Dist_Geo
tab Dist_Geo

*** Exposition aux medias

gen exposition_media = .

tab v169a // Possession d'un téléphone mobile
tab v169b // Utilisation du téléphone pour des transactions financières
tab v171a // Utilisation de l'internet
tab s656b // Television
tab s656d //Newspaper
tab s656a // Radio
* Possession d'un téléphone mobile (v169a) : Pas un media !
gen score_phone = .
replace score_phone = 1 if v169a == 1  // Oui = 1
replace score_phone = 0 if v169a == 0  // Non = 0
tab score_phone
* Utilisation du téléphone pour des transactions financières (v169b): Pas un Media!
gen score_financial = .
replace score_financial = 1 if v169b == 1  // Oui = 1
replace score_financial = 0 if v169b == 0  // Non = 0
tab score_financial

* Utilisation de l'internet (v171a)
gen score_internet = .
replace score_internet = 1 if v171a == 1  // Oui, last 12 months = 1
replace score_internet = 0.5 if v171a == 2  // Oui, before last 12 months = 0.5
replace score_internet = 0 if v171a == 0  // Never = 0
tab score_internet

*TV
gen score_tv = .
replace score_tv = 1 if s656b == 1  // Oui = 1
replace score_tv = 0 if s656b == 0  // Non = 0
tab score_tv

*Newspaper
gen score_newspaper = .
replace score_newspaper = 1 if s656d == 1  // Oui = 1
replace score_newspaper = 0 if s656d == 0  // Non = 0
tab score_newspaper

*Radio
gen score_radio = .
replace score_radio = 1 if s656a == 1  // Oui = 1
replace score_radio = 0 if s656a == 0  // Non = 0
tab score_radio


* 2. Créer la variable composite

egen composite_score = rowtotal(score_phone score_financial score_internet score_tv score_newspaper score_radio)


*Illustration visuel des combinaisons
egen score_final = concat(score_phone score_financial  score_internet  score_tv  score_newspaper  score_radio), punct("-")


* Définir les catégories d'exposition
replace exposition_media = .
replace exposition_media = 1 if composite_score >= 0 & composite_score <= 2  // Faible exposition
replace exposition_media = 2 if composite_score > 2 & composite_score <= 7  // Moyenne exposition 

//forte exposition (111) fusionné avec moyenne exposition (1145) car probleme de faible effectif 

* Appliquer des labels à la variable discrétisée
label define exposition_label 1 "Faible exposition" 2 "Exposition modérée/élevée" 
label values exposition_media exposition_label


*Verification
tab composite_score
tab exposition_media

**Coûts associés aux soins prénatal**
tab v467c
rename v467c Coût_Santé
tab Coût_Santé



// 2.3. Facteurs des besoins 


**Perception de la santé maternelle**
tab v244
rename v244 Perc_Santé
tab Perc_Santé

**Antécédents médicaux de la mère**
tab s1302
tab s1307
tab s1311
tab s1313
tab s1315
tab s1317
tab s1319
tab s1321
tab s472
tab v401
// Créer une nouvelle variable Antecdant medicaux
gen Ante_Med = 0  // Initialiser à 0 (Non)

// Mettre à jour pour ceux qui ont au moins un antécédent médical
replace Ante_Med = 1 if s1302 == 1 | s1307 == 1 | s1311 == 1 | s1313 == 1 | s1315 == 1 | s1317 == 1 | s1319 == 1 | s1321 == 1 | s472 == 1 | v401 == 1  // Oui si au moins un antécédent

// Créer des étiquettes pour les modalités
label define ante_med_labels 0 "Non" 1 "Oui"
label values Ante_Med ante_med_labels
tab Ante_Med


**Antécédents de mortalité infantile**
// Créer une nouvelle variable children_died
gen children_died = 0  // Initialiser à 0 (Non)

// Mettre à jour pour ceux qui ont perdu au moins un enfant
replace children_died = 1 if v206 > 0 | v207 > 0  // Oui si au moins un fils ou une fille est décédé(e)

// Créer des étiquettes pour les modalités
label define children_died_labels 0 "Non" 1 "Oui"
label values children_died children_died_labels
tab children_died
*------------------------------------------------
**2. ANALYSE MULTVARIEE: REG LOG MULTINOMIALE
*------------------------------------------------
gen poids = v005 / 1000000 

***************Effets bruts non ajustés (bivariés)************************

* Liste des variables indépendantes 
local vars age Par_Fem province Niv_Instr indep_decision religion1 Niv_Vie Mil_Res ///
    Dist_Geo Coût_Santé exposition_media Ante_Med children_died

foreach v of local vars {
    di "=== Estimation pour la variable : `v' ==="

    * Si la variable est religion1, définir la base 4
    if "`v'" == "religion1" {
        quietly mlogit ANC_access ib4.`v' [pweight=poids], baseoutcome(2) rrr
    }
    else {
        quietly mlogit ANC_access i.`v' [pweight=poids], baseoutcome(2) rrr
    }

    est store `v'
}


estout age Par_Fem province Niv_Instr indep_decision religion1 Niv_Vie Mil_Res ///
    Dist_Geo Coût_Santé exposition_media Ante_Med children_died ///
    using RRR_bruts2.csv, replace ///
    eform label ///
    cells("RRR(fmt(3)) ci(fmt(3) par([ , ])) p(fmt(3))") ///
    stats(ll r2_p, fmt(3 3) labels("Log-Likelihood" "Pseudo R² (McFadden)")) ///
    style(tab)
*----------------------------------------------------------
****************Effets ajustés********************

*M0: model nul
mlogit ANC_access [pweight=poids], baseoutcome(2) rrr
estimates store M0

* M1 : facteurs Predisposants
mlogit ANC_access i.age i.Par_Fem i.province i.Niv_Instr i.indep_decision ib4.religion1 ///
    [pweight=poids], baseoutcome(2) rrr
estimates store M1

testparm i.age i.Par_Fem i.province i.Niv_Instr i.indep_decision i.religion1 

* M2 : M1 + facteurs facilitants
mlogit ANC_access i.age i.Par_Fem i.province i.Niv_Instr i.indep_decision ib4.religion1 /// M1
    i.Niv_Vie i.Mil_Res i.Dist_Geo i.Coût_Santé i.exposition_media ///+
    [pweight=poids], baseoutcome(2) rrr
estimates store M2

testparm i.Niv_Vie i.Mil_Res i.Dist_Geo i.Coût_Santé i.exposition_media

* M3 : M2 + facteurs de besoin
mlogit ANC_access i.age i.Par_Fem i.province i.Niv_Instr i.indep_decision ib4.religion1 /// M1
    i.Niv_Vie i.Mil_Res i.Dist_Geo i.Coût_Santé i.exposition_media /// M2
    i.Ante_Med i.children_died /// +
    [pweight=poids], baseoutcome(2) rrr
estimates store M3

testparm i.Ante_Med i.children_died

* M4 : M3 + interactions
mlogit ANC_access i.age i.Par_Fem i.province i.Niv_Instr i.indep_decision ib4.religion1 /// M1
    i.Niv_Vie i.Mil_Res i.Dist_Geo i.Coût_Santé i.exposition_media /// M2
    i.Ante_Med i.children_died /// M3
    i.Niv_Instr##i.indep_decision /// +
    i.Mil_Res##i.Dist_Geo /// +
    i.Par_Fem##i.children_died /// +
    [pweight=poids], baseoutcome(2) rrr
estimates store M4


testparm i.Niv_Instr#i.indep_decision i.Mil_Res#i.Dist_Geo i.Par_Fem#i.children_died

***************
* Comparaison des effets (RRR), Log-Likelihood, Pseudo R² entre modèles 

estout M0 M1 M2 M3 M4 using Modeles1.csv, replace ///
    eform label ///
    cells("b(fmt(3)) ci(fmt(3) par([ , ])) p(fmt(3))") ///
    stats(ll r2_p, fmt(3 3) labels("Log-Likelihood" "Pseudo R² (McFadden)")) ///
    title("Comparaison des effets (RRR) avec IC95% et p-values exactes") ///
    style(tab) collabels("RRR" "IC95%" "p-value") ///
    varlabels(_cons "Constante")

shell start Modeles1.csv

*--------------------------------------------------------------

