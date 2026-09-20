**RAPPORT FINAL***06/01/2026***EVENT STORY ANALYSIS

global eventH "."

use "$eventH/menbio_rciH", clear
save menbio_rciH.dta, replace
*---------------------------------------------------------------------------------
*PRELIMINAIRES
*--------------------------------------------
** Creation des variables :
*an_naiss--date---datedeb--cohorte

gen an_naiss= 93-q10
bysort ident (q201 date): replace date=date[_n-1] + 0.5 if date[_n-1]==date
bysort ident (q201 date): gen datedeb=cond(_n==1, an_naiss, date[_n-1])
*--------------------------------------------
**Creation de la variable Cohorte
generate date_n = 1993 - q10
recode date_n (min/1949=0 "Avant 1950") (1950/1959 = 1 "De 1950 à 1959") (1960/1969 = 2 "De 1960 à 1969") (1970/1979 = 3 "De 1970 à 1979") (1980/max = 4 "Après 1980") , gene (cohorte)
*--------------------------------------------
**Creation de la variable Niveau d'éducation

recode q206 (0=1 "NS-Nalph") (1/4=2 "NS-alph") (5=3 "primaire") ///
                        (6/8=4 "secondaire +") (9 .=.), gen(tvc_niveau)
lab var tvc_niveau "Instruction variant dans le temps"

*--------------------------------------------
tab q206 tvc_niveau
tab mil_det_res mil_det_res_rec
tab cohorte

*------------------------------------------------------------------------------------------
****************************ANALYSES**************************
*-------------------------------------------------------------------------------------------
ssc install fre

fre tvc_niveau //Covariable de l'étude
fre cohorte
fre mil_det_res

**Identifiacation de la residence interne VS hors du pays (Seuls ceux qui sont en interne peuvent faire une migration interne)

capture drop residence
gen residence=mil_det_res<5 if mil_det_res!=.  

***Identification des migrations internes

capture drop censmigint
bysort ident (q201 date): gen censmigint = (residence == 1 & residence[_n+1] == 1) if residence == 1
compress

sort ident q201 datedeb
browse ident q10 q201 mil_det_res q203 an_naiss datedeb date residence censmigint
list ident an_naiss date q201 mil_det_res residence censmigint if ident==11001| ident==11201

stset  date , id(ident) failure(censmigint==1) ///
                        if(residence==1) time0(datedeb) origin(time an_naiss) ///
                        entry(time an_naiss+15)
						
			

sts graph, ///
    ci ///
    yline(.75 .5 .25, lcolor(gs10) lpattern(dash)) ///
    xscale(range(15 100)) ///
    xlabel(15(10)100, labsize(small) grid) ///
    ylabel(0(.25)1, labsize(small) angle(0) grid) ///
    xtitle("Âge (années)", size(medium)) ///
    ytitle("Probabilité de ne pas avoir encore effectué une migration interne", size(medium)) ///
    title("Calendrier de la première migration interne chez les hommes en Côte d'Ivoire", ///
          size(medium)) ///
    subtitle("Estimation de Kaplan–Meier (à partir de 15 ans)", size(small)) ///
    legend(order(2 "Fonction de survie" 1 "Intervalle de confiance à 95 %") ///
           size(small) ring(0) position(5)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium))

sts list//Indicateurs associés (quartiles et mediane)
sts list, at(0(5)80)  //par groupe d'age. 



sts graph, ///
    by(tvc_niveau) ///
    ci ///
    xscale(range(15 100)) ///
    xlabel(15(10)100, labsize(small) grid) ///
    ylabel(0(.25)1, angle(0) labsize(small) grid) ///
    yline(.75 .5 .25, lcolor(gs10) lpattern(dash)) ///
    xtitle("Âge (années)", size(medium)) ///
    ytitle("Probabilité de ne pas encore avoir migré en interne", size(medium)) ///
    title("Calendrier de la première migration interne des hommes", size(medium)) ///
    subtitle("Estimateur de survie de Kaplan–Meier, à partir de 15 ans", size(small)) ///
    legend(order(2 4 6 8) ///
           label(2 "NS – non alphabétisé") ///
           label(4 "NS – alphabétisé") ///
           label(6 "Primaire") ///
           label(8 "Secondaire et plus") ///
           size(small) ring(0) position(5)) ///
    note("Les zones ombrées représentent les intervalles de confiance à 95 %.", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium))

	

sts list at(0(5)80), by(tvc_niveau)


stset  date [pw=pondg_na] , id(ident) failure(censmigint==1) ///
                        if(residence==1) time0(datedeb) origin(time an_naiss) ///
                        entry(time an_naiss+15)
						
****Les risques (quotients)***********
sts graph, hazard ///
    tmax(60) ///
    kernel(rectangle) ///
    width(1) ///
    ci ///
    xlabel(15(5)60, labsize(small)) ///
    xtitle("Âge (années)", size(medium)) ///
    ytitle("Risque annuel", size(medium)) ///
    title("Risque lissé de la première migration interne des hommes", size(medium)) ///
    subtitle("", size(medium)) ///
	 note("Les zones ombrées représentent les intervalles de confiance à 95 %.", size(vsmall)) ///
    graphregion(color(white))
	plotregion(margin(medium))

sts list, at(15(5)60) failure

sts graph, hazard ///
    tmax(60) ///
    kernel(rectangle) ///
    ci ///
    by(tvc_niveau) ///
    legend(order(2 4 6 8) ///
           label(2 "Non alphabétisé") ///
           label(4 "Alphabétisé") ///
           label(6 "Primaire") ///
           label(8 "Secondaire et plus") ///
           cols(1) size(vmedium) ///
           region(lstyle(none)) ///
           position(10) ring(0)) ///
    xlabel(15(5)60, labsize(small)) ///
    xtitle("Âge (années)", size(medium)) ///
    ytitle("Risque annuel") ///
    title("Risque annuel lissé de la 1ère migration interne selon le niveau d'instruction", size(medium)) ///
    note("Les zones ombrées représentent les intervalles de confiance à 95 %.", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium))

sts list, at(15(5)35) failure by (tvc_niveau)

****Les risques (quotients)*********Evenenements repetés 
						
stset  date [pw=pondg_na] , id(ident) failure(censmigint==1) ///
                        if(residence==1) time0(datedeb) origin(time an_naiss) ///
                        entry(time an_naiss+15) exit(time .)						
						
sts graph, hazard ///
    tmax(60) ///
    kernel(rectangle) ///
    width(1) ///
    ci ///
    xlabel(15(5)60, labsize(small)) ///
    xtitle("Âge (années)", size(medium)) ///
    ytitle("Risque de migration interne repetée", size(medium)) ///
    title("Risque annuel lissé de migration interne repeté", size(medium)) ///
    subtitle("", size(medium)) ///
	 note("Les zones ombrées représentent les intervalles de confiance à 95 %.", size(vsmall)) ///
    graphregion(color(white))
	plotregion(margin(medium))
	
sts list, at(15(5)60) failure

	
sts graph, hazard ///
    tmax(60) ///
	kernel(rectangle) ///
    ci ///
    by(tvc_niveau) ///
    legend(order(2 4 6 8) ///
           label(2 "Non alphabétisé") ///
           label(4 "Alphabétisé") ///
           label(6 "Primaire") ///
           label(8 "Secondaire et plus") ///
           cols(1) size(vmedium) ///
           region(lstyle(none)) ///
           position(20) ring(0)) ///
    xlabel(15(5)60, labsize(small)) ///
    xtitle("Âge (années)", size(medium)) ///
    ytitle("Risque de migration interne repetée") ///
    title("Risque annuel lissé de migration interne repeté par niveau d'étude", size(medium)) ///
    note("Les zones ombrées représentent les intervalles de confiance à 95 %.", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium))	

sts list, at(15(5)35) failure
-------------------------------------------------------------------------------


****Analyse biographique approfondis : migrations internes repetés

**Cox model avec 2 covariables

stset  date [pw=pondg_na], id(ident) failure(censmigint==1) ///
                        if(residence==1) time0(datedeb) origin(time an_naiss) ///
                        entry(time an_naiss+15) exit(time .)
						
stcox ib1.tvc_niveau ib1.mil_det_res_rec ib0.cohorte


net install coefplot, replace from(https://raw.githubusercontent.com/benjann/coefplot/master/)
*Graph results with Smallest Effect Size of Interest (SESOI) of +10%/-10%


coefplot, eform levels(95) ///
    title("Effets des covariables sur le risque de migration repeté", size(medium)) ///
    note("Les barres représentent les IC à 95 %. HR > 1 : risque plus élevé ; HR < 1 : risque plus faible.", size(vsmall)) ///
    xscale(log) xlab(.8 1 1.25 1.5 2, labsize(small)) ///
    xline(1, lp(solid)) xline(.90909091, lp(dash)) xline(1.1, lp(dash)) ///
    xtitle("Hazard Ratio (échelle logarithmique)", size(medsmall)) ///
    baselevels ///
    mlabel format(%9.2f) mlabposition(12) mlabgap(*1.2) ///
    headings( ///
        1.tvc_niveau    = `"{bf:Niveau d'instruction}{break}Ref : Non alphabétisé"' ///
        1.mil_det_res_rec =`"{bf:Milieu de résidence}{break}Ref : Urbain"' ///
        1.cohorte       = `"{bf:Cohorte de naissance}{break}Ref : Avant 1950"' ///
    ) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    legend(off)

			 
			 
***Test de proportionalité (De preference fait un test de proportionalité sur une variable significative dans le modele de Cox)
			 
capture drop Sbase
capture drop Hbase
capture drop CHbase
stcox i.tvc_niveau i.mil_det_res_rec i.cohorte, basesurv(Sbase) basehc(Hbase) basech(CHbase)	
	
				
stphplot, by(tvc_niveau) nonegative ///
    adjustfor(i.mil_det_res_rec i.cohorte, atmeans) ///
    title("Nelson–Aalen : Test de proportionalité (par niveau d'instruction) ajusté ", size(medium)) ///
    subtitle("Ajustement : milieu de résidence + cohorte de naissance", size(small)) ///
    ytitle("Hazard cumulé (Nelson–Aalen)", size(medsmall)) ///
    xtitle("ln (analysis time))", size(medsmall)) ///
    legend(pos(6) ring(0) cols(1) size(small) region(lstyle(none))) ///
    plot1opts(connect(stairstep) msymbol(i) lwidth(medthick)) ///
    plot2opts(connect(stairstep) msymbol(i) lwidth(medthick)) ///
    plot3opts(connect(stairstep) msymbol(i) lwidth(medthick)) ///
    plot4opts(connect(stairstep) msymbol(i) lwidth(medthick)) ///
    graphregion(color(white)) plotregion(margin(medium)) ///
    saving(stphplot_adj, replace)


capture drop sch* 
capture drop sca*
stcox i.tvc_niveau i.mil_det_res_rec i.cohorte,  schoenfeld(sch*) scaledsch(sca*) 
estat phtest, detail
-----------------------------------------

******Evenements concurrents*******

gen censmigint_dest=censmigint
bysort ident (date q201): replace censmigint_dest=2 if mil_det_res[_n+1]==4 & censmigint==1
lab def cens_competing 0 "censored" 1 "urban" 2 "rural", modify
lab val censmigint_dest cens_competing
br ident an_naiss datedeb date residence mil_det_res censmigint censmigint_dest

* Identify observations at risk for the 1st migration using original censoring variable
stset date [pw=pondg_na], id(ident) failure(censmigint==1) ///
			if(residence==1) time0(datedeb) origin(time an_naiss) ///
			entry(time an_naiss+15) 

capture drop atrisk_firstmig
rename _st atrisk_firstmig // this is the variable identifying period at risk before 1st event
tab censmigint_dest mil_res if atrisk_firstmig==1

br ident an_naiss datedeb date residence mil_det_res censmigint censmigint_dest atrisk_firstmig

*1. Urbain -> Rural VS Urbain -> Urbain//depuis l'urbain 

stset date [pw=pondg_na], id(ident) failure(censmigint_dest==2) ///
			if(atrisk_firstmig==1 & mil_det_res<4) time0(datedeb) origin(time an_naiss) ///
			entry(time an_naiss+15) 
			
capture drop CIF*	
stcompet CIF=ci CIFhi=hi CIFlo=lo, compet1(1) 

gen CIFUrb=CIF if censmigint_dest==1
gen CIFRur=CIF if censmigint_dest==2

gen CIFloUrb=CIFlo if censmigint_dest==1
gen CIFloRur=CIFlo if censmigint_dest==2

gen CIFhiUrb=CIFhi if censmigint_dest==1
gen CIFhiRur=CIFhi if censmigint_dest==2

sts graph, tmin(10) tmax(70) xlab(10(5)70) noorigin ///
	title("") xtitle("All migrations from Urban") sav(KM.gph, replace)
* to compare CIF and KM estimates
	
twoway ///
    (connected CIFUrb _t if _t<70, sort ///
        connect(J) msymbol(O) msize(vsmall) ///
        lwidth(medthick)) ///
    (connected CIFRur _t if _t<70, sort ///
        connect(J) msymbol(D) msize(vsmall) ///
        lwidth(medthick)), ///
    xlabel(10(5)70, labsize(small) grid) ///
    ylabel(0(.25)1, labsize(small) grid) ///
    yscale(reverse range(0 1)) ///
    xtitle("Âge (années)", size(medsmall)) ///
    ytitle("Incidence cumulée (CIF) - axe inversé", size(medsmall)) ///
    title("Incidence cumulée de la première migration selon la destination", size(medium)) ///
    subtitle("Événements concurrents : U -> R vs U -> U", size(small)) ///
    legend(order(1 "Urbain -> Urbain" 2 "Urbain -> Rural") ///
           cols(1) size(small) ring(0) position(6) region(lstyle(none))) ///
    note("", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    name(CIF_inverse, replace)
	

graph combine CIF_reverse.gph KM.gph, ycommon xcommon
	
stcrreg i.tvc_niveau i.mil_det_res_rec i.cohorte , compete(censmigint_dest==1) 



coefplot, eform ///
    xline(.90909091, lp(dash)) xline(1.1, lp(dash)) xline(1, lp(solid)) ///
    baselevels ///
    mlabel format(%9.2f) mlabposition(12) mlabgap(*1.2) ///
    headings( ///
        *.tvc_niveau      = "{bf:Niveau d'éducation}" ///
        *.mil_det_res_rec = "{bf:Milieu de résidence}" ///
        *.cohorte         = "{bf:Cohorte de naissance}" ///
    ) ///
    xscale(log range(0.2 5)) ///
    xlab(.5 1 2 5) ///
    xtitle("Subdistribution Hazard Ratio (sHR) - échelle log", size(medsmall)) ///
    title("Effet des covariables sur la 1re migration interne (urbain → rural)", size(medium)) ///
    note("Les points représentent les sHR ; les barres indiquent les IC à 95 %.", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    saving("F&GU_R.gph", replace)

-*--------------------------
*2. Rural -> Urbain VS Rural -> Rural

stset date [pw=pondg_na], id(ident) failure(censmigint_dest==1) ///
			if(atrisk_firstmig==1 & mil_det_res==4) time0(datedeb) origin(time an_naiss) ///
			entry(time an_naiss+15) 
			
capture drop CIF*	
stcompet CIF=ci CIFhi=hi CIFlo=lo, compet1(2) 

gen CIFUrb=CIF if censmigint_dest==1
gen CIFRur=CIF if censmigint_dest==2

gen CIFloUrb=CIFlo if censmigint_dest==1
gen CIFloRur=CIFlo if censmigint_dest==2

gen CIFhiUrb=CIFhi if censmigint_dest==1
gen CIFhiRur=CIFhi if censmigint_dest==2

sts graph, tmin(10) tmax(70) xlab(10(5)70) noorigin ///
	title("") xtitle("All migrations from Rural") sav(KM1.gph, replace)
* to compare CIF and KM estimates

	
twoway ///
    (connected CIFRur _t if censmigint_dest!=0 & _t<70, sort ///
        connect(J) msymbol(O) msize(vsmall) lwidth(medthick)) ///
    (connected CIFUrb _t if censmigint_dest!=0 & _t<70, sort ///
        connect(J) msymbol(D) msize(vsmall) lwidth(medthick)), ///
    yscale(reverse range(0 1)) ///
    xlabel(10(5)70, labsize(small) grid) ///
    ylabel(0(.25)1, labsize(small) grid) ///
    xtitle("Âge (années)", size(medsmall)) ///
    ytitle("Incidence cumulée (CIF) - axe inversé", size(medsmall)) ///
    title("Incidence cumulée de la première migration selon la destination", size(medium)) ///
    subtitle("Événements concurrents : R -> U vs R -> R", size(small)) ///
    legend(order(1 "Rural -> Rural" 2 "Rural -> Urbain") ///
           cols(1) size(small) ring(0) position(6) region(lstyle(none))) ///
    note("", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    name(CIF_reverse2, replace)

graph save CIF_reverse2.gph, replace


graph combine CIF_reverse2.gph KM1.gph, ycommon xcommon

	
stcrreg i.tvc_niveau i.mil_det_res_rec i.cohorte , compete(censmigint_dest==2) 


coefplot, eform ///
    xline(.90909091, lp(dash)) xline(1.1, lp(dash)) xline(1, lp(solid)) ///
    baselevels ///
    mlabel format(%9.2f) mlabposition(12) mlabgap(*1.2) ///
    headings( ///
        *.tvc_niveau      = "{bf:Niveau d'éducation}" ///
        *.mil_det_res_rec = "{bf:Milieu de résidence}" ///
        *.cohorte         = "{bf:Cohorte de naissance}" ///
    ) ///
    xscale(log range(0.2 5)) ///
    xlab(.5 1 2 5) ///
    xtitle("Subdistribution Hazard Ratio (sHR) - échelle log", size(medsmall)) ///
    title("Effet des covariables sur la 1re migration int. (rural → urbain)", size(medium)) ///
    note("Les points représentent les sHR ; les barres indiquent les IC à 95 %.", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    saving("F&GR_U.gph", replace)
---------------------------------------
*3. Urbain -> Urbain

stset date [pw=pondg_na], id(ident) failure(censmigint_dest==1) ///
			if(atrisk_firstmig==1 & mil_det_res<4) time0(datedeb) origin(time an_naiss) ///
			entry(time an_naiss+15) 
			
sts graph, hazard ci kernel(rectangle) tmax(75) sav(hazardU_U, replace) ///
	title("1st migration from urban to urban")

stcrreg i.tvc_niveau i.mil_det_res_rec i.cohorte , compete(censmigint_dest==2)
 	
stcurve, cif ///
    title("Fonction d'incidence cumulée de la première migration interne", size(medium)) ///
    subtitle("Flux : urbain → urbain", size(small)) ///
    xtitle("Âge (années)", size(medsmall)) ///
    ytitle("Incidence cumulée (CIF)", size(medsmall)) ///
    xlabel(15(5)100, labsize(small) grid) ///
    ylabel(0(.1)1, labsize(small) grid) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    note("", size(vsmall)) ///
    saving(cif_UU, replace)



coefplot, eform ///
    xline(.90909091, lp(dash)) xline(1.1, lp(dash)) xline(1, lp(solid)) ///
    baselevels ///
    mlabel format(%9.2f) mlabposition(12) mlabgap(*1.2) ///
    headings( ///
        *.tvc_niveau      = "{bf:Niveau d'éducation}" ///
        *.mil_det_res_rec = "{bf:Milieu de résidence}" ///
        *.cohorte         = "{bf:Cohorte de naissance}" ///
    ) ///
    xscale(log range(0.2 5)) ///
    xlab(.5 1 2 5) ///
    xtitle("Subdistribution Hazard Ratio (sHR) - échelle log", size(medsmall)) ///
    title("Effet des covariables sur la 1re migration int. (urbain → urbain)", size(medium)) ///
    note("Les points représentent les sHR ; les barres indiquent les IC à 95 %.", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    saving("F&GU_U.gph", replace)
--------------------------------------
*4. Rural -> Rural 

stset date [pw=pondg_na], id(ident) failure(censmigint_dest==2) ///
			if(atrisk_firstmig==1 & mil_det_res==4) time0(datedeb) origin(time an_naiss) ///
			entry(time an_naiss+15) 

sts graph, hazard ci kernel(rectangle) tmax(75) sav(hazardR_R, replace) ///
	title("1st migration from Rural to Rural")
	
stcrreg i.tvc_niveau i.mil_det_res_rec i.cohorte , compete(censmigint_dest==1) 	
	
stcurve, cif ///
    title("Fonction d'incidence cumulée de la première migration interne", size(medium)) ///
    subtitle("Flux : rural → rural", size(small)) ///
    xtitle("Âge (années)", size(medsmall)) ///
    ytitle("Incidence cumulée (CIF)", size(medsmall)) ///
    xlabel(15(5)100, labsize(small) grid) ///
    ylabel(0(.1)1, labsize(small) grid) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    note("", size(vsmall)) ///
    saving(cif_RR, replace)

coefplot, eform ///
    xline(.90909091, lp(dash)) xline(1.1, lp(dash)) xline(1, lp(solid)) ///
    baselevels ///
    mlabel format(%9.2f) mlabposition(12) mlabgap(*1.2) ///
    headings( ///
        *.tvc_niveau      = "{bf:Niveau d'éducation}" ///
        *.mil_det_res_rec = "{bf:Milieu de résidence}" ///
        *.cohorte         = "{bf:Cohorte de naissance}" ///
    ) ///
    xscale(log range(0.2 5)) ///
    xlab(.5 1 2 5) ///
    xtitle("Subdistribution Hazard Ratio (sHR) - échelle log", size(medsmall)) ///
    title("Effet des covariables sur la 1re migration int. (rural → rural)", size(medium)) ///
    note("Les points représentent les sHR ; les barres indiquent les IC à 95 %.", size(vsmall)) ///
    graphregion(color(white)) ///
    plotregion(margin(medium)) ///
    saving("F&GR_R.gph", replace)


-----------------------------------------------------------
*Verification de l'hypothèse de causalité
*Rural-rural

stcrreg i.tvc_niveau ///
        i.cohorte , compete(censmigint_dest==1) 
		
stcrreg i.cohorte , compete(censmigint_dest==1) 