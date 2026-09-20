# Multilevel Analysis of Fertility and Community Context in the DRC

**Patrick Ntwali Matabaro · [Gpat95](https://github.com/Gpat95)**

**Stata · R · DHS · Multilevel modelling · Mapping**

## Overview

This project examines whether the association between women’s education and recent fertility varies across local communities and their religious context in the Democratic Republic of the Congo.

The study combines individual and community characteristics from Demographic and Health Survey (DHS) data. The modelled outcome is the number of births during the five years preceding the survey; the descriptive total fertility rate figures provide broader demographic context.

## Methods and workflow

1. Prepare birth and women’s recode files, construct the recent-birth outcome and define survey weights.
2. Build cluster-level measures of religious composition and community context.
3. Estimate multilevel Poisson models with random intercepts, random slopes and cross-level interactions.
4. Examine predictive margins and map spatial variation using R.

These are observational analyses: modelled associations should not be interpreted as established causal effects.

## Figures

Original figures are preserved in their original language. The English captions below describe the supplied outputs; the analyses have not been rerun for this portfolio.

<a id="figure-1"></a>

### Figure 1: Total fertility rate over time

![Total fertility rate over time](graphique_isf_rdc_1960_2024_avec_eds_zero_simple.jpg)

*Total fertility rate in the DRC, 1960–2024, comparing the World Bank WDI series with the DHS estimates shown in the original figure. The vertical axis is children per woman.*

<a id="figure-2"></a>

### Figure 2: Female school enrolment over time

![Female school enrolment over time](graphique_rdc_femmes_secondaire_superieur_wb.jpg)

*Female gross enrolment ratios in secondary and tertiary education in the DRC. Gaps correspond to years without observations in the supplied series; these are enrolment ratios, not educational attainment shares.*

<a id="figure-3"></a>

### Figure 3: Religious affiliation among surveyed women

![Religious affiliation among surveyed women](rel%20ind.png)

*Religious affiliation among surveyed women in the DRC DHS. Percentages use survey weights; the original figure also reports counts. Categories include Catholic, Protestant, revival churches, Muslim, and traditional or other affiliations.*

<a id="figure-4"></a>

### Figure 4: Spatial distribution of total fertility rates

![Spatial distribution of total fertility rates](carte_ISF_clusters_RDC.png)

*Cluster-level total fertility rate categories in the DRC DHS 2023–2024. Colours represent children per woman; NA identifies missing values in the original legend.*

<a id="figure-5"></a>

### Figure 5: Religious composition across DHS clusters

![Religious composition across DHS clusters](carte_REL_clusters_RDC.png)

*Spatial distribution of religious affiliation shares across DHS clusters, with separate panels for Catholic, revival, traditional, Muslim and Protestant groups. Colours represent within-cluster proportions.*

<a id="figure-6"></a>

### Figure 6: Spatial variation in the modelled education association

![Spatial variation in the modelled education association](Variation%20spatiale%20de%20l%27effet%20de%20l%27%C3%A9ducation%20sur%20la%20f%C3%A9condit%C3%A9_RDC4.jpg)

*Predicted incidence rate ratios associated with education across DHS clusters. Values below and above one indicate different directions of the modelled association; the map is not a map of statistical significance or a causal effect.*

<a id="figure-7"></a>

### Figure 7: Predicted births by education, religious context and residence

![Predicted births by education, religious context and residence](margines_residence.png)

*Predicted numbers of births by education and dominant community religion, shown separately for urban and rural residence. Blue represents primary education or less; red represents secondary education or higher. The two panels use different vertical scales.*

## Available files

| File | Contents |
|---|---|
| [01 — Data preparation](01_data_preparation.do) | Construct the birth outcome, merge records and prepare selected individual variables. |
| [02 — Community context](02_cluster_context.do) | Construct measures of cluster religious composition. |
| [03 — Multilevel models](03_multilevel_models.do) | Estimate Poisson models, interactions and predictive margins. |
| [Religious context and fertility maps](Descriptifs_cartes_Religion%26fecondit%C3%A9.R) | Descriptive mapping under R. |
| [Spatial variation in education associations](Descritives_cartes_Pente%20de%20l%27education.R) | Map cluster-level model outputs. |
| [Defence presentation — PDF](Presentation_Jun2026.pdf) | Original presentation of the research question, methods and findings. |
| [Complete figure gallery](GALLERY.md) | Seven original figures with English captions. |

## Data and reproducibility

DHS microdata and geographic inputs are not included. The numbered Stata files are curated portfolio scripts; they are not yet a self-contained pipeline. The first script expects `birth_recode.dta` and `individual_recode.dta`. Later scripts use prepared variables such as `age`, `cluster`, `religion` and `urban`; their mapping to DHS fields must be checked before execution. The R scripts require geographic data and intermediate tables.

The supplied figures come from the original research workflow and have not been regenerated from the curated scripts.

See [software dependencies](DEPENDANCES.md) for the tools identified in the supplied scripts.

[Back to portfolio](../../README.md) · [Reproducibility notes](../../REPRODUCTIBILITE.md) · [Use and attribution](../../CONDITIONS.md)

