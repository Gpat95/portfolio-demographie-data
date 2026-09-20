# Inequalities in Antenatal Care Utilisation in Madagascar

**Patrick Ntwali Matabaro · [Gpat95](https://github.com/Gpat95)**

**Stata · Survey weights · Data preparation · Multinomial logistic regression**

## Overview

This public-health project examines factors associated with different categories of antenatal care utilisation in Madagascar. It focuses on preparing an analytical dataset and comparing patterns of attendance across population groups.

## Methods and workflow

1. Construct survey weights and inspect the source variables.
2. Recode the antenatal-visit outcome and explanatory variables.
3. Describe the distribution of utilisation and relevant characteristics.
4. Fit multinomial logistic regression models to examine associations with utilisation categories.

The treatment of missing values and the exclusion of values above eight visits require justification before results are reused. The code is presented as analytical work, not as a published article or a validated policy conclusion.

## Available files

| File | Contents |
|---|---|
| [Antenatal care analysis](Dofile%20ANC%2017_11_2025_SHR.do) | Original Stata workflow for recoding, weighting and modelling. |
| [Statistical results](Results_ANC_2025-11-08%20%28version%201%29.xlsx) | Original tables of sample characteristics, unadjusted and adjusted associations, and interaction models. |

## Data and reproducibility

The script expects `Base complète 02_07_2025.dta`, which is not included. Review value labels, missing-value codes, the visit-count exclusions and the survey specification before running the analysis. Additional commands such as `fre` may be required. Stata and package versions are not pinned.

No separate figures or research article have been supplied in this folder. No analysis was rerun for this portfolio.

See [software dependencies](DEPENDANCES.md) for the tools identified in the supplied scripts.

[Back to portfolio](../../README.md) · [Reproducibility notes](../../REPRODUCTIBILITE.md) · [Use and attribution](../../CONDITIONS.md)

