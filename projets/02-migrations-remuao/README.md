# Internal Migration Dynamics in Côte d’Ivoire

**Patrick Ntwali Matabaro · [Gpat95](https://github.com/Gpat95)**

**Stata · Event-history analysis · Survival models · Competing risks**

## Overview

This project applies event-history methods to longitudinal residential biographies from REMUAO 1993 in Côte d’Ivoire. It examines the timing and determinants of internal migration among men, with particular attention to education, residential context and birth cohorts.

The project distinguishes first migration, repeated migration and competing destinations.

## Methods and workflow

1. Prepare residential episodes, starting dates, birth cohorts and time-varying covariates.
2. Describe first migration using Kaplan–Meier survival estimates and smoothed hazard functions.
3. Fit Cox proportional-hazards models and examine Schoenfeld residual diagnostics.
4. Analyse competing destinations using cumulative incidence functions and Fine–Gray regression.

The figures include outputs from the broader original workflow, including repeated migration. They are not all reproduced by the four curated examples.

## Selected figures

Original figures are preserved in their original language. The English captions below describe the supplied outputs; the analyses have not been rerun for this portfolio.

![First migration by education](Picture2.png)

*Kaplan–Meier survival curves for first internal migration by education. Categories distinguish no schooling without literacy, no schooling with literacy, primary, and secondary or higher education. Lower survival curves indicate earlier first migration.*

![Regression estimates for repeated migration](Picture8.png)

*Hazard ratios and 95% confidence intervals for education, residence and birth cohort in the repeated-migration model. The horizontal axis is logarithmic; one is the reference value.*

[Explore all nine figures](GALLERY.md).

## Available files

| File | Contents |
|---|---|
| [01 — Event-history setup](01_event_history_setup.do) | Prepare episode timing and selected covariates. |
| [02 — Survival analysis](02_survival_analysis.do) | Declare survival data and plot Kaplan–Meier curves. |
| [03 — Cox model](03_cox_model.do) | Estimate hazard ratios and inspect proportional-hazards diagnostics. |
| [04 — Competing risks](04_competing_risks.do) | Prepare competing events and estimate a Fine–Gray model. |
| [Original research script](Script.do) | Broader exploratory workflow, retained in its original language. |
| [Research report — PDF](Rapport_Event_story_Analysis.pdf) | Original report with methods, results and limitations. |
| [Complete figure gallery](GALLERY.md) | Nine figures with English captions. |

## Data and reproducibility

The original microdata are not distributed. The numbered examples use simplified variable names and require a prepared dataset with the expected identifiers, episode dates, weights and covariates. They should not be assumed to run directly on the original file without adaptation. `Script.do` expects the original `menbio_rciH.dta` data and includes file replacement commands.

The analysis describes associations rather than causal effects. The source scripts and figures have not been independently rerun.

See [software dependencies](DEPENDANCES.md) for the tools identified in the supplied scripts.

[Back to portfolio](../../README.md) · [Reproducibility notes](../../REPRODUCTIBILITE.md) · [Use and attribution](../../CONDITIONS.md)
