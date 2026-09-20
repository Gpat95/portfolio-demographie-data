# Public Health — Comparing Methods for Assessing Kidney Function

**Patrick Ntwali Matabaro · [Gpat95](https://github.com/Gpat95)**

**R · Method comparison · Descriptive statistics · Bias · Bland–Altman plots**

## Overview

This project compares methods used to assess kidney function, examines differences between their estimates and presents the comparison through statistical summaries and graphics.

The technical analysis compares estimates of glomerular filtration rate with a reference measurement based on plasma iohexol clearance. The presentation focuses on the broader method-comparison question.

## Methods and workflow

1. Prepare measurements and inspect descriptive distributions.
2. Compare estimates across methods and summarise their differences and bias.
3. Use Bland–Altman plots and related visualisations to assess agreement.
4. Document the limits of the comparison.

The project illustrates statistical methods; this repository does not provide individual-level clinical data or validated clinical recommendations.

## Available files

| File | Contents |
|---|---|
| [Method-comparison analysis](analyse_iohexol.R) | Original R analysis, including exploratory sections and visualisations. |

## Data and reproducibility

The script expects `Classeur1.xlsx` and intermediate objects such as `Iohexol3r_up`. Their preparation must be checked before execution. Source data are not included, and the exploratory script contains repeated sections. No standalone figure exports are supplied in this folder.

The full analysis has not been independently executed or validated for this portfolio.

See [software dependencies](DEPENDANCES.md) for the tools identified in the supplied scripts.

[Back to portfolio](../../README.md) · [Reproducibility notes](../../REPRODUCTIBILITE.md) · [Use and attribution](../../CONDITIONS.md)
