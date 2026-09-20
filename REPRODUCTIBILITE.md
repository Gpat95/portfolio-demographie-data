# Reproducibility Notes

## Scope

This repository organises research and applied-analysis material for a professional portfolio. Documentation and figure captions are in English; original scripts, figures, reports and slides are preserved as supplied. The documentation update is not an independent scientific validation, and no analysis has been rerun.

## What can be reviewed directly

Project descriptions, source code, PDF presentations, reports and figure galleries can be read without the original datasets. Curated examples demonstrate parts of a workflow and may not recreate every original output.

## Known limitations

- Source datasets and several intermediate files are not included.
- Original versions of R, Stata and packages are not fully documented or pinned.
- Some scripts require objects already present in the session, adapted variable names or a specific sequence of preprocessing steps.
- Some research scripts contain exploratory or interactive sections and overwrite output files.
- The energy application requires a prepared local dataset; the Quarto analysis additionally requires authorised SQL access.
- The supplied figures are original exports. Their underlying estimates, confidence intervals and model assumptions have not been recalculated or audited here.

## Before running a project

1. Read its README and software dependency list.
2. Obtain the source data and the appropriate permissions.
3. Work in a separate directory using copies of the inputs.
4. Check the data schema, variable definitions, intermediate objects and output paths.
5. Review the survey design, weights, missing-value handling and modelling assumptions.
6. Execute the workflow in stages, inspect diagnostics and record software versions.

The `.gitignore` file excludes many local data and output formats. It does not replace checking the actual files before publication.
