# WPP–WMFD Survey Catalogue Record Linkage and Task Automation

**Patrick Ntwali Matabaro · [Gpat95](https://github.com/Gpat95)**

**R · Data harmonisation · Record linkage · Validation · Automated exports**

## Overview

This project develops a workflow for linking survey records from the WPP and World Male Fertility Database (WMFD) catalogues. Survey names, reference years and naming conventions differ across sources, so an exact join alone is insufficient.

The objective is to identify reliable correspondences and transfer selected WPP metadata to WMFD while retaining a review stage for ambiguous records.

## Methods and workflow

### 1. Data preparation and harmonisation

Import the catalogues, construct country–survey keys, normalise labels, harmonise survey-year ranges and standardise known naming differences.

### 2. Deterministic matching

Compare country codes, survey names, survey types and reference years. Retain unambiguous correspondences and identify records requiring further checks.

### 3. Score-based candidate matching

Generate candidates within countries and combine label similarity, token similarity, year proximity and metadata consistency. Rank candidates into A (very likely), B (likely), C (possible) and D (low confidence). Automatic acceptance of A and B candidates also depends on uniqueness and score-margin conditions.

### 4. Manual validation

Separate ambiguous matches for review rather than forcing uncertain records into the final catalogue. Keep a record of validation decisions.

### 5. Cardinality controls

Check duplicate keys, one-to-one correspondences, one-to-many or many-to-one cases and unresolved records. Transfer metadata only after the required checks or an explicit manual resolution.

### 6. Integration and outputs

Build the enriched WMFD catalogue and export candidates, confidence classes and matching diagnostics.

```text
WPP + WMFD catalogues
        ↓
Data harmonisation → Deterministic matching
        ↓                    ↓
Remaining candidates    Accepted matches
        ↓                    │
Similarity scoring → Validation of ambiguous cases
        └────────────────────┘
                  ↓
         Cardinality checks
                  ↓
       Metadata transfer and exports
```

**Methodological note:** the script uses the term “probabilistic” for parts of its scoring workflow. The scores rank candidate matches; they are not calibrated probabilities or proof that two records describe the same survey. No match rate or time saving is claimed without execution on the source data.

## Available files

| File | Contents |
|---|---|
| [Catalogue linkage script](Matching_WPP_vers_WMFD_Script_byPat.R) | Preparation, matching, manual decisions, validation and Excel exports. |

## Data and reproducibility

The source catalogues are not distributed. The script expects `WPP2024_F02_Metadata.xlsx` (sheet `Overall`) and `catalog_WMFD.xlsx` (sheet `Sheet1`). The current script retains project-specific rules and manual matching decisions tied to the original records; these need review when inputs change.

The workflow has not been rerun for this portfolio. Its reproducibility depends on the input schema, source versions and validation decisions.

See [software dependencies](DEPENDANCES.md) for the tools identified in the supplied scripts.

[Back to portfolio](../../README.md) · [Reproducibility notes](../../REPRODUCTIBILITE.md) · [Use and attribution](../../CONDITIONS.md)
