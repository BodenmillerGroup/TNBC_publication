# Code repository - TNBC IMC study

This repository contains all analysis scripts for the triple negative breast cancer IMC study from Meyer et al., 2024.

**DISCLAIMER:** The manuscript associated with this code is in the peer-review process and thus the code will **change** accordingly.

## Repository structure

The `discovery/` directory contains all relevant data analysis scripts for *IMC* data analysis in the discovery cohort. It contains one sub-folder `/metadata/` with scripts for clinical metadata annotation. 

The `/validation/` directory contains all scripts for validation of findings (including scripts for *mIF* quantification / *TCGA BRCA* / *MDACC* / *PDL1-IHC* / *scRNAseq* organoid analysis).

### Software requirements

Most scripts are written in the statistical programming language **R**. All **R code** was run with `R version 4.3.2 (2023-10-31)` on Ubuntu 20.04.4. Each `.Rmd` file contains a **Software requirements** section that lists all relevant packages for data analysis and processing. File paths have to be adjusted by the user.

### Data access 

Upon publication, the raw data associated with this repository will available on **zenodo** or from the respective original research articles.
