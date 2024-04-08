# Code repository - TNBC IMC study

This repository contains all analysis scripts for the triple negative breast cancer IMC study from Meyer et al., 2024. It is structured as a [workflowr](https://workflowr.github.io/workflowr/) project.

**DISCLAIMER:** The manuscript associated with this code is in the peer-review process and thus the code will **change** accordingly.

## Repository structure

The `analysis/` directory contains all relevant data analysis scripts for *IMC* data analysis. It contains two sub-folders `/metadata/` and `/validation/` with respective scripts (including scripts for *IF* quantification / *TCGA BRCA* / *scRNAseq* organoid analysis).

### Software requirements

All **R code** was run with `R version 4.3.2 (2023-10-31)` on Ubuntu 20.04.4. Each `.Rmd` file contains a **Software requirements** section that lists all relevant packages for data analysis and processing. 

### Data access 

The raw data associated with this repository is available on **zenodo**: 1. IMC data [zenodo.org/TODO]() 2. IF data [zenodo.org/TODO]() or from the respective original articles.
