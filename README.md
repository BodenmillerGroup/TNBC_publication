
# A stratification system for breast cancer based on basoluminal tumor cells and spatial tumor architecture, Meyer et al., Cancer Cell, 2025 

This repository contains all code scripts for the breast cancer study from Meyer et al., 2025.

Read the **Cancer Cell paper** here: <https://doi.org/10.1016/j.ccell.2025.06.019>

## Abstract

Rapid recurrence is common in triple-negative breast cancer. To better understand drivers of recurrence, we use imaging mass cytometry to characterize the tumor phenotype landscapes of 215 triple-negative breast cancer patients. We observe high inter-tumor heterogeneity with eleven tumor cell phenotypes, each of which dominates in an individual patient, and identify a tumor cell phenotype with reduced basoluminal lineage fidelity and stem-like traits that is correlated with rapid disease recurrence. Scoring of tumor-CD8+ T cell interactions identifies patients with inflamed tumors and high HLADR expression. We combine these features in multi-omics analyses of 8 cohorts with 3737 patients across all molecular subtypes to propose five prognostic breast cancer subtypes distinguished by tumor cytokeratin expression profiles and CD8+ T cell spatial patterns. This stratification scheme has direct clinical implications: inflamed tumors show good prognosis and high immunotherapy response rates, whereas patients dominated by basoluminal tumor cells have poor prognosis. 

## Repository structure

This repository contains data analysis scripts for **re-creating all manuscript figures** based on provided data objects from **zenodo** (<https://zenodo.org/records/15304181>). 

Furthermore, the `/pipelines/` directory contains all other data analysis scripts used. It has a `/discovery/` subfolder that contains all relevant scripts for *IMC* data analysis in the discovery cohort, which also contains one sub-folder `/other/` with support scripts. The `/validation/` subfolder contains all scripts for validation of findings (including scripts for *mIF* quantification / *TCGA BRCA* / *MDACC* / *ISPY2* / *PDL1-IHC* / *scRNAseq* patient and organoid analysis).

## Software note

Most scripts are written in the statistical programming language **R**. All **R code** was run with `R version 4.3.2 (2023-10-31)` on Ubuntu 20.04.4. Each `.Rmd` file contains a **Software requirements** section that lists all relevant packages for data analysis and processing. File paths have to be adjusted by the user.

## Data access

Raw data is provided on **zenodo** (IMC: <https://zenodo.org/records/10890543>, IF: <https://zenodo.org/records/10942403>, R data: <https://zenodo.org/records/15304181>) and **GEO** (scRNA-seq organoid data - GEO Series number: GSE298343).

NOTE: We plan to add the processed dataset to the **imcdatasets** R/Bioconductor package (<https://github.com/BodenmillerGroup/imcdatasets>), which will facilitate easy community access and direct integration into computational workflows.

## Issues

For problems and questions, please open an issue [here](https://github.com/BodenmillerGroup/TNBC_publication/issues).

## Code-related contact

[Lasse Meyer](https://github.com/lassedochreden) lasse.meyer 'at' dqbm.uzh.ch
