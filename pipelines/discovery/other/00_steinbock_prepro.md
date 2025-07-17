# A stratification system for breast cancer based on basoluminal tumor cells and spatial tumor architecture, Meyer et al., 2025 

**DISCLAIMER**: Pipeline scripts contain all code used for data presented in the publication but also small exploratory data analysis parts. For reproducing the figures, please refer to the figure-related scripts and cleaned data objects provided on **Zenodo** (<https://zenodo.org/records/>). File paths have to be adjusted by the user. 

# Discovery 

Data analysis scripts for IMC data analysis in the discovery cohort. 

# Support Script: IMC data preprocessing

Here, we used the [steinbock](https://bodenmillergroup.github.io/steinbock/latest/) framework for IMC data preprocessing. 

We used the pre-trained deep-learning mesmer model to perform single-cell segmentation with different channel-combinations for testing.

**1. Collect the data**

To assemble the folder structure needed for steinbock, we copy all files of interest (.mcd/.txt) from to `/mnt/tnbc_volume/TNBC`. The latter folder will be the steinbock working directory.

**2. Install and run steinbock from docker container**

We define the alias to use steinbock from the command line:

`alias steinbock="docker run -v-v /mnt/rcc_volume/TNBC:/data ghcr.io/bodenmillergroup/steinbock:0.15.0"`

**3. Preprocess images**

Following call will generate a `panel.csv` file (in the steinbock panel file format).

`steinbock preprocess imc panel`

Using the newly generated panel, we will modify/add following columns: **deepcell**, **clean_target**.

Next, this call converts .mcd/.txt files in the raw data directory to TIFF and filters hot pixels.   
Output directory is `img`. 

`steinbock preprocess imc images --hpf 50`

**4. DeepCell segmentation**

We will use the pre-trained `Mesmer` model to perform single-cell segmentation with different cytoplasmic channels for testing.   
For more detailed information see [here](https://github.com/vanvalenlab/intro-to-deepcell/tree/master/pretrained_models#mesmer-segmentation-model).
Output directory is `masks`. 

`steinbock segment deepcell --minmax`

*Nuclear channels (1)*: HistoneH3, Irdium 191/193  
*Cytoplasmic channels (2)*:  
`masks_1` = *HistoneH3, Irdium 191/193,* panCK, ECad, Vimentin, SMA, CD3, CD68   
`masks_2` = *HistoneH3, Irdium 191/193,* panCK, ECad, Vimentin, SMA, CD3, CD8a, CD68, CD20  
`masks_3` = *HistoneH3, Irdium 191/193,* panCK, ECad, Vimentin, SMA, CD3, CD8a, CD68  
`masks_4` = *Irdium191/193,* panCK, ECad, Vimentin, SMA, CD3, CD8a, CD68, CD44  
`masks_5`= *Irdium191/193,* panCK, ECad, Vimentin, SMA, CD3, CD8a, CD68

After quality control of the segmentation masks, we selected masks_4 for downstream analysis.

**5. Measure single-cell features**

Following calls were used to measure object-specific (here cell-specific) features. Output directories are `intensities`, `regionprops`, `neighbors`.

```
steinbock measure intensities  
steinbock measure regionprops  
steinbock measure neighbors --type expansion --dmax 4
```
**6. Create TMA tissue masks**

In order to calculate cell densities, we estimated TMA tissue areas using in-house **ilastik** (for classification) and **cellprofiler** (for measurement) pipelines.  
```