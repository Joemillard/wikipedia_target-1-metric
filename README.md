# 

This repository contains all the scripts used for the text analysis carried out in the below paper:

> **Millard _et al_., The Digital Biodiversity Index (DBI): a Wikipedia-derived conservation culturomics metric for public biodiversity awareness and Aichi Target 1.**

There are 7 R scripts and 1 python script in this analysis, run sequentially from 01-05. All key data written to csv are also hosted here (with the exception of the raw view data which will be hosted elsewhere), as well as any additional csvs read in throughout the analysis. Most scripts read in script "00. functions.R", which calls in additional functions required throughout the analysis.

Note that this repository is written relative to an R project file (.Rproj). Those wanting to reproduce this analysis should download the whole repo, and then open via the .Rproj file. Also note that all required packages are included as a vector at the top of each script.

The required script are as follows:

```R/00. functions.R```<br>
```R/01. download_pollinator_views.py```<br>
```R/02. merge_IUCN_pollinating_data.R```<br>
```R/03. iucn_pollinator_trends.R```<br>
```R/03a. iucn_pollinator_trends_all``` <br>
```R/03b. iucn_pollinator_trends_amphib_rept.R```<br>
```R/04. models_lambda.R```<br>
```R/05. counting_views.R```<br>