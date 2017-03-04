# Qchlorophyll

R Package for geographical data analysis and prediction.

The package aims to offer a set of tools for analysing marine bioregions and applying machine learning techniques (supervised and unsupervised).

On [MilanoR](http://www.milanor.net/blog/r-satellite-data-to-identify-marine-bioregions/) you can find an article describing the main process in which the package was used.

Example plot of interpolated net heat flux over a user-specified spatial grid:

![Interpolation](https://cloud.githubusercontent.com/assets/13961654/23580417/bcd0d03e-0101-11e7-83da-cc78d9326852.jpeg)

---

## Functionalities
The package provides the following functions:

####  Functions for loading geographical .nc data easily and quickly in a nice dataframe

- A set of functions for loading and extracting geographical data from .nc files.
- A set of functions for loading geographical data with different frequency stored in .nc files.

#### Functions for data cleaning and descriptive statistics

- A set of functions for cleaning the data and calculating any user-defined descriptive statistics/index.

#### K-mean unsupervised analysis functions

- A set of functions for running k-means on the data and extracting information from the analysis.

#### Missing data imputation functions

- A set of functions for imputing missing data. 

#### Random forest fitting, prediction and plotting functions

- A set of functions for loading sets of yearly .csv files grouped in local folders.
- A set of functions for fitting a random forest model, getting information from the fitted model, predicting and plotting predictions in a geographical map.

#### Geographical data manipulating functions

- A set of functions for changing (increasing or decreasing) the resolution of geographical data. Given a spatial grid (ie a set of longitude and latitude coordinates) the functions convert the resolution of the data supplied to the selected resolution.

---

## Dependencies
Requires the following packages:

- ncdf4
- dplyr (>= 0.4.3)
- tidyr (>= 0.4.1)
- lubridate (>= 1.5.6)
- ggplot2 (>= 2.1.0)
- clusterSim
- mice
- lattice
- randomForest (>= 4.6-12)
- grid (>= 3.2.2)
- lazyeval (>= 0.1.10)
- stringi
- sp
- gstat

 
 ---
 
## Examples
The scripts folder contains some examples of use. In the following lines you can find a quick shortcut list to each .rmd example of use file.

- [Definitive guide to data loading in Qchlorophyll](https://github.com/pegoraro/qchlorophyll/blob/master/scripts/script-esempio-caricamento-dati-definitivo.rmd)
- [Descriptive summary statistics calculation with Qchlorophyll](https://github.com/pegoraro/qchlorophyll/blob/master/scripts/script-esempio-calcolo-statistiche.R)
- [K-means analysis example script with Qchlorophyll](https://github.com/pegoraro/qchlorophyll/blob/master/scripts/script-esempio-kmeans.R)
- [Random forest model: data loading, fitting and predicting with Qchlorophyll](https://github.com/pegoraro/qchlorophyll/blob/master/scripts/script-esempio-rf-presentazione_def.rmd)
- [Spatial data resizing in Qchlorophyll](https://github.com/pegoraro/qchlorophyll/blob/master/scripts/script-esempio-aumento-risoluzione.rmd).

---
Heat map of net heat flux:
![qnet_1](https://cloud.githubusercontent.com/assets/13961654/23580388/361ecadc-0101-11e7-9ee0-068ec40016f5.jpeg)
---
Example of partial dependence plot of y vs other variables
![bloom_start_vs_other](https://cloud.githubusercontent.com/assets/13961654/23548632/ba64a220-0008-11e7-9a6d-73b4f20b4a97.png)
---
Example of bioregion variables prediction on a predictive map for each year:
![predictive_map](https://cloud.githubusercontent.com/assets/13961654/23548646/c47bdcce-0008-11e7-9158-6c63e56321c7.png)
---
and average predicted map
![predictive_map_average](https://cloud.githubusercontent.com/assets/13961654/23548639/c08111fc-0008-11e7-9e94-8c860e5ec2d7.png)
---
Example of spatial data resizing on the qnet variable (net heat flux), here is a heat map of the outcome:
![qnet_interp_heat_map](https://cloud.githubusercontent.com/assets/13961654/23580417/bcd0d03e-0101-11e7-83da-cc78d9326852.jpeg)
---
and as a comparison, the original available data:
![qnet_interp_density](https://cloud.githubusercontent.com/assets/13961654/23580419/c075be16-0101-11e7-9afd-9f1b9422f5e2.jpeg)
