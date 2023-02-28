# Identifying Individuals From IU Walking Acceleration Data

This repository contains code to

1.  Read in and process the IU dataset from the raw files
2.  Visualize the acceleration data
3.  Fit models to identify subjects from their walking pattern

File Description

-   dataprocess.R

    -   Read in raw data, combine into one csv, filter to just walking

-   functions.R

    -   All functions needed for model fitting

-   pipeline.R

    -   Pipeline to fit models and return results
