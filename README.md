# spatialRF: R package for spatial regression with Random Forest

## Introduction

The package **spatialRF** facilitates fitting spatial regression models on regular or irregular data with Random Forest, using the **ranger** package under the hood [(Wright and Ziegler 2017)](file:///tmp/mozilla_blas0/v77i01.pdf), and does so by generating *spatial predictors* that allow the model to take into account the spatial structure of the training data. The end goal is minimizing the spatial autocorrelation of the model residuals as much as possible. Spatial autocorrelation in the residuals indicate that there is a source of spatial autocorrelation in the model variance that cannot be explained by the model predictors, and it is a clear sign that there are important variables missing from the model. In an ideal model, the residuals are not autocorrelated, and should be centered around zero, but this is rarely the case when working with spatial data.

This package implements two main methods to generate *spatial predictors* from the distance matrix of the data points:


  + Principal coordinate analysis of neighbor matrices [(Dray, Legendre, and Peres-Neto 2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925).
  + Distance matrix columns as explanatory variables [(Hengl et al. 2018)](https://peerj.com/articles/5518/).

The package provides as well a set of tools to identify variable interactions, tune random forest hyperparameters, assess model performance on spatially independent data folds, and examine the resulting models via importance plots, and response curves and surfaces. 

## Installing the package

The package is not yet in the CRAN repositories, so at the moment it must be installed from GitHub as follows.

```r
devtools::install_github(
  repo = "blasbenito/spatialRF", 
  ref = "main"
  )
```


## Fitting a spatial model with **spatialRF**

Let's start with a simple example, to then unpack what is happening inside the package functions.

### The data

The package includes an example dataset named **plant_richness_df**


