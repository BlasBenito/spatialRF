spatialRF: easy spatial regression with Random Forest
================

-   [Introduction](#introduction)
-   [Installing the package](#installing-the-package)
-   [Fitting spatial models with
    `spatialRF`](#fitting-spatial-models-with-spatialrf)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Introduction

The package **spatialRF** facilitates fitting spatial regression models
on regular or irregular data with Random Forest, using the **ranger**
package under the hood [(Wright and Ziegler
2017)](file:///tmp/mozilla_blas0/v77i01.pdf), and does so by generating
*spatial predictors* that allow the model to take into account the
spatial structure of the training data. The end goal is minimizing the
spatial autocorrelation of the model residuals as much as possible.
Spatial autocorrelation in the residuals indicate that there is a source
of spatial autocorrelation in the model variance that cannot be
explained by the model predictors, and it is a clear sign that there are
important variables missing from the model. In an ideal model, the
residuals are not autocorrelated, and should be centered around zero,
but this is rarely the case when working with spatial data.

This package implements two main methods to generate *spatial
predictors* from the distance matrix of the data points:

-   Principal coordinate analysis of neighbor matrices [(Dray, Legendre,
    and
    Peres-Neto 2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925).
-   Distance matrix columns as explanatory variables [(Hengl et
    al. 2018)](https://peerj.com/articles/5518/).

The package also provides a set of tools to identify variable
interactions, tune random forest hyperparameters, assess model
performance on spatially independent data folds, and examine the
resulting models via importance plots, and response curves and surfaces.

## Installing the package

The package is not yet in the CRAN repositories, so at the moment it
must be installed from GitHub as follows.

``` r
remotes::install_github(
  repo = "blasbenito/spatialRF", 
  ref = "main",
  quiet = TRUE
  )
library(spatialRF, quietly = TRUE)
```

    ## 
    ## Attaching package: 'spatialRF'

    ## The following object is masked from 'package:stats':
    ## 
    ##     rf

There are a few other libraries that will be useful during this
tutorial.

``` r
library(kableExtra)
options(knitr.table.format = "html")
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:kableExtra':
    ## 
    ##     group_rows

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
options(dplyr.summarise.inform = FALSE)
library(magrittr)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
```

## Fitting spatial models with `spatialRF`

Let’s start with a simple example, to then unpack what is happening
inside the package functions.

### The data

The package includes an example dataset named **plant\_richness\_df**, a
data frame with plant species richness and predictor variables for 227
ecoregions in the Americas; a distance matrix among the ecoregion edges
named, well, **distance\_matrix**, and **plant\_richness\_sf**, an sf
file containing the centroids of the polygons represented in
**plant\_richness\_df**.

``` r
data(plant_richness_df)
help(plant_richness_df)
data(plant_richness_sf)
data(distance_matrix)
```

The response variable of **plant\_richness\_df** is
“richness\_species\_vascular”, with the total count of vascular plant
species found on each ecoregion. The figure below shows the centroids of
each ecoregion along with their associated value of the response
variable.

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The predictors (columns 5 to 21) represent diverse factors such as
sampling bias, climatic variables, human presence and impact,
topography, geographical fragmentation, and features of the neighbors of
each ecoregion. The figure below shows the scatterplots of the response
variable (y axis) against each predictor (x axis).

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Fitting a non-spatial model

To fit a Random Forest model we use the `rf()` function, that takes the
data, the names of the response and the predictors, the distance matrix,
and a vector of distance thresholds (in the same units as the distances
in **distance\_matrix**). These distance thresholds are the
neighborhoods at which the model will check the spatial autocorrelation
of the residuals. Their values may depend on the spatial scale of the
data, and the ecological system under study.

``` r
model.non.spatial <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1500, 3000),
  seed = 100,
  verbose = FALSE
)
```

The spatial autocorrelation of the residuals can be plotted with
`plot_moran()`

``` r
plot_moran(model.non.spatial)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

According to the plot, the spatial autocorrelation of the residuals is
highly positive for the distances 0 and 1500, while it becomes
non-significant (p-value &gt; 0.05, whatever that means) at 3000km. This
model is definitely missing something.

### Fitting a spatial model

The non-spatial model fitted above can be converted into a spatial model
easily with `rf_spatial()` (this is because **model.non.spatial** has a
slot named “ranger.arguments” that contains the model configuration and
the data used to train it).

``` r
model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE
  )
```

The plot below compares the Moran’s I of the residuals of the spatial
(green) and non spatial (purple) models. It shows that `rf_spatial()`
has managed to reduce the spatial autocorrelation (measured via [Moran’s
I](https://en.wikipedia.org/wiki/Moran%27s_I) with the function
`moran_multithreshold()`) of the model residuals for the three selected
distances.

``` r
plot_moran(model.spatial)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

It has done so by generating spatial predictors from the distance
matrix, and introducing them into the model one by one until the spatial
autocorrelation of the residuals is reduced as much as possible. If we
compare the variable importance plots of both models, we can see that
the spatial model has an additional set of dots under the name
“spatial\_predictors”, and that the maximum importance of a few of these
spatial predictors matches the importance of the most relevant
non-spatial predictors.

``` r
p1 <- plot_importance(
  model.non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 
p2 <- plot_importance(
  model.spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")
p1 | p2 
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> If we take a
look to the five most important variables in **model.spatial** we will
see that a few of them are spatial predictors.

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:right;">
importance
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
human\_population
</td>
<td style="text-align:right;">
0.3518442
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_hypervolume
</td>
<td style="text-align:right;">
0.2845224
</td>
</tr>
<tr>
<td style="text-align:left;">
spatial\_predictor\_0\_2
</td>
<td style="text-align:right;">
0.2117031
</td>
</tr>
<tr>
<td style="text-align:left;">
spatial\_predictor\_0\_6
</td>
<td style="text-align:right;">
0.2113401
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average
</td>
<td style="text-align:right;">
0.2078517
</td>
</tr>
<tr>
<td style="text-align:left;">
spatial\_predictor\_1500\_2
</td>
<td style="text-align:right;">
0.1696063
</td>
</tr>
</tbody>
</table>

Spatial predictors, as shown below, are smooth surfaces representing
neighborhood among records at different spatial scales. But where do
they come from? How are they generated and included in the model?

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Generation and selection of spatial predictors

The final model includes 38 spatial predictors. In this particular
model, that uses the method “mem.moran.sequential” (Moran’s Eigenvector
Maps as described in [(Dray, Legendre, and Peres-Neto
2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925)),
the spatial predictors have been generated by the function
`mem_multithreshold()`, for every distance threshold selected by the
user, as follows:

-   `weights_from_distance_matrix()` sets the values of the distance
    matrix below a given distance threshold to 0, computes
    `1/distance_matrix` to convert distances into weights, sets the
    diagonal to 0, and normalizes the weights.
-   `double_center_distance_matrix()` double-centers the distance
    matrix, so the sum of each row and each column is zero.
-   `mem()` computes the positive eigenvectors of the double-centered
    matrix.

The function `rank_spatial_predictors()` ranks the spatial predictors
from higher to lower spatial autocorrelation (computed with `moran()`),
and `select_spatial_predictors_sequential()` introduces them one by one
into the model, and selects the smallest subset that minimizes the
Moran’s I of the residuals while maximizing the R squared of the model,
as shown in the optimization plot below (every dot represents one
spatial predictor, and its size matches their position in the ranking,
and their order of inclusioin in the model).

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> \#\#
Assessing model performance

Models fitted with `rf()` or `rf_spatial()` have a performance slot with
several performance measures.

``` r
print_performance(model.spatial)
```

    ## 
    ## Model performance 
    ##   - R squared (OOB):                 0.547
    ##   - Pseudo R squared:                0.739
    ##   - RMSE:                            2327.502
    ##   - Normalized RMSE:                 0.672

R squared is computed on the out-of-bag data (fraction of data not used
while training each regression tree in the forest), while the other
three performance measures are computed by comparing observations and
model predictions. These performance scores are not computed on
independent data, and therefore do not represent the model ability to
predict over *unseen* data.

The function `rf_evaluate()` separates the data into a number of
spatially independent training and testing folds, fits a model on each
training fold, predicts over each testing fold, and computes performance
measures, to finally aggregate them across model repetitions. Let’s see
how it works.

``` r
model.spatial <- rf_evaluate(
  model = model.spatial,
  xy = plant_richness_df[, c("x", "y")], #data coordinates
  repetitions = 30,                      #number of folds
  training.fraction = 0.75,              #training data fraction
  verbose = FALSE
)
```

The function generates a new slot in the model named “evaluation” with
several objects that summarize the spatial cross-validation results.

``` r
names(model.spatial$evaluation)
```

    ## [1] "training.fraction" "spatial.folds"     "per.fold"         
    ## [4] "per.model"         "aggregated"

The slot “spatial.folds”, produced by `make_spatial_folds()` contains
the indices of the training and testing cases for each cross-validation
repetition. The maps below show two sets of training and testing spatial
folds.

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

The functions `plot_evaluation()` and `print_evaluation()` allow to see
the evaluation results as a plot or as a table. The plot below shows the
performance scores of the “Full” model (original model introduced into
`rf_evaluate()`), the model fitted on the training data (“Training”),
and the results of the Training model predicted over the “Testing” data.
From these performance scores, only the ones labeled as “Testing”
represent model performance on unseen data.

``` r
plot_evaluation(model.spatial, notch = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

hola
