spatialRF: easy spatial regression with Random Forest
================

-   [Introduction](#introduction)
-   [Installing the package](#installing-the-package)
-   [Fitting a spatial model with
    **spatialRF**](#fitting-a-spatial-model-with-spatialrf)

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
  ref = "main"
  )
```

    ## Skipping install of 'spatialRF' from a github remote, the SHA1 (d12890a9) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(spatialRF)
```

    ## 
    ## Attaching package: 'spatialRF'

    ## The following object is masked from 'package:stats':
    ## 
    ##     rf

There are a few other libraries that will be useful during this
tutorial.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.6     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
```

## Fitting a spatial model with **spatialRF**

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
of the residuals.

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

This plot is telling us that the spatial autocorrelation of the
residuals is highly positive for the distances 0 and 1500, and from
there it decays until becoming neutral at 3000km. Here we can say that
the model is missing something.

### Fitting a spatial model

The non-spatial model fitted above can be converted into a spatial model
easily with **rf\_spatial()**.

``` r
model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential",
  verbose = FALSE
  )
```

The plot below compares the Moran’s I of the residuals of the spatial
(green) and non spatial (purple) models. It shows that **rf\_spatial()**
has managed to reduce the spatial autocorrelation (measured via [Moran’s
I](https://en.wikipedia.org/wiki/Moran%27s_I)) of the model residuals
for the three selected distances.

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
p1 <- plot_importance(model.non.spatial) + ggplot2::ggtitle("Non-spatial model")
p2 <- plot_importance(model.spatial) + ggplot2::ggtitle("Spatial model")
p1 | p2
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> Let’s check
the five most important variables of the spatial model.

``` r
kableExtra::kable(
  head(model.spatial$variable.importance$per.variable)
) %>% kableExtra::kable_styling()
```

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
0.2987865
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_hypervolume
</td>
<td style="text-align:right;">
0.2507690
</td>
</tr>
<tr>
<td style="text-align:left;">
spatial\_predictor\_0\_6
</td>
<td style="text-align:right;">
0.2368151
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average
</td>
<td style="text-align:right;">
0.2316968
</td>
</tr>
<tr>
<td style="text-align:left;">
spatial\_predictor\_0\_2
</td>
<td style="text-align:right;">
0.2028692
</td>
</tr>
<tr>
<td style="text-align:left;">
spatial\_predictor\_1500\_2
</td>
<td style="text-align:right;">
0.1724050
</td>
</tr>
</tbody>
</table>

Three of these are spatial predictors generated by **rf\_spatial()**.
How do these spatial predictors look?

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
