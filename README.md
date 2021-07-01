`spatialRF`: Easy Spatial Regression with Random Forest
================

-   [Introduction](#introduction)
-   [Development](#development)
-   [Applications](#applications)
-   [Citation](#citation)
-   [Install](#install)
-   [Data requirements](#data-requirements)
-   [Example data](#example-data)
-   [Reducing multicollinearity in the
    predictors](#reducing-multicollinearity-in-the-predictors)
-   [Finding promising variable
    interactions](#finding-promising-variable-interactions)
-   [Fitting a (non-spatial) Random Forest model with
    `rf()`](#fitting-a-non-spatial-random-forest-model-with-rf)
    -   [Residuals](#residuals)
    -   [Variable importance](#variable-importance)
    -   [Response curves and surfaces](#response-curves-and-surfaces)
    -   [Model performance](#model-performance)
    -   [Spatial cross-validation](#spatial-cross-validation)
    -   [Other important things stored in the
        model](#other-important-things-stored-in-the-model)
-   [Fitting a spatial model with
    `rf_spatial()`](#fitting-a-spatial-model-with-rf_spatial)
-   [Tuning Random Forest
    hyperparameters](#tuning-random-forest-hyperparameters)
-   [Repeating a model execution](#repeating-a-model-execution)
-   [Taking advantage of the `%>%` pipe](#taking-advantage-of-the-pipe)
-   [Comparing several models](#comparing-several-models)
-   [Working with a binomial
    response](#working-with-a-binomial-response)
-   [Generating spatial predictors for other modelling
    methods](#generating-spatial-predictors-for-other-modelling-methods)

<!---
[![R-CMD-check](https://github.com/BlasBenito/spatialRF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BlasBenito/spatialRF/actions/workflows/R-CMD-check.yaml)
-->
<!-- badges: start -->

[![Devel-version](https://img.shields.io/badge/devel%20version-1.1.2-blue.svg)](https://github.com/blasbenito/spatialRF)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN](https://img.shields.io/badge/CRAN-not_published_yet-red)](https://github.com/blasbenito/spatialRF)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4745208.svg)](https://doi.org/10.5281/zenodo.4745208)

<!-- badges: end -->

# Introduction

The package **spatialRF** facilitates fitting spatial regression models
on regular or irregular data with Random Forest. It does so by
generating *spatial predictors* that help the model “understand” the
spatial structure of the training data with the end goal of minimizing
the spatial autocorrelation of the model residuals and offering honest
variable importance scores.

Two main methods to generate *spatial predictors* from the distance
matrix of the data points are implemented in the package:

-   Moran’s Eigenvector Maps [(Dray, Legendre, and
    Peres-Neto 2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925).
-   Distance matrix columns as explanatory variables [(Hengl et
    al. 2018)](https://peerj.com/articles/5518/).

The package is designed to minimize the code required to fit a spatial
model from a training dataset, the names of the response and the
predictors, and a distance matrix, as shown below.

``` r
spatial.model <- spatialRF::rf_spatial(
  data = your_dataframe,
  dependent.variable.name = "your_response_variable",
  predictor.variable.names = c("predictor1", "predictor2", ..., "predictorN"),
  distance.matrix = your_distance_matrix
  )
```

**spatialRF** uses the fast and efficient `ranger` package under the
hood [(Wright and Ziegler 2017)](https://arxiv.org/abs/1508.04409). It
also provides tools to identify potentially interesting variable
interactions, tune random forest hyperparameters, assess model
performance on spatially independent data folds, and examine the
resulting models via importance plots, response curves, and response
surfaces.

# Development

This package is reaching its final form, and big changes are not
expected at this stage. However, it has many functions, and even though
all them have been tested, only one dataset has been used for those
tests. You will find bugs, and something will go wrong almost surely. If
you have time to report bugs, please, do so in any of the following
ways:

-   Open a new issue in the [Issues GitHub page of the
    package](https://github.com/BlasBenito/spatialRF/issues).
-   Send me an email explaining the issue and the error messages with
    enough detail at blasbenito at gmail dot com.
-   Send a direct message to [my twitter
    account](https://twitter.com/blasbenito) explaining the issue.

I will do my best to solve any issues ASAP!

# Applications

The goal of `spatialRF` is to help fitting *explanatory spatial
regression*, where the target is to understand how a set of predictors
and the spatial structure of the data influences response variable.
Therefore, the spatial analyses implemented in the package can be
applied to any spatial dataset, regular or irregular, with a sample size
between \~100 and \~5000 cases (the higher end will depend on the RAM
memory available), a quantitative or binary (values 0 and 1) response
variable, and a more or less large set of predictive variables.

All functions but `rf_spatial()` work with non-spatial data as well if
the arguments `distance.matrix` and `distance.thresholds` are not
provided In such case, the number of training cases is no longer limited
by the size of the distance matrix, and models can be trained with
hundreds of thousands of rows. In such case, the spatial autocorrelation
of the model’s residuals is not assessed.

However, **when the focus is on fitting spatial models**, and due to the
nature of the *spatial predictors* used to represent the spatial
structure of the training data, **there are many things this package
cannot do**:

-   Predict model results over raster data.

-   Predict a model result over another region with a different spatial
    structure.

-   Work with “big data”, whatever that means.

-   Imputation or extrapolation (it can be done, but models based on
    spatial predictors are hardly transferable).

-   Take temporal autocorrelation into account (but this is something
    that might be implemented later on).

If after considering these limitations you are still interested, follow
me, I will show you how it works.

# Citation

There is a paper in the making about this package. In the meantime, if
you find it useful for your academic work, please cite the `ranger`
package as well, it is the true core of `spatialRF`!

*Marvin N. Wright, Andreas Ziegler (2017). ranger: A Fast Implementation
of Random Forests for High Dimensional Data in C++ and R. Journal of
Statistical Software, 77(1), 1-17. <doi:10.18637/jss.v077.i01>*

*Blas M. Benito (2021). spatialRF: Easy Spatial Regression with Random
Forest. R package version 1.1.0. doi: 10.5281/zenodo.4745208. url:
<https://blasbenito.github.io/spatialRF/>*

# Install

The package is not yet in the CRAN repositories, so at the moment it
must be installed from GitHub as follows. There are several branches in
the repository:

-   `main`: latest stable version (1.1.0 currently).
-   `development`: development version, usually very broken.
-   `v.1.0.9`: archived version.
-   `v.1.1.0`: archived version.

``` r
remotes::install_github(
  repo = "blasbenito/spatialRF", 
  ref = "development",
  force = TRUE,
  quiet = TRUE
  )
library(spatialRF)
```

There are a few other libraries that will be useful during this
tutorial.

``` r
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(randomForestExplainer)
library(pdp)
```

# Data requirements

The data required to fit random forest models with `spatialRF` must
fulfill several conditions:

-   **The input format is data.frame**. At the moment, tibbles are not
    fully supported.
-   **The number of rows must be somewhere between 100 and \~5000**, at
    least if your target is fitting spatial models. This limitation
    comes from the fact that the distance matrix grows very fast with an
    increasing number of training records, so for large datasets, there
    might not be enough RAM in your machine.
-   **The number of predictors should be larger than 3**. Fitting a
    Random Forest model is moot otherwise.
-   **Factors in the response or the predictors are not explicitly
    supported in the package**. They may work, or they won’t, but in any
    case, I designed this package for quantitative data alone. However,
    binary responses with values 0 and 1 are partially supported.
-   **Must be free of `NA`**. You can check if there are NA records with
    `sum(apply(df, 2, is.na))`. If the result is larger than 0, then
    just execute `df <- na.omit(df)` to remove rows with empty cells.
-   **Columns cannot have zero variance**. This condition can be checked
    with `apply(df, 2, var) == 0`. Columns yielding TRUE should be
    removed.
-   **Columns must not yield `NaN` or `Inf` when scaled**. You can check
    each condition with `sum(apply(scale(df), 2, is.nan))` and
    `sum(apply(scale(df), 2, is.infinite))`. If higher than 0, you can
    find what columns are giving issues with
    `sapply(as.data.frame(scale(df)), function(x)any(is.nan(x)))` and
    `sapply(as.data.frame(scale(df)), function(x)any(is.infinite(x)))`.
    Any column yielding `TRUE` will generate issues while trying to fit
    models with `spatialRF`.

# Example data

The package includes an example dataset that fulfills the conditions
mentioned above, named
[`plant_richness_df`](https://blasbenito.github.io/spatialRF/reference/plant_richness_df.html).
It is a data frame with plant species richness and predictors for 227
ecoregions in the Americas, and a distance matrix among the ecoregion
edges named, well,
[`distance_matrix`](https://blasbenito.github.io/spatialRF/reference/distance_matrix.html).

The package follows a convention throughout functions:

-   The argument `data` requires a training data frame.
-   The argument `dependent.variable.name` is the column name of the
    response variable.
-   The argument `predictor.variable.names` contains the column names of
    the predictors.
-   The argument `xy` takes a data frame or matrix with two columns
    named “x” and “y”, in that order, with the case coordinates.
-   The argument `distance.matrix` requires a matrix of distances
    between the cases in `data`.
-   The argument `distance.thresholds` is a numeric vector of distances
    at with spatial autocorrelation wants to be computed.

It is therefore convenient to define these arguments at the beginning of
the workflow.

``` r
#loading training data and distance matrix from the package
data(plant_richness_df)
data(distance_matrix)

#names of the response variable and the predictors
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]

#coordinates of the cases
xy <- plant_richness_df[, c("x", "y")]

#distance matrix
distance.matrix <- distance_matrix

#distance thresholds (same units as distance_matrix)
distance.thresholds <- c(0, 1000, 2000, 4000, 8000)

#random seed for reproducibility
random.seed <- 1
```

The response variable of `plant_richness_df` is
“richness\_species\_vascular”, that represents the total count of
vascular plant species found on each ecoregion. The figure below shows
the centroids of each ecoregion along with their associated value of the
response variable.

``` r
world <- rnaturalearth::ne_countries(
  scale = "medium", 
  returnclass = "sf"
  )

ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world, 
    fill = "white"
    ) +
  ggplot2::geom_point(
    data = plant_richness_df,
    ggplot2::aes(
      x = x,
      y = y,
      color = richness_species_vascular
    ),
    size = 2.5
  ) +
  ggplot2::scale_color_viridis_c(
    direction = -1, 
    option = "F"
    ) +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Plant richness") +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Plant richness of the American ecoregions") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The predictors (columns 5 to 21) represent diverse factors that may
influence plant richness such as sampling bias, the area of the
ecoregion, climatic variables, human presence and impact, topography,
geographical fragmentation, and features of the neighbors of each
ecoregion. The figure below shows the scatterplots of the response
variable (y axis) against each predictor (x axis).

**Note:** Every plotting function in the package now allows changing the
colors of their main features via specific arguments such as
`point.color`, `line.color`, or `fill.color`.

``` r
spatialRF::plot_training_df(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  ncol = 3,
  point.color = viridis::viridis(100, option = "F"),
  line.color = "gray30"
  )
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The function
[`plot_training_df_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df_moran.html)
helps assessing the spatial autocorrelation of the response variable and
the predictors across different distance thresholds. Low Moran’s I and
p-values equal or larger than 0.05 indicate that there is no spatial
autocorrelation for the given variable and distance threshold.

``` r
spatialRF::plot_training_df_moran(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1
    ),
  point.color = "gray40"
)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Reducing multicollinearity in the predictors

The functions
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.html)
and
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.html)
help reduce redundancy in the predictors by using different criteria
(bivariate R squared vs. [variance inflation
factor](https://www.statisticshowto.com/variance-inflation-factor/)),
while allowing the user to define an *order of preference*, which can be
based either on domain expertise or on a quantitative assessment (e.g.,
order of preference based on variable importance scores or model
coefficients). The preference order is defined as a character vector in
the `preference.order` argument of both functions, and does not need to
include the names of all predictors, but just the ones the user would
like to keep in the analysis.

Notice that I have set `cor.threshold` and `vif.threshold` to low values
because the predictors in `plant_richness_df` already have little
multicollinearity,. The default values (`cor.threshold = 0.75` and
`vif.threshold = 5`) should work well when combined together for any
other set of predictors.

``` r
preference.order <- c(
    "climate_bio1_average_X_bias_area_km2",
    "climate_aridity_index_average",
    "climate_hypervolume",
    "climate_bio1_average",
    "climate_bio15_minimum",
    "bias_area_km2"
  )

predictor.variable.names <- spatialRF::auto_cor(
  x = plant_richness_df[, predictor.variable.names],
  cor.threshold = 0.6,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 2.5,
    preference.order = preference.order
  )
```

    ## [auto_cor()]: Removed variables: human_footprint_average

    ## [auto_vif()]: Variables are not collinear.

The output of `auto_cor()` or `auto_vif()` has the class
“variable\_selection”, which can be used as input in every function
having the argument `predictor.variable.names`.

``` r
names(predictor.variable.names)
```

    ## [1] "vif"                   "selected.variables"    "selected.variables.df"

The slot `selected.variables` contains the names of the selected
predictors.

``` r
predictor.variable.names$selected.variables
```

    ##  [1] "climate_aridity_index_average"   "climate_hypervolume"            
    ##  [3] "climate_bio1_average"            "climate_bio15_minimum"          
    ##  [5] "bias_area_km2"                   "bias_species_per_record"        
    ##  [7] "climate_velocity_lgm_average"    "neighbors_count"                
    ##  [9] "neighbors_percent_shared_edge"   "human_population_density"       
    ## [11] "topography_elevation_average"    "landcover_herbs_percent_average"
    ## [13] "fragmentation_cohesion"          "fragmentation_division"         
    ## [15] "neighbors_area"                  "human_population"

# Finding promising variable interactions

Random Forests already takes into account variable interactions of the
form “variable `a` becomes important when `b` is higher than x”.
However, Random Forest can also take advantage of variable interactions
of the form `a * b`, across the complete ranges of the predictors, as
they are commonly defined in regression models, and “interactions” (not
the proper name, but used here for simplicity) represented by the first
component of a PCA on the predictors `a` and `b`.

The function
[`rf_interactions()`](https://blasbenito.github.io/spatialRF/reference/rf_interactions.html)
tests all possible interactions of both types among the most important
predictors, and suggesting the ones not correlated among themselves and
with the other predictors inducing an increase in the model’s R squared
on independent data.

``` r
interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  xy = xy,
  importance.threshold = 0.50, #uses 50% best predictors
  cor.threshold = 0.60, #max corr between interactions and predictors
  seed = random.seed,
  repetitions = 100,
  verbose = TRUE
  )
```

    ## Fitting and evaluating a model without interactions.

    ## Testing 28 candidate interactions.

    ## Interactions identified: 5

    ##  ┌──────────────────┬──────────────────┬──────────────────┬──────────────────┐
    ##  │ Interaction      │ Importance (% of │        R-squared │     Max cor with │
    ##  │                  │             max) │      improvement │       predictors │
    ##  ├──────────────────┼──────────────────┼──────────────────┼──────────────────┤
    ##  │ bias_area_km2..x │             59.5 │            0.096 │            0.60  │
    ##  │ ..bias_species_p │                  │                  │                  │
    ##  │ er_record        │                  │                  │                  │
    ##  ├──────────────────┼──────────────────┼──────────────────┼──────────────────┤
    ##  │ climate_bio1_ave │             97.6 │            0.067 │            0.34  │
    ##  │ rage..pca..human │                  │                  │                  │
    ##  │ _population_dens │                  │                  │                  │
    ##  │ ity              │                  │                  │                  │
    ##  ├──────────────────┼──────────────────┼──────────────────┼──────────────────┤
    ##  │ climate_bio1_ave │             96.3 │            0.049 │            0.24  │
    ##  │ rage..pca..neigh │                  │                  │                  │
    ##  │ bors_count       │                  │                  │                  │
    ##  ├──────────────────┼──────────────────┼──────────────────┼──────────────────┤
    ##  │ human_population │             68.8 │            0.021 │            0.55  │
    ##  │ ..x..bias_specie │                  │                  │                  │
    ##  │ s_per_record     │                  │                  │                  │
    ##  ├──────────────────┼──────────────────┼──────────────────┼──────────────────┤
    ##  │ bias_area_km2..p │             63.4 │            0.029 │            0.305 │
    ##  │ ca..neighbors_pe │                  │                  │                  │
    ##  │ rcent_shared_edg │                  │                  │                  │
    ##  │ e                │                  │                  │                  │
    ##  └──────────────────┴──────────────────┴──────────────────┴──────────────────┘

    ## Comparing models with and without interactions via spatial cross-validation.

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> The upper
panel in the plot plot above shows the relationship between the
interaction and the response variable. It also indicates the gain in R
squared (+R2), the importance, in percentage, when used in a model along
the other predictors (Imp. (%)), and the maximum Pearson correlation of
the interaction with the predictors. The violin-plot shows a comparison
of the model with and without the selected interaction made via spatial
cross-validation using 100 repetitions (see
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.html)
and
[`rf_compare()`](https://blasbenito.github.io/spatialRF/reference/rf_compare.html)
for further details).

The function also returns a data frame with all the interactions
considered. The columns are:

-   `interaction.name`: Interactions computed via multiplication are
    named `a..x..b`, while interactions computed via PCA are named
    `a..pca..b`.
-   `interaction.importance`: Importance of the interaction expressed as
    a percentage. If `interaction.importance == 100`, that means that
    the interaction is the most important predictor in the model fitted
    with the interaction and the predictors named in
    `predictor.variable.names`.
-   `interaction.metric.gain`: Difference in R squared (or AUC for
    models fitting a binary response) between a model with and a model
    without the interaction.
-   `max.cor.with.predictors`: The maximum Pearson correlation of the
    interaction with the predictors named in `predictor.variable.names`.
    Gives an idea of the amount of multicollinearity the interaction
    introduces in the model.
-   `variable.a.name` and `variable.b.name`: Names of the predictors
    involved in the interaction.
-   `selected`: `TRUE` if the interaction fulfills the selection
    criteria (importance higher than a threshold, positive gain in R
    squared or AUC, and Pearson correlation with other predictors lower
    than a threshold). The selected interactions have a correlation
    among themselves always lower than the value of the argument
    `cor.threshold`.

``` r
kableExtra::kbl(
  head(interactions$screening, 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
interaction.name
</th>
<th style="text-align:right;">
interaction.importance
</th>
<th style="text-align:right;">
interaction.metric.gain
</th>
<th style="text-align:right;">
max.cor.with.predictors
</th>
<th style="text-align:left;">
variable.a.name
</th>
<th style="text-align:left;">
variable.b.name
</th>
<th style="text-align:left;">
selected
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
bias\_area\_km2..x..bias\_species\_per\_record
</td>
<td style="text-align:right;">
59.547
</td>
<td style="text-align:right;">
0.096
</td>
<td style="text-align:right;">
0.5962899
</td>
<td style="text-align:left;">
bias\_area\_km2
</td>
<td style="text-align:left;">
bias\_species\_per\_record
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:right;">
97.590
</td>
<td style="text-align:right;">
0.067
</td>
<td style="text-align:right;">
0.3369664
</td>
<td style="text-align:left;">
climate\_bio1\_average
</td>
<td style="text-align:left;">
human\_population\_density
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average..pca..neighbors\_count
</td>
<td style="text-align:right;">
96.310
</td>
<td style="text-align:right;">
0.049
</td>
<td style="text-align:right;">
0.2441858
</td>
<td style="text-align:left;">
climate\_bio1\_average
</td>
<td style="text-align:left;">
neighbors\_count
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average..pca..neighbors\_percent\_shared\_edge
</td>
<td style="text-align:right;">
85.298
</td>
<td style="text-align:right;">
0.066
</td>
<td style="text-align:right;">
0.2215337
</td>
<td style="text-align:left;">
climate\_bio1\_average
</td>
<td style="text-align:left;">
neighbors\_percent\_shared\_edge
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
human\_population..x..bias\_species\_per\_record
</td>
<td style="text-align:right;">
68.786
</td>
<td style="text-align:right;">
0.021
</td>
<td style="text-align:right;">
0.5462406
</td>
<td style="text-align:left;">
human\_population
</td>
<td style="text-align:left;">
bias\_species\_per\_record
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average..pca..bias\_species\_per\_record
</td>
<td style="text-align:right;">
70.269
</td>
<td style="text-align:right;">
0.018
</td>
<td style="text-align:right;">
0.3469834
</td>
<td style="text-align:left;">
climate\_bio1\_average
</td>
<td style="text-align:left;">
bias\_species\_per\_record
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_area\_km2..pca..neighbors\_percent\_shared\_edge
</td>
<td style="text-align:right;">
63.380
</td>
<td style="text-align:right;">
0.029
</td>
<td style="text-align:right;">
0.3046471
</td>
<td style="text-align:left;">
bias\_area\_km2
</td>
<td style="text-align:left;">
neighbors\_percent\_shared\_edge
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_hypervolume..x..human\_population\_density
</td>
<td style="text-align:right;">
48.192
</td>
<td style="text-align:right;">
-0.006
</td>
<td style="text-align:right;">
0.5599486
</td>
<td style="text-align:left;">
climate\_hypervolume
</td>
<td style="text-align:left;">
human\_population\_density
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
neighbors\_count..pca..neighbors\_percent\_shared\_edge
</td>
<td style="text-align:right;">
63.965
</td>
<td style="text-align:right;">
0.016
</td>
<td style="text-align:right;">
0.1584006
</td>
<td style="text-align:left;">
neighbors\_count
</td>
<td style="text-align:left;">
neighbors\_percent\_shared\_edge
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_area\_km2..pca..neighbors\_count
</td>
<td style="text-align:right;">
56.843
</td>
<td style="text-align:right;">
0.012
</td>
<td style="text-align:right;">
0.2166191
</td>
<td style="text-align:left;">
bias\_area\_km2
</td>
<td style="text-align:left;">
neighbors\_count
</td>
<td style="text-align:left;">
TRUE
</td>
</tr>
</tbody>
</table>

The function returns a data frame with the response variables, the
predictors, and the selected interactions that can be used right away as
a training data frame. However, the function cannot say whether an
interaction *makes sense*, and it is up to the user to choose wisely
whether to select an interaction or not. In this particular case, and
just for the sake of simplicity, we will be using the resulting data
frame as training data.

``` r
#adding interaction column to the training data
plant_richness_df <- interactions$data

#adding interaction name to predictor.variable.names
predictor.variable.names <- interactions$predictor.variable.names
```

# Fitting a (non-spatial) Random Forest model with `rf()`

The function
[`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.html) is a
convenient wrapper for `ranger::ranger()` used in every modelling
function of the *spatialRF* package. It takes the training data, the
names of the response and the predictors, and optionally (to assess the
spatial autocorrelation of the residuals), the distance matrix, and a
vector of distance thresholds (in the same units as the distances in
**distance\_matrix**).

These distance thresholds are the neighborhoods at which the model will
check the spatial autocorrelation of the residuals. Their values may
depend on the spatial scale of the data, and the ecological system under
study.

Notice that here I plug the object `predictor.variable.names`, output of
`auto_cor()` and `auto_vif()`, directly into the
`predictor.variable.names` argument of the `rf()` function to fit a
random forest model.

``` r
model.non.spatial <- spatialRF::rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  seed = random.seed,
  verbose = FALSE
)
```

The output is a list with several slots containing the information
required to interpret the model. The information available in these
slots can be plotted (functions named `plot_...()`), printed to screen
(`print_...()`) and captured for further analyses (`get_...()`).

## Residuals

The slot **residuals** (`model.non.spatial$residuals`) stores the values
of the residuals and the results of the normality and spatial
autocorrelation tests. The best function to interrogate it is
\[`plot_residuals_diagnostics()`\].

``` r
spatialRF::plot_residuals_diagnostics(
  model.non.spatial,
  verbose = FALSE
  )
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The upper panels show the results of the normality test (interpretation
in the title), the middle panel shows the relationship between the
residuals and the fitted values, important to understand the behavior of
the residuals, and the lower panel shows the Moran’s I of the residuals
across distance thresholds and their respective p-values (positive for 0
and 1000 km).

## Variable importance

### Global variable importance

The slot **importance** (`model.non.spatial$variable.importance`)
contains the variable importance scores. These can be plotted with
[`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.html),
printed with
[`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.html),
and the dataframe retrieved with
[`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.html)

``` r
spatialRF::plot_importance(
  model.non.spatial,
  verbose = FALSE
  )
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Variable importance represents the increase in mean error (computed on
the out-of-bag data) across trees when a predictor is permuted. Values
lower than zero would indicate that the variable performs worse than a
random one.

If the argument `scaled.importance = TRUE` is used, the variable
importance scores are computed from the scaled data, making the
importance scores easier to compare across different models.

The package
[`randomForestExplainer`](https://github.com/ModelOriented/randomForestExplainer)
offers a couple of interesting options to deepen our understanding on
variable importance scores. The first one is `measure_importance()`,
which analyzes the forest to find out the average minimum tree depth at
which each variable can be found (`mean_min_depth`), the number of nodes
in which a variable was selected to make a split (`no_of_nodes`), the
number of times the variable was selected as the first one to start a
tree (`times_a_root`), and the probability of a variable to be in more
nodes than what it would be expected by chance (`p_value`).

``` r
importance.df <- randomForestExplainer::measure_importance(
  model.non.spatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
  )
```

``` r
kableExtra::kbl(
  importance.df %>% 
    dplyr::arrange(mean_min_depth) %>% 
    dplyr::mutate(p_value = round(p_value, 4)),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:right;">
mean\_min\_depth
</th>
<th style="text-align:right;">
no\_of\_nodes
</th>
<th style="text-align:right;">
times\_a\_root
</th>
<th style="text-align:right;">
p\_value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
climate\_bio1\_average..pca..neighbors\_count
</td>
<td style="text-align:right;">
2.811087
</td>
<td style="text-align:right;">
2098
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
0.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
human\_population
</td>
<td style="text-align:right;">
2.989940
</td>
<td style="text-align:right;">
2222
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
0.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average
</td>
<td style="text-align:right;">
3.117996
</td>
<td style="text-align:right;">
1979
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
0.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_hypervolume
</td>
<td style="text-align:right;">
3.200133
</td>
<td style="text-align:right;">
2072
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
0.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:right;">
3.201070
</td>
<td style="text-align:right;">
1781
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
0.4909
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_area\_km2..x..bias\_species\_per\_record
</td>
<td style="text-align:right;">
3.379787
</td>
<td style="text-align:right;">
1797
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
0.3406
</td>
</tr>
<tr>
<td style="text-align:left;">
human\_population\_density
</td>
<td style="text-align:right;">
3.454233
</td>
<td style="text-align:right;">
1900
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0.0020
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_species\_per\_record
</td>
<td style="text-align:right;">
3.564676
</td>
<td style="text-align:right;">
2321
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_area\_km2..pca..neighbors\_percent\_shared\_edge
</td>
<td style="text-align:right;">
3.959485
</td>
<td style="text-align:right;">
1712
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
0.9519
</td>
</tr>
<tr>
<td style="text-align:left;">
human\_population..x..bias\_species\_per\_record
</td>
<td style="text-align:right;">
3.977952
</td>
<td style="text-align:right;">
1780
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0.5006
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_area\_km2
</td>
<td style="text-align:right;">
4.186849
</td>
<td style="text-align:right;">
1800
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.3144
</td>
</tr>
<tr>
<td style="text-align:left;">
neighbors\_count
</td>
<td style="text-align:right;">
4.204326
</td>
<td style="text-align:right;">
1325
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
1.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
topography\_elevation\_average
</td>
<td style="text-align:right;">
4.306636
</td>
<td style="text-align:right;">
1732
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.8795
</td>
</tr>
<tr>
<td style="text-align:left;">
neighbors\_percent\_shared\_edge
</td>
<td style="text-align:right;">
4.409251
</td>
<td style="text-align:right;">
1700
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.9749
</td>
</tr>
<tr>
<td style="text-align:left;">
neighbors\_area
</td>
<td style="text-align:right;">
4.643590
</td>
<td style="text-align:right;">
1621
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
fragmentation\_cohesion
</td>
<td style="text-align:right;">
4.770930
</td>
<td style="text-align:right;">
1518
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
1.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_velocity\_lgm\_average
</td>
<td style="text-align:right;">
4.845155
</td>
<td style="text-align:right;">
1658
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.9986
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_aridity\_index\_average
</td>
<td style="text-align:right;">
4.858302
</td>
<td style="text-align:right;">
1682
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.9919
</td>
</tr>
<tr>
<td style="text-align:left;">
landcover\_herbs\_percent\_average
</td>
<td style="text-align:right;">
4.984346
</td>
<td style="text-align:right;">
1656
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.9988
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio15\_minimum
</td>
<td style="text-align:right;">
5.057002
</td>
<td style="text-align:right;">
1552
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1.0000
</td>
</tr>
<tr>
<td style="text-align:left;">
fragmentation\_division
</td>
<td style="text-align:right;">
5.229187
</td>
<td style="text-align:right;">
1468
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.0000
</td>
</tr>
</tbody>
</table>

### Local variable importance

Random forest also computes the average increase in error when a
variable is permuted for each case. This is named “local importance”, is
stored in `model.non.spatial$importance$local` as a data frame, and can
be retrieved with
[`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.html).

``` r
local.importance <- spatialRF::get_importance_local(model.non.spatial)
```

The table below shows the first few records and columns. Larger values
indicate larger average errors when estimating a case with the permuted
version of the variable, so more important variables will show larger
values.

``` r
kableExtra::kbl(
  round(local.importance[1:10, 1:5], 0),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
climate\_aridity\_index\_average
</th>
<th style="text-align:right;">
climate\_hypervolume
</th>
<th style="text-align:right;">
climate\_bio1\_average
</th>
<th style="text-align:right;">
climate\_bio15\_minimum
</th>
<th style="text-align:right;">
bias\_area\_km2
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
-120
</td>
<td style="text-align:right;">
705
</td>
<td style="text-align:right;">
214
</td>
<td style="text-align:right;">
231
</td>
<td style="text-align:right;">
-274
</td>
</tr>
<tr>
<td style="text-align:right;">
502
</td>
<td style="text-align:right;">
-400
</td>
<td style="text-align:right;">
-431
</td>
<td style="text-align:right;">
375
</td>
<td style="text-align:right;">
489
</td>
</tr>
<tr>
<td style="text-align:right;">
-182
</td>
<td style="text-align:right;">
-155
</td>
<td style="text-align:right;">
1152
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
-81
</td>
</tr>
<tr>
<td style="text-align:right;">
384
</td>
<td style="text-align:right;">
769
</td>
<td style="text-align:right;">
695
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
-538
</td>
</tr>
<tr>
<td style="text-align:right;">
-399
</td>
<td style="text-align:right;">
-706
</td>
<td style="text-align:right;">
-669
</td>
<td style="text-align:right;">
350
</td>
<td style="text-align:right;">
-627
</td>
</tr>
<tr>
<td style="text-align:right;">
248
</td>
<td style="text-align:right;">
1113
</td>
<td style="text-align:right;">
715
</td>
<td style="text-align:right;">
483
</td>
<td style="text-align:right;">
416
</td>
</tr>
<tr>
<td style="text-align:right;">
195
</td>
<td style="text-align:right;">
705
</td>
<td style="text-align:right;">
513
</td>
<td style="text-align:right;">
286
</td>
<td style="text-align:right;">
332
</td>
</tr>
<tr>
<td style="text-align:right;">
-247
</td>
<td style="text-align:right;">
-629
</td>
<td style="text-align:right;">
506
</td>
<td style="text-align:right;">
-332
</td>
<td style="text-align:right;">
98
</td>
</tr>
<tr>
<td style="text-align:right;">
335
</td>
<td style="text-align:right;">
-519
</td>
<td style="text-align:right;">
1016
</td>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
-246
</td>
</tr>
<tr>
<td style="text-align:right;">
275
</td>
<td style="text-align:right;">
1154
</td>
<td style="text-align:right;">
430
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
251
</td>
</tr>
</tbody>
</table>

When case coordinates are joined with the local importance scores, it is
possible to draw maps showing how variable importance changes over
space.

``` r
#adding coordinates
local.importance <- cbind(
  xy,
  local.importance
  )
```

``` r
#colors
color.low <- viridis::viridis(
    3,
    option = "F"
    )[2]
color.high <- viridis::viridis(
    3,
    option = "F"
    )[1]

#plot of climate_bio1_average
p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world,
    fill = "white"
  ) +
  ggplot2::geom_point(
    data = local.importance,
    ggplot2::aes(
      x = x,
      y = y,
      color = climate_bio1_average
    )
  ) +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80)) +
  ggplot2::scale_color_gradient2(
    low = color.low, 
    high = color.high
    ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::ggtitle("climate_bio1_average") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.key.width = ggplot2::unit(1,"cm")
    ) + 
  ggplot2::labs(color = "Importance") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world,
    fill = "white"
  ) +
  ggplot2::geom_point(
    data = local.importance,
    ggplot2::aes(
      x = x,
      y = y,
      color = human_population
    )
  ) +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80)) +
  ggplot2::scale_color_gradient2(
    low = color.low, 
    high = color.high
    ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::ggtitle("human_population") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.key.width = ggplot2::unit(1,"cm")
    ) + 
  ggplot2::labs(color = "Importance") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p1 + p2
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

In these maps, values lower than 0 indicate that for a given record, the
permuted version of the variable led to an accuracy score even higher
than the one of the non-permuted variable, so again these negative
values can be interpreted as “worse than chance”.

## Response curves and surfaces

The variable importance scores are also used by the function
[`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.html)
to plot partial dependence curves for the predictors (by default, only
the ones with an importance score above the median). Building the
partial dependency curve of a predictor requires setting the other
predictors to their quantiles (0.1, 0.5, and 0.9 by default). This helps
to understand how the response curve of a variable changes when all the
other variables have low, centered, or high values. The function also
allows to see the training data

``` r
spatialRF::plot_response_curves(
  model.non.spatial,
  quantiles = c(0.1, 0.5, 0.9),
  line.color = viridis::viridis(
    3, #same number of colors as quantiles
    option = "F", 
    end = 0.9
    ),
  ncol = 3,
  show.data = TRUE
  )
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

Setting the argument `quantiles` to 0.5 and setting `show.data` to
`FALSE` (default optioin) accentuates the shape of the response curves.

``` r
spatialRF::plot_response_curves(
  model.non.spatial,
  quantiles = 0.5,
  ncol = 3
  )
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

The package [`pdp`](https://bgreenwell.github.io/pdp/index.html)
provides a general way to plot partial dependence plots.

``` r
pdp::partial(
  model.non.spatial, 
  train = plant_richness_df, 
  pred.var = "climate_bio1_average", 
  plot = TRUE, 
  grid.resolution = 200
  )
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

If you need to do your own plots in a different way, the function
[`get_response_curves()`](https://blasbenito.github.io/spatialRF/reference/get_response_curves.html)
returns a data frame with the required data.

``` r
reponse.curves.df <- spatialRF::get_response_curves(model.non.spatial)
```

``` r
kableExtra::kbl(
  head(reponse.curves.df, n = 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
response
</th>
<th style="text-align:right;">
predictor
</th>
<th style="text-align:left;">
quantile
</th>
<th style="text-align:right;">
model
</th>
<th style="text-align:left;">
predictor.name
</th>
<th style="text-align:left;">
response.name
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.428994
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.393562
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.358129
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.322697
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.287265
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.251833
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.216400
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.180968
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.145536
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
<tr>
<td style="text-align:right;">
3081.941
</td>
<td style="text-align:right;">
-4.110104
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:left;">
richness\_species\_vascular
</td>
</tr>
</tbody>
</table>

Interactions between two variables can be plotted with
[`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.html)

``` r
spatialRF::plot_response_surface(
  model.non.spatial,
  a = "climate_bio1_average",
  b = "neighbors_count"
  )
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

This can be done as well with the `pdp` package, that uses a slightly
different algorithm to plot interaction surfaces.

``` r
pdp::partial(
  model.non.spatial, 
  train = plant_richness_df, 
  pred.var = c("climate_bio1_average", "neighbors_count"), 
  plot = TRUE
  )
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

## Model performance

The **performance** slot (in `model.non.spatial$performance`) contains
the values of several performance measures. It be printed via the
function
[`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.html).

``` r
spatialRF::print_performance(model.non.spatial)
```

    ## 
    ## Model performance 
    ##   - R squared (oob):                  0.6075314
    ##   - R squared (cor(obs, pred)^2):     0.9543769
    ##   - Pseudo R squared (cor(obs, pred)):0.9769221
    ##   - RMSE (oob):                       2111.241
    ##   - RMSE:                             902.1424
    ##   - Normalized RMSE:                  0.2604337

-   `R squared (oob)` and `RMSE (oob)` are the R squared of the model
    and its root mean squared error when predicting the out-of-bag data
    (fraction of data not used to train individual trees). From all the
    values available in the `performance` slot, probably these the most
    honest ones, as it is the closer trying to get a performance
    estimate on independent data. However, out-of-bag data is not fully
    independent, and therefore will still be inflated, especially if the
    data is highly aggregated in space.
-   `R squared` and `pseudo R squared` are computed from the
    observations and the predictions, and indicate to what extent model
    outcomes represent the input data. These values will usually be high
    the data is highly aggregated in space.
-   The `RMSE` and its normalized version are computed via
    [`root_mean_squared_error()`](https://blasbenito.github.io/spatialRF/reference/root_mean_squared_error.html),
    and are linear with `R squared` and `pseudo R squared`.

## Spatial cross-validation

The function
[rf\_evaluate()](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.html)
overcomes the limitations of the performance scores explained above by
providing honest performance based on *spatial cross-validation*. The
function separates the data into a number of spatially independent
training and testing folds. Then, it fits a model on each training fold,
predicts over each testing fold, and computes statistics of performance
measures across folds. Let’s see how it works.

``` r
model.non.spatial <- spatialRF::rf_evaluate(
  model = model.non.spatial,
  xy = xy,                  #data coordinates
  repetitions = 30,         #number of spatial folds
  training.fraction = 0.75, #training data fraction on each fold
  metrics = "r.squared",
  seed = random.seed,
  verbose = FALSE
)
```

The function generates a new slot in the model named **evaluation**
(`model.non.spatial$evaluation`) with several objects that summarize the
spatial cross-validation results.

``` r
names(model.non.spatial$evaluation)
```

    ## [1] "metrics"           "training.fraction" "spatial.folds"    
    ## [4] "per.fold"          "per.fold.long"     "per.model"        
    ## [7] "aggregated"

The slot “spatial.folds”, produced by
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.html),
contains the indices of the training and testing cases for each
cross-validation repetition. The maps below show two sets of training
and testing folds.

``` r
pr <- plant_richness_df[, c("x", "y")]
pr$group.2 <- pr$group.1 <- "Training"
pr[model.non.spatial$evaluation$spatial.folds[[1]]$testing, "group.1"] <- "Testing"
pr[model.non.spatial$evaluation$spatial.folds[[25]]$testing, "group.2"] <- "Testing"

p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(data = pr,
          ggplot2::aes(
            x = x,
            y = y,
            color = group.1
            ),
          size = 2
          ) +
  ggplot2::scale_color_viridis_d(
    direction = -1, 
    end = 0.5, 
    alpha = 0.8, 
    option = "F"
    ) +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Group") +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Spatial fold 1") + 
  ggplot2::theme(
    legend.position = "none", 
    plot.title = ggplot2::element_text(hjust = 0.5)
  ) + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(data = pr,
          ggplot2::aes(
            x = x,
            y = y,
            color = group.2
            ),
          size = 2
          ) +
  ggplot2::scale_color_viridis_d(
    direction = -1, 
    end = 0.5, 
    alpha = 0.8, 
    option = "F"
    ) +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Group") +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80)) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5)
  ) + 
  ggplot2::ggtitle("Spatial fold 25") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2
```

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

The information available in this new slot can be accessed with the
functions
[`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.html),
[`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.html),
and
[`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.html).

``` r
spatialRF::plot_evaluation(model.non.spatial)
```

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

`Full` represents the R squared of the model trained on the full
dataset. `Training` are the R-squared of the models fitted on the
spatial folds (named `Training` in the maps above), and `Testing` are
the R-squared of the same models on “unseen” data (data not used to
train the model, named `Testing` in the maps above). The median, median
absolute deviation (MAD), minimum, and maximum R-squared values on the
testing folds can be printed with `print_evaluation()`.

``` r
spatialRF::print_evaluation(model.non.spatial)
```

    ## 
    ## Spatial evaluation 
    ##   - Training fraction:             0.75
    ##   - Spatial folds:                 29
    ## 
    ##     Metric Median   MAD Minimum Maximum
    ##  r.squared  0.517 0.085   0.122   0.781

## Other important things stored in the model

The model predictions are stored in the slot **predictions**, the
arguments used to fit the model in **ranger.arguments**, and the model
itself, used to predict new values (see code chunk below), is in the
**forest** slot.

``` r
predicted <- stats::predict(
  object = model.non.spatial,
  data = plant_richness_df,
  type = "response"
  )$predictions
```

# Fitting a spatial model with `rf_spatial()`

The spatial autocorrelation of the residuals of a model like
`model.non.spatial`, measured with [Moran’s
I](https://en.wikipedia.org/wiki/Moran%27s_I), can be plotted with
[`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.html).

``` r
spatialRF::plot_moran(
  model.non.spatial, 
  verbose = FALSE
  )
```

![](README_files/figure-gfm/unnamed-chunk-37-1.png)<!-- --> According to
the plot, the spatial autocorrelation of the residuals of
`model.non.spatial` is highly positive for a neighborhood of 0 and 1000
km, while it becomes non-significant (p-value &gt; 0.05) at 2000, 4000,
and 8000 km. To reduce the spatial autocorrelation of the residuals as
much as possible, the non-spatial model can be transformed into a
*spatial model* very easily with the function
[`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.html).
This function is the true core of the package!

``` r
model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random.seed
  )
```

The plot below shows the Moran’s I of the residuals of the spatial
model, and indicates that the residuals are not autocorrelated at any
distance.

``` r
spatialRF::plot_moran(
  model.spatial, 
  verbose = FALSE
  )
```

![](README_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

If we compare the variable importance plots of both models, we can see
that the spatial model has an additional set of dots under the name
“spatial\_predictors”, and that the maximum importance of a few of these
*spatial predictors* matches the importance of the most relevant
non-spatial predictors.

``` r
p1 <- spatialRF::plot_importance(
  model.non.spatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

p2 <- spatialRF::plot_importance(
  model.spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

p1 | p2 
```

![](README_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

If we look at the ten most important variables in `model.spatial` we
will see that a few of them are *spatial predictors*. Spatial predictors
are named `spatial_predictor_X_Y`, where `X` is the neighborhood
distance at which the predictor has been generated, and `Y` is the index
of the predictor.

``` r
kableExtra::kbl(
  head(model.spatial$importance$per.variable, n = 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
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
spatial\_predictor\_0\_2
</td>
<td style="text-align:right;">
1069.763
</td>
</tr>
<tr>
<td style="text-align:left;">
human\_population
</td>
<td style="text-align:right;">
1013.699
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average..pca..neighbors\_count
</td>
<td style="text-align:right;">
999.135
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_hypervolume
</td>
<td style="text-align:right;">
990.940
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average
</td>
<td style="text-align:right;">
943.800
</td>
</tr>
<tr>
<td style="text-align:left;">
climate\_bio1\_average..pca..human\_population\_density
</td>
<td style="text-align:right;">
931.109
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_area\_km2
</td>
<td style="text-align:right;">
796.926
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_species\_per\_record
</td>
<td style="text-align:right;">
730.700
</td>
</tr>
<tr>
<td style="text-align:left;">
spatial\_predictor\_0\_1
</td>
<td style="text-align:right;">
700.458
</td>
</tr>
<tr>
<td style="text-align:left;">
bias\_area\_km2..pca..neighbors\_percent\_shared\_edge
</td>
<td style="text-align:right;">
692.798
</td>
</tr>
</tbody>
</table>

But what are spatial predictors? Spatial predictors, as shown below, are
smooth surfaces representing neighborhood among records at different
spatial scales. They are computed from the distance matrix in different
ways. The ones below are the eigenvectors of the double-centered
distance matrix of weights (a.k.a, Moran’s Eigenvector Maps). They
represent the effect of spatial proximity among records, helping to
represent biogeographic and spatial processes not considered by the
non-spatial predictors.

``` r
spatial.predictors <- spatialRF::get_spatial_predictors(model.spatial)
pr <- data.frame(spatial.predictors, plant_richness_df[, c("x", "y")])

p1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      color = spatial_predictor_0_2
    ),
    size = 2.5
  ) +
  ggplot2::scale_color_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Variable: spatial_predictor_0_2") + 
  ggplot2::theme(legend.position = "bottom")+ 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude")

p2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white") +
  ggplot2::geom_point(
    data = pr,
    ggplot2::aes(
      x = x,
      y = y,
      color = spatial_predictor_0_5,
    ),
    size = 2.5
  ) +
  ggplot2::scale_color_viridis_c(option = "F") +
  ggplot2::theme_bw() +
  ggplot2::labs(color = "Eigenvalue") +
  ggplot2::scale_x_continuous(limits = c(-170, -30)) +
  ggplot2::scale_y_continuous(limits = c(-58, 80))  +
  ggplot2::ggtitle("Variable: spatial_predictor_0_5") + 
  ggplot2::theme(legend.position = "bottom") + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("")

p1 | p2
```

![](README_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

The spatial predictors are included in the model one by one, in the
order of their Moran’s I (spatial predictors with Moran’s I lower than 0
are removed). The selection procedure is performed by the function
[`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.html),
which finds the smaller subset of spatial predictors maximizing the
model’s R squared, and minimizing the Moran’s I of the residuals. This
is shown in the optimization plot below (dots linked by lines represent
the selected spatial predictors).

``` r
p <- spatialRF::plot_optimization(model.spatial)
```

![](README_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

# Tuning Random Forest hyperparameters

The model fitted above was based on the default random forest
hyperparameters of `ranger()`, and those might not be the most adequate
ones for a given dataset. The function
[`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.html)
helps the user to choose sensible values for three Random Forest
hyperparameters that are critical to model performance:

-   `num.trees`: number of regression trees in the forest.
-   `mtry`: number of variables to choose from on each tree split.
-   `min.node.size`: minimum number of cases on a terminal node.

These values can be modified in any model fitted with the package using
the `ranger.arguments` argument. The example below shows how to fit a
spatial model with a given set of hyperparameters.

``` r
model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  ranger.arguments = list(
    mtry = 5,
    min.node.size = 20,
    num.trees = 1000
  ),
  verbose = FALSE,
  seed = random.seed
  )
```

The usual method for model tuning relies on a grid search exploring the
results of all the combinations of hyperparameters selected by the user.
In `spatialRF`, model tuning is done via spatial cross-validation, to
ensure that the selected combination of hyperparameters maximizes the
ability of the model to predict over data not used to train it.
**Warning**: model tuning consumes a lot of computational resources,
using it on large datasets might freeze your computer.

``` r
model.spatial.tuned <- rf_tuning(
  model = model.spatial,
  xy = xy,
  repetitions = 30,
  num.trees = c(500, 1000),
  mtry = seq(
    2,
    length(model.spatial$ranger.arguments$predictor.variable.names), #number of predictors
    by = 9),
  min.node.size = c(5, 15),
  seed = random.seed,
  verbose = FALSE
)
```

The results of the function indicate that the model has a higher
transferability (ability to predict on data not used to train the model
assessed via spatial cross-validation with `rf_evaluate()`) when `mtry`
is 29, `min.node.size` is 5, and `num.trees` is 500, but also indicates
that the gain in transferability is rather low (increased R squared of
`model.spatial.tuned$tuning$performance.gain`). The function returns a
tuned model only if the tuning finds a solution better than the original
model. Otherwise the original model is returned (only a new slot named
`tuning`is added).

# Repeating a model execution

Random Forest is an stochastic algorithm that yields slightly different
results on each run unless a random seed is set. This particularity has
implications for the interpretation of variable importance scores and
response curves. The function
[`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.html)
repeats a model execution and yields the distribution of importance
scores of the predictors across executions. **NOTE**: this function
works better when used at the end of a workflow

``` r
model.spatial.tuned.repeat <- rf_repeat(
  model = model.spatial.tuned, 
  repetitions = 30,
  seed = random.seed,
  verbose = FALSE
)
```

The importance scores of a model fitted with `rf_repeat()` are plotted
as a violin plot, with the distribution of the importance scores of each
predictor across repetitions.

``` r
spatialRF::plot_importance(
  model.spatial.tuned.repeat, 
  verbose = FALSE
  )
```

![](README_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

The response curves of models fitted with `rf_repeat()` can be plotted
with `plot_response_curves()` as well. The median prediction is shown
with a thicker line.

``` r
spatialRF::plot_response_curves(
  model.spatial.tuned.repeat, 
  quantiles = 0.5,
  ncol = 3
  )
```

![](README_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

The function `print_performance()` generates a summary of the
performance scores across model repetitions. As every other function of
the package involving repetitions, the provided stats are the median,
and the median absolute deviation (mad).

``` r
spatialRF::print_performance(model.spatial.tuned.repeat)
```

    ## 
    ## Model performance (median +/- mad) 
    ##   - R squared (oob):              0.6 +/- 0.0041
    ##   - R squared (cor(obs, pred)^2): 0.959 +/- 0.0019
    ##   - Pseudo R squared:             0.979 +/- 0.001
    ##   - RMSE (oob):                   2130.602 +/- 10.8783
    ##   - RMSE:                         869.37 +/- 13.4708
    ##   - Normalized RMSE:              0.251 +/- 0.0039

# Taking advantage of the `%>%` pipe

The modeling functions of `spatialRF` are designed to facilitate using
the pipe to combine them. The code below fits a spatial model, tunes its
hyperparameters, evaluates it using spatial cross-validation, and
repeats the execution several times, just by passing the model from one
function to another. Replace `eval = FALSE` with `eval = TRUE` if you
want to execute the code chunk.

``` r
model.full <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance.thresholds,
  xy = xy
) %>%
  rf_tuning() %>%
  rf_evaluate() %>%
  rf_repeat()
```

The code structure shown above can also be used to take advantage of a
custom cluster, either defined in the local host, or a Beowulf cluster.

``` r
my.cluster <- parallel::makeCluster(
  parallel::detectCores() - 1,
  type = "PSOCK"
)

#fitting, tuning, evaluating, and repeating a model
model.full <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance.thresholds,
  xy = xy,
  cluster = my.cluster #is passed via pipe to the other functions
) %>%
  rf_tuning() %>%
  rf_evaluate() %>%
  rf_repeat()

#stopping the cluster
parallel::stopCluster(cl = my.cluster)
```

# Comparing several models

The function
[`rf_compare()`](https://blasbenito.github.io/spatialRF/reference/rf_compare.html)
takes named list with as many models as the user needs to compare, and
applies `rf_evaluate()` to each one of them to compare their predictive
performances across spatial folds.

``` r
comparison <- rf_compare(
  models = list(
    `Non-spatial` = model.non.spatial,
    `Spatial` = model.spatial,
    `Spatial tuned` = model.spatial.tuned
  ),
  xy = xy,
  repetitions = 30,
  training.fraction = 0.8,
  metrics = "r.squared",
  seed = random.seed
  )
```

![](README_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

``` r
x <- comparison$comparison.df %>% 
    dplyr::group_by(model, metric) %>% 
    dplyr::summarise(value = round(median(value), 3)) %>% 
    dplyr::arrange(metric) %>% 
    as.data.frame()
colnames(x) <- c("Model", "Metric", "Median")
kableExtra::kbl(
  x,
  format = "html"
  ) %>%
  kableExtra::kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:left;">
Metric
</th>
<th style="text-align:right;">
Median
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Non-spatial
</td>
<td style="text-align:left;">
r.squared
</td>
<td style="text-align:right;">
0.494
</td>
</tr>
<tr>
<td style="text-align:left;">
Spatial
</td>
<td style="text-align:left;">
r.squared
</td>
<td style="text-align:right;">
0.302
</td>
</tr>
<tr>
<td style="text-align:left;">
Spatial tuned
</td>
<td style="text-align:left;">
r.squared
</td>
<td style="text-align:right;">
0.325
</td>
</tr>
</tbody>
</table>

# Working with a binomial response

This package can also perform binomial regression on response variables
with zeros and ones. Let’s work on a quick example by turning the
response variable of the previous models into a binomial one.

``` r
plant_richness_df$response_binomial <- ifelse(
  plant_richness_df$richness_species_vascular > 5000,
  1,
  0
)
```

The new response variable, `response_binomial`, will have ones where
`richness_species_vascular > 5000`, and zeros otherwise. This would be
equivalent to having the classes “high richness” (represented by the
ones) and “low richness”, represented by the zeros. The binomial
regression model would then have as objective to compute the probability
of each ecoregion to belong to the “high richness” class.

There is something important to notice before moving forward though. The
number of zeros in the new response variable is larger than the number
of ones.

``` r
table(plant_richness_df$response_binomial)
```

    ## 
    ##   0   1 
    ## 165  62

This means that there is **class imbalance**, and under this scenario,
any random forest model is going to get better at predicting the most
abundant class, while in our case the “target” is the less abundant one.
But the function `rf()` is ready to deal with this issue. Let’s fit a
model to see what am I talking about.

``` r
model.non.spatial <- spatialRF::rf(
  data = plant_richness_df,
  dependent.variable.name = "response_binomial",
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  seed = random.seed,
  verbose = FALSE
)
```

The function detects that the response variable is binary (using the
function
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.html)),
and computes *case weights* for the ones and the zeros. These case
weights are stored in the `ranger.arguments` slot of the model, and are
used to give preference to the cases with larger weights during the
selection of the out-of-bag data (check the `case.weights` argument in
`ranger::ranger()`). As a result, each individual tree in the forest is
trained with a similar proportion of zeros and ones, which helps
mitigate the class imbalance issue. This method is named *weighted
Random Forest*, and is very well explained in this [white
paper](https://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf)
that includes the father of Random Forest, Leo Breiman, as coauthor.

``` r
unique(model.non.spatial$ranger.arguments$case.weights)
```

    ## [1] 0.006060606 0.016129032

This model could be projected right away onto a raster stack with maps
of the predictors, so, in fact, `spatialRF` can be used to fit Species
Distribution Models, when it actually wasn’t really designed with such a
purpose in mind. And as an additional advantage, the model can be
evaluated with `rf_evaluate()`, which is way better than
cross-validation via random data-splitting ([this blog
post](https://methodsblog.com/2018/11/29/blockcv-english/) explains
explains why).

``` r
model.non.spatial <- spatialRF::rf_evaluate(
  model.non.spatial,
  xy = xy,
  metrics = "auc", #surprise!
  verbose = FALSE
)

spatialRF::print_evaluation(model.non.spatial)
```

    ## 
    ## Spatial evaluation 
    ##   - Training fraction:             0.75
    ##   - Spatial folds:                 29
    ## 
    ##  Metric Median   MAD Minimum Maximum
    ##     auc  0.925 0.019   0.803   0.977

The **take away message** here is that you can work with a binomial
response with `spatialRF`, just as you would do with a continuous
response, as long as it is represented with zeros and ones. Just
remember that the class imbalance problem is tackled via case weights,
and that predictive performance is also measured using the Area Under
the ROC Curve (AUC).

# Generating spatial predictors for other modelling methods

You might not love Random Forest, but `spatialRF` loves you, and as
such, it gives you tools to generate spatial predictors for other models
anyway.

The first step requires generating Moran’s Eigenvector Maps (MEMs) from
the distance matrix. Here there are two options, computing MEMs for a
single neighborhood distance with
[`mem()`](https://blasbenito.github.io/spatialRF/reference/mem.html),
and computing MEMs for several neighborhood distances at once with
[`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.html).

``` r
#single distance (0km by default)
mems <- spatialRF::mem(distance.matrix = distance_matrix)

#several distances
mems <- spatialRF::mem_multithreshold(
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds
)
```

In either case the result is a data frame with Moran’s Eigenvector Maps
(“just” the positive eigenvectors of the double-centered distance
matrix).

``` r
kableExtra::kbl(
  head(mems[, 1:4], n = 10),
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
spatial\_predictor\_0\_1
</th>
<th style="text-align:right;">
spatial\_predictor\_0\_2
</th>
<th style="text-align:right;">
spatial\_predictor\_0\_3
</th>
<th style="text-align:right;">
spatial\_predictor\_0\_4
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0.0259217
</td>
<td style="text-align:right;">
0.0052203
</td>
<td style="text-align:right;">
0.0416969
</td>
<td style="text-align:right;">
-0.0363324
</td>
</tr>
<tr>
<td style="text-align:right;">
0.0996679
</td>
<td style="text-align:right;">
0.0539713
</td>
<td style="text-align:right;">
0.1324480
</td>
<td style="text-align:right;">
0.3826928
</td>
</tr>
<tr>
<td style="text-align:right;">
0.0010477
</td>
<td style="text-align:right;">
-0.0143046
</td>
<td style="text-align:right;">
-0.0443602
</td>
<td style="text-align:right;">
-0.0031386
</td>
</tr>
<tr>
<td style="text-align:right;">
0.0165695
</td>
<td style="text-align:right;">
0.0047991
</td>
<td style="text-align:right;">
0.0307457
</td>
<td style="text-align:right;">
0.0005170
</td>
</tr>
<tr>
<td style="text-align:right;">
0.0225761
</td>
<td style="text-align:right;">
0.0019595
</td>
<td style="text-align:right;">
0.0230368
</td>
<td style="text-align:right;">
-0.0524239
</td>
</tr>
<tr>
<td style="text-align:right;">
0.0155252
</td>
<td style="text-align:right;">
0.0023742
</td>
<td style="text-align:right;">
0.0197953
</td>
<td style="text-align:right;">
-0.0338956
</td>
</tr>
<tr>
<td style="text-align:right;">
0.0229197
</td>
<td style="text-align:right;">
0.0039860
</td>
<td style="text-align:right;">
0.0312561
</td>
<td style="text-align:right;">
-0.0416697
</td>
</tr>
<tr>
<td style="text-align:right;">
-0.2436009
</td>
<td style="text-align:right;">
-0.1155295
</td>
<td style="text-align:right;">
0.0791452
</td>
<td style="text-align:right;">
0.0189996
</td>
</tr>
<tr>
<td style="text-align:right;">
0.0150725
</td>
<td style="text-align:right;">
-0.0158684
</td>
<td style="text-align:right;">
-0.1010284
</td>
<td style="text-align:right;">
0.0095590
</td>
</tr>
<tr>
<td style="text-align:right;">
-0.1187381
</td>
<td style="text-align:right;">
-0.0471879
</td>
<td style="text-align:right;">
0.0359881
</td>
<td style="text-align:right;">
0.0065211
</td>
</tr>
</tbody>
</table>

But not all MEMs are made equal, and you will need to rank them by their
Moran’s I. The function
[`rank_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/rank_spatial_predictors.html)
will help you do so.

``` r
mem.rank <- spatialRF::rank_spatial_predictors(
  distance.matrix = distance_matrix,
  spatial.predictors.df = mems,
  ranking.method = "moran"
)
```

The output of `rank_spatial_predictors()` is a list with three slots:
“method”, a character string with the name of the ranking method;
“criteria”, an ordered data frame with the criteria used to rank the
spatial predictors; and “ranking”, a character vector with the names of
the spatial predictors in the order of their ranking (it is just the
first column of the “criteria” data frame). We can use this “ranking”
object to reorder or `mems` data frame.

``` r
mems <- mems[, mem.rank$ranking]

#also:
#mems <- mem.rank$spatial.predictors.df
```

From here, spatial predictors can be included in any model one by one,
in the order of the ranking, until the spatial autocorrelation of the
residuals becomes neutral, if possible. A little example with a linear
model follows.

``` r
#model definition
predictors <- c(
  "climate_aridity_index_average ",
  "climate_bio1_average",
  "bias_species_per_record",
  "human_population_density",
  "topography_elevation_average",
  "fragmentation_division"
)
model.formula <- as.formula(
  paste(
    dependent.variable.name,
    " ~ ",
    paste(
      predictors,
      collapse = " + "
    )
  )
)

#scaling the data
model.data <- scale(plant_richness_df) %>% 
  as.data.frame()

#fitting the model
m <- lm(model.formula, data = model.data)

#Moran's I test of the residuals
moran.test <- spatialRF::moran(
  x = residuals(m),
  distance.matrix = distance_matrix,
  verbose = FALSE
)
moran.test$plot
```

![](README_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

According to the Moran’s I test, the model residuals show spatial
autocorrelation. Let’s introduce MEMs one by one until the problem is
solved.

``` r
#add mems to the data and applies scale()
model.data <- data.frame(
  plant_richness_df,
  mems
) %>%
  scale() %>%
  as.data.frame()

#initialize predictors.i
predictors.i <- predictors

#iterating through MEMs
for(mem.i in colnames(mems)){
  
  #add mem name to model definintion
  predictors.i <- c(predictors.i, mem.i)
  
  #generate model formula with the new spatial predictor
  model.formula.i <- as.formula(
    paste(
      dependent.variable.name,
      " ~ ",
      paste(
        predictors.i,
        collapse = " + "
      )
    )
  )
  
  #fit model
  m.i <- lm(model.formula.i, data = model.data)
  
  #Moran's I test
  moran.test.i <- moran(
    x = residuals(m.i),
    distance.matrix = distance_matrix,
    verbose = FALSE
  )
  
  #stop if no autocorrelation
  if(moran.test.i$test$interpretation == "No spatial correlation"){
    break
  }
  
}#end of loop

#last moran test
moran.test.i$plot
```

![](README_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

Now we can compare the model without spatial predictors `m` and the
model with spatial predictors `m.i`.

``` r
comparison.df <- data.frame(
  Model = c("Non-spatial", "Spatial"),
  Predictors = c(length(predictors), length(predictors.i)),
  R_squared = round(c(summary(m)$r.squared, summary(m.i)$r.squared), 2),
  AIC = round(c(AIC(m), AIC(m.i)), 0),
  BIC = round(c(BIC(m), BIC(m.i)), 0),
  `Moran I` = round(c(moran.test$test$moran.i, moran.test.i$test$moran.i), 2)
)

kableExtra::kbl(
  comparison.df,
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)
```

<table class=" lightable-paper lightable-hover" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Model
</th>
<th style="text-align:right;">
Predictors
</th>
<th style="text-align:right;">
R\_squared
</th>
<th style="text-align:right;">
AIC
</th>
<th style="text-align:right;">
BIC
</th>
<th style="text-align:right;">
Moran.I
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Non-spatial
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.38
</td>
<td style="text-align:right;">
551
</td>
<td style="text-align:right;">
578
</td>
<td style="text-align:right;">
0.21
</td>
</tr>
<tr>
<td style="text-align:left;">
Spatial
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
533
</td>
<td style="text-align:right;">
615
</td>
<td style="text-align:right;">
0.06
</td>
</tr>
</tbody>
</table>

According to the model comparison, it can be concluded that the addition
of spatial predictors, in spite of the increase in complexity, has
improved the model. In any case, this is just a simple demonstration of
how spatial predictors generated with functions of the `spatialRF`
package can still help you fit spatial models with other modeling
methods.
