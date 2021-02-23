
# `spatialRF`: easy spatial regression with Random Forest

<!-- badges: start -->

[![R-CMD-check](https://github.com/BlasBenito/spatialRF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BlasBenito/spatialRF/actions/workflows/R-CMD-check.yaml)

[![Devel-version](https://img.shields.io/badge/devel%20version-1.0.1-blue.svg)](https://github.com/blasbenito/spatialRF)

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

[![CRAN](https://img.shields.io/badge/CRAN-not_published-red)](https://github.com/blasbenito/spatialRF)

[![License](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

# Introduction

The package **spatialRF** facilitates fitting spatial regression models
on regular or irregular data with Random Forest by generating *spatial
predictors* that allow the model to take into account the spatial
structure of the training data. The end goal is minimizing the spatial
autocorrelation of the model residuals as much as possible.

Two main methods to generate *spatial predictors* from the distance
matrix of the data points are implemented in the package:

-   Principal coordinate analysis of neighbor matrices [(Dray, Legendre,
    and
    Peres-Neto 2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925).
-   Distance matrix columns as explanatory variables [(Hengl et
    al. 2018)](https://peerj.com/articles/5518/).

The package is designed to minimize the amount of code required to fit a
spatial model from a training dataset, the names of the response and the
predictors, and a distance matrix, as the example below shows.

``` r
spatial.model <- rf_spatial(
  data = your_dataframe,
  dependent.variable.name = "your_response_variable",
  predictor.variable.names = c("predictor1", "predictor2", ..., "predictorN"),
  distance.matrix = your_distance_matrix
  )
```

The package, that uses the `ranger` package under the hood [(Wright and
Ziegler 2017)](https://arxiv.org/abs/1508.04409), also provides tools to
identify potentially interesting variable interactions, tune random
forest hyperparameters, assess model performance on spatially
independent data folds, and examine the resulting models via importance
plots, response curves, and response surfaces.

# Install

The package is not yet in the CRAN repositories, so at the moment it
must be installed from GitHub as follows.

``` r
remotes::install_github(
  repo = "blasbenito/spatialRF", 
  ref = "main",
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
```

# Working with `spatialRF`

## The example data

The package includes an example dataset named **plant\_richness\_df**, a
data frame with plant species richness and predictors for 227 ecoregions
in the Americas, and a distance matrix among the ecoregion edges named,
well, **distance\_matrix**.

``` r
data(plant_richness_df)
help(plant_richness_df)
data(distance_matrix)

#names of the response variable and the predictors
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
```

The response variable of **plant\_richness\_df** is
“richness\_species\_vascular”, with the total count of vascular plant
species found on each ecoregion. The figure below shows the centroids of
each ecoregion along with their associated value of the response
variable.

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The predictors (columns 5 to 21) represent diverse factors that may
influence plant richness such as sampling bias, the area of the
ecoregion, climatic variables, human presence and impact, topography,
geographical fragmentation, and features of the neighbors of each
ecoregion. The figure below shows the scatterplots of the response
variable (y axis) against each predictor (x axis).

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## The workflow

In this section I describe, step-by-step, a typical workflow built with
`spatialRF`.

### Completing the training dataset: finding relevant variable interactions

Random Forests already takes into account variable interactions of the
form “variable `a` becomes important when `b` is higher than x”.
However, Random Forest can also take advantage of variable interactions
of the form `a * b`, as they are commonly defined in regression models.

The function `rf_interactions()` tests all possible interactions among
predictors by using each one of them in a separate model, and suggesting
the ones with the higher potential contribution to the model’s R squared
and the higher relative importance (presented as a percentage of the
maximum importance of a variable in the model).

``` r
interactions <- rf_interactions(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names
  )
```

    ## Testing 36 candidate interactions.

    ## 3 potential interactions identified.

    ##       ┌─────────────────────────┬───────────────────────┬────────────────┐
    ##       │ Interaction             │ Importance (% of max) │ R2 improvement │
    ##       ├─────────────────────────┼───────────────────────┼────────────────┤
    ##       │ climate_bio1_average_X_ │                  81.6 │         0.014  │
    ##       │ bias_area_km2           │                       │                │
    ##       ├─────────────────────────┼───────────────────────┼────────────────┤
    ##       │ human_population_X_bias │                  79.5 │         0.025  │
    ##       │ _area_km2               │                       │                │
    ##       ├─────────────────────────┼───────────────────────┼────────────────┤
    ##       │ bias_area_km2_X_bias_sp │                  61.7 │         0.0419 │
    ##       │ ecies_per_record        │                       │                │
    ##       └─────────────────────────┴───────────────────────┴────────────────┘

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Here `rf_interactions()` suggests several candidate interactions ordered
by their impact on the model. The function cannot say whether an
interaction *makes sense*, and it is up to the user to choose wisely
whether to select an interaction or not.

For the sake of the example, I will choose
`climate_bio1_average_X_bias_area_km2`, hypothesizing that ecoregions
with higher area (bias\_area\_km2) and energy (represented by the annual
temperature, climate\_bio1\_average) will have more species of vascular
plants (this is just an example, many other rationales are possible when
choosing between candidate interactions). The data required to add the
interaction to the training data is in the output of
`rf_interactions()`.

``` r
#adding interaction column to the training data
plant_richness_df[, "climate_bio1_average_X_bias_area_km2"] <- interactions$columns[, "climate_bio1_average_X_bias_area_km2"]

#adding interaction name to predictor.variable.names
predictor.variable.names <- c(predictor.variable.names, "climate_bio1_average_X_bias_area_km2")
```

### Reducing multicollinearity in the predictors

The functions `auto_cor()` and `auto_vif()` help reduce redundancy in
the predictors by using different criteria (bivariate R squared
vs. [variance inflation
factor](https://www.statisticshowto.com/variance-inflation-factor/)),
while allowing the user to define an *order of preference*, which can be
based either on domain expertise or on a quantitative assessment. The
preference order is defined as a character vector in the
`preference.order` argument of both functions, and does not need to
include the names of all predictors, but just the ones the user would
like to keep in the analysis.

In the example below I give preference to the interaction suggested by
`rf_interactions()` over it’s two components, and prioritize climate
over other types of predictors (any other choice would be valid, it just
depends on the scope of the study). These rules are applied to both
`auto_cor()` and `auto_vif()`, that are executed sequentially by using
the `%>%` pipe from the [magrittr](https://magrittr.tidyverse.org/)
package.

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

predictor.variable.names <- auto_cor(
  x = plant_richness_df[, predictor.variable.names],
  cor.threshold = 0.6,
  preference.order = preference.order
) %>% 
  auto_vif(
    vif.threshold = 2.5,
    preference.order = preference.order
  )
```

    ## [auto_cor()]: Removed variables: bias_area_km2, human_footprint_average

    ## [auto_vif()]: Removed variables: human_population

The output of `auto_cor()` or `auto_vif()` is of the class
“variable\_selection”, that can be used as input for the argument
`predictor.variable.names` of any modeling function within the package.
An example is shown in the next section.

### Fitting a non-spatial Random Forest model

To fit basic Random Forest models `spatialRF` provides the `rf()`
function. It takes the training data, the names of the response and the
predictors, and optionally (to assess the spatial autocorrelation of the
residuals), the distance matrix, and a vector of distance thresholds (in
the same units as the distances in **distance\_matrix**).

These distance thresholds are the neighborhoods at which the model will
check the spatial autocorrelation of the residuals. Their values may
depend on the spatial scale of the data, and the ecological system under
study.

Notice that here I plug the object `predictor.variable.names`, output of
`auto_cor()` and `auto_vif()`, directly into the
`predictor.variable.names` argument.

``` r
model.non.spatial <- rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = c(0, 1500, 3000),
  seed = 100, #just for reproducibility
  verbose = FALSE
)
```

The model output can be printed or plotted with a plethora of functions
such as `print()`, `print_importance()`, `print_performance()`,
`plot_importance()`, `print_moran()`, `plot_response_curves()`,
`plot_response_surfaces)`, or `plot_moran()`, among others.

``` r
plot_response_curves(model.non.spatial)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> In the
response curves above, the other predictors are set to their quantiles
0.1, 0.5, and 0.8, but the user can change this behavior by modifying
the values of the `quantiles` argument.

``` r
plot_response_surfaces(
  x = model.non.spatial,
  a = "climate_bio1_average",
  b = "neighbors_count"
  )
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

In this response surface, the predictors that are not shown are set to
their medians (but other quantiles are possible).

``` r
plot_importance(model.non.spatial, verbose = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

**Predicting onto new data**

Models fitted with `rf()` and other `rf_X()` functions within the
package can be predicted onto new data just as it is done with
`ranger()` models:

``` r
predicted <- stats::predict(
  object = model.non.spatial,
  data = plant_richness_df,
  type = "response"
  )$predictions
```

**Repeating a model execution**

Random Forest is an stochastic algorithm that yields slightly different
results on each run unless a random seed is set. This particularity has
implications for the interpretation of variable importance scores. For
example, in the plot above, the difference in importance between the
predictors `climate_hypervolume` and
`climate_bio1_average_X_bias_area_km2` could be just the result of
chance. The function `rf_repeat()` repeats a model execution and yields
the distribution of importance scores of the predictors across
executions.

``` r
model.non.spatial.repeat <- rf_repeat(
  model = model.non.spatial, 
  repetitions = 10,
  verbose = FALSE
)

plot_importance(model.non.spatial.repeat, verbose = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

After 30 model repetitions it is clear that the difference in importance
between `climate_hypervolume` and `climate_bio1_average_X_bias_area_km2`
is not the result of chance.

### Tuning Random Forest hyperparameters

The model fitted above was based on the default hyperparameter values
provided by `ranger()`, and those might not be the most adequate ones
for a given dataset. The function `rf_tuning()` helps the user to choose
sensible values for three Random Forest hyperparameters that are
critical to model performance:

-   `num.trees`: number of regression trees in the forest.
-   `mtry`: number of variables to choose from on each tree split.
-   `min.node.size`: minimum number of cases on a terminal node.

Model tuning can be done on out-of-bag (`method = "oob"`) or spatial
cross-validation (`method = "spatial.cv"`) R squared values. The example
below shows the out-of-bag approach because I will explain spatial
cross-validation with `rf_evaluate()` later in this document.

``` r
model.non.spatial.tuned <- rf_tuning(
  model = model.non.spatial,
  method = "spatial.cv",
  xy = plant_richness_df[, c("x", "y")],
  repetitions = 30,
  num.trees = c(500, 1000),
  mtry = seq(2, 14, by = 2),
  min.node.size = c(5, 10, 20)
)
```

    ## Exploring 42 combinations of hyperparameters.

    ## Best hyperparameters:

    ##   - num.trees:     1000

    ##   - mtry:          14

    ##   - min.node.size: 5

    ## R squared gain: 0.06432

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

The function `rf_tuning()` returns a model fitted with the same data as
the original model, but using the best hyperparameters found during
tuning. Model tuning has helped to a very small improvement in
performance measures (+ 0.064 R squared), so from here, we can keep
working with `model.non.spatial.tuned`.

### Fitting a spatial model

The spatial autocorrelation of the residuals of `model.non.spatial`,
measured with [Moran’s I](https://en.wikipedia.org/wiki/Moran%27s_I),
can be plotted with `plot_moran()`

``` r
plot_moran(model.non.spatial.tuned, verbose = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

According to the plot, the spatial autocorrelation of the residuals is
highly positive for a neighborhood of 0 km, while it becomes
non-significant (p-value &gt; 0.05, whatever that means) at 1500 and
3000 km. To reduce the spatial autocorrelation of the residuals as much
as possible, the non-spatial tuned model fitted above can be converted
into a spatial model easily with `rf_spatial()`.

``` r
model.spatial <- rf_spatial(
  model = model.non.spatial.tuned,
  method = "mem.moran.sequential", #default method
  verbose = FALSE
  )
```

The plot below compares the Moran’s I of the residuals of the spatial
(green) and non spatial (purple) models. It shows that `rf_spatial()`
has managed to remove the spatial autocorrelation of the model residuals
for every neighborhood distance.

``` r
plot_moran(model.spatial, verbose = FALSE)
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

If we compare the variable importance plots of both models, we can see
that the spatial model has an additional set of dots under the name
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

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

If we take a look to the ten most important variables in
**model.spatial** we will see that a few of them are spatial predictors.

| variable                                   | importance |
|:-------------------------------------------|-----------:|
| climate\_bio1\_average\_X\_bias\_area\_km2 |      0.405 |
| climate\_bio1\_average                     |      0.345 |
| spatial\_predictor\_0\_2                   |      0.334 |
| climate\_hypervolume                       |      0.328 |
| bias\_species\_per\_record                 |      0.151 |
| spatial\_predictor\_0\_6                   |      0.126 |
| spatial\_predictor\_0\_1                   |      0.125 |
| spatial\_predictor\_0\_5                   |      0.103 |
| human\_population\_density                 |      0.095 |
| neighbors\_count                           |      0.091 |

Spatial predictors are named `spatial_predictor_X_Y`, where `X` is the
neighborhood distance at which the predictor has been generated, and `Y`
is the index of the predictor.

Spatial predictors, as shown below, are smooth surfaces representing
neighborhood among records at different spatial scales.

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

The spatial predictors in the spatial model have been generated using
the method “mem.moran.sequential” (function’s default), that mimics the
Moran’s Eigenvector Maps method described in [(Dray, Legendre, and
Peres-Neto
2006)](https://www.sciencedirect.com/science/article/abs/pii/S0304380006000925).

In brief, the method consist on transforming the distance matrix into a
double-centered matrix of normalized weights, to then compute the
positive eigenvectors of the weights matrix (a.k.a, Moran’s Eigenvector
Maps, or MEMs).

The MEMs are included in the model one by one in the order of their
Moran’s I, and the subset of MEMs maximizing the model’s R squared and
minimizing the Moran’s I of the residuals and the number of MEMs added
to the model are selected, as optimization plot below shows (dots linked
by lines represent the selected spatial predictors).

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Assessing model performance on spatially independent folds

The function `rf_evaluate()` separates the training data into a number
of spatially independent training and testing folds, fits a model on
each training fold, predicts over each testing fold, and computes
performance measures, to finally aggregate them across model
repetitions. Let’s see how it works.

``` r
model.spatial <- rf_evaluate(
  model = model.spatial,
  xy = plant_richness_df[, c("x", "y")], #data coordinates
  repetitions = 30,                      #number of folds
  training.fraction = 0.8,               #training data fraction
  metrics = c("r.squared", "rmse"),
  verbose = FALSE
)
```

The function generates a new slot in the model named “evaluation” with
several objects that summarize the spatial cross-validation results.

``` r
names(model.spatial$evaluation)
```

    ## [1] "training.fraction" "spatial.folds"     "per.fold"         
    ## [4] "per.fold.long"     "per.model"         "aggregated"

The slot “spatial.folds”, produced by `make_spatial_folds()`, contains
the indices of the training and testing cases for each cross-validation
repetition. The maps below show two sets of training and testing spatial
folds.

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

The functions `plot_evaluation()` and `print_evaluation()` allow to see
the evaluation results as a plot or as a table. The boxplot below shows
the original performance scores of the “Full” model
(`model.spatial.tuned`), and the distribution of performance scores of
the model fitted on the training data (“Training”), its prediction over
the “Testing” data. From these performance scores, only the ones labeled
as “Testing” represent model performance on unseen data.

``` r
plot_evaluation(model.spatial, notch = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Comparing two models

The function `rf_evaluate()` only assesses the predictive performance on
unseen data of one model at a time. If the goal is to compare two
models, `rf_evaluate()` can be indeed ran twice, but `spatialRF` offers
a more convenient option named `rf_compare()`.

``` r
comparison <- rf_compare(
  models = list(
    `Non-spatial` = model.non.spatial.tuned,
    `Spatial` = model.spatial
  ),
  xy = plant_richness_df[, c("x", "y")],
  metrics = c("r.squared", "rmse"),
  notch = TRUE
  )
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

| Model       | Metric    |     Mean |
|:------------|:----------|---------:|
| Non-spatial | r.squared |    0.487 |
| Spatial     | r.squared |    0.415 |
| Non-spatial | rmse      | 2450.496 |
| Spatial     | rmse      | 2694.532 |

The comparison shows that the non-spatial model performed slightly
better than the spatial on, but with overlapping notches, indicating
that the medians of the R squared and RMSE distributions are not
statistically different. That is a small trade-off considering that the
spatial model incorporates information about the spatial structure of
the data, and its residuals show no spatial autocorrelation.
