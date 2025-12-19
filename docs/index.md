# `spatialRF` Easy Spatial Modeling with Random Forest

# Introduction

The package **spatialRF** fits **explanatory spatial regression models**
by combining Random Forest with *spatial predictors* that help the model
minimizing the spatial autocorrelation of the residuals and offering
honest variable importance scores.

The package is designed to minimize the code required to fit a spatial
model from a training dataset, the names of the response and the
predictors, and a distance matrix, as shown in the mock-up call below.

``` r
spatial.model <- spatialRF::rf_spatial(
  data = df,
  dependent.variable.name = "response",
  predictor.variable.names = c("pred1", "pred2", ..., "predN"),
  distance.matrix = distance_matrix
  )
```

**spatialRF** uses the fast and efficient `ranger` package under the
hood [(Wright and Ziegler 2017)](https://arxiv.org/abs/1508.04409), so
please, cite the `ranger` package when using `spatialRF`!

This package also provides tools to identify potentially interesting
variable interactions, tune random forest hyperparameters, assess model
performance on spatially independent data folds, and examine the
resulting models via importance plots, response curves, and response
surfaces.

However, there are several things this package cannot do:

- Predict model results over raster data.

- Predict a model over a different place with a different spatial
  structure.

- Work with “big data”, whatever that means.

- Imputation or extrapolation (it can be done, but models based on
  spatial predictors are hardly transferable).

- Take temporal autocorrelation into account.

# Data requirements

The data required to fit spatial models with `spatialRF` must fulfill
several conditions:

- **The input format is data.frame**. At the moment, tibbles are not
  fully supported.
- **The number of rows must be somewhere between 100 and ~10000** (but
  depends on your RAM!).
- **Factors in the response or the predictors are not explicitly
  supported in the package**. They may work, or they won’t, but in any
  case, I designed this package for **quantitative responses alone**.
  However, binary responses with values 0 and 1 are partially supported.
- **Must be free of `NA`**.
- **Columns cannot have zero variance**. This condition can be checked
  with `apply(df, 2, var) == 0`. Columns yielding TRUE should be
  removed.
- **Columns must not yield `NaN` or `Inf` when scaled**. You can check
  each condition with `sum(apply(scale(df), 2, is.nan))` and
  `sum(apply(scale(df), 2, is.infinite))`. If higher than 0, you can
  find what columns are giving issues with
  `sapply(as.data.frame(scale(df)), function(x)any(is.nan(x)))` and
  `sapply(as.data.frame(scale(df)), function(x)any(is.infinite(x)))`.
  Any column yielding `TRUE` will generate issues while trying to fit
  models with `spatialRF`.

# Citation

If you find `spatialRF` useful, please cite the `ranger` package as
well.

*Marvin N. Wright, Andreas Ziegler (2017). ranger: A Fast Implementation
of Random Forests for High Dimensional Data in C++ and R. Journal of
Statistical Software, 77(1), 1-17. <doi:10.18637/jss.v077.i01>*

*Blas M. Benito (2025). spatialRF: Easy Spatial Regression with Random
Forest. R package version 1.1.5. doi: 10.5281/zenodo.4745208. url:
<https://blasbenito.github.io/spatialRF/>*

# Install

The version 1.1.5 can be installed from CRAN:

``` r
install.packages("spatialRF")
```

The package can also be installed from GitHub as follows. There are
several branches in the repository:

- `main`: latest stable version (1.1.0 currently).
- `development`: development version, usually very broken.
- `v.1.0.9` to `v.1.1.4`: archived versions.

``` r
remotes::install_github(
  repo = "blasbenito/spatialRF",
  ref = "main",
  force = TRUE,
  quiet = TRUE
  )
```

# Getting Started

This README provides a quick introduction to `spatialRF`. For detailed
tutorials, see:

## Tutorials

- **[Non-Spatial Random Forest
  Models](https://blasbenito.github.io/spatialRF/articles/non_spatial_models.html)**:
  Learn how to fit and interpret standard random forest models using
  [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md).
  Covers data exploration, variable interactions, model evaluation,
  importance scores, response curves, spatial cross-validation, and
  prediction. *Start here if you’re new to spatialRF.*

- **[Spatial Random Forest
  Models](https://blasbenito.github.io/spatialRF/articles/spatial_models.html)**:
  Discover how to address spatial autocorrelation using
  [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md).
  Demonstrates spatial predictor generation with Moran’s Eigenvector
  Maps, optimization, hyperparameter tuning, and model comparison.

## Quick Example

``` r
library(spatialRF)

# Load example data
data(plants_df, plants_response, plants_predictors, plants_distance)

# Fit a spatial random forest model
model.spatial <- rf_spatial(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(100, 1000, 2000, 4000)
)

# Check residual autocorrelation
plot_moran(model.spatial)

# View variable importance
plot_importance(model.spatial)
```

# Learn More

Visit the [package website](https://blasbenito.github.io/spatialRF/) for
complete function reference and additional articles.
