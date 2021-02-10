#' @title Optimization equation to select spatial predictors
#' @description Optimizes the selection of spatial predictors using two different methods: "moran.i", and "p.value".
#' @param x Optimization data frame generated internally by [select_spatial_predictors_optimized()] or [select_spatial_predictors_optimized()]. Default: `NULL`
#' @param weight.r.squared Numeric between 0 and 1, weight of R-squared in the optimization process. Default: `NULL`
#' @param weight.penalization.n.predictors Numeric between 0 and 1, weight of the penalization on the number of added spatial predictors. Default: `NULL`
#' @param optimization.method Character, one of "moran.i", and "p.value". Default: `"moran.i"`
#' @return A numeric vector with the optimization criteria.
#' @details The method "moran.i" tries to maximize `1 - Moran's` I while taking into account the R-squared of the model and a penalization on the number of introduced spatial predictors through the expression
#'
#'
#' (1 - Moran's I) + w1 * r.squared - w2 * penalization
#'
#' The method "p.value" uses a binary version of the p-values of Moran's I (1 if >= 0.05, 0 otherwise), and uses the expression
#'
#'
#' max(1 - Moran's I, binary p-value) + w1 * r.squared - w2 * penalization
#'
#' The "moran.i" method generally selects more spatial predictors than the "p.value" method.
#' @seealso [select_spatial_predictors_optimized()], [select_spatial_predictors_optimized()]
#' @rdname optimization_function
#' @export
optimization_function <- function(
  x = NULL,
  weight.r.squared = NULL,
  weight.penalization.n.predictors = NULL,
  optimization.method = "moran.i"
){

  #using Moran's I and its p-value
  if(optimization.method == "p.value"){

    optimization <- rescale_vector(
      pmax(
        rescale_vector(1 - x$moran.i),
        x$p.value.binary
      ) +
        (weight.r.squared * rescale_vector(x$r.squared)) -
        (weight.penalization.n.predictors * rescale_vector(x$penalization.per.variable))
    )

  }

  #Using only Moran's I
  if(optimization.method == "moran.i"){

    optimization <- rescale_vector(rescale_vector(1 - x$moran.i) +
        (weight.r.squared * rescale_vector(x$r.squared)) -
        (weight.penalization.n.predictors * rescale_vector(x$penalization.per.variable)))

  }

  optimization

}
