#' @title Moran's I test on a numeric vector
#' @description Computes the spatial correlation coefficient (Moran's I) of a vector given a distance matrix, and a distance threshold used to define neighborhood.
#' @usage
#' moran(
#'   x = NULL,
#'   distance.matrix = NULL,
#'   distance.threshold = 0
#' )
#' @param x Numeric vector, generally model residuals, Default: `NULL`
#' @param distance.matrix Distance matrix among cases in `x`. The number of rows of this matrix must be equal to the length of `x`. Default: `NULL`
#' @param distance.threshold numeric value in the range of values available in `distance.matrix`. Distances below such threshold are set to 0. Default: `0`
#' @return A list with three named slots:
#'  \itemize{
#'    \item `moran.i`: Moran's I of `x`.
#'    \item `p.value`: P-value of the Moran's I estimate.
#'    \item `interpretation`: Interpretation of the Moran's I value according to the p-value. One of "Positive spatial correlation", "Negative spatial correlation", and "No spatial correlation".
#'  }
#' @details Inspired in the `Moran.I()` function of the [ape](https://cran.r-project.org/package=ape) package.
#' @seealso [moran_multithreshold()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'  data(plant_richness)
#'
#'  moran.out <- moran(
#'    x = plant_richness$richness_species_vascular,
#'    distance.matrix = distance_matrix
#'    )
#'  moran.out
#'
#' }
#' }
#' @rdname moran
#' @importFrom stats pnorm sd
#' @export
moran <- function(
  x = NULL,
  distance.matrix = NULL,
  distance.threshold = 0
){

  #extracting weights from distance matrix
  x.distance.weights <- weights_from_distance_matrix(
    x = distance.matrix,
    distance.threshold = distance.threshold
  )

  #length of the input vector
  x.length <- length(x)

  #computing expected Moran I
  moran.i.expected <- -1/(x.length - 1)

  #sum of weights
  x.distance.weights.sum <- sum(x.distance.weights)

  #centering x
  x.mean <- mean(x)
  x.centered <- x - x.mean

  #upper term of the Moran's I equation
  #sum of cross-products of the lags
  #x.centered %o% x.centered equals (xi - x.mean) * (xj - x.mean)
  cross.product.sum <- sum(x.distance.weights * x.centered %o% x.centered)

  #lower term of the Moran's I equation
  #variance of the centered x
  x.centered.variance <- sum(x.centered^2)

  #observed Moran's I
  moran.i.observed <- (x.length / x.distance.weights.sum) * (cross.product.sum/x.centered.variance)

  #components of the expected standard deviation
  s1 <- 0.5 * sum((x.distance.weights + t(x.distance.weights))^2)

  s2 <- sum((apply(x.distance.weights, 1, sum) + apply(x.distance.weights, 2, sum))^2)

  s3 <- x.distance.weights.sum^2

  s4 <- (sum(x.centered^4) / x.length) /
    (x.centered.variance / x.length)^2

  expected.standard.deviation <- sqrt(
    (x.length *
       ((x.length^2 - 3 * x.length + 3) * s1 - x.length * s2 + 3 * s3) -
       s4 *
       (x.length * (x.length - 1) * s1 - 2 * x.length*s2 + 6 * s3)) /
      ((x.length - 1) * (x.length - 2) * (x.length - 3) * s3) - 1 /
      ((x.length - 1)^2)
  )

  #p.value
  p.value <- pnorm(
    moran.i.observed,
    mean = moran.i.expected,
    sd = expected.standard.deviation
  )
  p.value <- ifelse(
    moran.i.observed <= moran.i.expected,
    2 * p.value,
    2 * (1 - p.value)
  )

  #adding interpretation
  if(moran.i.observed > moran.i.expected & p.value <= 0.05){
    interpretation <- "Positive spatial correlation"
  }
  if(moran.i.observed < moran.i.expected & p.value <= 0.05){
    interpretation <- "Negative spatial correlation"
  }
  if(p.value > 0.05){
    interpretation <- "No spatial correlation"
  }

  #preparing output
  out <- data.frame(
    distance.threshold = distance.threshold,
    moran.i.null = moran.i.expected,
    moran.i = moran.i.observed,
    p.value = p.value,
    interpretation = interpretation
  )

  out

}
