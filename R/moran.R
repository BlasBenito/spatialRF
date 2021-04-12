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

  #check x and distance matrix
  if(is.null(x) | !is.vector(x)){
    stop("x must be a numeric vector.")
  }
  if(nrow(distance.matrix) != length(x)){
    stop("length(x) and nrow(distance.matrix) must be equal.")
  }

  #extracting weights from distance matrix
  x.distance.weights <- weights_from_distance_matrix(
    x = distance.matrix,
    distance.threshold = distance.threshold
  )

  #computing expected Moran I
  x.length <- length(x)
  moran.expected <- round(-1/(x.length - 1), 4)

  #computing observed Moran I
  sum.x.distance.weights <- sum(x.distance.weights)

  #centering x
  x.mean <- mean(x)
  x.centered <- x - x.mean #centering x


  cv <- sum(x.distance.weights * x.centered %o% x.centered)
  v <- sum(x.centered^2)
  observed.moran <- (x.length/sum.x.distance.weights) * (cv/v)
  i.max <- (x.length/sum.x.distance.weights) * (sd(rowSums(x.distance.weights) * x.centered)/sqrt(v/(x.length - 1)))
  observed.moran <- round(observed.moran/i.max, 4)

  #computing p-value
  s1 <- 0.5 * sum((x.distance.weights + t(x.distance.weights))^2)
  s2 <- sum((apply(x.distance.weights, 1, sum) + apply(x.distance.weights, 2, sum))^2)
  s.sq <- sum.x.distance.weights^2
  k <- (sum(x.centered^4)/x.length) / (v/x.length)^2
  expected.standard.deviation <- sqrt(
    (x.length*((x.length^2 - 3*x.length + 3)*s1 - x.length*s2 + 3*s.sq) -
       k*(x.length*(x.length - 1)*s1 - 2*x.length*s2 + 6*s.sq)) /
      ((x.length - 1)*(x.length - 2)*(x.length - 3)*s.sq) - 1/((x.length - 1)^2)
  )
  p.value <- pnorm(observed.moran, mean = moran.expected, sd = expected.standard.deviation)
  p.value <- if (observed.moran <= moran.expected) 2*p.value else 2*(1 - p.value)
  p.value <- round(p.value, 4)

  #adding interpretation
  if(observed.moran > 0 & p.value <= 0.05){
    interpretation <- "Positive spatial correlation"
  }
  if(observed.moran < 0 & p.value <= 0.05){
    interpretation <- "Negative spatial correlation"
  }
  if(p.value > 0.05){
    interpretation <- "No spatial correlation"
  }

  #preparing output
  out <- data.frame(
    moran.i = observed.moran,
    p.value = p.value,
    interpretation = interpretation
  )

  out

}
