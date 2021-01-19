#' @title moran
#' @description Computes the spatial correlation coefficient (Moran's I) of a vector given a distance matrix.
#' @param x numeric vector, generally a model residuals, Default: NULL
#' @param distance.matrix distance matrix among the records represented in the numeric vector. The number of rows of this matrix must be equal to the length of x. Default: NULL
#' @param distance.threshold (optional) numeric, distances below this value in the distance matrix are set to 0., Default: NULL
#' @return a list with three named slots:
#'  \describe{
#'  \item{moran.i}{Moran's I (spatial correlation) of x.}
#'  \item{p.value}{p-value of the Moran's I estimate.}
#'  \item{interpretation}{interpretation of the Moran's I value according to the p-value. One of "Positive spatial correlation", "Negative spatial correlation", and "No spatial correlation".}
#'  }
#'
#' @details This function is based on \link[ape]{Moran.I}.
#'
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(distance_matrix)
#'  data(plant_richness)
#'  moran.out <- moran(
#'    x = plant_richness$richness_species_vascular,
#'    distance.matrix = distance_matrix
#'    )
#'  moran.out
#'  }
#' }
#' @rdname moran
#' @importFrom stats pnorm sd
#' @export
moran <- function(
  x = NULL,
  distance.matrix = NULL,
  distance.threshold = NULL
){

  #check x and distance matrix
  if(is.null(x) | !is.vector(x)){
    stop("x must be a numeric vector.")
  }
  if(nrow(distance.matrix) != length(x)){
    stop("length(x) and nrow(distance.matrix) must be equal.")
  }
  if(!is.null(distance.threshold) & !is.numeric(distance.threshold)){
    stop("distance.threshold must be numeric.")
  }

  #thresholding distance matrix
  if(!is.null(distance.threshold)){
    distance.matrix[distance.matrix < distance.threshold] <- 0
  }

  #computing weights
  weight <- 1/distance.matrix
  weight[is.infinite(weight)] <- 1
  diag(weight) <- 0

  #normalizing weights
  weight.rowsums <- rowSums(weight)
  weight.rowsums[weight.rowsums == 0] <- 1
  weight <- weight/weight.rowsums

  #computing expected Moran I
  n <- length(x)
  expected.moran <- round(-1/(n - 1), 4)

  #computing observed Moran I
  s <- sum(weight)
  m <- mean(x)
  y <- x - m #centering x
  cv <- sum(weight * y %o% y)
  v <- sum(y^2)
  observed.moran <- (n/s) * (cv/v)
  i.max <- (n/s) * (sd(rowSums(weight) * y)/sqrt(v/(n - 1)))
  observed.moran <- round(observed.moran/i.max, 4)

  #computing p-value
  s1 <- 0.5 * sum((weight + t(weight))^2)
  s2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)
  s.sq <- s^2
  k <- (sum(y^4)/n) / (v/n)^2
  expected.standard.deviation <- sqrt(
    (n*((n^2 - 3*n + 3)*s1 - n*s2 + 3*s.sq) -
       k*(n*(n - 1)*s1 - 2*n*s2 + 6*s.sq)) /
      ((n - 1)*(n - 2)*(n - 3)*s.sq) - 1/((n - 1)^2)
  )
  p.value <- pnorm(observed.moran, mean = expected.moran, sd = expected.standard.deviation)
  p.value <- if (observed.moran <= expected.moran) 2*p.value else 2*(1 - p.value)
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
