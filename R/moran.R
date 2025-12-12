#' @title Moran's I test
#' @description Computes the spatial correlation coefficient (Moran's I) of a vector given a distance matrix, and a distance threshold used to define "neighborhood".
#' @usage
#' moran(
#'   x = NULL,
#'   distance.matrix = NULL,
#'   distance.threshold = NULL,
#'   verbose = TRUE
#' )
#' @param x Numeric vector, generally model residuals, Default: `NULL`
#' @param distance.matrix Distance matrix among cases in `x`. The number of rows of this matrix must be equal to the length of `x`. Default: `NULL`
#' @param distance.threshold numeric value in the range of values available in `distance.matrix`. Distances below such threshold are set to 0. Default: `NULL` (which defaults to 0).
#' @param verbose Logical, if `TRUE`, prints a Moran's I plot. Default: `TRUE`
#' @return A list with three named slots:
#'  \itemize{
#'    \item `test`: Data frame with observed and expected Moran's I values, p-value, and interpretation.
#'    \item `plot`: Moran's plot of the vector x against the spatial lags of x.
#'    \item `plot.df`: Data used in the Moran's plot.
#'  }
#' @details Inspired in the `Moran.I()` function of the [ape](https://cran.r-project.org/package=ape) package.
#' @seealso [moran_multithreshold()]
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#'  data(distance_matrix)
#'  data(plant_richness)
#'
#'  #Moran's I of the response variable
#'  out <- moran(
#'    x = plant_richness$richness_species_vascular,
#'    distance.matrix = distance_matrix
#'    )
#'  out
#'
#' }
#' @rdname moran
#' @importFrom stats pnorm sd
#' @export
moran <- function(
  x = NULL,
  distance.matrix = NULL,
  distance.threshold = NULL,
  verbose = TRUE
) {
  #stopping if no x
  if (is.null(x)) {
    stop("The argument 'x' is missing.")
  }

  #stopping if no distance matrix
  if (is.null(distance.matrix)) {
    stop("The argument 'distance.matrix' is missing.")
  }

  #default for distance threshold
  if (is.null(distance.threshold)) {
    distance.threshold <- 0
  }

  #extracting weights from distance matrix
  x.distance.weights <- weights_from_distance_matrix(
    distance.matrix = distance.matrix,
    distance.threshold = distance.threshold
  )

  #length of the input vector
  x.length <- length(x)

  #computing expected Moran I
  moran.i.expected <- -1 / (x.length - 1)

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
  moran.i.observed <- (x.length / x.distance.weights.sum) *
    (cross.product.sum / x.centered.variance)

  #components of the expected standard deviation
  s1 <- 0.5 * sum((x.distance.weights + t(x.distance.weights))^2)

  s2 <- sum(
    (apply(x.distance.weights, 1, sum) + apply(x.distance.weights, 2, sum))^2
  )

  s3 <- x.distance.weights.sum^2

  s4 <- (sum(x.centered^4) / x.length) /
    (x.centered.variance / x.length)^2

  #standard deviation
  expected.standard.deviation <- sqrt(
    (x.length *
      ((x.length^2 - 3 * x.length + 3) * s1 - x.length * s2 + 3 * s3) -
      s4 *
        (x.length * (x.length - 1) * s1 - 2 * x.length * s2 + 6 * s3)) /
      ((x.length - 1) * (x.length - 2) * (x.length - 3) * s3) -
      1 /
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
  if (moran.i.observed > moran.i.expected & p.value <= 0.05) {
    interpretation <- "Positive spatial correlation"
  }
  if (moran.i.observed < moran.i.expected & p.value <= 0.05) {
    interpretation <- "Negative spatial correlation"
  }
  if (p.value > 0.05) {
    interpretation <- "No spatial correlation"
  }

  #preparing moran plot
  #computing weighted mean of the lag
  x.lag <- apply(
    X = x.distance.weights,
    MARGIN = 1,
    FUN = function(y) {
      y %*% x
    }
  )

  #preparing title
  plot.title <- paste0(
    "Distance = ",
    distance.threshold,
    "   Moran's I = ",
    round(moran.i.observed, 3),
    "   p-value = ",
    round(p.value, 4)
  )

  #preparing data frame
  plot.df <- data.frame(
    x = x,
    x.lag = x.lag
  )

  #moran plot
  #plot
  moran.plot <- ggplot2::ggplot(data = plot.df) +
    ggplot2::theme_bw() +
    ggplot2::aes(
      x = x,
      y = x.lag,
      color = x + x.lag
    ) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "black",
      linetype = "dotted"
    ) +
    ggplot2::scale_color_viridis_c(
      end = 0.9
    ) +
    ggplot2::geom_smooth(
      method = "lm",
      color = "gray30",
      fill = "gray50",
      se = TRUE,
      alpha = 0.2,
      formula = y ~ x,
      linewidth = 0.6
    ) +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::xlab("Variable values") +
    ggplot2::ylab("Average lag values") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(plot.title)

  if (verbose == TRUE) {
    suppressWarnings(print(moran.plot))
  }

  #df with Moran's I test
  out.df <- data.frame(
    distance.threshold = distance.threshold,
    moran.i.null = moran.i.expected,
    moran.i = moran.i.observed,
    p.value = p.value,
    interpretation = interpretation
  )

  #out.list
  out.list <- list()
  out.list$test <- out.df
  out.list$plot <- moran.plot
  out.list$plot.df <- plot.df

  class(out.list) <- "moran"

  out.list
}
