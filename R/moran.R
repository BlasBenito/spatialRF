#' @title Moran's I test for spatial autocorrelation
#' @description Computes Moran's I, a measure of spatial autocorrelation that tests whether values are more similar (positive autocorrelation) or dissimilar (negative autocorrelation) among spatial neighbors than expected by chance.
#' @param x Numeric vector to test for spatial autocorrelation. Typically model residuals or a response variable.
#' @param distance.matrix Numeric distance matrix between observations. Must have the same number of rows as the length of `x`.
#' @param distance.threshold Numeric value specifying the maximum distance for spatial neighbors. Distances above this threshold are set to zero during weighting. Default: `NULL` (automatically set to `0`, meaning no thresholding).
#' @param verbose Logical. If `TRUE`, displays a Moran's scatterplot. Default: `TRUE`.
#' @return List of class "moran" with three elements:
#'  \itemize{
#'    \item `test`: Data frame containing:
#'      \itemize{
#'        \item `distance.threshold`: The distance threshold used
#'        \item `moran.i.null`: Expected Moran's I under null hypothesis of no spatial autocorrelation
#'        \item `moran.i`: Observed Moran's I statistic
#'        \item `p.value`: Two-tailed p-value from normal approximation
#'        \item `interpretation`: Text interpretation of the result
#'      }
#'    \item `plot`: ggplot object showing Moran's scatterplot (values vs. spatial lag values with linear fit).
#'    \item `plot.df`: Data frame with columns `x` (original values) and `x.lag` (spatially lagged values) used to generate the plot.
#'  }
#' @details
#' Moran's I is a measure of spatial autocorrelation that quantifies the degree to which nearby observations have similar values. The statistic ranges approximately from -1 to +1:
#' \itemize{
#'   \item **Positive values**: Similar values cluster together (positive spatial autocorrelation)
#'   \item **Values near zero**: Random spatial pattern (no spatial autocorrelation)
#'   \item **Negative values**: Dissimilar values are adjacent (negative spatial autocorrelation, rare in practice)
#' }
#'
#' **Statistical testing:**
#'
#' The function compares the observed Moran's I to the expected value under the null hypothesis of no spatial autocorrelation (E[I] = -1/(n-1)). The p-value is computed using a normal approximation. Results are interpreted at 0.05 significance level.
#'
#' **Moran's scatterplot:**
#'
#' The plot shows original values (x-axis) against spatially lagged values (y-axis). The slope of the fitted line approximates Moran's I. Points in quadrants I and III indicate positive spatial autocorrelation; points in quadrants II and IV indicate negative spatial autocorrelation.
#'
#' This implementation is inspired by the `Moran.I()` function in the [ape](https://cran.r-project.org/package=ape) package.
#' @seealso [moran_multithreshold()], [get_moran()]
#' @examples
#' data(plants_df, plants_distance, plants_response)
#'
#' # Test for spatial autocorrelation in response variable
#' moran_test <- moran(
#'   x = plants_df[[plants_response]],
#'   distance.matrix = plants_distance,
#'   distance.threshold = 1000
#' )
#'
#' # View test results
#' moran_test$test
#'
#' # Access components
#' moran_test$test$moran.i  # Observed Moran's I
#' moran_test$test$p.value  # P-value
#' moran_test$test$interpretation  # Text interpretation
#'
#' @rdname moran
#' @family spatial_analysis
#' @export
#' @autoglobal
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
    (rowSums(x.distance.weights) + colSums(x.distance.weights))^2
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
  p.value <- stats::pnorm(
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
  if (moran.i.observed > moran.i.expected && p.value <= 0.05) {
    interpretation <- "Positive spatial correlation"
  }
  if (moran.i.observed < moran.i.expected && p.value <= 0.05) {
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
    ggplot2::scale_color_gradientn(
      colors = grDevices::hcl.colors(
        n = 100,
        palette = "Zissou 1",
        rev = FALSE,
        alpha = 1
      )
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

  if (verbose) {
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
