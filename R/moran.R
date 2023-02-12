#' @title Moran's I test for one or several neighborhood distances
#' @description Computes the spatial correlation coefficient (Moran's I) of a vector given a distance matrix, and a distance threshold (for the function [moran()]) or a set of different distance thresholds (for the function [moran_multithreshold()] used to define a "neighborhood".
#' @param x Numeric vector, generally model residuals, Default: `NULL`
#' @param distance.matrix Distance matrix among cases in `x`. The number of rows of this matrix must be equal to the length of `x`. Default: `NULL`
#' @param distance.threshold Argument of [moran()]. Numeric value in the range of values available in `distance.matrix`. Distances below such threshold are set to 0. Default: `0`
#' @param distance.thresholds Argument of [moran_multithreshold()]. Numeric vector, distances below each value are set to 0 on separated copies of the distance matrix for the computation of Moran's I at different neighborhood distances. If `NULL`, it defaults to `seq(0, max(distance.matrix)/4, length.out = 2)`. Default: `NULL`
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
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_continuous_response
#'   )
#'
#'  #Moran's I of the response variable
#'  out <- moran(
#'    x = ecoregions_df[, ecoregions_continuous_response],
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.threshold = 100
#'    )
#'  out
#'
#'  #computing Moran's I for the response variable at several neighborhood distances
#'  out <- moran_multithreshold(
#'    x = ecoregions_df[, ecoregions_continuous_response],
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = c(100, 1000, 10000)
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
  distance.threshold = 0,
  verbose = TRUE
){

  #stopping if no x
  if(is.null(x)){
    stop("The argument 'x' is missing.")
  }

  #stopping if no distance matrix
  if(is.null(distance.matrix)){
    stop("The argument 'distance.matrix' is missing.")
  }

  if(distance.threshold < 0){
    stop("Argument 'distance.threshold' must be positive.")
  }

  #extracting weights from distance matrix
  x.distance.weights <- distmatrix_to_weights(
    distance.matrix = distance.matrix,
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

  #standard deviation
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

  #preparing moran plot
  #computing weighted mean of the lag
  x.lag <- apply(
    X = x.distance.weights,
    MARGIN = 1,
    FUN = function(y){y %*% x}
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
      size = 0.6
    ) +
    ggplot2::coord_fixed(expand = FALSE) +
    ggplot2::xlab("Variable values") +
    ggplot2::ylab("Average lag values") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(plot.title)


  if(verbose == TRUE){
    suppressWarnings(print(moran.plot))
  }

  #df with Moran's I test
  out.df <- data.frame(
    distance_threshold = distance.threshold,
    moran_i_null = moran.i.expected,
    moran_i = moran.i.observed,
    p_value = p.value,
    interpretation = interpretation
  )

  #out.list
  out.list <- list()
  out.list$test <- out.df
  out.list$plot <- moran.plot
  out.list$plot_df <- plot.df

  class(out.list) <- "moran"

  out.list

}

#' @rdname moran
#' @export
moran_multithreshold <- function(
    x = NULL,
    distance.matrix = NULL,
    distance.thresholds = NULL,
    verbose = TRUE
){

  #check x and distance matrix
  if(is.null(x) | !is.vector(x)){
    stop("Argument 'x' must be a numeric vector.")
  }

  if(is.null(distance.matrix)){
    stop("Argument 'distance.matrix' is missing.`")
  }

  #creating distance thresholds
  if(is.null(distance.thresholds)){
    distance.thresholds <- default_distance_thresholds(distance.matrix = distance.matrix)
  } else {
    #removing distance thresholds larger than max(distance.matrix)
    distance.thresholds <- distance.thresholds[distance.thresholds < max(distance.matrix, na.rm = TRUE)]
  }

  #create output dataframe
  out.df <- data.frame(
    distance_threshold = distance.thresholds,
    moran_i = NA,
    moran_i_null = NA,
    p_value = NA,
    interpretation = NA
  )

  #iterating over out.df
  for(i in seq(1, nrow(out.df))){

    #compute Moran's I
    moran.out <- moran(
      x = x,
      distance.matrix = distance.matrix,
      distance.threshold = out.df[i, "distance_threshold"],
      verbose = FALSE
    )

    out.df[i, "moran_i"] <- moran.out$test$moran_i
    out.df[i, "moran_i_null"] <- moran.out$test$moran_i_null
    out.df[i, "p_value"] <- moran.out$test$p_value
    out.df[i, "interpretation"] <- moran.out$test$interpretation

  }

  #getting scale of max moran
  distance.threshold.max.moran <- out.df[which.max(out.df$moran_i), "distance_threshold"]

  #preparing output list
  out.list <- list()
  out.list$per_distance <- out.df
  out.list$max_moran <- max(out.df$moran_i)
  out.list$max_moran_distance_threshold <- distance.threshold.max.moran
  # out.list$plot <- plot_moran(
  #   model = out.df,
  #   verbose = verbose
  # )

  out.list

}

