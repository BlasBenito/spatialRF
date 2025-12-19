#' @title Moran's I test across multiple distance thresholds
#' @description Computes Moran's I at multiple distance thresholds to assess spatial autocorrelation across different neighborhood scales. Identifies the distance threshold with the strongest spatial autocorrelation.
#' @param x Numeric vector to test for spatial autocorrelation. Typically model residuals or a response variable.
#' @param distance.matrix Numeric distance matrix between observations. Must have the same number of rows as the length of `x`.
#' @param distance.thresholds Numeric vector of distance thresholds defining different neighborhood scales. Each threshold specifies the maximum distance for spatial neighbors at that scale. Default: `NULL` (automatically computed with [default_distance_thresholds()]).
#' @param verbose Logical. If `TRUE`, displays a plot of Moran's I values across distance thresholds. Default: `TRUE`.
#' @return List with four elements:
#'  \itemize{
#'  \item `per.distance`: Data frame with one row per distance threshold, containing columns:
#'    \itemize{
#'      \item `distance.threshold`: Distance threshold used
#'      \item `moran.i`: Observed Moran's I statistic
#'      \item `moran.i.null`: Expected Moran's I under null hypothesis
#'      \item `p.value`: Two-tailed p-value
#'      \item `interpretation`: Text interpretation of the result
#'    }
#'  \item `plot`: ggplot object showing how Moran's I varies across distance thresholds, highlighting significant results.
#'  \item `max.moran`: Numeric value of the maximum Moran's I observed across all thresholds.
#'  \item `max.moran.distance.threshold`: Distance threshold (in distance matrix units) where Moran's I is maximized.
#'  }
#' @details
#' This function applies [moran()] at multiple distance thresholds to explore spatial autocorrelation at different spatial scales. This multi-scale approach is valuable for several reasons:
#' \itemize{
#'   \item **Scale exploration**: Different processes may operate at different spatial scales. Testing multiple thresholds reveals the scale(s) at which spatial autocorrelation is strongest.
#'   \item **Optimal neighborhood definition**: Identifies the distance threshold that best captures the spatial structure in the data.
#'   \item **Uncertainty assessment**: Spatial neighborhoods are often uncertain in ecological and spatial data. Testing multiple thresholds accounts for this uncertainty.
#' }
#'
#' **Interpreting results:**
#'
#' The plot shows Moran's I values across distance thresholds. Peaks in Moran's I indicate spatial scales where autocorrelation is strongest. The `max.moran` and `max.moran.distance.threshold` values identify the optimal scale. Significant results (p equal or lower than 0.05) indicate spatial autocorrelation at that particular scale.
#'
#' This function is commonly used to:
#' \enumerate{
#'   \item Detect spatial autocorrelation in model residuals at multiple scales
#'   \item Determine appropriate distance thresholds for generating spatial predictors with [mem_multithreshold()]
#'   \item Assess whether spatial patterns vary across scales
#' }
#' @seealso [moran()], [mem_multithreshold()], [default_distance_thresholds()], [get_moran()]
#' @examples
#' data(plants_df, plants_distance, plants_response)
#'
#' # Test spatial autocorrelation at multiple distance thresholds
#' moran_multi <- moran_multithreshold(
#'   x = plants_df[[plants_response]],
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000, 5000)
#' )
#'
#' # View results for all thresholds
#' moran_multi$per.distance
#'
#' # Find optimal distance threshold
#' moran_multi$max.moran.distance.threshold
#' moran_multi$max.moran
#'
#' # Plot shows spatial autocorrelation across scales
#' moran_multi$plot
#'
#' @rdname moran_multithreshold
#' @family spatial_analysis
#' @export
moran_multithreshold <- function(
  x = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  verbose = TRUE
) {
  #check x and distance matrix
  if (is.null(x) | !is.vector(x)) {
    stop("Argument 'x' must be a numeric vector.")
  }
  if (is.null(distance.matrix)) {
    stop("Argument 'distance.matrix' is missing.`")
  }
  #creating distance thresholds
  if (is.null(distance.thresholds)) {
    distance.thresholds <- default_distance_thresholds(
      distance.matrix = distance.matrix
    )
  }

  #create output dataframe
  out.df <- data.frame(
    distance.threshold = distance.thresholds,
    moran.i = NA,
    moran.i.null = NA,
    p.value = NA,
    interpretation = NA
  )

  #iterating over out.df
  for (i in seq(1, nrow(out.df))) {
    #compute Moran's I
    moran.out <- moran(
      x = x,
      distance.matrix = distance.matrix,
      distance.threshold = out.df[i, "distance.threshold"],
      verbose = FALSE
    )

    out.df[i, "moran.i"] <- moran.out$test$moran.i
    out.df[i, "moran.i.null"] <- moran.out$test$moran.i.null
    out.df[i, "p.value"] <- moran.out$test$p.value
    out.df[i, "interpretation"] <- moran.out$test$interpretation
  }

  #getting scale of max moran
  distance.threshold.max.moran <- out.df[
    which.max(out.df$moran.i),
    "distance.threshold"
  ]

  #preparing output list
  out.list <- list()
  out.list$per.distance <- out.df
  out.list$max.moran <- max(out.df$moran.i)
  out.list$max.moran.distance.threshold <- distance.threshold.max.moran
  out.list$plot <- plot_moran(
    model = out.df,
    verbose = verbose
  )

  out.list
}
