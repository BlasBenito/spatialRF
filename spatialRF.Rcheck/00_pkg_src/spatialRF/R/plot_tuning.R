#' @title Plots a tuning object produced by [rf_tuning()]
#' @description Plots the tuning of the hyperparameters `num.trees`, `mtry`, and `min.node.size` performed by [rf_tuning()].
#' @usage
#' plot_tuning(
#'   model,
#'   point.color = viridis::viridis(
#'     100,
#'     option = "F"
#'   ),
#'   verbose = TRUE
#' )
#' @param model A model fitted with [rf_tuning()]. Default: `NULL`
#' @param point.color Colors of the plotted points. Can be a single color name (e.g. "red4"), a character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F")`
#' @param verbose Logical, if `TRUE`, the plot is printed. Default: `TRUE`
#' @return A ggplot.
#' @seealso [rf_tuning()]
#' @examples
#'
#' if(interactive()){
#'   data(
#'     plants_rf,
#'     plants_xy
#'   )
#'
#'   plants_rf_tuned <- rf_tuning(
#'     model = plants_rf,
#'     num.trees = c(25, 50),
#'     mtry = c(5, 10),
#'     min.node.size = c(10, 20),
#'     xy = plants_xy,
#'     repetitions = 5,
#'     n.cores = 1
#'   )
#'
#'   plot_tuning(plants_rf_tuned)
#' }
#'
#' @rdname plot_tuning
#' @family visualization
#' @export
plot_tuning <- function(
  model,
  point.color = viridis::viridis(
    100,
    option = "F"
  ),
  verbose = TRUE
) {
  #declaring variables
  metric <- NULL
  value <- NULL

  if (!("tuning" %in% names(model))) {
    stop("Object 'x' does not have a 'tuning' slot.")
  }

  #extracting tuning data frame
  tuning <- model$tuning$tuning.df

  #getting metric name
  metric.name <- model$tuning$metric

  #changing name of metric column
  colnames(tuning)[colnames(tuning) == metric.name] <- "metric"

  #to long format
  tuning.long <- tidyr::pivot_longer(
    tuning,
    cols = 1:3,
    names_to = "parameter",
    values_to = "value"
  ) %>%
    as.data.frame()

  #choose smoothing method based on grid size
  #loess needs sufficient data points; use lm for small grids
  n_points <- nrow(tuning)
  smooth_method <- if (n_points >= 15) "loess" else "lm"

  #tuning plot
  p <- ggplot2::ggplot(
    data = tuning.long,
    ggplot2::aes(
      y = metric,
      x = value
    )
  ) +
    ggplot2::geom_smooth(
      se = TRUE,
      method = smooth_method,
      color = "gray20",
      alpha = 0.5,
      formula = y ~ x
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = metric),
      shape = 21,
      alpha = 0.75,
      size = 3
    ) +
    ggplot2::facet_wrap(
      "parameter",
      ncol = 1,
      scales = "free"
    ) +
    ggplot2::scale_fill_gradientn(
      colors = point.color,
      name = metric.name
    ) +
    ggplot2::theme_bw() +
    ggplot2::xlab("") +
    ggplot2::ylab(metric.name) +
    ggplot2::ggtitle("Model tuning via spatial cross-validation") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p
}
