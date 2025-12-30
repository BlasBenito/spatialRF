#' @title Optimization plot of a selection of spatial predictors
#' @description Plots optimization data frames produced by [select_spatial_predictors_sequential()]
#' and [select_spatial_predictors_recursive()].
#' @param model A model produced by [rf_spatial()], or an optimization data frame produced by [select_spatial_predictors_sequential()] or [select_spatial_predictors_recursive()].
#' @param point.color Colors of the plotted points. Can be a single color name (e.g. "red4"), a character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `grDevices::hcl.colors(100)`). Default: `grDevices::hcl.colors(100, palette = "Zissou 1", rev = FALSE)`
#' @param verbose Logical, if `TRUE` the plot is printed. Default: `TRUE`
#' @details The function returns `NULL` invisibly (without plotting) when:
#' \itemize{
#'   \item The method used to fit a model with [rf_spatial()] is "hengl" (no optimization required)
#'   \item No spatial predictors were selected during model fitting
#'   \item The model is non-spatial
#' }
#' @return A ggplot, or `NULL` invisibly if no optimization data is available.
#' @rdname plot_optimization
#' @examples
#'
#' data(plants_rf_spatial)
#'
#' plot_optimization(plants_rf_spatial)
#'
#' @family visualization
#' @export
#' @autoglobal
plot_optimization <- function(
  model,
  point.color = grDevices::hcl.colors(
    100,
    palette = "Zissou 1",
    rev = FALSE
  ),
  verbose = TRUE
) {
  #getting optimization df from the model
  if (inherits(model, "rf_spatial")) {
    x <- model$spatial$optimization
  } else {
    x <- model
  }

  #check if optimization data exists
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
    if (verbose) {
      message(
        "No optimization data available. This happens when:\n",
        "  - No spatial predictors were selected\n",
        "  - The method used was 'hengl' (no optimization required)\n",
        "  - The model is non-spatial"
      )
    }
    return(invisible(NULL))
  }

  #plot
  p <- ggplot2::ggplot(data = x) +
    ggplot2::aes(
      y = moran.i,
      x = r.squared,
      color = optimization
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = spatial.predictor.index)
    ) +
    ggplot2::scale_color_gradientn(colors = point.color) +
    ggplot2::geom_point(
      data = x[x$selected, ],
      ggplot2::aes(
        y = moran.i,
        x = r.squared
      ),
      colour = "black",
      size = 5,
      shape = 1,
      alpha = 0.3
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_path(
      data = x[x$selected, ],
      ggplot2::aes(
        y = moran.i,
        x = r.squared
      ),
      linewidth = 0.5,
      color = "black",
      alpha = 0.3
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      col = "gray10",
      linewidth = 0.7,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      size = "Number of \nspatial predictors",
      color = "Weighted \noptimization index"
    ) +
    ggplot2::ylab("Maximum Moran's I of the residuals") +
    ggplot2::xlab("Model's R squared (out-of-bag)") +
    ggplot2::ggtitle(
      "Selection of spatial predictors (selection path shown in gray)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p
}
