#' @title Optimization plot of a selection of spatial predictors
#' @description Plots optimization data frames produced by [select_spatial_predictors_sequential()]
#' and [select_spatial_predictors_optimized()].
#' @usage
#' plot_optimization(
#'   x,
#'   verbose = TRUE
#' )
#' @param x A model produced by [rf_spatial()], or an optimization data frame produced by [select_spatial_predictors_sequential()] or [select_spatial_predictors_optimized()].
#' @param verbose Logical, if `TRUE` the plot is printed. Default: `TRUE`
#' @details If the method used to fit a model with [rf_spatial()] is "hengl", the function returns nothing, as this method does not require optimization.
#' @return A ggplot.
#' @rdname plot_optimization
#' @export
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_c geom_path geom_vline labs xlab ylab ggtitle
plot_optimization <- function(x, verbose = TRUE){

  #declaring variables
  moran.i <- NULL
  optimization <- NULL
  r.squared <- NULL
  spatial.predictor.index <- NULL

  #getting optimization df from the model
  if(inherits(x, "rf_spatial")){
    x <- x$selection.spatial.predictors$optimization
  }

  #plot
  p <- ggplot2::ggplot(data = x) +
    ggplot2::aes(
      y = moran.i,
      x = r.squared,
      color = optimization,
      size = spatial.predictor.index
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_color_viridis_c(direction = 1) +
    ggplot2::geom_point(
      data = x[x$selected, ],
      aes(
        y = moran.i,
        x = r.squared),
      colour="black",
      size = 5,
      shape = 1,
      alpha = 0.3
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_path(data = x[x$selected, ],
                       aes(
                         y = moran.i,
                         x = r.squared
                       ),
                       size = 0.5,
                       color = "black",
                       alpha = 0.3
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      col = "gray10",
      size = 0.7,
      linetype = "dashed") +
    ggplot2::labs(
      size = "Number of spatial predictors",
      color = "Weighted optimization index"
    ) +
    ggplot2::ylab("Moran's I of the model residuals") +
    ggplot2::xlab("Model's R squared") +
    ggplot2::ggtitle("Selection of spatial predictors (selection path shown in gray)") +
    ggplot2::theme_bw()

  if(verbose == TRUE){
    suppressMessages(print(p))
  }

}
