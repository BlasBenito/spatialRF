#' @title plot_optimization
#' @description plots optimization data frames produced by [select_spatial_predictors_sequential] and [select_spatial_predictors_optimized].
#' @param x an optimization data frame produced by [select_spatial_predictors_sequential] or [select_spatial_predictors_optimized].
#' @param verbose logical, if TRUE the plot is printed Default: TRUE
#' @return an optimization plot.
#' @rdname plot_optimization
#' @export
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_c geom_path geom_vline labs xlab ylab ggtitle
plot_optimization <- function(x, verbose = TRUE){

  #declaring variables
  moran.i <- NULL

  if(!("optimization" %in% names(x))){
    stop("There is no 'optimization' data frame in x.")
  }

  #getting plot df
  plot.df <- x$optimization

  #declare some variables to avoid check complaints
  optimization <- NULL
  r.squared <- NULL
  spatial.predictor.index <- NULL

  #plot
  p <- ggplot2::ggplot(data = plot.df) +
    ggplot2::aes(
      x = moran.i,
      y = r.squared,
      color = optimization,
      size = spatial.predictor.index
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_color_viridis_c(direction = -1) +
    ggplot2::geom_point(
      data = plot.df[plot.df$selected, ],
      aes(
        x = moran.i,
        y = r.squared),
      colour="black",
      size = 5,
      shape = 1,
      alpha = 0.3
    ) +
    ggplot2::geom_path(data = plot.df[plot.df$selected, ],
                       aes(
                         x = moran.i,
                         y = r.squared
                       ),
                       size = 0.5,
                       color = "black",
                       alpha = 0.3
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      col = "gray10",
      size = 0.7,
      linetype = "dashed") +
    ggplot2::labs(
      size = "Added spatial predictors",
      color = "Weighted optimization index"
    ) +
    ggplot2::xlab("Moran's I of the model residuals") +
    ggplot2::ylab("Model's R-squared") +
    ggplot2::ggtitle("Selection of spatial predictors (selection path shown in gray)")

  if(verbose == TRUE){
    suppressMessages(print(p))
  }

  return(p)

}
