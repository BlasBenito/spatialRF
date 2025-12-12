#' @title Optimization plot of a selection of spatial predictors
#' @description Plots optimization data frames produced by [select_spatial_predictors_sequential()]
#' and [select_spatial_predictors_recursive()].
#' @usage
#' plot_optimization(
#'   model,
#'   point.color = viridis::viridis(
#'     100,
#'     option = "F",
#'     direction = -1
#'   ),
#'   verbose = TRUE
#' )
#' @param model A model produced by [rf_spatial()], or an optimization data frame produced by [select_spatial_predictors_sequential()] or [select_spatial_predictors_recursive()].
#' @param point.color Colors of the plotted points. Can be a single color name (e.g. "red4"), a character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1)`
#' @param verbose Logical, if `TRUE` the plot is printed. Default: `TRUE`
#' @details If the method used to fit a model with [rf_spatial()] is "hengl", the function returns nothing, as this method does not require optimization.
#' @return A ggplot.
#' @rdname plot_optimization
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#'  data(distance_matrix)
#'  data(plant_richness_df)
#'
#'  #names of the response and predictors
#'  dependent.variable.name <- "richness_species_vascular"
#'  predictor.variable.names <- colnames(plant_richness_df)[5:21]
#'
#'  #spatial model
#'  model <- rf_spatial(
#'    data = plant_richness_df,
#'    dependent.variable.name = dependent.variable.name,
#'    predictor.variable.names = predictor.variable.names,
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = 0,
#'    method = "mem.moran.sequential",
#'    n.cores = 1,
#'    seed = 1
#'  )
#'
#'  #plotting selection of spatial predictors
#'  plot_optimization(model = model)
#'
#'
#' }
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_c geom_path geom_vline labs xlab ylab ggtitle
#' @export
plot_optimization <- function(
  model,
  point.color = viridis::viridis(
    100,
    option = "F",
    direction = -1
  ),
  verbose = TRUE
) {
  #declaring variables
  moran.i <- NULL
  optimization <- NULL
  r.squared <- NULL
  spatial.predictor.index <- NULL

  #getting optimization df from the model
  if (inherits(model, "rf_spatial")) {
    x <- model$spatial$optimization
  } else {
    x <- model
  }

  #plot
  p <- ggplot2::ggplot(data = x) +
    ggplot2::aes(
      y = moran.i,
      x = r.squared,
      color = optimization
    ) +
    ggplot2::geom_point(
      aes(size = spatial.predictor.index)
    ) +
    ggplot2::scale_color_gradientn(colors = point.color) +
    ggplot2::geom_point(
      data = x[x$selected, ],
      aes(
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
      aes(
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
    ggplot2::theme(plot.title = element_text(hjust = 0.5))

  if (verbose == TRUE) {
    suppressMessages(print(p))
  }
}
