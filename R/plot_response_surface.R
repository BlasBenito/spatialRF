#' @title Plots the response surfaces of a random forest model
#' @description Plots response surfaces for any given pair of predictors in a [rf()], [rf_repeat()], or [rf_spatial()] model.
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()]. Default `NULL`
#' @param a Character string, name of a model predictor. If `NULL`, the most important variable in `model` is selected. Default: `NULL`
#' @param b Character string, name of a model predictor. If `NULL`, the second most important variable in `model` is selected. Default: `NULL`
#' @param quantiles Numeric vector between 0 and 1. Argument `probs` of the function \link[stats]{quantile}. Quantiles to set the other variables to. Default: `0.5`
#' @param grid.resolution Integer between 20 and 500. Resolution of the plotted surface Default: `100`
#' @param point.size.range Numeric vector of length 2 with the range of point sizes used by \link[ggplot2]{geom_point}. Using `c(-1, -1)` removes the points. Default: `c(0.5, 2.5)`
#' @param point.alpha Numeric between 0 and 1, transparency of the points. Setting it to `0` removes all points. Default: `1`.
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1, alpha = 0.9)`
#' @param point.color Character vector with a color name (e.g. "red4"). Default: `gray30`
#' @param verbose Logical, if TRUE the plot is printed. Default: `TRUE`
#' @return A list with slots named after the selected `quantiles`, each one with a ggplot.
#' @details All variables that are not `a` or `b` in a response curve are set to the values of their respective quantiles to plot the response surfaces. The output list can be plotted all at once with `patchwork::wrap_plots(p)` or `cowplot::plot_grid(plotlist = p)`, or one by one by extracting each plot from the list.
#' @seealso [plot_response_curves()]
#' @examples
#'
#' data(plants_rf)
#'
#' plot_response_surface(
#'   model = plants_rf,
#'   a = "climate_bio1_average",
#'   b = "human_population",
#'   grid.resolution = 50
#' )
#'
#' @rdname plot_response_surface
#' @family visualization
#' @export
#' @importFrom ggplot2 ggplot geom_tile aes theme_bw geom_point scale_size_continuous labs ggtitle .data
#' @importFrom viridis scale_fill_viridis
#' @importFrom patchwork wrap_plots
plot_response_surface <- function(
  model = NULL,
  a = NULL,
  b = NULL,
  quantiles = 0.5,
  grid.resolution = 100,
  point.size.range = c(0.5, 2.5),
  point.alpha = 1,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1,
    alpha = 0.9
  ),
  point.color = "gray30",
  verbose = TRUE
) {
  if (is.null(model)) {
    stop("Argument 'x' must not be empty.")
  }

  grid.resolution <- floor(grid.resolution)
  if (grid.resolution > 500) {
    grid.resolution <- 500
  }
  if (grid.resolution < 20) {
    grid.resolution <- 20
  }

  quantiles <- quantiles[quantiles >= 0]
  quantiles <- quantiles[quantiles <= 1]

  data <- model$ranger.arguments$data

  #response variable and predictors
  response.variable <- model$ranger.arguments$dependent.variable.name
  predictors <- model$ranger.arguments$predictor.variable.names
  if (inherits(model, "rf_spatial")) {
    predictors <- predictors[!(predictors %in% model$spatial$names)]
  }

  #default values for a and b
  if (is.null(a)) {
    a <- model$importance$per.variable[
      model$importance$per.variable$variable %in% predictors,
      "variable"
    ][1]
  }
  if (is.null(b)) {
    b <- model$importance$per.variable[
      model$importance$per.variable$variable %in% predictors,
      "variable"
    ][2]
  }

  if (!(a %in% colnames(data))) {
    stop("Argument 'a' must be a column name of model$ranger.arguments$data.")
  }
  if (!(b %in% colnames(data))) {
    stop("Argument 'b' must be a column name of model$ranger.arguments$data.")
  }

  #names of the other variables
  other.variables <- setdiff(
    model$ranger.arguments$predictor.variable.names,
    c(a, b)
  )

  #generating grid
  ab.grid <- expand.grid(
    seq(
      min(data[[a]]),
      max(data[[a]]),
      length.out = grid.resolution
    ),
    seq(
      min(data[[b]]),
      max(data[[b]]),
      length.out = grid.resolution
    )
  )
  colnames(ab.grid) <- c(a, b)

  #list by quantile
  ab.grid.quantiles <- list()

  #iterating through quantiles
  for (quantile.i in quantiles) {
    #ab.grid.copy
    ab.grid.i <- ab.grid

    #iterating through variables
    for (variable in other.variables) {
      ab.grid.i[, variable] <- quantile(data[, variable], quantile.i)
    }

    #predicting the response
    ab.grid.i[, response.variable] <- predict(
      model,
      ab.grid.i
    )$predictions

    #saving plot
    ab.grid.quantiles[[as.character(quantile.i)]] <- ggplot2::ggplot(
      data = ab.grid.i
    ) +
      ggplot2::geom_tile(
        ggplot2::aes(
          x = !!rlang::sym(a),
          y = !!rlang::sym(b),
          fill = !!rlang::sym(response.variable)
        )
      ) +
      ggplot2::scale_fill_gradientn(colors = fill.color) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(
          x = !!rlang::sym(a),
          y = !!rlang::sym(b),
          size = !!rlang::sym(response.variable)
        ),
        shape = 21,
        alpha = point.alpha,
        color = point.color
      ) +
      ggplot2::scale_size_continuous(range = point.size.range) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(
        fill = "Predicted",
        size = "Observed"
      ) +
      ggplot2::ggtitle(paste0("Other variables set to quantile ", quantile.i)) +
      ggplot2::guides(size = ggplot2::guide_legend(reverse = TRUE)) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))
  }

  if (length(quantiles) > 1) {
    ab.grid.quantiles <- patchwork::wrap_plots(ab.grid.quantiles)
  } else {
    ab.grid.quantiles <- ab.grid.quantiles[[1]]
  }

  if (verbose) {
    ab.grid.quantiles
  }

  ab.grid.quantiles
}
