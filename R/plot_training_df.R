#' @title Scatterplots of a training data frame
#' @description Plots the dependent variable against each predictor.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [collinear::case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param ncol Number of columns of the plot. Argument `ncol` of \link[patchwork]{wrap_plots}.
#' @param method Method for \link[ggplot2]{geom_smooth}, one of: "lm", "glm", "gam", "loess", or a function, for example `mgcv::gam` Default: 'loess'
#' @param point.color Colors of the plotted points. Can be a single color name (e.g. "red4"), a character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `grDevices::hcl.colors(100)`). Default: `grDevices::hcl.colors(100, palette = "Zissou 1")`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"gray30"`
#' @return A \link[patchwork]{wrap_plots} object.
#' @examples
#'
#' data(
#'   plants_df,
#'   plants_response,
#'   plants_predictors
#' )
#'
#' plot_training_df(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors[1:4]
#' )
#'
#' @rdname plot_training_df
#' @family visualization
#' @export
#' @autoglobal
plot_training_df <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  ncol = 4,
  method = "loess",
  point.color = grDevices::hcl.colors(
    100,
    palette = "Zissou 1"
  ),
  line.color = "gray30"
) {
  if (
    is.null(data) ||
      is.null(dependent.variable.name) ||
      is.null(predictor.variable.names)
  ) {
    stop("No variables to plot.")
  }

  plot.list <- list()
  for (variable in predictor.variable.names) {
    plot.list[[variable]] <- ggplot2::ggplot(
      data = data,
      ggplot2::aes(
        x = .data[[variable]],
        y = .data[[dependent.variable.name]],
        color = .data[[dependent.variable.name]]
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::scale_color_gradientn(colors = point.color) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_smooth(
        method = method,
        col = line.color,
        formula = y ~ x,
        se = FALSE,
        alpha = 0.75
      )
  }

  p <- patchwork::wrap_plots(plot.list, ncol = ncol)

  p
}
