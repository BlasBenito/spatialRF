#' @title Scatterplots of a training data frame
#' @description Plots the dependent variable against each predictor.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Optionally, the result of [auto_cor()] or [auto_vif()] Default: `NULL`
#' @param ncol Number of columns of the plot. Argument `ncol` of \link[patchwork]{wrap_plots}.
#' @param method Method for \link[ggplot2]{geom_smooth}, one of: "lm", "glm", "gam", "loess", or a function, for example `mgcv::gam` Default: 'loess'
#' @return A \link[patchwork]{wrap_plots} object.
#' @examples
#' \dontrun{
#' if(interactive()){
#'    data(plant_richness_df)
#'    plot_training_data(
#'      data = plant_richness_df,
#'      dependent.variable.name = "richness_species_vascular",
#'      predictor.variable.names = colnames(plant_richness_df)[5:21]
#'      )
#'  }
#' }
#' @rdname plot_training_df
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_point scale_color_viridis_c theme_bw theme geom_smooth
#' @importFrom patchwork wrap_plots
plot_training_df <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  ncol = 4,
  method = "loess"
){

  if(
    is.null(data) |
    is.null(dependent.variable.name) |
    is.null(predictor.variable.names)
  ){
    stop("No variables to plot.")
  }

  plot.list <- list()
  for(variable in predictor.variable.names){
    plot.list[[variable]] <- ggplot2::ggplot(
      data = data,
      ggplot2::aes_string(
        x = variable,
        y = dependent.variable.name,
        color = dependent.variable.name
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::scale_color_viridis_c(direction = -1) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_smooth(
        method = method,
        col = "gray20",
        formula = y ~ x
      )
  }

  p <- patchwork::wrap_plots(plot.list, ncol = ncol)

  p

}
