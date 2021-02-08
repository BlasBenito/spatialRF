#' @title response_surface
#' @description Plots response surfaces for a given pair of predictors from an [rf()], [rf_repeat()], or an [rf_spatial()] model.
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param a Character string, name of a model predictor. If `NULL`, the most important variable in `model` is selected. Default: `NULL`.
#' @param b Character string, name of a model predictor. If `NULL`, the second most important variable in `model` is selected. Default: `NULL`.
#' @param quantiles Numeric vector between 0 and 1. Argument *probs* of the function \link[stats]{quantile}. Quantiles to set the other variables to. Default: c(0.1, 0.5, 0.9)
#' @param grid.resolution Integer between 20 and 500. Resolution of the plotted surface Default: 100
#' @param point.size.range Numeric vector of length 2 with the range of point sizes used by \link[ggplot2]{geom_point}, Default: c(0.5, 2.5)
#' @return A list with slots named after the selected `quantiles` with a ggplot.
#' @details All variables that are not `a` or `b` are set to the values of their respective quantiles to plot the response surfaces. The output list can be plotted all at once with `patchwork::wrap_plots(p)` or `cowplot::plot_grid(plotlist = p)`, or one by one by extracting each plot with print(p[[1]]).
#' @examples
#' \dontrun{
#' if(interactive()){
#'data(plant_richness_df)
#'
#'m <- rf(
#'  data = plant_richness_df,
#'  dependent.variable.name = "richness_species_vascular",
#'  predictor.variable.names = colnames(plant_richness_df)[5:21],
 #'  verbose = FALSE
#')
#'
#'p <- response_surface(model = m)
#'  }
#' }
#' @rdname response_surface
#' @export
#' @importFrom ggplot2 ggplot geom_tile aes_string theme_bw geom_point scale_size_continuous labs ggtitle
#' @importFrom viridis scale_fill_viridis
#' @importFrom patchwork wrap_plots
response_surface <- function(
  model = NULL,
  a = NULL,
  b = NULL,
  quantiles = c(0.10, 0.5, 0.90),
  grid.resolution = 100,
  point.size.range = c(0.5, 2.5)
  ){

  if(is.null(model)){
    stop("Argument 'model' must not be empty.")
  }

  grid.resolution <- floor(grid.resolution)
  if(grid.resolution > 500){grid.resolution <- 500}
  if(grid.resolution < 20){grid.resolution <- 20}

  quantiles <- quantiles[quantiles >= 0]
  quantiles <- quantiles[quantiles <= 1]

  data <- model$ranger.arguments$data

  #default values for a and b
  if(is.null(a)){
    a <- model$variable.importance$per.variable$variable[1]
  }
  if(is.null(b)){
    b <- model$variable.importance$per.variable$variable[2]
  }

  if(!(a %in% colnames(data))){
    stop("Argument 'a' must be a column name of model$ranger.arguments$data.")
  }
  if(!(b %in% colnames(data))){
    stop("Argument 'b' must be a column name of model$ranger.arguments$data.")
  }

  #names of the other variables
  other.variables <- setdiff(colnames(data), c(a, b))
  response.variable <- model$ranger.arguments$dependent.variable.name

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
      length.out = grid.resolution)
  )
  colnames(ab.grid) <- c(a, b)

  #list by quantile
  ab.grid.quantiles <- list()

  #iterating through quantiles
  for(quantile.i in quantiles){

    #ab.grid.copy
    ab.grid.i <- ab.grid

    #iterating through variables
    for(variable in other.variables){
      ab.grid.i[, variable] <- quantile(data[, variable], quantile.i)
    }

    #predicting the response
    ab.grid.i[, response.variable] <- predict(
      model,
      ab.grid.i)$predictions

    #heatmap
    #saving output
    ab.grid.quantiles[[as.character(quantile.i)]] <- ggplot2::ggplot(data = ab.grid.i) +
      ggplot2::geom_tile(
        ggplot2::aes_string(
          x = a,
          y = b,
          fill = response.variable
          )
        ) +
      viridis::scale_fill_viridis(
        direction = -1,
        begin = 0.1
      ) +
      ggplot2::theme_bw() +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes_string(
          x = a,
          y = b,
          size = response.variable
        ),
        shape = 21,
        alpha = 0.5
      ) +
      ggplot2::scale_size_continuous(range = point.size.range) +
      ggplot2::labs(
        fill = "Predicted",
        size = "Observed"
      ) +
      ggplot2::ggtitle(paste0("Quantile ", quantile.i))

  }

  print(patchwork::wrap_plots(ab.grid.quantiles))

  ab.grid.quantiles

}
