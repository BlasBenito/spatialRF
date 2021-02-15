#' @title Plots the response curves of a random forest x
#' @description Plots the response curves of models fitted with [rf()], [rf_repeat()], or  [rf_spatial()].
#' @param x A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param variables Character vector, names of predictors to plot. If `NULL`, the most important variables (importance higher than the median) in `x` are selected. Default: `NULL`.
#' @param quantiles Numeric vector with values between 0 and 1, argument `probs` of \link[stats]{quantile}. Quantiles to set the other variables to. Default: `c(0.1, 0.5, 0.9)`
#' @param grid.resolution Integer between 20 and 500. Resolution of the plotted curve Default: `100`
#' @param show.data Logical, if `TRUE`, the observed data is plotted along with the response curves. Default. `FALSE`
#' @param verbose Logical, if TRUE the plot is printed. Default: `TRUE`
#' @return A list with slots named after the selected `variables`, with one ggplot each.
#' @details All variables that are not plotted in a particular response curve are set to the values of their respective quantiles, and the response curve for each one of these quantiles is shown in the plot. The output list can be plotted all at once with `patchwork::wrap_plots(p)` or `cowplot::plot_grid(plotlist = p)`, or one by one by extracting each plot from the list.
#' @seealso [plot_response_surfaces()]
#' @examples
#' \dontrun{
#' if(interactive()){
#'data(plant_richness_df)
#'
#'out <- rf(
#'  data = plant_richness_df,
#'  dependent.variable.name = "richness_species_vascular",
#'  predictor.variable.names = colnames(plant_richness_df)[5:21],
 #'  verbose = FALSE
#')
#'
#'p <- plot_response_curves(x = out)
#'
#' }
#' }
#' @rdname plot_response_curves
#' @export
plot_response_curves <- function(
  x = NULL,
  variables = NULL,
  quantiles = c(0.1, 0.5, 0.9),
  grid.resolution = 100,
  show.data = FALSE,
  verbose = TRUE
  ){

  if(is.null(x)){
    stop("Argument 'x' must not be empty.")
  }

  grid.resolution <- floor(grid.resolution)
  if(grid.resolution > 500){grid.resolution <- 500}
  if(grid.resolution < 20){grid.resolution <- 20}

  quantiles <- quantiles[quantiles >= 0]
  quantiles <- quantiles[quantiles <= 1]

  data <- x$ranger.arguments$data

  #getting the response variable
  response.variable <- x$ranger.arguments$dependent.variable.name
  predictors <- x$ranger.arguments$predictor.variable.names
  if(inherits(x, "rf_spatial")){
    predictors <- predictors[!(predictors %in% x$selection.spatial.predictors$names)]
  }

  #default values for variables
  if(is.null(variables)){
    variables <- x$variable.importance$per.variable[x$variable.importance$per.variable$variable %in% predictors, "variable"][1:floor(length(predictors) / 2)]
  }

  if(sum(variables %in% colnames(data)) != length(variables)){
    stop("Variable names in 'variables' must be column names of x$ranger.arguments$data.")
  }

  #list to store plots
  variables.plots <- list()

  #iterating through variables
  for(variable.i in variables){

    #names of the other variables
    other.variables <- setdiff(x$ranger.arguments$predictor.variable.names, variable.i)

    #generating grid
    variable.i.grid <- data.frame(
      variable = seq(
        min(data[, variable.i]),
        max(data[, variable.i]),
        length.out = grid.resolution
      )
    )
    colnames(variable.i.grid) <- variable.i

    #list to store quantile results
    variable.i.quantiles <- list()

    #iterating through quantiles
    for(quantile.i in quantiles){

      #grid copy
      variable.i.grid.copy <- variable.i.grid

      #iterating through variables
      for(variable.j in other.variables){
        variable.i.grid.copy[, variable.j] <- quantile(data[, variable.j], quantile.i)
      }

      #predicting
      variable.i.grid.copy[, response.variable] <- predict(
        x,
        variable.i.grid.copy)$predictions

      #adding quantile column
      variable.i.grid.copy$quantile <- quantile.i

      #save to list
      variable.i.quantiles[[as.character(quantile.i)]] <- variable.i.grid.copy

    }

    #plot data frame
    variable.i.df <- do.call("rbind", variable.i.quantiles)
    variable.i.df$quantile <- factor(variable.i.df$quantile)

    if(show.data == TRUE){

      p.i <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes_string(
            x = variable.i,
            y = response.variable
          ),
          shape = 16,
          alpha = 0.15
        ) +
        ggplot2::geom_line(
          data = variable.i.df,
          ggplot2::aes_string(
            x = variable.i,
            y = response.variable,
            group = "quantile",
            color = "quantile"
          ),
          size = 1
        )  +
      ggplot2::scale_color_viridis_d(
          end = 0.8,
          direction = -1
        ) +
        ggplot2::theme_bw() +
        # ggplot2::theme(legend.position = "none") +
        ggplot2::labs(
          color = "   Quantiles of \n other predictors"
        )

    } else {

      p.i <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = variable.i.df,
          ggplot2::aes_string(
            x = variable.i,
            y = response.variable,
            group = "quantile",
            color = "quantile"
          ),
          size = 1
        )  +
        ggplot2::scale_color_viridis_d(
          end = 0.8,
          direction = -1
        ) +
        ggplot2::theme_bw() +
        # ggplot2::theme(legend.position = "none") +
        ggplot2::labs(
          color = "   Quantiles of \n other predictors"
        )

    }

    variables.plots[[variable.i]] <- p.i

  }#end of iterations through variables

  #add legend to the last slot


  variables.plots.out <- patchwork::wrap_plots(
    variables.plots,
    guides = "collect"
    )

  if(verbose == TRUE){
    variables.plots.out
  }

}
