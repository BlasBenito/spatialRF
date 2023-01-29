#' @title Moran's I plots of a training data frame
#' @description Plots the the Moran's I test of the response and the predictors in a training data frame.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param response.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictors.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Optionally, the result of [mc_auto_cor()] or [mc_auto_vif()] Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector, distances below each value are set to 0 on separated copies of the distance matrix for the computation of Moran's I at different neighborhood distances. If `NULL`, it defaults to `seq(0, max(distance.matrix)/4, length.out = 2)`. Default: `NULL`
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1)`
#' @param point.color Character vector with a color name (e.g. "red4"). Default: `gray30`
#' @return A ggplot2 object.
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_numeric_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#'    #plot Moran's I of training data
#'    plot_moran_training_data(
#'      data = ecoregions_df,
#'      response.name = ecoregions_continuous_response,
#'      predictors.names = ecoregions_numeric_predictors,
#'      distance.matrix = ecoregions_distance_matrix,
#'      distance.thresholds = c(
#'        0,
#'        2000,
#'        4000,
#'        6000,
#'        8000
#'        )
#'      )
#' }
#' @rdname plot_training_df_moran
#' @export
plot_training_df_moran <- function(
  data = NULL,
  response.name = NULL,
  predictors.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1
    ),
  point.color = "gray30"
){

  #declaring variables to avoid check complaints
  distance.threshold <- NULL
  p_value_binary <- NULL
  moran.i <- NULL
  x <- NULL

  if(
    is.null(data) |
    is.null(response.name) |
    is.null(predictors.names)
  ){
    stop("No variables to plot.")
  }

  #predictors.names comes from mc_auto_vif or mc_auto_cor
  if(!is.null(predictors.names)){
    if(inherits(predictors.names, "variable_selection")){
      predictors.names <- predictors.names$selected.variables
    }
  }

  if(is.null(distance.matrix)){
    stop("distance.matrix is missing.")
  }

  #creating distance thresholds
  if(is.null(distance.thresholds)){
    distance.thresholds <- default_distance_thresholds(distance.matrix = distance.matrix)
  }


  #iterating through variables in the training data frame
  df.list <- list()

  for(variable in c(
    response.name,
    predictors.names
  )
  ){

    #computing moran for different distance thresholds
    temp.df <- moran_multithreshold(
      x = as.vector(data[, variable]),
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      verbose = FALSE
    )$per_distance

    #adding the variable name to the data frame
    temp.df$variable <- variable

    df.list[[variable]] <- temp.df

  }

  #to data frame
  plot.df <- do.call("rbind", df.list)
  rownames(plot.df) <- NULL

  #adding binary p.value
  plot.df$p_value_binary <- "< 0.05"
  plot.df[plot.df$p.value >= 0.05, "$p_value_binary"] <- ">= 0.05"
  plot.df$p_value_binary <- factor(
    plot.df$p_value_binary,
    levels = c("< 0.05", ">= 0.05")
    )

  #variable as factor
  plot.df$variable <- factor(
    plot.df$variable,
    levels = c(
      rev(predictors.names),
      response.name
      )
  )

  #plotting moran's i
  p <- ggplot2::ggplot(data = plot.df) +
    ggplot2::scale_fill_gradientn(colors = fill.color) +
    ggplot2::geom_tile(
      ggplot2::aes(
        x = factor(distance.threshold),
        y = variable,
        fill = moran.i
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = factor(distance.threshold),
        y = variable,
        size = x$p_value_binary
      ),
      color = point.color,
      pch = 1
    ) +
    ggplot2::scale_size_manual(
      breaks = c("< 0.05", ">= 0.05"),
      values = c(2.5, 5),
      drop = FALSE
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::ylab("") +
    ggplot2::xlab("Distance threshold") +
    ggplot2::labs(
      fill = "Moran's I",
      size = "p-value"
      )

  p

}
