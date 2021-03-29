#' @title Moran's I plots of a training data frame
#' @description Plots the the Moran's I test of the response and the predictors in a training data frame.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Optionally, the result of [auto_cor()] or [auto_vif()] Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector, distances below each value are set to 0 on separated copies of the distance matrix for the computation of Moran's I at different neighborhood distances. If `NULL`, it defaults to `seq(0, max(distance.matrix)/4, length.out = 2)`. Default: `NULL`
#' @return A ggplot2 object.
#' @examples
#' \dontrun{
#' if(interactive()){
#'    data(plant_richness_df)
#'    data(distance_matrix)
#'    plot_moran_training_data(
#'      data = plant_richness_df,
#'      dependent.variable.name = "richness_species_vascular",
#'      predictor.variable.names = colnames(plant_richness_df)[5:21],
#'      distance.matrix = distance_matrix,
#'      distance.thresholds = c(
#'        0,
#'        2000,
#'        4000,
#'        6000,
#'        8000
#'        )
#'      )
#'  }
#' }
#' @rdname plot_training_df_moran
#' @export
plot_training_df_moran <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL
){

  #declaring variables to avoid check complaints
  distance.threshold <- NULL
  p.value.binary <- NULL
  moran.i <- NULL

  if(
    is.null(data) |
    is.null(dependent.variable.name) |
    is.null(predictor.variable.names)
  ){
    stop("No variables to plot.")
  }

  if(is.null(distance.matrix)){
    stop("distance.matrix is missing.")
  }

  #creating distance thresholds
  if(is.null(distance.thresholds) == TRUE){
    distance.thresholds <- pretty(
      floor(
        seq(
          0,
          max(distance.matrix)/2,
          length.out = 8
        )
      )
    )
  }

  #iterating through variables in the training data frame
  df.list <- list()
  for(variable in c(
    dependent.variable.name,
    predictor.variable.names
  )
  ){

    #computing moran for different distance thresholds
    temp.df <- moran_multithreshold(
      x = as.vector(data[, variable]),
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      verbose = FALSE
    )$per.distance

    #adding the variable name to the data frame
    temp.df$variable <- variable

    df.list[[variable]] <- temp.df

  }

  #to data frame
  plot.df <- do.call("rbind", df.list)
  rownames(plot.df) <- NULL

  #adding binary p.value
  plot.df$p.value.binary <- "< 0.05"
  plot.df[plot.df$p.value >= 0.05, "p.value.binary"] <- ">= 0.05"
  plot.df$p.value.binary <- factor(
    plot.df$p.value.binary,
    levels = c("< 0.05", ">= 0.05")
    )

  #variable as factor
  plot.df$variable <- factor(
    plot.df$variable,
    levels = c(
      rev(predictor.variable.names),
      dependent.variable.name
      )
  )

  #plotting moran's i
  p <- ggplot2::ggplot(data = plot.df) +
    ggplot2::scale_fill_viridis_c(
      alpha = 0.9,
      begin = 0.05,
      end = 0.95
      ) +
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
        size = p.value.binary
      ),
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
