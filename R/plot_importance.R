#' @title Plots the variable importance of a model
#' @description Plots variable importance scores of [rf()], [rf_repeat()], and [rf_spatial()] models. Distributions of importance scores produced with [rf_repeat()] are plotted using `ggplot2::geom_violin`, which shows the median of the density estimate rather than the actual median of the data. However, the violin plots are ordered from top to bottom by the real median of the data to make small differences in median importance easier to spot. Ths function does not plot the result of [rf_importance()] yet, but you can find it under `model$importance$cv.per.variable.plot`.
#' @usage
#' plot_importance(
#'   model,
#'   fill.color = viridis::viridis(
#'     100,
#'     option = "F",
#'     direction = -1,
#'     alpha = 1,
#'     end = 0.9
#'    ),
#'   line.color = "white",
#'   verbose = TRUE
#' )
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()], or a data frame with variable importance scores (only for internal use within the package functions).
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1, alpha = 0.8, end = 0.9)`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"white"`
#' @param verbose Logical, if `TRUE`, the plot is printed. Default: `TRUE`
#' @return A ggplot.
#' @seealso [print_importance()], [get_importance()]
#' @examples

#' if(interactive()){
#'
#' #loading example data
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' #fitting a random forest model
#' rf.model <- rf(
#'  data = plant_richness_df,
#'  dependent.variable.name = "richness_species_vascular",
#'  predictor.variable.names = colnames(plant_richness_df)[5:21],
#'  distance.matrix = distance_matrix,
#'  distance.thresholds = 0,
#'  n.cores = 1,
#'  verbose = FALSE
#' )
#'
#' #plotting variable importance scores
#' plot_importance(model = rf.model)
#'
#' }
#' @rdname plot_importance
#' @export
#' @importFrom ggplot2 ggplot aes geom_point scale_fill_viridis_c ylab xlab theme geom_boxplot scale_fill_viridis_d
#' @importFrom grDevices colorRampPalette
plot_importance <- function(
  model,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1,
    alpha = 1,
    end = 0.9
  ),
  line.color = "white",
  verbose = TRUE
  ){

  #declaring variables
  importance <- NULL
  variable <- NULL
  importance.oob <- NULL

  #if x is not a data frame
  if(!is.data.frame(model)){

    #importance from rf
    if(inherits(model, "rf") & !inherits(model, "rf_spatial") & !inherits(model, "rf_repeat")){
      x <- model$importance$per.variable

      if("importance.oob" %in% colnames(x)){
        x <- dplyr::rename(
          x,
          importance = importance.oob
        )
      }

    }

    #importance from rf_repeat
    if(inherits(model, "rf_repeat") & !inherits(model, "rf_spatial")){
      x <- model$importance$per.repetition
    }

    #importance from rf_spatial and rf
    if(inherits(model, "rf_spatial")){
      x <- model$importance$spatial.predictors
    }

  } else {
    x <- model
  }

  #find duplicates in "variable"
  variable.duplicated <- duplicated(x$variable)

  #no duplicates, rf
  if(sum(variable.duplicated) == 0){

    p <- ggplot2::ggplot(data = x) +
      ggplot2::aes(
        x = importance,
        y = reorder(
          variable,
          importance,
          FUN = max
        ),
        fill = importance
      ) +
      ggplot2::geom_point(
        size = 4,
        shape = 21,
        color = line.color
        ) +
      ggplot2::scale_fill_gradientn(colors = fill.color) +
      ggplot2::ylab("") +
      ggplot2::xlab("Mean error increase when permuted") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle("Permutation importance\ncomputed on the out-of-bag data")

  } else {

    #no "spatial_predictors" in variable, rf_repeat
    if(!("spatial_predictors" %in% x$variable)){

      #adapting palette
      n.variables <- length(unique(x$variable))
      if(length(fill.color) != 1){
        if(length(fill.color) > length(n.variables)){
          fill.colors.function <- grDevices::colorRampPalette(
            fill.color,
            alpha = TRUE
            )
          fill.color <- fill.colors.function(n.variables)
        }
      }

      p <- ggplot2::ggplot(data = x) +
        ggplot2::aes(
          x = importance,
          y = reorder(
            variable,
            importance,
            FUN = stats::median
          ),
          fill = reorder(
            variable,
            importance,
            FUN = stats::median
          )
        ) +
        ggplot2::geom_violin(
          draw_quantiles = 0.5,
          color = line.color,
          scale = "width"
          ) +
        ggplot2::scale_fill_manual(values = fill.color) +
        ggplot2::ylab("") +
        ggplot2::xlab("Increase in error when permuted") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Permutation importance\ncomputed on the out-of-bag data")

    }

    #spatial_predictors, rf_spatial
    if("spatial_predictors" %in% x$variable){

      #if no predictors duplicated, rf_spatial rf
      if(sum(duplicated(x$variable[x$variable != "spatial_predictors"])) == 0){

        p <- ggplot2::ggplot(data = x) +
          ggplot2::aes(
            x = importance,
            y = reorder(
              variable,
              importance,
              FUN = max
            ),
            fill = importance
          ) +
          ggplot2::geom_point(
            size = 4,
            shape = 21,
            color = line.color
          ) +
          ggplot2::scale_fill_gradientn(colors = fill.color) +
          ggplot2::ylab("") +
          ggplot2::xlab("Increase in error when permuted") +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "none") +
          ggplot2::ggtitle("Permutation importance\ncomputed on the out-of-bag data")

        #rf_spatial rf_repeat
      } else {

        #adapting palette
        n.variables <- length(unique(x$variable))
        if(length(fill.color) != 1){
          if(length(fill.color) > length(n.variables)){
            fill.colors.function <- grDevices::colorRampPalette(
              fill.color,
              alpha = TRUE
              )
            fill.color <- fill.colors.function(n.variables)
          }
        }

        p <- ggplot2::ggplot(data = x) +
          ggplot2::aes(
            x = importance,
            y = reorder(
              variable,
              importance,
              FUN = stats::median
            ),
            fill = reorder(
              variable,
              importance,
              FUN = stats::median
            )
          ) +
          ggplot2::geom_violin(
            draw_quantiles = 0.5,
            color = line.color,
            scale = "width"
            ) +
          ggplot2::scale_fill_manual(values = fill.color) +
          ggplot2::ylab("") +
          ggplot2::xlab("Increase in error when permuted") +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "none")+
          ggplot2::ggtitle("Permutation importance\ncomputed on the out-of-bag data")

      }

    }

  }

  if(verbose == TRUE){
    suppressWarnings(print(p))
  }

  return(p)

}
