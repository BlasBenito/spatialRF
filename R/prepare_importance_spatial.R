#' @title Prepares variable importance objects for spatial models
#' @description Prepares variable importance data frames and plots for models fitted with [rf_spatial()].
#' @param x An importance data frame with spatial predictors.
#' @return A list with importance data frames in different formats depending on whether the model was fitted with [rf()] or [rf_repeat()].
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'  data(plant_richness_df)
#'
#'  model <- rf_spatial(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds =  c(0, 500, 1000)
#'  )
#'
#'  importance <- prepare_importance_spatial(model)
#'  names(importance)
#'
#'  }
#' }
#' @rdname prepare_importance_spatial
#' @importFrom stats median
#' @export
prepare_importance_spatial <- function(x){

  importance <- NULL
  variable <- NULL

  #no rf repeat
  if(!inherits(x, "rf_repeat")){

    importance.df <- x$variable.importance$per.variable

  }

  #rf repeat
  if(inherits(x, "rf_repeat")){

    importance.df <- x$variable.importance$per.repetition

  }

  #spatial predictors only
  spatial.predictors <- importance.df[grepl(
    "spatial_predictor",
    importance.df$variable
  ),]
  spatial.predictors$variable <- "spatial_predictors"

  #non-spatial predictors
  non.spatial.predictors <- importance.df[!grepl(
    "spatial_predictor",
    importance.df$variable
  ),]

  #joining for plot
  importance.plot.df <- rbind(
    spatial.predictors,
    non.spatial.predictors
  )

  #aggregating spatial predictors
  #min, max, median and mean of the spatial predictors
  spatial.predictors.stats <- data.frame(
    variable = c(
      "spatial_predictors (max)",
      "spatial_predictors (min)",
      "spatial_predictors (mean)",
      "spatial_predictors (median)"
    ),
    importance = c(
      max(spatial.predictors$importance),
      min(spatial.predictors$importance),
      mean(spatial.predictors$importance),
      median(spatial.predictors$importance)
    )
  )

  #no rf repeat
  if(!inherits(x, "rf_repeat")){

    importance.df <- rbind(
      non.spatial.predictors,
      spatial.predictors.stats
    ) %>%
      dplyr::arrange(dplyr::desc(importance))

  }

  #rf repeat
  if(inherits(x, "rf_repeat")){

    importance.df <- non.spatial.predictors %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(importance = round(mean(importance), 3)) %>%
      as.data.frame() %>%
      rbind(spatial.predictors.stats) %>%
      dplyr::arrange(dplyr::desc(importance))

  }

  #preparing out list
  variable.importance <- list()

  #common slots
  variable.importance$per.variable <- x$variable.importance$per.variable
  variable.importance$per.variable.plot <- plot_importance(
    x$variable.importance$per.variable,
    verbose = FALSE
    )
  variable.importance$spatial.predictors <- importance.plot.df
  variable.importance$spatial.predictors.plot <- plot_importance(
    importance.plot.df,
    verbose = FALSE
  )
  variable.importance$spatial.predictors.stats <- importance.df
  variable.importance$spatial.predictors.stats.plot <- plot_importance(
    importance.df,
    verbose = FALSE
  )

  #filling rf repeat
  if(inherits(x, "rf_repeat")){

    variable.importance$per.repetition <- x$variable.importance$per.repetition
    variable.importance$per.repetition.plot <- plot_importance(
      x$variable.importance$per.repetition,
      verbose = FALSE
    )

  }

  #returning the list
  variable.importance

}
