#' @title Aggregates importance data frame with spatial predictors
#' @description Aggregates the importance of spatial predictors within an importance data frame into min, max, mean, and median.
#' @param x An importance data frame with spatial predictors.
#' @return A data frame with aggregated importance scores for the spatial predictors.
#' @examples
#' \dontrun{
#' if(interactive()){
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
#'  aggregate_importance(model$variable.importance$per.variable)
#'
#'  }
#' }
#' @rdname aggregate_importance
#' @export
aggregate_importance <- function(x){

  #spatial predictors only
  spatial.predictors <- x[grepl(
    "spatial_predictor",
    x$variable
  ),]
  spatial.predictors$variable <- "spatial_predictors"

  #non-spatial predictors
  non.spatial.predictors <- x[!grepl(
    "spatial_predictor",
    x$variable
  ),]

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

  #final data frame
  importance <- NULL
  out <- rbind(
    non.spatial.predictors,
    spatial.predictors.stats
  ) %>%
    dplyr::arrange(dplyr::desc(importance))

  out

}
