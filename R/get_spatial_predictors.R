#' @title get_spatial_predictors
#' @description Returns spatial predictors from a model fitted with [rf_spatial()] in order of importance.
#' @param x A model fitted with [rf_spatial()]. Default: NULL
#' @return A data frame with the spatial predictors included in the model.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(distance_matrix)
#'  data(plant_richness_df)

#'  #mem.moran.sequential
#'  model <- rf_spatial(
#'    data = data,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = olnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 500, 1000),
#'    method = "mem.moran.sequential"
#'  )
#'
#' spatial.predictors <- get_spatial_predictors(x = model)
#'  }
#' }
#' @rdname get_spatial_predictors
#' @export
get_spatial_predictors <- function(x){

  if(!inherits(x, "rf_spatial")){
    stop("This function only works on models fitted with 'rf_spatial'")
  }

  spatial.predictors.order <- x$variable.importance$per.variable$variable[x$variable.importance$per.variable$variable %in% x$selection.spatial.predictors$names]

  x$ranger.arguments$data[, spatial.predictors.order, drop = FALSE]

}
