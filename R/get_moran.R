#' @title get_moran
#' @description Returns the Moran's I test of a model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @param x A model produced by [rf()], [rf_repeat()], or [rf_spatial()]
#' @return A data frame with moran test results produced by [moran_multithreshold()].
#' @examples
#'  data(plant_richness_df)
#'  data(distance.matrix)
#'
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000),
#'    verbose = FALSE
#'  )
#'
#'  x <- get_moran(rf.model)
#' @rdname get_moran
#' @export
get_moran <- function(x){
   x$spatial.correlation.residuals$per.distance
}
