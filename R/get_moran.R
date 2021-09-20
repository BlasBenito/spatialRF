#' @title Gets Moran's I test of model residuals
#' @description Returns the Moran's I test on the residuals of a model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()]
#' @return A data frame with Moran's I test results produced by [moran_multithreshold()].
#' @seealso [moran()], [moran_multithreshold()], [plot_moran()], [print_moran()].
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#'  data(plant_richness_df)
#'  data(distance_matrix)
#'
#'  #fitting a random forest model
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000),
#'    verbose = FALSE
#'  )
#'
#'  #getting Moran's I of the residuals
#'  x <- get_moran(rf.model)
#'
#' }
#' @rdname get_moran
#' @export
get_moran <- function(model){
   model$residuals$autocorrelation$per.distance
}
