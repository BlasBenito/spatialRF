#' @title Gets the local importance data frame from a model
#' @description Gets local importance scores from [rf()], [rf_repeat()], and [rf_spatial()] models.
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()]. Default: NULL
#' @return A data frame with variable names and local importance scores.
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [plot_importance()], [print_importance()].
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' #fittinga random forest model
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = 0,
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #getting importance scores
#' x <- get_importance_local(rf.model)
#' x
#'
#' }
#' @rdname get_importance_local
#' @export
get_importance_local <- function(model){

  model$importance$local

}
