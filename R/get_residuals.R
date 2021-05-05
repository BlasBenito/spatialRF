#' @title Gets model residuals
#' @description Returns the residuals of models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param x A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A vector with model residuals, or the median of model residuals across repetitions if the model was fitted with [rf_repeat()].
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#'
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   verbose = FALSE
#' )
#'
#' x <- get_residuals(x = rf.model)
#' x
#'
#' }
#' }
#' @rdname get_residuals
#' @export
get_residuals <- function(x){

    x$residuals$values

}
