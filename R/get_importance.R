#' @title Gets the global importance data frame from a model
#' @description Gets variable importance scores from [rf()], [rf_repeat()], and [rf_spatial()] models.
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()]. Default: NULL
#' @return A data frame with variable names and importance scores.
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [plot_importance()], [print_importance()].
#' @examples
#' if(interactive()){
#'
#' data(plant_richness_df)
#' data(distance_matrix)
#'
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
#' x <- get_importance(rf.model)
#' x
#'
#' }
#' @rdname get_importance
#' @export
get_importance <- function(model){

  #declaring variables
  importance <- NULL

  #importance from rf
  if((inherits(model, "rf") & !inherits(model, "rf_spatial")) | (inherits(model, "rf_repeat") & !inherits(model, "rf_spatial"))){
    x <- model$importance$per.variable
  }

  #importance from rf_repeat
  if(inherits(model, "rf_spatial")){

    if(!is.null(model$ranger.arguments$repetitions)){
      repetitions <- model$ranger.arguments$repetitions
    } else {
      repetitions <- 1
    }

    #count non-spatial predictors
    length.non.spatial.predictors <- sum(model$importance$spatial.predictors$variable != "spatial_predictors") / repetitions

    length.spatial.predictors <- sum(model$importance$spatial.predictors$variable == "spatial_predictors") / repetitions

    #get spatial.predictor.stats if too many spatial predictors
    if(length.spatial.predictors >= length.non.spatial.predictors){
      x <- model$importance$spatial.predictor.stats
    } else {
      x <- model$importance$per.variable
    }
  }

  if(is.null(x)){
    stop("This model doesn't have a 'variable.importance' slot")
  }

  #arranging
  x <- dplyr::arrange(x, dplyr::desc(importance))

  #return importance
  x

}
