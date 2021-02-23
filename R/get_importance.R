#' @title Gets importance data frame from a model
#' @description Gets variable importance scores from [rf()], [rf_repeat()], and [rf_spatial()] models.
#' @param x A model fitted with [rf()], [rf_repeat()], or [rf_spatial()]. Default: NULL
#' @return A data frame with variable names and importance scores.
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [plot_importance()], [print_importance()].
#' @examples
#' \donttest{
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
#'   distance.thresholds = c(0, 1000, 2000),
#'   verbose = FALSE
#' )
#'
#' x <- get_importance(x = rf.model)
#' x
#'
#' }
#' }
#' @rdname get_importance
#' @export
get_importance <- function(x){

  #declaring variables
  importance <- NULL

  #importance from rf
  if((inherits(x, "rf") & !inherits(x, "rf_spatial")) | (inherits(x, "rf_repeat") & !inherits(x, "rf_spatial"))){
    x <- x$variable.importance$per.variable
  }

  #importance from rf_repeat
  if(inherits(x, "rf_spatial")){

    if(!is.null(x$ranger.arguments$repetitions)){
      repetitions <- x$ranger.arguments$repetitions
    } else {
      repetitions <- 1
    }

    #count non-spatial predictors
    length.non.spatial.predictors <- sum(x$variable.importance$spatial.predictors$variable != "spatial_predictors") / repetitions

    length.spatial.predictors <- sum(x$variable.importance$spatial.predictors$variable == "spatial_predictors") / repetitions

    #get spatial.predictor.stats if too many spatial predictors
    if(length.spatial.predictors >= length.non.spatial.predictors){
      x <- x$variable.importance$spatial.predictor.stats
    } else {
      x <- x$variable.importance$per.variable
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
