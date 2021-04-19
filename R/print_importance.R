#' @title Prints variable importance
#' @description Prints variable importance scores from [rf], [rf_repeat], and [rf_spatial] models.
#' @param model A model fitted with [rf], [rf_repeat], or [rf_spatial].
#' @usage
#' print_importance(
#'   model,
#'   verbose = TRUE
#' )
#' @param verbose Logical, if `TRUE`, variable importance is returned. Default: `TRUE`
#' @return A table printed to the standard output.
#' @seealso [plot_importance()], [get_importance()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#' data(distance.matrix)
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
#' print_importance(model = rf.model)
#'
#' }
#' }
#' @rdname print_importance
#' @export
print_importance <- function(model, verbose = TRUE){

  #declaring variables
  importance <- NULL


  #if x is not a data frame
  if(!is.data.frame(model)){

    #importance from rf
    if((inherits(model, "rf") & !inherits(model, "rf_spatial")) | (inherits(model, "rf_repeat") & !inherits(model, "rf_spatial"))){
      x <- model$variable.importance$per.variable
    }

    #importance from rf_repeat
    if(inherits(model, "rf_spatial")){

      if(!is.null(model$ranger.arguments$repetitions)){
        repetitions <- model$ranger.arguments$repetitions
      } else {
        repetitions <- 1
      }

      #count non-spatial predictors
      length.non.spatial.predictors <- sum(model$variable.importance$spatial.predictors$variable != "spatial_predictors") / repetitions

      length.spatial.predictors <- sum(model$variable.importance$spatial.predictors$variable == "spatial_predictors") / repetitions

      #get spatial.predictor.stats if too many spatial predictors
      if(length.spatial.predictors >= length.non.spatial.predictors){
        x <- model$variable.importance$spatial.predictors.stats
      } else {
        x <- model$variable.importance$per.variable
      }
    }
  }

  if(is.null(x)){
    stop("This model doesn't have a 'variable.importance' slot")
  }

  #arranging
  x <- dplyr::arrange(x, dplyr::desc(importance))

  #pretty colnames
  colnames(x) <- c("Variable", "Importance")

  #preparing huxtable
  x.hux <- huxtable::hux(x) %>%
    huxtable::set_bold(
      row = 1,
      col = huxtable::everywhere,
      value = TRUE
    ) %>%
    huxtable::set_all_borders(TRUE)
  huxtable::number_format(x.hux)[, 2] <- 3
  huxtable::print_screen(x.hux, colnames = FALSE)

}



