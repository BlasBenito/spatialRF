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
#' if(interactive()){
#'
#' #loading example data
#' data(plant_richness_df)
#' data(distance.matrix)
#'
#' #fitting a random forest model
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
#' #printing variable importance scores
#' print_importance(model = rf.model)
#'
#' }
#' @rdname print_importance
#' @export
print_importance <- function(model, verbose = TRUE){

  #declaring variables
  importance <- NULL

  #if x is not a data frame
  if(!is.data.frame(model)){

    if(("importance" %in% names(model)) == FALSE){
      stop("The model does not have a slot named 'importance'")
    }

    #importance from rf
    if((inherits(model, "rf") & !inherits(model, "rf_spatial")) | (inherits(model, "rf_repeat") & !inherits(model, "rf_spatial"))){
      x <- model$importance$per.variable
    }

    #importance from rf_repeat
    if(inherits(model, "rf_spatial")){

      #count spatial predictors
      length.spatial.predictors <- length(model$spatial$names)

      #count non-spatial predictors
      length.non.spatial.predictors <- length(model$ranger.arguments$predictor.variable.names) - length.spatial.predictors


      #get spatial.predictor.stats if too many spatial predictors
      if(length.spatial.predictors > length.non.spatial.predictors){
        x <- model$importance$spatial.predictors.stats
      } else {
        x <- model$importance$per.variable
      }
    }
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



