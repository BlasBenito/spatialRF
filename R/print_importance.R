#' @title print_importance
#' @description prints variable importance scores from [rf], [rf_repeat], and [rf_spatial] models.
#' @param x A model produced by [rf], [rf_repeat], or [rf_spatial], or a data frame with importance scores. Default: NULL
#' @param verbose Logical, if TRUE, variable importance is returned. Default: TRUE
#' @return a ggplot
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#' data(plant_richness_df)
#' data(distance.matrix)
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = c(0, 1000, 2000),
#'   verbose = FALSE
#' )
#' print_importance(x = rf.model)
#'  }
#' }
#' @rdname print_importance
#' @export
print_importance <- function(x = NULL, verbose = TRUE){

  #declaring variables
  importance <- NULL
  variable <- NULL

  #if x is not a data frame
  if(!is.data.frame(x)){

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



