#' @title Prints results of a Moran's I test
#' @description Prints the results of a Moran's I test on the residuals of a model.
#' @usage
#' print_moran(
#'   model,
#'   caption = NULL,
#'   verbose = TRUE
#' )
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param caption Character, caption of the output table, Default: `NULL`
#' @param verbose Logical, if `TRUE`, the resulting table is printed into the console, Default: `TRUE`
#' @return Prints a table in the console using the \link[huxtable]{huxtable} package.
#' @seealso [moran()], [moran_multithreshold()], [get_moran()], [plot_moran()]
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#'  data(plant_richness_df)
#'  data(distance.matrix)
#'
#'  #fitting random forest model
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000),
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
#'
#'  #printing Moran's I of model's residuals
#'  print_moran(rf.model)
#'
#' }
#' @rdname print_moran
#' @export
#' @importFrom huxtable hux set_bold everywhere set_all_borders number_format caption print_screen
#' @importFrom dplyr group_by summarise arrange
print_moran <- function(model, caption = NULL, verbose = TRUE){

  #if model is not a data frame
  if(inherits(model, "rf") | inherits(model, "rf_repeat") | inherits(model, "rf_spatial")){
      x <- model$residuals$autocorrelation$per.distance
  }

  #subsetting columns
  x <- x[, c("distance.threshold", "moran.i", "p.value", "interpretation")]

    #for models rf and rf_repeat
    if(!("model" %in% colnames(x))){

      #adding pretty colnames
      colnames(x) <- c("Distance", "Moran's I", "P value", "Interpretation")

      #preparing the huxtable
      x.hux <-
        huxtable::hux(x) %>%
        huxtable::set_bold(
          row = 1,
          col = huxtable::everywhere,
          value = TRUE
        ) %>%
        huxtable::set_all_borders(TRUE)
      huxtable::number_format(x.hux)[2:nrow(x.hux), 2:3] <- 3
      huxtable::number_format(x.hux)[2:nrow(x.hux), 1] <- 1

    }

    #for rf_spatial with rf
    if("model" %in% colnames(x) & !("repetition" %in% colnames(x))){

      #adding pretty colnames
      colnames(x) <- c("Distance", "Moran's I", "P value", "Interpretation", "Model")

      #reordering x
      x <- x[, c("Model", "Distance", "Moran's I", "P value", "Interpretation")]

      #preparing the huxtable
      x.hux <-
        huxtable::hux(x) %>%
        huxtable::set_bold(
          row = 1,
          col = huxtable::everywhere,
          value = TRUE
        ) %>%
        huxtable::set_bold(
          col = 1,
          row = huxtable::everywhere,
          value = TRUE
        ) %>%
        huxtable::set_all_borders(TRUE)
      huxtable::number_format(x.hux)[2:nrow(x.hux), 3:4] <- 3
      huxtable::number_format(x.hux)[2:nrow(x.hux), 2] <- 1

    }

    #add caption
    if(!is.null(caption)){
      huxtable::caption(x.hux) <- caption
    }

    #print to screen
    huxtable::print_screen(x.hux, colnames = FALSE)


}
