#' @title print_moran
#' @description Prints spatial correlation tests.
#' @param x A model produced by [rf()], [rf_repeat()], or [rf_spatial()], or a data frame resulting from [moran_multithreshold]. Default: NULL
#' @param caption character, caption to attach to the output, Default: NULL
#' @param verbose logical, if TRUE, the resulting table is printed into the console, Default: TRUE
#' @return Prints a table in the console using the \link[huxtable]{huxtable} package.
#' @examples
#' \dontrun{
#' if(interactive()){
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
#'  print_moran(rf.model)
#'  }
#' }
#' @rdname print_moran
#' @export
#' @importFrom huxtable hux set_bold everywhere set_all_borders number_format caption print_screen
#' @importFrom dplyr group_by summarise arrange
print_moran <- function(x, caption = NULL, verbose = TRUE){

  #declaring variables
  model <- NULL
  distance.threshold <- NULL
  moran.i <- NULL
  p.value <- NULL
  interpretation <- NULL

  #if x is not a data frame
  if(inherits(x, "rf") | inherits(x, "rf_repeat") | inherits(x, "rf_spatial")){
      x <- x$spatial.correlation.residuals$per.distance
  }

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
