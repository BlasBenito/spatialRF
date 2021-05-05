#' @title Plot residuals diagnostics
#' @description Plots normality and autocorrelation tests (if available) of a model's residuals.
#' @param model A model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A patchwork object.
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  data(plant_richness_df)
#'  data(distance_matrix)
#'
#'  x <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix
#'  )
#'
#'  plot_residuals_diagnostics(x)
#'
#'  }
#' }
#' @rdname plot_residuals_diagnostics
#' @export
plot_residuals_diagnostics <- function(model){

  #getting normality plot
  normality.test <- model$residuals$normality$plot

  #getting autocorrelation if available
  if("autocorrelation" %in% names(model$residuals)){

    #getting autocorrelation plot
    autocorrelation.test <- model$residuals$autocorrelation$plot

    #combined plot
    p1 <- normality.test / autocorrelation.test

  } else {

    p1 <- normality.test

  }

  p1


}
