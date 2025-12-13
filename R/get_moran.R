#' @title Gets Moran's I test of model residuals
#' @description Returns the Moran's I test on the residuals of a model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()]
#' @return A data frame with Moran's I test results produced by [moran_multithreshold()].
#' @seealso [moran()], [moran_multithreshold()], [plot_moran()], [print_moran()].
#' @examples
#'
#'  #loading example data
#'  data(plants_df)
#'  data(plants_distance)
#'
#'  #fitting a random forest model
#'  rf.model <- rf(
#'    data = plants_df,
#'    dependent.variable.name = plants_response,
#'    predictor.variable.names = plants_predictors,
#'    distance.matrix = plants_distance,
#'    distance.thresholds = c(0, 1000, 2000),
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
#'
#'  #getting Moran's I of the residuals
#'  x <- get_moran(rf.model)
#'  x
#'
#' @rdname get_moran
#' @export
get_moran <- function(model) {
  model$residuals$autocorrelation$per.distance
}
