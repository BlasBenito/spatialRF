#' @title Gets Moran's I test of model residuals
#' @description Returns the Moran's I test on the residuals of a model produced by [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()]
#' @return A data frame with Moran's I test results produced by [moran_multithreshold()].
#' @seealso [moran()], [moran_multithreshold()], [plot_moran()], [print_moran()].
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predvar_names,
#'   ecoregions_depvar_name
#'   )
#'
#'  #fitting random forest model
#'  rf.model <- rf(
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_depvar_name,
#'    predictor.variable.names = ecoregions_predvar_names,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
#'
#'  #getting Moran's I of the residuals
#'  x <- get_moran(rf.model)
#'
#' }
#' @rdname get_moran
#' @export
get_moran <- function(model){
   model$residuals$autocorrelation$per.distance
}
