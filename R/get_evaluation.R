#' @title get_evaluation
#' @description Returns an evaluation performed with [rf_evaluate()] as a data frame.
#' @param x A model resulting from [rf_evaluate()]
#' @return A data frame with evaluation scores.
#' @examples
#' \dontrun{
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
#' rf.model <- rf_evaluate(
#'   model = rf.model,
#'   xy = plant_richness_df[, c("x", "y")],
#'   verbose = FALSE
#' )
#'
#' x <- get_evaluation(x = rf.model)
#'  }
#' }
#' @rdname get_evaluation
#' @export
get_evaluation <- function(x){

  #stop if no evaluation slot
  if(!inherits(x, "rf_evaluate")){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  x <- x$evaluation$aggregated[, c("model", "metric", "mean", "sd", "min", "max")]

  x

}
