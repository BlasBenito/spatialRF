#' @title get_evaluation
#' @description Returns an evaluation performed with [rf_evaluate()] as a data frame.
#' @param x A model resulting from [rf_evaluate()]
#' @return A data frame.
#' @rdname get_evaluation
#' @export
get_evaluation <- function(x){

  #stop if no evaluation slot
  if(!inherits(x, "rf_evaluate")){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  x <- x$evaluation$aggregated[, c("model", "performance.measure", "mean", "sd", "min", "max")]

  x

}
