#' @title Gets out-of-bag performance scores from a model
#' @description Returns the performance slot of an [rf()], [rf_repeat()], or [rf_spatial()] model computed on the out-of-bag data.
#' @param x Model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A data frame with four columns:
#' \itemize{
#'   \item `metric` Name of the performance metric.
#'   \item `mean` Value of the performance metric. Truly a mean only if the model is fitted with [rf_repeat()].
#'   \item `standard_error` Standard error of the mean, only if the model is fitted with [rf_repeat()], and `NA` otherwise.
#'   \item `standard_deviation` Standard deviation of the mean, only if the model is fitted with [rf_repeat()], and `NA` otherwise.
#' }
#' @seealso [print_performance()]
#' @examples
#' \dontrun{
#' if(interactive()){
#'
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
#'  x <- get_performance(rf.model)
#'  x
#'
#' }
#' }
#' @rdname get_performance
#' @export
get_performance <- function(x){

  if(inherits(x, "rf_repeat")){

    x.mean <- sapply(x$performance, FUN = mean)
    x.se <- sapply(x$performance, FUN = standard_error)
    x.sd <- sapply(x$performance, FUN = sd)

  } else {

    x.mean <- unlist(x$performance)
    x.se <- NA
    x.sd <- NA

  }

  out.df <- data.frame(
    metric = names(x.mean),
    mean = x.mean,
    standard_error = x.se,
    standard_deviation = round(x.sd, 3)
  )

  rownames(out.df) <- NULL

  out.df

}
