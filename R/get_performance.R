#' @title get_performance()
#' @description Returns the performance slot of an [rf()], [rf_repeat()], or [rf_spatial()] model.
#' @param x Model fitted by [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A data frame with four columns:
#' \itemize{
#'   \item *metric* Name of the performance metric.
#'   \item *mean* Value of the performance metric. Only really a mean if the model is fitted with [rf_repeat()].
#'   \item *standard_error* Standard error of the mean, only if the model is fitted with [rf_repeat()], and `NA` otherwise.
#'   \item *standard_deviation* Standard deviation of the mean, only if the model is fitted with [rf_repeat()], and `NA` otherwise.
#' }
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
#'  x <- get_performance(rf.model)
#'  }
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

  out.df

}
