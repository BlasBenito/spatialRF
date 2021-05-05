#' @title Gets out-of-bag performance scores from a model
#' @description Returns the performance slot of an [rf()], [rf_repeat()], or [rf_spatial()] model computed on the out-of-bag data.
#' @param model Model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @return A data frame with four columns:
#' \itemize{
#'   \item `metric` Name of the performance metric.
#'   \item `median` Value of the performance metric. Truly a median only if the model is fitted with [rf_repeat()].
#'   \item `median_absolute_deviation` median absolute deviation (MAD), only if the model is fitted with [rf_repeat()], and `NA` otherwise.
#' }
#' @seealso [print_performance()]
#' @examples
#' \donttest{
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
get_performance <- function(model){

  if(inherits(model, "rf_repeat")){

    x.median <- sapply(model$performance, FUN = median)
    x.mad <- sapply(model$performance, FUN = mad)

  } else {

    x.median <- unlist(model$performance)
    x.mad <- NA

  }

  out.df <- data.frame(
    metric = names(x.median),
    median = x.median,
    median_absolute_deviation = x.mad
  )

  if(inherits(model , "rf_repeat") == FALSE){
    colnames(out.df)[2] <- "value"
  }

  rownames(out.df) <- NULL

  out.df <- out.df[,colSums(is.na(out.df)) < nrow(out.df)]

  out.df <- na.omit(out.df)

  out.df

}
