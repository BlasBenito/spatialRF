#' @title Extract out-of-bag performance metrics from model
#' @description Extracts out-of-bag (OOB) performance metrics from models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model Model object from [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Data frame with performance metrics:
#' \itemize{
#'   \item For [rf()] and [rf_spatial()]: columns `metric` and `value`
#'   \item For [rf_repeat()]: columns `metric`, `median`, and `median_absolute_deviation` (MAD across repetitions)
#' }
#' @details
#' Out-of-bag (OOB) performance is computed using observations not included in bootstrap samples during model training. Metrics typically include R-squared, pseudo R-squared, RMSE, and normalized RMSE. For repeated models, the median and median absolute deviation summarize performance across repetitions.
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [print_performance()]
#' @examples
#' data(plants_rf)
#'
#' # Extract OOB performance metrics
#' performance <- get_performance(plants_rf)
#' performance
#'
#' # For repeated models, median and MAD are returned
#' # (example would require rf_repeat model)
#'
#' @rdname get_performance
#' @family model_info
#' @export
#' @autoglobal
get_performance <- function(model) {
  if (inherits(model, "rf_repeat")) {
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

  if (!inherits(model, "rf_repeat")) {
    colnames(out.df)[2] <- "value"
  }

  rownames(out.df) <- NULL

  out.df <- out.df[, colSums(is.na(out.df)) < nrow(out.df)]

  out.df <- stats::na.omit(out.df)

  out.df
}
