#' @title Create multiple spatially independent training and testing folds
#' @description Applies [make_spatial_fold()] to every row in `xy.selected`, generating one spatially independent fold centered on each focal point. Used for spatial cross-validation in [rf_evaluate()].
#' @param data Data frame containing response variable and predictors. Required only for binary response variables.
#' @param dependent.variable.name Character string with the name of the response variable. Must be a column name in `data`. Required only for binary response variables.
#' @param xy.selected Data frame with columns "x" (longitude), "y" (latitude), and "id" (record identifier). Defines the focal points for fold creation. Typically a spatially thinned subset of `xy` created with [thinning()] or [thinning_til_n()].
#' @param xy Data frame with columns "x" (longitude), "y" (latitude), and "id" (record identifier). Contains all spatial coordinates for the dataset.
#' @param distance.step.x Numeric value specifying the buffer growth increment along the x-axis. Default: `NULL` (automatically set to 1/1000th of the x-coordinate range).
#' @param distance.step.y Numeric value specifying the buffer growth increment along the y-axis. Default: `NULL` (automatically set to 1/1000th of the y-coordinate range).
#' @param training.fraction Numeric value between 0.1 and 0.9 specifying the fraction of records to include in the training fold. Default: `0.75`.
#' @return List where each element corresponds to a row in `xy.selected` and contains:
#' \itemize{
#'   \item `training`: Integer vector of record IDs (from `xy$id`) in the training fold.
#'   \item `testing`: Integer vector of record IDs (from `xy$id`) in the testing fold.
#' }
#' @details
#' This function creates multiple spatially independent folds for spatial cross-validation by calling [make_spatial_fold()] once for each row in `xy.selected`. Each fold is created by growing a rectangular buffer from the corresponding focal point until the desired `training.fraction` is achieved.
#'
#' **Parallel execution:**
#'
#' The function uses the future ecosystem for parallelization. Control the parallel strategy externally via `future::plan()`:
#' \itemize{
#'   \item `future::plan(future::sequential)` - No parallelization (default)
#'   \item `future::plan(future::multisession, workers = 4)` - Parallel execution with 4 workers
#'   \item `future::plan(future::multicore, workers = 4)` - Fork-based (Unix/Mac only)
#' }
#'
#' **Progress reporting:**
#'
#' This function supports progress bars via the progressr package. To enable progress reporting, set handlers before calling the function:
#' \itemize{
#'   \item `progressr::handlers(global = TRUE)` - Enable progress bars for all functions
#'   \item `progressr::handlers("progress")` - Use a specific handler type
#' }
#'
#' **Typical workflow:**
#' \enumerate{
#'   \item Thin spatial points with [thinning()] or [thinning_til_n()] to create `xy.selected`
#'   \item Optionally configure parallel plan with `future::plan()`
#'   \item Optionally enable progress bars with `progressr::handlers(global = TRUE)`
#'   \item Create spatial folds with this function
#'   \item Use the folds for spatial cross-validation in [rf_evaluate()]
#' }
#' @seealso [make_spatial_fold()], [rf_evaluate()], [thinning()], [thinning_til_n()]
#' @examples
#' data(plants_df, plants_xy)
#'
#' # Thin to 10 focal points to speed up example
#' xy.thin <- thinning_til_n(
#'   xy = plants_xy,
#'   n = 10
#' )
#'
#' # Basic usage - sequential execution
#' folds <- make_spatial_folds(
#'   xy.selected = xy.thin,
#'   xy = plants_xy,
#'   distance.step.x = 0.05,
#'   training.fraction = 0.6
#' )
#'
#' # Each element is a fold with training and testing indices
#' length(folds)  # 10 folds
#' names(folds[[1]])  # "training" and "testing"
#'
#' \donttest{
#' # With progress bars and parallel execution
#' library(future)
#' library(progressr)
#'
#' # Enable progress reporting
#' progressr::handlers(global = TRUE)
#'
#' # Set parallel strategy
#' future::plan(future::multisession, workers = 2)
#'
#' # Create folds with progress bars
#' folds_parallel <- make_spatial_folds(
#'   xy.selected = xy.thin,
#'   xy = plants_xy,
#'   distance.step.x = 0.05,
#'   training.fraction = 0.6
#' )
#'
#' # Reset to sequential
#' future::plan(future::sequential)
#' }
#'
#' # Visualize first fold (training = red, testing = blue, center = black)
#' if (interactive()) {
#'   plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
#'   points(plants_xy[folds[[1]]$training, c("x", "y")], col = "red4", pch = 15)
#'   points(plants_xy[folds[[1]]$testing, c("x", "y")], col = "blue4", pch = 15)
#'   points(
#'     plants_xy[folds[[1]]$training[1], c("x", "y")],
#'     col = "black",
#'     pch = 15,
#'     cex = 2
#'   )
#' }
#'
#' @rdname make_spatial_folds
#' @family preprocessing
#' @export
#' @autoglobal
make_spatial_folds <- function(
  data = NULL,
  dependent.variable.name = NULL,
  xy.selected = NULL,
  xy = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  training.fraction = 0.75
) {
  # Detect user's plan
  plan_workers <- future::nbrOfWorkers()

  # Create progressor outside
  p <- progressr::progressor(along = seq_len(nrow(xy.selected)))

  # Parallel execution with progress
  spatial.folds <- future.apply::future_lapply(
    X = seq_len(nrow(xy.selected)),
    FUN = function(i) {
      spatial.fold.i <- spatialRF::make_spatial_fold(
        data = data,
        dependent.variable.name = dependent.variable.name,
        xy.i = xy.selected[i, ],
        xy = xy,
        distance.step.x = distance.step.x,
        distance.step.y = distance.step.y,
        training.fraction = training.fraction
      )

      # Signal progress
      p()

      return(spatial.fold.i)
    },
    future.packages = "spatialRF"
  )

  spatial.folds
}
