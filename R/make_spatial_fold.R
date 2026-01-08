#' @title Create spatially independent training and testing folds
#' @description Generates two spatially independent data folds by growing a rectangular buffer from a focal point until a specified fraction of records falls inside. Used internally by [make_spatial_folds()] and [rf_evaluate()] for spatial cross-validation.
#' @param data Data frame containing response variable and predictors. Required only for binary response variables.
#' @param dependent.variable.name Character string with the name of the response variable. Must be a column name in `data`. Required only for binary response variables.
#' @param xy.i Single-row data frame with columns "x" (longitude), "y" (latitude), and "id" (record identifier). Defines the focal point from which the buffer grows.
#' @param xy Data frame with columns "x" (longitude), "y" (latitude), and "id" (record identifier). Contains all spatial coordinates for the dataset.
#' @param distance.step Numeric value specifying the buffer growth increment. Applied to the longer axis of the coordinate bounding box; shorter axis step is calculated proportionally to maintain aspect ratio. Default: `NULL` (automatically set to 1/1000th of range independently per axis). Replaces deprecated distance.step.x/distance.step.y arguments.
#' @param distance.step.x `r lifecycle::badge("deprecated")` Use distance.step instead.
#' @param distance.step.y `r lifecycle::badge("deprecated")` Use distance.step instead.
#' @param training.fraction Numeric value between 0.1 and 0.9 specifying the fraction of records to include in the training fold. Default: `0.8`.
#' @return Logical vector with length equal to `nrow(xy)`, where `TRUE` indicates a record is in the training fold and `FALSE` indicates it is in the testing fold. The vector is ordered by row position in `xy`.
#' @details
#' This function creates spatially independent training and testing folds for spatial cross-validation. The algorithm works as follows:
#' \enumerate{
#'   \item Starts with a small rectangular buffer centered on the focal point (`xy.i`)
#'   \item Grows the buffer incrementally by `distance.step.x` and `distance.step.y`
#'   \item Continues growing until the buffer contains the desired number of records (`training.fraction * total records`)
#'   \item Assigns records inside the buffer to training and records outside to testing
#' }
#'
#' **Special handling for binary response variables:**
#'
#' When `data` and `dependent.variable.name` are provided and the response is binary (0/1), the function ensures that `training.fraction` applies to the number of presences (1s), not total records. This prevents imbalanced sampling in presence-absence models.
#' @seealso [make_spatial_folds()], [rf_evaluate()], [is_binary()]
#' @examples
#' data(plants_df, plants_xy)
#'
#' # Create spatial fold centered on first coordinate
#' fold <- make_spatial_fold(
#'   xy.i = plants_xy[1, ],
#'   xy = plants_xy,
#'   training.fraction = 0.6
#' )
#'
#' # View fold (TRUE = training, FALSE = testing)
#' head(fold)
#' sum(fold)  # Number of training records
#'
#' # Visualize the spatial split (training = red, testing = blue, center = black)
#' if (interactive()) {
#'   plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
#'   points(plants_xy[fold, c("x", "y")], col = "red4", pch = 15)
#'   points(plants_xy[!fold, c("x", "y")], col = "blue4", pch = 15)
#'   points(plants_xy[1, c("x", "y")], col = "black", pch = 15, cex = 2)
#' }
#'
#' @rdname make_spatial_fold
#' @family preprocessing
#' @export
#' @autoglobal
make_spatial_fold <- function(
  data = NULL,
  dependent.variable.name = NULL,
  xy.i = NULL,
  xy = NULL,
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  training.fraction = 0.8
) {
  if (sum(c("id", "x", "y") %in% colnames(xy.i)) != 3) {
    stop("xy.i must contain the column names 'id', 'x', and 'y'.")
  }
  if (sum(c("id", "x", "y") %in% colnames(xy)) != 3) {
    stop("xy must contain the column names 'id', 'x', and 'y'.")
  }
  if (training.fraction >= 1) {
    stop("training.fraction should be a number between 0.1 and 0.9")
  }

  # Error if both old and new provided
  if (!is.null(distance.step) && (!is.null(distance.step.x) || !is.null(distance.step.y))) {
    stop(
      "Cannot specify both 'distance.step' and 'distance.step.x'/'distance.step.y'. ",
      "Use 'distance.step' for automatic calculation.",
      call. = FALSE
    )
  }

  # Lifecycle deprecation warnings for old arguments
  if (!is.null(distance.step.x)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "make_spatial_fold(distance.step.x)",
      with = "make_spatial_fold(distance.step)",
      details = "The distance.step argument automatically calculates proportional step sizes based on coordinate bounding box aspect ratio."
    )
  }

  if (!is.null(distance.step.y)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "make_spatial_fold(distance.step.y)",
      with = "make_spatial_fold(distance.step)",
      details = "The distance.step argument automatically calculates proportional step sizes based on coordinate bounding box aspect ratio."
    )
  }

  # Handle new distance.step argument with aspect ratio calculation
  if (!is.null(distance.step)) {

    # Calculate bounding box ranges
    x_range <- max(xy$x) - min(xy$x)
    y_range <- max(xy$y) - min(xy$y)

    # Longer axis gets distance.step value, shorter gets proportional reduction
    if (x_range >= y_range) {
      # X-axis is longer (or equal)
      distance.step.x <- distance.step
      distance.step.y <- distance.step * (y_range / x_range)
    } else {
      # Y-axis is longer
      distance.step.y <- distance.step
      distance.step.x <- distance.step * (x_range / y_range)
    }

    # Convert vector to scalar if needed (handles raster::res() output)
    if (length(distance.step.x) > 1) {
      distance.step.x <- distance.step.x[1]
    }
    if (length(distance.step.y) > 1) {
      distance.step.y <- distance.step.y[1]
    }
  }

  #initiating distance.step.x
  if (is.null(distance.step.x)) {
    #range of x coordinates
    x.range <- range(xy$x)

    #getting the 1%
    distance.step.x <- (max(x.range) - min(x.range)) / 1000
  } else {
    #in case it comes from raster::res()
    if (length(distance.step.x) > 1) {
      distance.step.x <- distance.step.x[1]
    }
  }

  #initiating distance.step.x
  if (is.null(distance.step.y)) {
    #range of x coordinates
    y.range <- range(xy$y)

    #getting the 1%
    distance.step.y <- (max(y.range) - min(y.range)) / 1000
  } else {
    #in case it comes from raster::res()
    if (length(distance.step.y) > 1) {
      distance.step.y <- distance.step.y[1]
    }
  }

  #getting details of xy.i
  xy.i.x <- xy.i[1, "x"]
  xy.i.y <- xy.i[1, "y"]

  #finding out if data is binary
  is.binary <- FALSE
  if (!is.null(data) && !is.null(dependent.variable.name)) {
    is.binary <- is_binary(
      data = data,
      dependent.variable.name = dependent.variable.name
    )
  }

  #number of records to select
  if (is.binary) {
    records.to.select <- floor(
      training.fraction * sum(data[, dependent.variable.name])
    )
  } else {
    records.to.select <- floor(training.fraction * nrow(xy))
  }

  #generating first buffer
  old.buffer.x.min <- xy.i.x - distance.step.x
  old.buffer.x.max <- xy.i.x + distance.step.x
  old.buffer.y.min <- xy.i.y - distance.step.y
  old.buffer.y.max <- xy.i.y + distance.step.y

  #track count without materializing dataframe (memory optimization)
  records_count <- 0

  #growing buffer
  while (records_count < records.to.select) {
    #new buffer
    new.buffer.x.min <- old.buffer.x.min - distance.step.x
    new.buffer.x.max <- old.buffer.x.max + distance.step.x
    new.buffer.y.min <- old.buffer.y.min - distance.step.y
    new.buffer.y.max <- old.buffer.y.max + distance.step.y

    #find indices in buffer (single pass)
    buffer_idx <- which(
      xy$x >= new.buffer.x.min & xy$x <= new.buffer.x.max &
      xy$y >= new.buffer.y.min & xy$y <= new.buffer.y.max
    )

    #if binary, count presences only
    if (is.binary) {
      #data and xy are row-aligned, use indices directly
      records_count <- sum(data[buffer_idx, dependent.variable.name] == 1)
    } else {
      records_count <- length(buffer_idx)
    }

    #resetting old.buffer
    old.buffer.x.min <- new.buffer.x.min
    old.buffer.x.max <- new.buffer.x.max
    old.buffer.y.min <- new.buffer.y.min
    old.buffer.y.max <- new.buffer.y.max
  }

  #for binary, filter buffer_idx to only include presences (1s)
  if (is.binary) {
    buffer_idx <- buffer_idx[
      data[buffer_idx, dependent.variable.name] == 1
    ]
  }

  #create logical vector (TRUE = training, FALSE = testing)
  fold_logical <- rep(FALSE, nrow(xy))
  fold_logical[buffer_idx] <- TRUE

  fold_logical
}
