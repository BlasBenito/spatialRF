#' @title Create spatially independent training and testing folds
#' @description Generates two spatially independent data folds by growing a rectangular buffer from a focal point until a specified fraction of records falls inside. Used internally by [make_spatial_folds()] and [rf_evaluate()] for spatial cross-validation.
#' @param data Data frame containing response variable and predictors. Required only for binary response variables.
#' @param dependent.variable.name Character string with the name of the response variable. Must be a column name in `data`. Required only for binary response variables.
#' @param xy.i Single-row data frame with columns "x" (longitude), "y" (latitude), and "id" (record identifier). Defines the focal point from which the buffer grows.
#' @param xy Data frame with columns "x" (longitude), "y" (latitude), and "id" (record identifier). Contains all spatial coordinates for the dataset.
#' @param distance.step.x Numeric value specifying the buffer growth increment along the x-axis. Default: `NULL` (automatically set to 1/1000th of the x-coordinate range).
#' @param distance.step.y Numeric value specifying the buffer growth increment along the y-axis. Default: `NULL` (automatically set to 1/1000th of the y-coordinate range).
#' @param training.fraction Numeric value between 0.1 and 0.9 specifying the fraction of records to include in the training fold. Default: `0.8`.
#' @return List with two elements:
#' \itemize{
#'   \item `training`: Integer vector of record IDs (from `xy$id`) in the training fold.
#'   \item `testing`: Integer vector of record IDs (from `xy$id`) in the testing fold.
#' }
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
#' # View training and testing record IDs
#' fold$training
#' fold$testing
#'
#' # Visualize the spatial split (training = red, testing = blue, center = black)
#' if (interactive()) {
#'   plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
#'   points(plants_xy[fold$training, c("x", "y")], col = "red4", pch = 15)
#'   points(plants_xy[fold$testing, c("x", "y")], col = "blue4", pch = 15)
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

  #select first batch of presences
  records.selected <- xy[
    xy$x >= old.buffer.x.min &
      xy$x <= old.buffer.x.max &
      xy$y >= old.buffer.y.min &
      xy$y <= old.buffer.y.max,
  ]

  #growing buffer
  while (nrow(records.selected) < records.to.select) {
    #new buffer
    new.buffer.x.min <- old.buffer.x.min - distance.step.x
    new.buffer.x.max <- old.buffer.x.max + distance.step.x
    new.buffer.y.min <- old.buffer.y.min - distance.step.y
    new.buffer.y.max <- old.buffer.y.max + distance.step.y

    #number of selected presences
    records.selected <- xy[
      xy$x >= new.buffer.x.min &
        xy$x <= new.buffer.x.max &
        xy$y >= new.buffer.y.min &
        xy$y <= new.buffer.y.max,
    ]

    #subset ones if it's binary
    if (is.binary) {
      records.selected <- records.selected[
        data[data$id %in% records.selected$id, dependent.variable.name] == 1,
      ]
    }

    #resetting old.buffer
    old.buffer.x.min <- new.buffer.x.min
    old.buffer.x.max <- new.buffer.x.max
    old.buffer.y.min <- new.buffer.y.min
    old.buffer.y.max <- new.buffer.y.max
  }

  #select from xy.all if response is binary
  #selecting ones if binary
  if (is.binary) {
    records.selected <- xy[
      xy$x >= new.buffer.x.min &
        xy$x <= new.buffer.x.max &
        xy$y >= new.buffer.y.min &
        xy$y <= new.buffer.y.max,
    ]
  }

  #out list
  out.list <- list()
  out.list$training <- records.selected$id
  out.list$testing <- setdiff(xy$id, records.selected$id)

  out.list
}
