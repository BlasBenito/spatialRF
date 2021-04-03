#' @title Makes one training and one testing spatial folds
#' @description Used internally by [make_spatial_folds()] and [rf_evaluate()]. Uses the coordinates of a point `xy.i` to generate two spatially independent data folds from the data frame `xy`. It does so by growing a rectangular buffer from `xy.i` until a number of records defined by `training.fraction` is inside the buffer. The indices of these records are then stored as "training" in the output list. The indices of the remaining records outside of the buffer are stored as "testing". These training and testing records can be then used to evaluate a model on independent data via cross-validation.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param xy.i One row data frame with at least three columns: "x" (longitude), "y" (latitude), and "id" (integer, id of the record). Can be a row of `xy`. Default: `NULL`.
#' @param xy A data frame with at least three columns: "x" (longitude), "y" (latitude), and "id" (integer, index of the record). Default: `NULL`.
#' @param distance.step Numeric, distance step used during the thinning iterations. If `NULL`, the maximum distance between two points in `xy` divided by 1000 is used. Default: `NULL`
#' @param training.fraction Numeric, fraction of the data to be included in the training fold, Default: `0.6`.
#' @return A list with two slots named `training` and `testing` with the former having the indices of the training records selected from `xy`, and the latter having the indices of the testing records.
#' @seealso [make_spatial_folds()], [rf_evaluate()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(plant_richness_df)
#'
#'  xy <- plant_richness_df[, 1:3]
#'  colnames(xy) <- c("id", "x", "y")
#'  out <- make_spatial_fold(
#'    xy.i = xy[1, ],
#'    xy = xy,
#'    distance.step = 0.05, #degrees
#'    training.fraction = 0.6
#'  )
#'
#'  out$training
#'  out$testing
#'
#'  plot(xy[ c("x", "y")], type = "n", xlab = "", ylab = "")
#'  #plots training points
#'  points(xy[out$training, c("x", "y")], col = "red4", pch = 15)
#'  #plots testing points
#'  points(xy[out$testing, c("x", "y")], col = "blue4", pch = 15)
#'  #plots xy.i
#'  points(xy[1, c("x", "y")], col = "black", pch = 15, cex = 2)
#'
#' }
#' }
#' @rdname make_spatial_fold
#' @export
make_spatial_fold <- function(
  data = NULL,
  dependent.variable.name = NULL,
  xy.i = NULL,
  xy = NULL,
  distance.step = NULL,
  training.fraction = 0.6
){

  if(sum(c("id", "x", "y") %in% colnames(xy.i)) != 3){
    stop("xy.i must contain the column names 'id', 'x', and 'y'.")
  }
  if(sum(c("id", "x", "y") %in% colnames(xy)) != 3){
    stop("xy must contain the column names 'id', 'x', and 'y'.")
  }
  if(training.fraction >= 1){
    stop("training.fraction should be a number between 0.1 and 0.9")
  }
  #initiating distances
  if(is.null(distance.step)){

    #getting all distances among points
    xy.distances <- sort(as.vector(dist(xy[, c("x", "y")])))

    #remove zero distances
    xy.distances <- xy.distances[xy.distances != 0]

    #getting the minimum
    # distance.step <- min(xy.distances)

    #getting the 1%
    distance.step <- max(xy.distances) / 1000

    rm(xy.distances)
  }

  #getting details of xy.i
  xy.i.x <- xy.i[1, "x"]
  xy.i.y <- xy.i[1, "y"]

  #finding out if data is binary
  if(!is.null(data) & !is.null(dependent.variable.name)){
    is.binary <- is_binary(
      data = data,
      dependent.variable.name = dependent.variable.name
    )
  }

  #number of records to select
  records.to.select <- floor(training.fraction * nrow(xy))

  #generating first buffer
  old.buffer.x.min <- xy.i.x - distance.step
  old.buffer.x.max <- xy.i.x + distance.step
  old.buffer.y.min <- xy.i.y - distance.step
  old.buffer.y.max <- xy.i.y + distance.step

  #select first batch of presences
  records.selected <- xy[
    xy$x >= old.buffer.x.min &
    xy$x <= old.buffer.x.max &
    xy$y >= old.buffer.y.min &
    xy$y <= old.buffer.y.max, ]

  #growing buffer
  while(nrow(records.selected) < records.to.select){

    #new buffer
    new.buffer.x.min <- old.buffer.x.min - distance.step
    new.buffer.x.max <- old.buffer.x.max + distance.step
    new.buffer.y.min <- old.buffer.y.min - distance.step
    new.buffer.y.max <- old.buffer.y.max + distance.step

    #number of selected presences
    records.selected <- xy[
      xy$x >= new.buffer.x.min &
      xy$x <= new.buffer.x.max &
      xy$y >= new.buffer.y.min &
      xy$y <= new.buffer.y.max, ]

    #resetting old.buffer
    old.buffer.x.min <- new.buffer.x.min
    old.buffer.x.max <- new.buffer.x.max
    old.buffer.y.min <- new.buffer.y.min
    old.buffer.y.max <- new.buffer.y.max

  }

  #select from xy.all if response is binary
  #selecting ones if binary
  if(is.binary == TRUE){
    xy <- xy.all
    records.selected <- xy[
      xy$x >= new.buffer.x.min &
        xy$x <= new.buffer.x.max &
        xy$y >= new.buffer.y.min &
        xy$y <= new.buffer.y.max, ]
  }

  #out list
  out.list <- list()
  out.list$training <- records.selected$id
  out.list$testing <- setdiff(xy$id, records.selected$id)

  out.list

}
