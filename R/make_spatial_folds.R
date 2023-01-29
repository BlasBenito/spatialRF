#' @title Makes training and testing spatial folds for spatial cross-validation
#' @description
#'
#' This function generates a list of lists. Each nested list contains two numeric vectors named "training" and "testing". The training vector contains the indices of a set of contiguous cases in `data` to be used as training set during spatial-cross validation. The testing vector contains the cases to be used as testing set.
#'
#' The training set is selected as follows:
#' \itemize{
#'   \item 1. The first row of the data frame `fold.centers` is selected. This data frame is the result of applying [thinning_til_n()] to `data`. The coordinates in this row represent the "training fold center".
#'   \item 2. A rectangular buffer is grown from the training fold center one step at a time until it encloses a number of records in `data` as close as possible to the `training.fraction`. The step wise growth of this buffer is controlled by the argument `distance.step`.
#'   \item 3. The indices of all records in `data` within the buffer are written to the "training" vector of the nested list.
#'   \item 4. The indices of the remaining records are written to the "testing" list.
#' }
#'
#' This method ensures that the training set is geographically contiguous, and has a spatial structure that is as representative as possible of the overall spatial structure of the full data set.
#'
#' @param xy (required; data frame) data frame with columns "x" (longitude) and "y" (latitude). Default: `NULL`.
#' @param fold.centers (required; data frame) Subset of `xy` usually obtained via [thinning()] or [thinning_til_n()]. Default: `NULL`.
#' @param fold.center (required; data frame) One row of `fold.centers`.  Default: `NULL`
#' @param training.fraction (optional, numeric) Training fraction. Numeric between 0.1 and  0.9. When the response variable is binary with values zero and one, this fraction refers to the number of ones to be used in the training set. Notice that in most cases the algorithm will return a number of training cases as approximate as possible to this fraction. Default: `0.75`
#' @param data (optional, data frame). Training data frame. Default: `NULL`
#' @param response.name (optional; character string). Name of the response variable in `data`. Used to check if the response is binary with values 0 and 1. Default: `NULL`
#' @param distance.step (optional; numeric) Numeric vector of length one or two. Distance step used during the growth of the buffer containing the training cases. Must be in the same units as the coordinates in `xy`. When only one distance is provided, the same growth is applied to the x and y axes. If two distances are provided, the first one is applied to the x axis, and the second one to the y. When `NULL`, it uses 1/1000th of the range of each axis as distance. The smaller this number is, the easier is to achieve an accurate `training.fraction`, but the slower the algorithm becomes. Default: `NULL`
#' @param swap.spatial.folds (optional; logical) If true, the cases inside the rectangular buffer are used as testing set instead of training. This can help in edge cases when the data distribution is highly irregular. Default: `FALSE`
#' @param n.cores (optional; integer) Number of cores used for parallel execution. Default: `NULL`
#' @param cluster (optional; cluster) A cluster definition generated with `parallel::makeCluster()` or \code{\link{start_cluster}}. If provided, overrides `n.cores`. The function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` at the end of your pipeline. Default: `parallel::detectCores() - 1`
#'
#' @return A list with as many nested lists as rows are in `fold.centers`. Each nested list has two slots named `training` and `testing`, with the former having the indices of the training records selected from xy, and the latter having the indices of the testing records.
#' @seealso [make_spatial_fold()], [rf_evaluate()]
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#'  data(ecoregions_df)
#'
#'  #getting case coordinates
#'  xy <- ecoregions_df[, c("x", "y")]
#'
#'  #thining til 20 cases
#'  fold.centers <- thinning_til_n(
#'    xy = xy,
#'    n = 20
#'    )
#'
#'  #making spatial folds centered on these 20 cases
#'  out <- make_spatial_folds(
#'    fold.centers = fold.centers,
#'    xy = xy,
#'    distance.step = 0.05, #degrees
#'    training.fraction = 0.6,
#'    n.cores = 1
#'  )
#'
#'  #plotting training and testing folds
#'  plot(xy[ c("x", "y")], type = "n", xlab = "", ylab = "")
#'  #plots training points
#'  points(xy[out[[10]]$training, c("x", "y")], col = "red4", pch = 15)
#'  #plots testing points
#'  points(xy[out[[10]]$testing, c("x", "y")], col = "blue4", pch = 15)
#'  #plots fold.center
#'  points(xy[10, c("x", "y")], col = "black", pch = 15, cex = 2)
#'
#' }
#' @rdname make_spatial_folds
#' @importFrom foreach %dopar%
#' @export
make_spatial_folds <- function(
  xy = NULL,
  fold.centers = NULL,
  training.fraction = 0.75,
  data = NULL,
  response.name = NULL,
  distance.step = NULL,
  swap.spatial.folds = FALSE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
){

  #handling parallelization
  #handling parallelization
  if("cluster" %in% class(cluster)){

    #registering cluster
    doParallel::registerDoParallel(cl = cluster)

    #parallel iterator
    `%iterator%` <- foreach::`%dopar%`

    #restricting the number of cores
    n.cores <- 1

  } else {

    #sequential iterator
    `%iterator%` <- foreach::`%do%`

  }

  #registering cluster
  doParallel::registerDoParallel(cl = cluster)

  #parallelized loop
  i <- NULL
  spatial.folds <- foreach::foreach(
    i = seq_len(nrow(fold.centers))
  ) %iterator% {

    set.seed(i)

    spatial.fold.i <- spatialRF::make_spatial_fold(
      xy = xy,
      fold.center = fold.centers[i, ],
      training.fraction = training.fraction,
      data = data,
      response.name = response.name,
      distance.step = distance.step
    )

    return(spatial.fold.i)

  }

  spatial.folds

}

#' @rdname make_spatial_folds
#' @export
make_spatial_fold <- function(
    xy = NULL,
    fold.center = NULL,
    training.fraction = 0.75,
    data = NULL,
    response.name = NULL,
    distance.step = NULL,
    swap.spatial.folds = FALSE
){

  x <- NULL
  y <- NULL

  #adding id to xy
  xy$id <- 1:nrow(xy)

  if(training.fraction > 0.9){
    training.fraction <- 0.9
  }
  if(training.fraction < 0.1){
    training.fraction <- 0.1
  }

  if(swap.spatial.folds == TRUE){
    training.fraction <- 1 - training.fraction
  }

  #initiating distance.step
  if(is.null(distance.step)){

    #range of x coordinates
    x.range <- range(xy$x)

    #range fraction
    distance.step.x <- (max(x.range) - min(x.range)) / 1000

    #range of x coordinates
    y.range <- range(xy$y)

    #range fraction
    distance.step.y <- (max(y.range) - min(y.range)) / 1000

    distance.step <- c(
      distance.step.x,
      distance.step.y
    )

  } else {

    if(length(distance.step) == 1){

      distance.step <- c(
        distance.step,
        distance.step
      )

    }

  }
  names(distance.step) <- c("x", "y")

  #getting details of fold.center
  fold.center.x <- fold.center[[x]]

  fold.center.y <- fold.center[[y]]

  #finding out if data is binary
  is.binary <- FALSE
  if(!is.null(data) & !is.null(response.name)){
    is.binary <- is_binary_response(
      x = dplyr::pull(
        data,
        response.name
        )
    )
  }

  #number of records to select
  if(is.binary == TRUE){
    records.to.select <- floor(training.fraction * sum(data[, response.name]))
  } else {
    records.to.select <- floor(training.fraction * nrow(xy))
  }

  #generating first buffer
  old.buffer.x.min <- fold.center.x - distance.step[["x"]]
  old.buffer.x.max <- fold.center.x + distance.step[["x"]]
  old.buffer.y.min <- fold.center.y - distance.step[["y"]]
  old.buffer.y.max <- fold.center.y + distance.step[["y"]]

  #select first batch of presences
  records.selected <- xy[
    xy$x >= old.buffer.x.min &
      xy$x <= old.buffer.x.max &
      xy$y >= old.buffer.y.min &
      xy$y <= old.buffer.y.max, ]

  #growing buffer
  while(nrow(records.selected) < records.to.select){

    #new buffer
    new.buffer.x.min <- old.buffer.x.min - distance.step[["x"]]
    new.buffer.x.max <- old.buffer.x.max + distance.step[["x"]]
    new.buffer.y.min <- old.buffer.y.min - distance.step[["y"]]
    new.buffer.y.max <- old.buffer.y.max + distance.step[["y"]]

    #number of selected presences
    records.selected <- xy[
      xy$x >= new.buffer.x.min &
        xy$x <= new.buffer.x.max &
        xy$y >= new.buffer.y.min &
        xy$y <= new.buffer.y.max, ]

    #subset ones if it's binary
    if(is.binary == TRUE){
      records.selected <- records.selected[
        data[data$id %in% records.selected$id,
             response.name] == 1,
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
  if(is.binary == TRUE){
    records.selected <- xy[
      xy$x >= new.buffer.x.min &
        xy$x <= new.buffer.x.max &
        xy$y >= new.buffer.y.min &
        xy$y <= new.buffer.y.max, ]
  }

  #out list
  out.list <- list()
  if(swap.spatial.folds == TRUE){
    out.list$training <- setdiff(
      x = xy$id,
      y = records.selected$id
      )
    out.list$testing <- records.selected$id
  } else {
    out.list$training <- records.selected$id
    out.list$testing <- setdiff(
      x = xy$id,
      y = records.selected$id
      )
  }

  out.list

}

