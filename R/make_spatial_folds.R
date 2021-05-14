#' @title Makes training and testing spatial folds
#' @description Applies [make_spatial_fold()] to every record in a data frame `xy.selected` to generate as many spatially independent folds over the dataset `xy` as rows are in `xy.selected`.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param xy.selected Data frame with at least three columns: "x" (longitude), "y" (latitude), and "id" (integer, id of the record). Usually a subset of `xy`. Usually the result of applying [thinning()] or [thinning_til_n()] to 'xy' Default: `NULL`.
#' @param xy data frame with at least three columns: "x" (longitude), "y" (latitude), and "id" (integer, index of the record). Default: `NULL`.
#' @param distance.step.x Numeric, distance step used during the growth in the x axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the x coordinates).
#' @param distance.step.y Numeric, distance step used during the growth in the y axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the y coordinates).
#' @param training.fraction numeric, fraction of the data to be included in the growing buffer as training data, Default: `0.8`
#' @param n.cores number of cores to use to generate spatial folds in parallel. Default: `NULL`.
#' @return A list with as many slots as rows are in `xy.selected`. Each slot has two slots named `training` and `testing`, with the former having the indices of the training records selected from xy, and the latter having the indices of the testing records.
#' @seealso [make_spatial_fold()], [rf_evaluate()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(plant_richness_df)
#'
#'  xy <- plant_richness_df[, 1:3]
#'  colnames(xy) <- c("id", "x", "y")
#'  xy.selected <- thinning_til_n(
#'    xy = xy,
#'    n = 20
#'    )
#'
#'  out <- make_spatial_folds(
#'    xy.selected = xy.selected,
#'    xy = xy,
#'    distance.step = 0.05, #degrees
#'    training.fraction = 0.6,
#'    n.cores = 1
#'  )
#'
#'  length(out)
#'
#'  plot(xy[ c("x", "y")], type = "n", xlab = "", ylab = "")
#'  #plots training points
#'  points(xy[out[[10]]$training, c("x", "y")], col = "red4", pch = 15)
#'  #plots testing points
#'  points(xy[out[[10]]$testing, c("x", "y")], col = "blue4", pch = 15)
#'  #plots xy.i
#'  points(xy[10, c("x", "y")], col = "black", pch = 15, cex = 2)
#' }
#' }
#' @rdname make_spatial_folds
#' @importFrom foreach %dopar%
#' @export
make_spatial_folds <- function(
  data = NULL,
  dependent.variable.name = NULL,
  xy.selected = NULL,
  xy = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  training.fraction = 0.8,
  n.cores = NULL
){

  #setup of parallel execution
  if(is.null(n.cores)){

    n.cores <- parallel::detectCores() - 1
    `%dopar%` <- foreach::`%dopar%`

  } else {

    #only one core, no cluster
    if(n.cores == 1){

      #replaces dopar (parallel) by do (serial)
      `%dopar%` <- foreach::`%do%`
      on.exit(`%dopar%` <- foreach::`%dopar%`)

    } else {

      `%dopar%` <- foreach::`%dopar%`

    }

  }

  if(.Platform$OS.type == "windows"){
    temp.cluster <- parallel::makeCluster(
      n.cores,
      type = "PSOCK"
    )
  } else {
    temp.cluster <- parallel::makeCluster(
      n.cores,
      type = "FORK"
    )
  }

  #register cluster and close on exit
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  #parallelized loop
  i <- NULL
  spatial.folds <- foreach::foreach(
    i = seq(1, nrow(xy.selected), by = 1)
  ) %dopar% {

    spatial.fold.i <- spatialRF::make_spatial_fold(
      data = data,
      dependent.variable.name = dependent.variable.name,
      xy.i = xy.selected[i, ],
      xy = xy,
      distance.step.x = distance.step.x,
      distance.step.y = distance.step.y,
      training.fraction = training.fraction
    )

    return(spatial.fold.i)

  }

  spatial.folds

}
