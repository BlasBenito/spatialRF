#' @title Makes training and testing spatial folds
#' @description Applies [make_spatial_fold()] to every record in a data frame `xy.selected` to generate as many spatially independent folds over the dataset `xy` as rows are in `xy.selected`.
#' @param xy.selected Data frame with at least three columns: "x" (longitude), "y" (latitude), and "id" (integer, id of the record). Usually a subset of `xy`. Usually the result of applying [thinning()] or [thinning_til_n()] to 'xy' Default: `NULL`.
#' @param xy data frame with at least three columns: "x" (longitude), "y" (latitude), and "id" (integer, index of the record). Default: `NULL`.
#' @param distance.step numeric, distance the buffer around xy.i is grown on each iteration. If NULL, it defaults to the minimum distance between points in `xy` divided by 2. Default: `NULL`.
#' @param training.fraction numeric, fraction of the data to be included in the growing buffer as training data, Default: 0.6
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
  xy.selected = NULL,
  xy = NULL,
  distance.step = NULL,
  training.fraction = 0.6,
  n.cores = NULL
){

  #prepare cluster for parallelized loop
  if(is.null(n.cores)){n.cores <- parallel::detectCores() - 1}
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
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))
  parallel::clusterExport(cl = temp.cluster, "make_spatial_fold")

  #parallelized loop
  i <- NULL #define i to avoid complains from devtools::check()
  spatial.folds <- foreach::foreach(
    i = 1:nrow(xy.selected)
  ) %dopar% {

    spatial.fold.i <- make_spatial_fold(
      xy.i = xy.selected[i, ],
      xy = xy,
      distance.step = distance.step,
      training.fraction = training.fraction
    )

    return(spatial.fold.i)

  }

  spatial.folds

}
