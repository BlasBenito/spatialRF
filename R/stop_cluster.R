#' Stops clusters
#'
#' @description Stops and removes any object with the class "cluster" in the global environment by default, or a given named cluster when the argument `cluster` is provided.
#'
#' @param cluster a cluster created with `parallel::makeCluster()` or `spatialRF::make_cluster()`. Default: `NULL`
#'
#' @return Nothing
#' @export
#'
#' @examples
#'
#' #removing one given cluster
#' cluster <- spatialRF::make_cluster(cluster.cores = 1)
#' spatialRF::stop_cluster(cluster = cluster)
#'
#' #removing several clusters
#' cl1 <- spatialRF::make_cluster(cluster.cores = 1)
#' cl2 <- spatialRF::make_cluster(cluster.cores = 1)
#' spatialRF::stop_cluster()
#'
stop_cluster <- function(cluster = NULL){

  #if cluster is null, stop all clusters
  if(is.null(cluster)){

    #identifying cluster objects
    clusters <- Filter(
      function(x) "cluster" %in% class(get(x)),
      ls(envir = .GlobalEnv)
    )

    #unregistering registered clusters
    if(exists(".foreachGlobals")){
     rm(list = ls(name = .foreachGlobals), pos = .foreachGlobals)
    } else {
      .foreachGlobals <- NULL
    }

    #closing all connections
    closeAllConnections()

    #removing all clusters
    rm(list = clusters, envir = .GlobalEnv)

  } else {

    parallel::stopCluster(cl = cluster)
    rm(
      list = deparse(substitute(cluster)),
      pos = .GlobalEnv
      )

  }

}
