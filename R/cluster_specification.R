#' @title cluster_specification
#' @description specifies a cluster for \link[parallel]{makeCluster}
#' @param cluster.ips character vector, IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores numeric integer vector, number of cores on each machine.
#' @param cluster.user character string, name of the user (should be the same throughout machines), Defaults to the current system user.
#' @return a list ready to be used as input for the "spec" argument of the function \link[parallel]{makeCluster}.
#' @details This function is used internally by several other functions of the package, but can be useful for a user using \link[foreach]{foreach} to parallelize loops in a cluster.
#' @examples
#'  cluster.spec <- cluster_specification(
#'    cluster.ips = c("10.0.1.40", "10.0.1.41"),
#'    cluster.cores = c(6, 4),
#'    cluster.user = Sys.info()[["user"]]
#'  )
#'  cluster.spec
#' @rdname cluster_specification
#' @export
cluster_specification <- function(
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]]
){

  if(is.null(cluster.ips)){
    stop("Argument ips cannot be empty.")
  }
  if(is.null(cluster.cores)){
    stop("Argument cores cannot be empty.")
  }

  #creating initial list
  spec <- list()

  for(i in 1:length(cluster.ips)){
    spec[[i]] <- list()
    spec[[i]]$host <- cluster.ips[i]
    spec[[i]]$user <- cluster.user
    spec[[i]]$ncore <- cluster.cores[i]
  }

  #generating nodes from the list of machines
  spec <- lapply(
    spec,
    function(spec.i) rep(
      list(
        list(
          host = spec.i$host,
          user = spec.i$user)
      ),
      spec.i$ncore
    )
  )

  #formating into a list of lists
  spec <- unlist(
    spec,
    recursive = FALSE
  )

  spec

}
