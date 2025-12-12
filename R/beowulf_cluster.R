#' @title Defines a beowulf cluster
#' @description Defines a Beowulf cluster from the IPs of the machines in the cluster, the number of cores of each machine, and the user name. The returned cluster has to be registered with `doParallel::registerDoParallel()`.
#' @param cluster.ips Character vector with the IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed. Default: `NULL`.
#' @param cluster.cores Numeric integer vector, number of cores on each machine. Default: `NULL`.
#' @param cluster.user Character string, name of the user (should be the same throughout machines), Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @param outfile Where to direct the messages provided by the workers. When working on a local computer, `""` prints the worker's messages in the console. A text file path will append worker's messages on the given file.  Default: `/dev/null` en Linux and  `nul:` on windows.
#' @return A list ready to be used as input for the `spec` argument of the function \link[parallel]{makeCluster}.
#' @examples
#' if(interactive()){
#'
#'beowulf.cluster <- beowulf_cluster(
#'  cluster.ips = c(
#'    "10.42.0.1",
#'    "10.42.0.34",
#'    "10.42.0.104"
#'    ),
#' cluster.cores = c(7, 4, 4),
#' cluster.user = "blas",
#' cluster.port = "11000"
#')
#'
#'
#'doParallel::registerDoParallel(cl = beowulf.cluster)
#'
#'#PARALLELIZED foreach LOOP HERE
#'
#'parallel::stopCluster(cl = beowulf.cluster)
#'
#'}
#'
#' @rdname beowulf_cluster
#' @export
beowulf_cluster <- function(
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000",
  outfile = NULL
) {
  if (is.null(cluster.ips)) {
    stop("Argument ips cannot be empty.")
  }
  if (is.null(cluster.cores)) {
    stop("Argument cores cannot be empty.")
  }

  #creating initial list
  spec <- list()

  for (i in seq(1, length(cluster.ips), by = 1)) {
    spec[[i]] <- list()
    spec[[i]]$host <- cluster.ips[i]
    spec[[i]]$user <- cluster.user
    spec[[i]]$ncore <- cluster.cores[i]
  }

  #generating nodes from the list of machines
  spec <- lapply(
    spec,
    function(spec.i) {
      rep(
        list(
          list(
            host = spec.i$host,
            user = spec.i$user
          )
        ),
        spec.i$ncore
      )
    }
  )

  #formating into a list of lists
  spec <- unlist(
    spec,
    recursive = FALSE
  )

  #defining beowulf cluster
  beowulf.cluster <- parallel::makeCluster(
    master = "10.42.0.1",
    spec = spec,
    port = cluster.port,
    homogeneous = FALSE,
    outfile = outfile
  )

  beowulf.cluster
}
