#' @title Defines a cluster
#' @description Defines a cluster. With default arguments it returns a cluster defined in the local machine. If the arguments `cluster.ips` and `cluster.cores` are provided, then a Beowulf cluster spanning as many machines as IPs is returned. All functions taking clusters as inputs in this package will register the cluster themselves using `doParallel::registerDoParallel()` internally, but if you are going to use this function for other purposes, please register your cluster before using it. Notice that Beowulf clusters require a particular configuration for your local network (see section `Network settings` at \href{https://www.blasbenito.com/post/01_home_cluster/}{https://www.blasbenito.com/post/01_home_cluster/}).
#' @param cluster.ips Character vector with the IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed. Default: `NULL`.
#' @param cluster.cores Numeric integer vector, number of cores on each machine. Default: `parallel::detectCores() - 1`.
#' @param cluster.user Character string, name of the user (should be the same throughout machines), Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @param outfile Where to direct the messages provided by the workers. When working on a local computer, `""` prints the worker's messages in the console. A text file path will append worker's messages on the given file.  Default: `/dev/null` en Linux and  `nul:` on windows.
#' @return A list ready to be used as input for the `spec` argument of the function \link[parallel]{makeCluster}.
#' @examples
#' if(interactive()){
#'
#' local.cluster <- make_cluster(cluster.cores = 1)
#' #this cluster is registered by the packaage functions when used
#' stop_cluster(cluster = local.cluster)
#'
#' beowulf.cluster <- make_cluster(
#'   cluster.ips = c(
#'    "10.42.0.1",
#'    "10.42.0.34",
#'    "10.42.0.104"
#'    ),
#'  cluster.cores = c(1, 1, 1),
#'  cluster.user = "blas",
#'  cluster.port = "11000"
#' )
#'
#' stop_cluster(cluster = beowulf.cluster)
#'}
#'
#' @rdname make_cluster
#' @export
make_cluster <- function(
  cluster.ips = NULL,
  cluster.cores = parallel::detectCores() - 1,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000",
  outfile = NULL
){

  #if no IPs are provided, return local cluster
  if(is.null(cluster.ips)){

    #local cluster
    cluster <- parallel::makeCluster(
      spec = cluster.cores[1],
      type = "PSOCK"
    )

    return(cluster)

  }

  if(length(cluster.cores) != length(cluster.ips)){
    stop("Arguments 'cluster.cores' and 'cluster.ips' must have the same length.")
  }

  #creating initial list
  spec <- list()

  for(i in seq(1, length(cluster.ips), by = 1)){
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

  #defining beowulf cluster
  cluster <- parallel::makeCluster(
    master = cluster.ips[1],
    spec = spec,
    port = cluster.port,
    homogeneous = FALSE,
    outfile = outfile
  )

  cluster

}
