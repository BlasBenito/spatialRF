#' @title Create a Beowulf cluster for parallel computing
#' @description Creates a Beowulf cluster configuration from machine IPs, core counts, and user credentials.
#' @param cluster.ips Character vector of machine IP addresses in the cluster. The first IP is the main node (typically the machine running this code). Default: `NULL`.
#' @param cluster.cores Integer vector of core counts for each machine. Must match the length of `cluster.ips`. Default: `NULL`.
#' @param cluster.user Character string for the user name across all machines. Default: current system user.
#' @param cluster.port Character string specifying the communication port. Default: `"11000"`.
#' @param outfile Character string or `NULL`. Path to append worker messages, `""` to print to console, or `NULL` (default) for `/dev/null` (Linux) or `nul:` (Windows).
#' @return Cluster object created by [parallel::makeCluster()], ready for registration with [doParallel::registerDoParallel()].
#' @details
#' \strong{Network requirements}: Firewalls on all machines must allow traffic on the specified port.
#'
#' \strong{Usage workflow}:
#' \enumerate{
#'   \item Create cluster with this function
#'   \item Register with [doParallel::registerDoParallel()]
#'   \item Run parallelized code (e.g., foreach loops)
#'   \item Stop cluster with [parallel::stopCluster()]
#' }
#' @examples
#' \dontrun{
#' # Create cluster with 3 machines
#' beowulf.cluster <- beowulf_cluster(
#'   cluster.ips = c(
#'     "192.168.1.10",  # main node
#'     "192.168.1.11",
#'     "192.168.1.12"
#'   ),
#'   cluster.cores = c(7, 4, 4),
#'   cluster.user = "username",
#'   cluster.port = "11000"
#' )
#'
#' # Register cluster for parallel processing
#' doParallel::registerDoParallel(cl = beowulf.cluster)
#'
#' # Run parallelized code (e.g., foreach loop)
#' # your_parallel_code_here
#'
#' # Stop cluster when done
#' parallel::stopCluster(cl = beowulf.cluster)
#' }
#'
#'
#' @rdname beowulf_cluster
#' @family utilities
#' @export
#' @autoglobal
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
