#' @title Setup parallel execution with automatic backend detection
#' @description Internal helper to manage parallel backend setup with support
#'   for user-managed backends, external clusters, and internal clusters.
#' @param cluster A cluster object from parallel::makeCluster(), or NULL
#' @param n.cores Number of cores for internal cluster creation
#' @return A list with:
#'   \itemize{
#'     \item cluster: The cluster object to pass to child functions (or NULL)
#'     \item mode: One of "user_backend", "external_cluster", "internal_cluster", "sequential"
#'     \item cleanup: A function to call in on.exit() for proper cleanup
#'   }
#' @family utilities
#' @export
setup_parallel_execution <- function(
  cluster = NULL,
  n.cores = parallel::detectCores() - 1
) {
  # Handle NULL n.cores (can happen when called from rf_compare)
  if (is.null(n.cores)) {
    n.cores <- parallel::detectCores() - 1
  }

  # Check if user has already registered a backend
  user.registered.backend <- foreach::getDoParRegistered()
  backend.name <- foreach::getDoParName()

  # Only consider it a user backend if it's NOT the default sequential backend
  has.parallel.backend <- user.registered.backend && backend.name != "doSEQ"

  # If parallel backend is registered AND cluster is provided, assume cluster is being
  # reused across function calls (not a user-managed backend)
  if (has.parallel.backend && inherits(x = cluster, what = "cluster")) {
    # Backend already registered, cluster provided - reuse existing setup
    return(list(
      cluster = cluster,
      mode = "external_cluster_reused",
      cleanup = function() {} # Caller manages external cluster
    ))
  }

  # If parallel backend is registered but NO cluster provided, it's user-managed
  if (has.parallel.backend) {
    # User-managed backend detected - don't interfere
    return(list(
      cluster = NULL,
      mode = "user_backend",
      cleanup = function() {} # No cleanup needed
    ))
  }

  # No user backend - check for provided cluster
  if (inherits(x = cluster, what = "cluster")) {
    # External cluster provided - register but don't manage lifecycle
    doParallel::registerDoParallel(cl = cluster)
    return(list(
      cluster = cluster,
      mode = "external_cluster",
      cleanup = function() {} # Caller manages external cluster
    ))
  }

  # No cluster provided - create internal cluster or use sequential
  if (n.cores > 1) {
    # Create internal cluster
    cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
    doParallel::registerDoParallel(cl = cluster)

    return(list(
      cluster = cluster,
      mode = "internal_cluster",
      cleanup = function() {
        foreach::registerDoSEQ()
        try(parallel::stopCluster(cluster), silent = TRUE)
      }
    ))
  } else {
    # Sequential mode
    foreach::registerDoSEQ()
    return(list(
      cluster = NULL,
      mode = "sequential",
      cleanup = function() {} # Already registered SEQ
    ))
  }
}
