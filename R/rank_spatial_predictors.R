#' @title rank_spatial_predictors
#' @description ranks spatial predictors generated from the PCA of a distance matrix (or columns of the distance matrix itself) by either their effect in reducing the Moran's I of the model residuals (ranking.method = "moran.i.reduction"), or by their own Moran's I (ranking.method = "mem"). In the former case, one model of the type `y ~ predictors + spatial_predictor_X` is fitted per spatial predictor (this is a computationally intensive function), and the Moran's I of its residuals is compared with the one of the model `y ~ predictors`, to finally order the spatial predictor from maximum to minimum Moran's I difference. In the latter case the spatial predictors are ordered by their Moran's I alone (this is the faster option). In both cases, spatial predictors with no effect (no reduction of Moran's I  or Moran's I of the spatial predictor equal or lower than 0) are removed, while the remaining ones undergo a multicollinearity filtering through [auto_cor] or [auto_vif], in order to reduce as much as possible the total number of spatial predictors to reduce computation time downstream. The purpose of this function is to provide criteria on how to include spatial predictors in a model. This function has been designed to be used internally by rf_spatial rather than by directly by a user.
#' @param data (required) data frame with a response variable and a set of (preferably uncorrelated) predictors, Default: NULL
#' @param dependent.variable.name (required) string with the name of the response variable. Must be in the column names of 'data', Default: NULL
#' @param predictor.variable.names (required) character vector with the names of the predictive variables. Every element must be in the column names of 'data', Default: NULL
#' @param reference.moran.i Moran's I of the residuals of a model fitted without spatial predictors. Default: 1.
#' @param distance.matrix (optional) a squared matrix with the distances among the records in 'data'. Notice that the rows of 'distance.matrix' and 'data' must be the same. If not provided, the computation of the Moran's I of the residuals is ommited. Default: NULL.
#' @param distance.thresholds (optional) numeric vector, distances below each value in the distance matrix are set to 0 for the computation of Moran's I. If NULL, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: NULL.
#' @param ranger.arguments (optional) list with \link[ranger]{ranger} arguments. See [rf] or [rf_repeat] for further details.
#' @param spatial.predictors.df data frame of spatial predictors, either a distance matrix, or the PCA factors of the distance matrix produced by [pca_distance_matrix].
#' @param ranking.method string, one of "moran.i.reduction" and "mem". The former option ranks spatial predictors according how much each predictor reduces Moran's I of the model residuals.

#' @param n.cores number of cores to use to compute repetitions. If NULL, all cores but one are used, unless a cluster is used.
#' @param cluster.ips character vector, IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores numeric integer vector, number of cores on each machine.
#' @param cluster.user character string, name of the user (should be the same throughout machines), Defaults to the current system user. Default: user name of the current session.
#' @param cluster.port integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: 11000.
#' @param multicollinearity.filter method to reduce multicollinearity in the ranked spatial predictors, one of "vif" (triggers [auto_vif]), "cor" (triggers [auto_cor]), and "none" (does not apply a multicollinearity filter).
#' @return a list with two slots:
#' \itemize{
#'  \item{ranking.criteria}{data frame with two different configurations depending on the ranking method. If ranking.method = "moran.i.reduction", the columns contain the name of the spatial predictor, the r-squared of the model, the Moran's I of the model residuals, the difference between this Moran's I and the Moran's I of the model fitted without spatial predictors (named `ranking.criteria`, and the interpretation of the Moran's I value. If ranking.method = "mem", only the name of the spatial predictor, it's Moran's I and the interpretation are available.}
#'  \item{ranking}{ordered character vector with the names of the spatial predictors selected after the multicollinearity filtering (if applied)}
#' }
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data("distance_matrix")
#'
#'  spatial.predictors.df <- pca_distance_matrix(
#'    x = distance_matrix[1:50, 1:50],
#'    distance.thresholds = c(0, 100, 1000)
#'  )
#'
#'  #ranking by the Moran's I of the spatial predictor
#'  rank <- rank_spatial_predictors(
#'    distance.matrix = distance_matrix[1:50, 1:50],
#'    distance.thresholds = c(0, 100, 1000),
#'    spatial.predictors.df = spatial.predictors.df,
#'    ranking.method = "mem",
#'    n.cores = 1,
#'    multicollinearity.filter = "vif"
#'  )
#'  rank$ranking.criteria
#'  rank$ranking
#'  }
#' }
#' @rdname rank_spatial_predictors
#' @export
rank_spatial_predictors <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  spatial.predictors.df = NULL,
  ranking.method = c("moran.i.reduction", "mem"),
  reference.moran.i = 1,
  multicollinearity.filter = c("vif", "cor", "none"),
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #testing method argument
  ranking.method <- match.arg(ranking.method)
  multicollinearity.filter <- match.arg(multicollinearity.filter)

  #preparing cluster for stand alone machine
  if(is.null(cluster.ips) == TRUE){

    #number of available cores
    if(is.null(n.cores)){
      n.cores <- parallel::detectCores() - 1
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

    #preparing beowulf cluster
  } else {

    #preparing the cluster specification
    cluster.spec <- cluster_specification(
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user
    )

    #setting parallel port
    Sys.setenv(R_PARALLEL_PORT = cluster.port)

    #cluster setup
    temp.cluster <- parallel::makeCluster(
      master = cluster.ips[1],
      spec = cluster.spec,
      port = Sys.getenv("R_PARALLEL_PORT"),
      outfile = "",
      homogeneous = TRUE
    )

  }
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  #add write.forest = FALSE to ranger.arguments
  if(!is.null(ranger.arguments)){
    ranger.arguments$write.forest = FALSE
  }

  #3.2.3 PREPARING PARALLELIZED LOOP TO ITERATE THROUGH distance.matrix.pca
  spatial.predictors.i <- NULL
  spatial.predictors.order <- foreach::foreach(
    spatial.predictors.i = 1:ncol(spatial.predictors.df),
    .combine = "rbind",
    .packages = c(
      "ranger",
      "magrittr"
    ),
    .export = c(
      "root_mean_squared_error",
      "rescale_vector",
      "moran_multithreshold",
      "auto_vif",
      "scale_robust"
    )
  ) %dopar% {

    #3.2.3.1 preparing data

    #spatial predictor name
    spatial.predictors.name.i <- colnames(spatial.predictors.df)[spatial.predictors.i]

    #computing reduction in Moran's I
    if(ranking.method == "moran.i.reduction"){

      #training data
      data.i <- data.frame(
        data,
        spatial.predictors.df[, spatial.predictors.i]
      )
      colnames(data.i)[ncol(data.i)] <- spatial.predictors.name.i

      #new predictor.variable.names
      predictor.variable.names.i <- c(predictor.variable.names, spatial.predictors.name.i)

      #fitting model I
      m.i <- rf(
        data = data.i,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names.i,
        seed = spatial.predictors.i,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        scaled.importance = FALSE,
        ranger.arguments = ranger.arguments
      )

      #out.df
      out.i <- data.frame(
        spatial.predictors.name = spatial.predictors.name.i,
        model.r.squared = m.i$r.squared,
        model.moran.i = m.i$spatial.correlation.residuals$max.moran,
        ranking.criteria = reference.moran.i - m.i$spatial.correlation.residuals$max.moran,
        interpretation = m.i$spatial.correlation.residuals$df[which.max(m.i$spatial.correlation.residuals$df$moran.i), "interpretation"]
      )

    }

    #computing Moran's I of the spatial predictors
    if(ranking.method == "mem"){

      #moran's I of spatial predictor
      m.i <- moran(
        x = spatial.predictors.df[, spatial.predictors.i],
        distance.matrix = distance.matrix,
        distance.threshold = distance.thresholds[1]
      )

      #out.df
      out.i <- data.frame(
        spatial.predictors.name = spatial.predictors.name.i,
        ranking.criteria = m.i$moran.i,
        interpretation = m.i$interpretation
      )

    }

    #returning output
    return(out.i)

  } #end of parallelized loop

  #order dataframe
  ranking.criteria <- NULL
  spatial.predictors.order <- spatial.predictors.order %>%
    dplyr::arrange(dplyr::desc(ranking.criteria))

  #selected spatial.predictorss
  spatial.predictors.order.selected <- dplyr::filter(
    spatial.predictors.order,
    ranking.criteria > 0
  )

  #apply vif filtering if requested
  if(multicollinearity.filter == "vif"){

    multicollinearity.df <- auto_vif(
      x = spatial.predictors.df[, spatial.predictors.order.selected$spatial.predictors.name],
      preference.order = spatial.predictors.order.selected$spatial.predictors.name,
      verbose = FALSE
    )

  }

  if(multicollinearity.filter == "cor"){

    multicollinearity.df <- auto_cor(
      x = spatial.predictors.df[, spatial.predictors.order.selected$spatial.predictors.name],
      preference.order = spatial.predictors.order.selected$spatial.predictors.name,
      cor.threshold = 0.75
    )

  }

  #subset spatial.predictors.order
  if(multicollinearity.filter != "none"){

    spatial.predictors.order.selected <- spatial.predictors.order.selected[spatial.predictors.order.selected$spatial.predictors.name %in% multicollinearity.df$selected.variables, ]

  }

  #return output
  out.list <- list()
  out.list$ranking.criteria <- spatial.predictors.order
  out.list$ranking <- spatial.predictors.order.selected$spatial.predictors.name

  #returning output list
  out.list

}

