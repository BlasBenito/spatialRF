#' @title rf_repeat
#' @description Repeats a given random forest model several times in order to capture the effect of the stochasticity of the algorithm on importance scores and accuracy measures. The function is prepared to run on a cluster if the IPs, number of cores, and user name are provided (see [cluster_specification]).
#' @param model (optional) a model produced by [rf]. If used, the arguments `data`, `dependent.variable.name`, `predictor.variable.names`, `distance.matrix`, `distance.thresholds`, `ranger.arguments`, `trees.per.variable`, and `scaled.importance` are taken directly from the model definition. Default: NULL
#' @param data (required) data frame with a response variable and a set of (preferably uncorrelated) predictors, Default: NULL
#' @param dependent.variable.name (required) string with the name of the response variable. Must be in the column names of 'data', Default: NULL
#' @param predictor.variable.names (required) character vector with the names of the predictive variables. Every element must be in the column names of 'data', Default: NULL
#' @param distance.matrix (optional) a squared matrix with the distances among the records in 'data'. Notice that the rows of 'distance.matrix' and 'data' must be the same. If not provided, the computation of the Moran's I of the residuals is ommited. Default: NULL.
#' @param distance.thresholds (optional) numeric vector, distances below each value in the distance matrix are set to 0 for the computation of Moran's I. If NULL, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: NULL.
#' @param ranger.arguments (optional) list with \link[ranger]{ranger} arguments. All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param trees.per.variable (optional) number of individual regression trees to fit per variable in 'predictor.variable.names'. This is an alternative way to define ranger's 'num.trees'. If NULL, 'num.trees' is 500. Default: NULL
#' @param scaled.importance (optional) boolean. If TRUE, and 'importance = "permutation', the function scales 'data' with [scale_robust] and fits a new model to compute scaled variable importance scores. Default: TRUE
#' @param repetitions (required) integer, number of random forest models to fit. Default: 5
#' @param keep.models boolean, if TRUE, the fitted models are returned in the "models" slot. Default: FALSE.
#' @param verbose Boolean. If TRUE, messages and plots generated during the execution of the function are displayed, Default: TRUE
#' @param n.cores number of cores to use to compute repetitions. If NULL, all cores but one are used, unless a cluster is used.
#' @param cluster.ips character vector, IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores numeric integer vector, number of cores on each machine.
#' @param cluster.user character string, name of the user (should be the same throughout machines), Defaults to the current system user. Default: user name of the current session.
#' @param cluster.port integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: 11000.
#' @return a ranger model with several new slots:
#' \itemize{
#'   \item{ranger.arguments}{stores the values of the arguments used to fit the ranger model}
#'   \item{predictions}{a list with the predictions obtained on each repetition stored in a data frame named 'per.repetition' and the average of the predictions in a data frame named 'mean'}
#'   \item{variable.importance}{a list containing a data frame with the mean importance of each predictor across repetitions (df), the values obtained on eah repetition (df.long), and a boxplot showing the distribution of the importance scores across repetitions}
#'   \item{pseudo.r.squared}{pseudo R-squared values throughout repetitions}
#'   \item{rmse}{rmse obtained on each repetition}
#'   \item{nrmse}{normalizad rmse obtained on each repetition}
#'   \item{residuals}{the residuals obtained on each repetition (df.wide), their mean (df) and their stats (stats)}
#'   \item{spatial.correlation.residuals}{the result of [moran_multithreshold] applied to the results of each repetition (df.long), the mean of Moran's I across repetitions (df), and a plot with the results of every repetition (plot)}
#' }
#' @details Please read the help file of [rf] and \link[ranger]{ranger} for further details.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data("plant_richness_df")
#'  data("distance_matrix")
#'  out <- rf_repeat(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 100, 1000, 10000),
#'    repetitions = 10,
#'    n.cores = 1
#'  )
#'
#'  class(out)
#'
#'  #data frame with ordered variable importance
#'  out$variable.importance$per.variable
#'
#'  #per repetition
#'  out$variable.importance$per.repetition
#'
#'  #variable importance plot
#'  out$variable.importance$plot
#'
#'  #pseudo R-squared
#'  out$pseudo.r.squared
#'
#'  #rmse and nrmse
#'  out$rmse
#'  out$nrmse
#'
#'  #spatial correlation of the residuals for different distance thresholds
#'  out$spatial.correlation.residuals$df
#'
#'  #plot of the Moran's I of the residuals for different distance thresholds
#'  out$spatial.correlation.residuals$plot
#'
#'  #using a model as an input for rf_repeat()
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[8:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000)
#'    )
#'  rf.model$performance
#'  rf.model$variable.importance$plot
#'
#'  rf.repeat <- rf_repeat(model = rf.model)
#'  rf.repeat$performance
#'  rf.repeat$variable.importance$plot
#'  }
#' }
#' @importFrom tidyselect all_of
#' @rdname rf_repeat
#' @export
rf_repeat <- function(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  trees.per.variable = NULL,
  scaled.importance = TRUE,
  repetitions = 5,
  keep.models = FALSE,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = 11000

){

  #declaring some variables
  variable <- NULL
  importance <- NULL
  distance.threshold <- NULL
  moran.i <- NULL

  #checking repetitions
  if(!is.integer(repetitions)){
    repetitions <- floor(repetitions)
  }
  if(repetitions < 5){
    repetitions <- 5
  }
  if(repetitions > 30){
    message("Large numbers of 'repetitions' may lead to long computation times.")
  }

  #getting arguments from model rather than ranger.arguments
  if(!is.null(model)){
    if(!is.null(ranger.arguments)){
      ranger.arguments <- NULL
      data <- NULL
      dependent.variable.name <- NULL
      predictor.variable.names <- NULL
      distance.matrix = NULL
      distance.thresholds <- NULL
      trees.per.variable <- NULL
      scaled.importance <- TRUE
    }
    ranger.arguments <- model$ranger.arguments
    list2env(ranger.arguments, envir=environment())
    seed <- NULL
  }

  #initializes local.importance
  if(is.null(ranger.arguments$local.importance)){
    local.importance <- FALSE
  } else {
    local.importance <- ranger.arguments$local.importance
  }

  #initializes local.importance
  if(!is.null(ranger.arguments$trees.per.variable)){
    trees.per.variable <- ranger.arguments$trees.per.variable
  }

  #SCALING DATA
  if(scaled.importance == TRUE){

    data.scaled <- scale_robust(
      x = data[, c(
        dependent.variable.name,
        predictor.variable.names
      )]
    )

    #if scaling fails, use regular scaling
    if(
      sum(is.nan(data.scaled[, 1])) > 0 |
      sum(is.infinite(data.scaled[, 1])) > 0
    ){
      data.scaled <- as.data.frame(scale(data))
    }

  }

  #INITIALIZING CLUSTER

  #preparing cluster for stand alone machine
  if(is.null(cluster.ips) == TRUE){

    #number of available cores
    if(is.null(n.cores)){
      n.cores <- parallel::detectCores() - 1
    }
    if(n.cores == 1){
      if(is.null(ranger.arguments)){
        ranger.arguments <- list()
      }
      ranger.arguments$num.threads <- 1
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

  #PARALLELIZED LOOP
  i <- NULL
  repeated.models <- foreach::foreach(
    i = 1:repetitions,
    .packages = c(
      "ranger",
      "magrittr"
    ),
    .export = c(
      "rescale_vector",
      "root_mean_squared_error",
      "rescale_vector",
      "moran_multithreshold",
      "moran"
    )
  ) %dopar% {

    set.seed(i)

    #model on raw data
    m.i <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      trees.per.variable = trees.per.variable,
      verbose = FALSE
    )

    #model on scaled data
    if(scaled.importance == TRUE){

      m.i.scaled <- rf(
        data = data.scaled,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        ranger.arguments = ranger.arguments,
        trees.per.variable = trees.per.variable,
        verbose = FALSE
      )

    }

    #gathering results
    out <- list()
    out$predictions <- m.i$predictions
    if(local.importance == TRUE){
      out$variable.importance.local <- m.i.scaled$variable.importance.local
    }
    if(scaled.importance == TRUE){
      out$variable.importance <- m.i.scaled$variable.importance$per.variable
    } else {
      out$variable.importance <- m.i$variable.importance$per.variable
    }
    out$prediction.error <- m.i$prediction.error
    out$r.squared <- m.i$performance$r.squared
    out$pseudo.r.squared <- m.i$performance$pseudo.r.squared
    out$rmse <- m.i$performance$rmse
    out$nrmse <- m.i$performance$nrmse
    out$residuals <- m.i$residuals
    out$spatial.correlation.residuals <- m.i$spatial.correlation.residuals
    if(keep.models == TRUE){
      out$model <- m.i
    }

    return(out)

  }#end of parallelized loop

  #fitting model to allow plotting partial dependence curves
  m.curves <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    ranger.arguments = ranger.arguments,
    trees.per.variable = trees.per.variable,
    seed = 1,
    verbose = FALSE
  )

  #PARSING OUTPUT OF PARALLELIZED LOOP

  #names of repetitions columns
  repetition.columns <- paste("repetition", 1:repetitions, sep = "_")

  #gathering predictions
  predictions.per.repetition <- as.data.frame(
    do.call(
      "cbind",
      lapply(
        repeated.models,
        "[[",
        "predictions"
      )
    )
  )
  colnames(predictions.per.repetition) <- repetition.columns
  predictions.mean <- data.frame(
    prediction_mean = rowMeans(predictions.per.repetition),
    standard_deviation = apply(predictions.per.repetition, 1, sd)
  )
  m.curves$predictions <- NULL
  m.curves$predictions$per.repetition <- predictions.per.repetition
  m.curves$predictions$mean <- predictions.mean

  #gathering variable.importance.local
  if(local.importance == TRUE){
    m.curves$variable.importance.local <- as.data.frame(
      apply(
        simplify2array(
          lapply(
            repeated.models,
            "[[",
            "variable.importance.local"
          )
        ),
        1:2,
        mean
      )
    )
  }


  #gathering variable.importance
  m.curves$variable.importance <- NULL

  #per repetition
  variable.importance.per.repetition <- as.data.frame(
    do.call(
      "rbind",
      lapply(
        repeated.models,
        "[[",
        "variable.importance"
      )
    )
  )

  #mean
  variable.importance.per.variable <- variable.importance.per.repetition %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(importance = mean(importance)) %>%
    dplyr::arrange(dplyr::desc(importance)) %>%
    as.data.frame()

  m.curves$variable.importance <- list()
  m.curves$variable.importance$per.variable <- variable.importance.per.variable
  m.curves$variable.importance$per.repetition <- variable.importance.per.repetition
  m.curves$variable.importance$plot <- plot_importance(
    x = variable.importance.per.repetition,
    verbose = verbose
  )

  if(verbose == TRUE){
    suppressWarnings(print(m.curves$variable.importance$plot))
  }

  #gathering prediction.error
  m.curves$prediction.error <- unlist(
    lapply(
      repeated.models,
      "[[",
      "prediction.error"
    )
  )

  #gathering r.squared
  m.curves$performance <- list()
  m.curves$performance$r.squared <- unlist(
    lapply(
      repeated.models,
      "[[",
      "r.squared"
    )
  )
  m.curves$performance$r.squared <- round(m.curves$performance$r.squared, 3)

  #gathering pseudo R squared
  m.curves$performance$pseudo.r.squared <- unlist(
    lapply(
      repeated.models,
      "[[",
      "pseudo.r.squared"
    )
  )

  #gathering rmse
  m.curves$performance$rmse <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rmse"
    )
  )
  names(m.curves$performance$rmse) <- NULL

  #gathering nrmse
  m.curves$performance$nrmse <- unlist(
    lapply(
      repeated.models,
      "[[",
      "nrmse"
    )
  )
  names(m.curves$performance$nrmse) <- NULL

  #gathering spatial.correlation.residuals
  spatial.correlation.residuals.per.repetition <- do.call(
    "rbind",
    lapply(
      lapply(
        repeated.models,
        "[[",
        "spatial.correlation.residuals"
      ),
      "[[",
      1
    )
  ) %>%
    dplyr::arrange(distance.threshold)
  spatial.correlation.residuals.per.repetition$repetition <- rep(
    1:repetitions,
    length(unique(spatial.correlation.residuals.per.repetition$distance.threshold))
    )

  p.value <- NULL
  interpretation <- NULL
  spatial.correlation.residuals.mean <- spatial.correlation.residuals.per.repetition %>%
    dplyr::group_by(distance.threshold) %>%
    dplyr::summarise(
      moran.i = mean(moran.i),
      p.value = mean(p.value),
      interpretation = statistical_mode(interpretation)
    ) %>%
    as.data.frame()

  repetition <- NULL
  m.curves$spatial.correlation.residuals <- list()
  m.curves$spatial.correlation.residuals$per.distance <- spatial.correlation.residuals.mean
  m.curves$spatial.correlation.residuals$per.repetition <- spatial.correlation.residuals.per.repetition
  m.curves$spatial.correlation.residuals$plot <- plot_moran(
    x = spatial.correlation.residuals.per.repetition,
    verbose = verbose
  )

  m.curves$spatial.correlation.residuals$max.moran <-  mean(
    unlist(
      lapply(
        lapply(
          repeated.models,
          "[[",
          "spatial.correlation.residuals"
        ),
        "[[",
        3
      )
    )
  )

  m.curves$spatial.correlation.residuals$max.moran.distance.threshold <- statistical_mode(
    unlist(
      lapply(
        lapply(
          repeated.models,
          "[[",
          "spatial.correlation.residuals"
        ),
        "[[",
        4
      )
    )
  )

  #gathering residuals
  residuals <- as.data.frame(do.call("cbind", lapply(
    repeated.models,
    "[[",
    "residuals"
  )))
  colnames(residuals) <- repetition.columns

  residuals.mean <- data.frame(
    residuals_mean = rowMeans(residuals),
    standard_deviation = apply(residuals, 1, sd),
    row.names = NULL
  )

  m.curves$residuals <- NULL
  m.curves$residuals$mean <- residuals.mean
  m.curves$residuals$per.repetition <- residuals
  m.curves$residuals$stats <- summary(residuals.mean$residuals_mean)

  #gathering models
  if(keep.models == TRUE){
    m.curves$models <- repeated.models
  }

  #adding repetitions to ranger.arguments
  m.curves$ranger.arguments$repetitions <- repetitions
  m.curves$ranger.arguments$keep.models <- keep.models

  #adding class to the model
  class(m.curves) <- c("ranger", "rf_repeat")

  #print model
  if(verbose == TRUE){
    print(m.curves)
  }

  #return m.curves
  m.curves


}
