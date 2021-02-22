#' @title Evaluates random forest models with spatial cross-validation
#' @description Evaluates the performance of random forest on unseen data over independent spatial folds.
#' @param model Model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the number of records to be used in model training. Default: `0.8`
#' @param metrics Character vector, names of the performance metrics selected. The possible values are: "r.squared" (`cor(obs, pred) ^ 2`), "pseudo.r.squared" (`cor(obs, pred)`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`). Default: `c("r.squared", "pseudo.r.squared", "rmse", "nrmse")`
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A model of the class "rf_evaluate" with a new slot named "evaluation", that is a list with the following slots:
#' \itemize{
#'   \item `training.fraction`: Value of the argument `training.fraction`.
#'   \item `spatial.folds`: Result of applying [make_spatial_folds()] on the data coordinates. It is a list with as many slots as `repetitions` are indicated by the user. Each slot has two slots named "training" and "testing", each one having the indices of the cases used on the training and testing models.
#'   \item `per.fold`: Data frame with the evaluation results per spatial fold (or repetition). It contains the ID of each fold, it's central coordinates, the number of training and testing cases, and the training and testing performance measures: R squared, pseudo R squared (cor(observed, predicted)), rmse, and normalized rmse.
#'   \item `per.model`: Same data as above, but organized per fold and model ("Training", "Testing", and "Full").
#'   \item `aggregated`: Same data, but aggregated by model and performance measure.
#' }
#' @details The evaluation algorithm works as follows: the number of `repetitions` and the input dataset (stored in `model$ranger.arguments$data`) are used as inputs for the function [thinning_til_n()], that applies [thinning()] to the input data until as many cases as `repetitions` are left, and as separated as possible. Each of these remaining records will be used as a "fold center". From that point, the fold grows, until a number of points equal (or close) to `training.fraction` is reached. The indices of the records within the grown spatial fold are stored as "training" in the output list, and the remaining ones as "testing". Then, for each spatial fold, a "training model" is fitted using the cases corresponding with the training indices, and predicted over the cases corresponding with the testing indices. The model predictions on the "unseen" data are compared with the observations, and the performance measures (R squared, pseudo R squared, RMSE and NRMSE) computed.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = c(0, 1000, 2000),
#'   verbose = FALSE
#' )
#'
#' rf.model <- rf_evaluate(
#'   model = rf.model,
#'   xy = plant_richness_df[, c("x", "y")],
#'   n.cores = 1
#' )
#'
#' plot_evaluation(rf.model)
#' print_evaluation(rf.model)
#' x <- get_evaluation(rf.model)
#'
#' }
#' }
#' @rdname rf_evaluate
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom stats predict
#' @importFrom dplyr select contains group_by summarise
#' @importFrom tidyr pivot_longer
rf_evaluate <- function(
  model = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.8,
  metrics = c("r.squared", "pseudo.r.squared", "rmse", "nrmse"),
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
){

  #declaring variables
  i <- NULL
  metric <- NULL
  value <- NULL

  #capturing user options
  user.options <- options()
  #avoid dplyr messages
  options(dplyr.summarise.inform = FALSE)
  on.exit(options <- user.options)

  #testing method argument
  metrics <- match.arg(
    arg = metrics,
    choices = c("r.squared", "pseudo.r.squared", "rmse", "nrmse"),
    several.ok = TRUE
  )

  #checking repetitions
  repetitions <- floor(repetitions)
  if(repetitions <= 5){
    stop("Argument 'repetitions' must be an integer equal or larger than 5")
  }

  #training fraction limits
  if(training.fraction < 0.5){
    training.fraction <- 0.5
  }
  if(training.fraction > 0.9){
    training.fraction <- 0.9
  }

  #getting data and ranger arguments from the model
  data <- model$ranger.arguments$data
  dependent.variable.name <- model$ranger.arguments$dependent.variable.name
  predictor.variable.names <- model$ranger.arguments$predictor.variable.names
  ranger.arguments <- model$ranger.arguments
  ranger.arguments$data <- NULL
  ranger.arguments$dependent.variable.name <- NULL
  ranger.arguments$predictor.variable.names <- NULL
  ranger.arguments$importance <- "none"
  ranger.arguments$data <- NULL
  ranger.arguments$scaled.importance <- FALSE
  ranger.arguments$distance.matrix <- NULL
  ranger.arguments$num.threads <- 1

  #preparing xy
  #if null, stop
  if(is.null(xy)){
    stop("Argument 'xy' requires a matrix or data frame with longitude-latitude coordinates with columns named 'x' and 'y'.")
  } else {
    if(sum(c("x", "y") %in% colnames(xy)) < 2){
      stop("The column names of 'xy' must be 'x' and 'y'.")
    }
  }

  #check nrow of xy and data
  if(nrow(xy) != nrow(data)){
    stop("nrow(xy) and nrow(data) (stored in model$ranger.arguments$data) must be the same.")
  }

  #add id to data and xy
  data$id <- xy$id <- 1:nrow(data)

  #thinning coordinates to get a more systematic sample of reference points
  if(repetitions < nrow(xy)){
    xy.reference.records <- thinning_til_n(
      xy = xy,
      n = repetitions
    )
  } else {
    xy.reference.records <- xy
  }

  #computing distance step for spatial folds
  distance.step <- min(dist(xy)) / 2

  #generates spatial folds
  ####################################
  spatial.folds <- spatialRF::make_spatial_folds(
    xy.selected = xy.reference.records,
    xy = xy,
    distance.step = distance.step,
    training.fraction = training.fraction,
    n.cores = n.cores
  )

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

  #local cluster
  if(is.null(cluster.ips) & n.cores > 1){

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

  }

  #beowulf cluster
  if(!is.null(cluster.ips)){


    #cluster port
    Sys.setenv(R_PARALLEL_PORT = cluster.port)

    #preparing the cluster specification
    cluster.spec <- cluster_specification(
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user
    )

    #cluster setup
    if(verbose == TRUE){
      outfile <- ""
    } else {
      if(.Platform$OS.type == "windows"){
        outfile <- "nul:"
      } else {
        outfile <- "/dev/null"
      }
    }
    temp.cluster <- parallel::makeCluster(
      master = cluster.ips[1],
      spec = cluster.spec,
      port = cluster.port,
      outfile = outfile,
      homogeneous = TRUE
    )

    #register cluster and close on exit
    doParallel::registerDoParallel(cl = temp.cluster)
    on.exit(parallel::stopCluster(cl = temp.cluster))

  }

  #loop to evaluate models
  #####################################
  evaluation.df <- foreach::foreach(
    i = 1:length(spatial.folds),
    .combine = "rbind"
    ) %dopar% {

    #separating training and testing data
    data.training <- data[data$id %in% spatial.folds[[i]]$training, ]
    data.testing <- data[data$id %in% spatial.folds[[i]]$testing, ]

    #training model
    m.training <- spatialRF::rf(
      data = data.training,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      ranger.arguments = ranger.arguments,
      verbose = FALSE
    )

    #predicting over data.testing
    predicted <- stats::predict(
      object = m.training,
      data = data.testing,
      type = "response",
      num.threads = 1
    )$predictions
    observed <- data.testing[, dependent.variable.name]

    #computing evaluation scores
    out.df <- data.frame(
      fold.id = xy.reference.records[i, "id"],
      fold.center.x = xy.reference.records[i, "x"],
      fold.center.y = xy.reference.records[i, "y"],
      training.records = nrow(data.training),
      testing.records = nrow(data.testing)
      )

    if("r.squared" %in% metrics){
      out.df$training.r.squared = m.training$performance$r.squared
      out.df$testing.r.squared = round(cor(observed, predicted) ^ 2, 3)
    }
    if("pseudo.r.squared" %in% metrics){
      out.df$training.pseudo.r.squared = m.training$performance$pseudo.r.squared
      out.df$testing.pseudo.r.squared = round(cor(
        observed,
        predicted
      ), 3)
    }
    if("rmse" %in% metrics){
      out.df$training.rmse = m.training$performance$rmse
      out.df$testing.rmse = round(spatialRF::root_mean_squared_error(
        o = observed,
        p = predicted,
        normalization = NULL
      ), 3)
    }
    if("nrmse" %in% metrics){
      out.df$training.nrmse = m.training$performance$nrmse
      out.df$testing.nrmse = round(spatialRF::root_mean_squared_error(
        o = observed,
        p = predicted,
        normalization = "iq"
      ), 3)
    }
    rownames(out.df) <- NULL

    return(out.df)

  }#end of parallelized loop

  #preparing data frames for plotting and printing
  #select columns with "training"
  performance.training <- dplyr::select(
    evaluation.df,
    dplyr::contains("training")
    )
  performance.training[, 1] <- NULL
  performance.training$model <- "Training"

  #select columns with "testing"
  performance.testing <- dplyr::select(
    evaluation.df,
    dplyr::contains("testing")
    )
  performance.testing[, 1] <- NULL
  performance.testing$model <- "Testing"

  #full model
  performance.full <- data.frame(
    r.squared = model$performance$r.squared,
    pseudo.r.squared = model$performance$pseudo.r.squared,
    rmse = model$performance$rmse,
    nrmse = model$performance$nrmse,
    model = "Full"
  )
  performance.full <- performance.full[, c(metrics, "model")]

  #set colnames
  colnames(performance.training) <- colnames(performance.testing) <- colnames(performance.full) <- c(
    metrics,
    "model"
  )

  #rbind
  performance.df <- rbind(
    performance.training,
    performance.testing,
    performance.full
  )

  #to long format
  performance.df.long <- performance.df %>%
    tidyr::pivot_longer(
      cols = 1:length(metrics),
      names_to = "metric",
      values_to = "value"
    ) %>%
    as.data.frame()

  #aggregating
  performande.df.aggregated <- performance.df.long %>%
    dplyr::group_by(model, metric) %>%
    dplyr::summarise(
      median = median(value),
      q1 = quantile(value, 0.25),
      q3 = quantile(value, 0.75),
      mean = mean(value),
      se = standard_error(value),
      sd = sd(value),
      min = min(value),
      max = max(value)
    ) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  #stats to NA if "Full" only once in performance.df
  if(sum("Full" %in% performance.df$model) == 1){
    performande.df.aggregated[performande.df.aggregated$model == "Full", c("median", "q1", "q3", "se", "sd", "min", "max")] <- NA
  }

  #add spatial folds to the model
  model$evaluation <- list()
  model$evaluation$training.fraction <- training.fraction
  model$evaluation$spatial.folds <- spatial.folds
  model$evaluation$per.fold <- evaluation.df
  model$evaluation$per.fold.long <- performance.df.long
  model$evaluation$per.model <- performance.df
  model$evaluation$aggregated <- performande.df.aggregated

  class(model) <- c(class(model), "rf_evaluate")

  if(verbose == TRUE){
    print_evaluation(model)
  }

  return(model)

}
