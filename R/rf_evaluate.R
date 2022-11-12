#' @title Evaluates random forest models with spatial cross-validation
#' @description Evaluates the performance of random forest on unseen data over independent spatial folds.
#' @param model (required; model) Fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param xy (required; data frame or matrix) Must have two columns named "x" and "y" containing the geographic coordinates of the sampling locations. Default: `NULL`
#' @param repetitions (optional; integer) Number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the training data. Default: `30`
#' @param training.fraction (optional; numeric) Proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param distance.step (optional; numeric). Argument `distance.step` of [thinning_til_n()]. It's used to tune the selection of the pairs of coordinates that originate each training fold. Its default value is 1/1000th the maximum distance within records in `xy`. Try lower values if the number of training folds is lower than expected. Default: `NULL`
#' @param distance.step.x  (optional; numeric) Like `distance.step`, but for the longitude axis alone. It is set automatically to the range of longitudes  Default: `NULL`
#' @param distance.step.y (optional; numeric) Like `distance.step.x` but for the latitude. Useful when the height of the study area is at least two times its width Default: `NULL`
#' @param grow.testing.folds (optional; logical) If `TRUE`, grows testing (instead of training) folds from fold centers. This option might be useful when the training data has a spatial structure that does not match well with the default behavior of the function. Default: `FALSE`
#' @param seed (optional; integer) Random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose (optional; logical) If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores (optional; integer) Number of cores used by \code{\link[ranger]{ranger}} for parallel execution (used as value for the argument `num.threads` in `ranger()`). Default: `parallel::detectCores() - 1`
#' @param cluster (optional; cluster object) Cluster definition generated with `parallel::makeCluster()` or [start_cluster()]. Overrides `n.cores`. Faster than using `n.cores` for smaller models. This function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` or [stop_cluster()] at the end of your pipeline. Default: `NULL`
#' @return A model of the class "rf_evaluate" with a new slot named "evaluation". This is a list with the following slots:
#' \itemize{
#'   \item `metrics`: names of the metrics used for the evaluation.
#'   \item `spatial.folds`: list with the training and testing spatial folds used during the evaluation.
#'   \item `training.fraction`: Value of the argument `training.fraction`.
#'   \item `per.fold`: Data frame with the evaluation results per spatial fold and evaluation set ("training" and "testing"). It contains the ID of each fold, it's central coordinates, the number of training and testing cases, and the training and testing performance measures: R squared, rmse, and normalized rmse.
#'   \item `aggregated`: Aggregated version of `per.fold` with median, median absolute deviation, quartile 1, quartile 3, mean, standard error, standard deviation, minimum, and maximum performance scores across spatial folds.
#' }
#' @details The evaluation algorithm works as follows:
#'
#' Generation of contiguous training folds:
#'
#' \itemize{
#'   \item The function [thinning_til_n()] finds a set of coordinates of size `repetitions` in `xy`. These pairs of coordinates are as separated as possible, and will be used as "training-fold centers".
#'   \item From each training-fold center, a quadrangular buffer is grown one step at a time, until it encloses a proportion of records as close as possible to `training.fraction`. The amount of buffer growth on each step is controlled by `distance.step`, `distance.step.x`, and `distance.step.y`.
#'   \item For each "training-fold" a model is fitted with the records within the buffer (training records), and predicted over the records outside of the buffer (testing records).
#'   \item The values of the response in the testing records are compared with the predictions over these same records to compute model performance metrics.
#'   \item If there are training folds yielding identical performance metrics, only one of them is kept to avoid pseudorreplication.
#' }
#'
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predictor_variable_names,
#'   ecoregions_dependent_variable_name
#'   )
#'
#' #fitting random forest model
#' rf.model <- rf(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_dependent_variable_name,
#'   predictor.variable.names = ecoregions_predictor_variable_names,
#'   distance.matrix = ecoregions_distance_matrix,
#'   distance.thresholds = 0,
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #evaluation with spatial cross-validation
#' rf.model <- rf_evaluate(
#'   model = rf.model,
#'   xy = ecoregions_df[, c("x", "y")],
#'   n.cores = 1
#' )
#'
#' #checking evaluation results
#' plot_evaluation(rf.model)
#' print_evaluation(rf.model)
#' x <- get_evaluation_aggregated(rf.model)
#'
#' }
#' @rdname rf_evaluate
#' @export
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom stats predict mad
#' @importFrom dplyr select contains group_by summarise
#' @importFrom tidyr pivot_longer
rf_evaluate <- function(
  model = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  grow.testing.folds = FALSE,
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
){

  #declaring variables
  i <- NULL
  metric <- NULL
  value <- NULL
  evaluation.set <- NULL
  name <- NULL

  if(is.null(model)){

    stop("The argument 'model' is empty, there is no model to evaluate.")

  } else {

    #getting data and ranger arguments from the model
    data <- model$ranger.arguments$data
    dependent.variable.name <- model$ranger.arguments$dependent.variable.name
    predictor.variable.names <- model$ranger.arguments$predictor.variable.names
    ranger.arguments <- model$ranger.arguments
    ranger.arguments$data <- NULL
    ranger.arguments$dependent.variable.name <- NULL
    ranger.arguments$predictor.variable.names <- NULL
    ranger.arguments$importance <- "none"
    ranger.arguments$local.importance <- FALSE
    ranger.arguments$data <- NULL
    ranger.arguments$scaled.importance <- FALSE
    ranger.arguments$distance.matrix <- NULL

    #check if input is tibble
    if(tibble::is_tibble(data) == TRUE){
      return.tibble <- TRUE
    } else {
      return.tibble <- FALSE
    }

    #getting xy
    if(is.null(xy)){
      if(is.null(model$ranger.arguments$xy)){
        stop("The argument 'xy' is required for spatial cross-validation.")
      } else {
        xy <- model$ranger.arguments$xy
      }
    }

  }

  if(sum(c("x", "y") %in% colnames(xy)) < 2){
    stop("The column names of 'xy' must be 'x' and 'y'.")
  }

  if(nrow(xy) != nrow(data)){
    stop("nrow(xy) and nrow(data) must be the same.")
  }

  #if data is binary, "auc" is used
  if(is_binary_response(
    x = dplyr::pull(data, dependent.variable.name)
  )){
    metrics <- "auc"
  } else {
    metrics <- metrics <- c(
      "r.squared",
      "rmse",
      "nrmse"
    )
  }


  #checking repetitions
  repetitions <- floor(repetitions)
  if(repetitions < 1){
    stop("Argument 'repetitions' must be an integer equal or larger than 1")
  }
  if(repetitions > nrow(xy)){
    if(verbose == TRUE){
      message("Argument 'repetitions' larger than number of cases, setting it to the number of cases.")
    }
    repetitions <- nrow(xy)
  }


  #capturing user options
  user.options <- options()
  #avoid dplyr messages
  options(dplyr.summarise.inform = FALSE)
  on.exit(options <- user.options)


  #training fraction limits
  if(training.fraction < 0.1){
    training.fraction <- 0.1
  }
  if(training.fraction > 0.9){
    training.fraction <- 0.9
  }

  #flipping training fraction if grow.testing.folds is TRUE
  if(grow.testing.folds == TRUE){
    training.fraction <- 1 - training.fraction
  }

  #add id to data and xy
  data$id <- xy$id <- seq(1, nrow(data))

  #thinning coordinates to get a systematic sample of reference points

  if(verbose == TRUE){
    message("Selecting centers of training folds.")
  }

  xy.reference.records <- thinning_til_n(
    xy = xy,
    n = repetitions,
    distance.step = distance.step
  )

  #copy of ranger arguments for training mdoels
  ranger.arguments.training <- ranger.arguments

  #getting cluster from model if "cluster" is not provided
  #handling parallelization
  if("cluster" %in% class(cluster)){

    #registering cluster
    doParallel::registerDoParallel(cl = cluster)

    #parallel iterator
    `%iterator%` <- foreach::`%dopar%`

    #restricting the number of cores
    n.cores <- 1

  } else {

    #sequential iterator
    `%iterator%` <- foreach::`%do%`

  }

  #iterations
  evaluation.list <- foreach::foreach(
    i = seq(1, nrow(xy.reference.records), by = 1),
    .verbose = FALSE
  ) %iterator% {

    if(is.null(seed)){
      set.seed(i)
    } else {
      set.seed(i + seed)
    }

    #generating spatial folds
    training.testing.folds <- make_spatial_fold(
      data = data,
      dependent.variable.name = dependent.variable.name,
      xy.i = xy.reference.records[i, ],
      xy = xy,
      distance.step.x = distance.step.x,
      distance.step.y = distance.step.y,
      training.fraction = training.fraction
    )

    #flipping spatial folds if grow.testing.folds = TRUE
    if(grow.testing.folds == TRUE){
      for(j in 1:length(training.testing.folds)){
        names(training.testing.folds[[j]]) <- c("testing", "training")
      }
    }

    #separating training and testing data
    data.training <- data[data$id %in% training.testing.folds$training, ]
    data.testing <- data[data$id %in% training.testing.folds$testing, ]

    #subsetting case.weights if defined
    if(!is.null(ranger.arguments.training$case.weights)){
      ranger.arguments.training$case.weights <- ranger.arguments$case.weights[training.testing.folds$training]
    }

    #training model
    m.training <- spatialRF::rf(
      data = data.training,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      ranger.arguments = ranger.arguments.training,
      seed = seed,
      n.cores = n.cores,
      verbose = FALSE
    )

    #predicting over data.testing
    predicted <- stats::predict(
      object = m.training,
      data = data.testing,
      type = "response",
      num.threads = n.cores
    )$predictions

    #getting observed data
    observed <- dplyr::pull(
      data.testing,
      dependent.variable.name
      )

    #computing evaluation scores
    performance.df <- data.frame(
      fold.id = i,
      fold.center.x = xy.reference.records[i, "x"],
      fold.center.y = xy.reference.records[i, "y"],
      training.records = nrow(data.training),
      testing.records = nrow(data.testing)
    )

    if("r.squared" %in% metrics){

      performance.df$training_r.squared <- m.training$performance$r.squared.ib

      performance.df$testing_r.squared <- cor(observed, predicted) ^ 2

      if(is.na(performance.df$training_r.squared)){
        performance.df$testing_r.squared <- NA
      }

    }

    if("rmse" %in% metrics){

      performance.df$training_rmse <- m.training$performance$rmse.ib

      performance.df$testing_rmse <- spatialRF::root_mean_squared_error(
        o = observed,
        p = predicted,
        normalization = NULL
      )

      if(is.na(performance.df$training_rmse)){
        performance.df$testing_rmse <- NA
      }

    }

    if("nrmse" %in% metrics){

      performance.df$training_nrmse <- m.training$performance$nrmse.ib

      performance.df$testing_nrmse = spatialRF::root_mean_squared_error(
        o = observed,
        p = predicted,
        normalization = "iq"
      )

      if(is.na(performance.df$training_nrmse)){
        performance.df$testing_nrmse <- NA
      }

    }

    if("auc" %in% metrics){
      performance.df$training_auc <- m.training$performance$auc.ib

      performance.df$testing_auc <- spatialRF::auc(
          o = observed,
          p = predicted
        )

      if(is.na(performance.df$training_auc)){
        performance.df$testing_auc <- NA
      }
    }

    rownames(performance.df) <- NULL

    out.list <- list()
    out.list$spatial.folds <- training.testing.folds
    out.list$performance.df <- performance.df

    return(out.list)

  } #end of parallel loop

  #getting evaluation data frames
  performance.df <- as.data.frame(
    do.call(
      "rbind",
      lapply(
        evaluation.list,
        "[[",
        "performance.df"
      )
    )
  )

  #getting spatial folds list
  spatial.folds <- lapply(
    evaluation.list,
    "[[",
    "spatial.folds"
  )

  #copy of results
  performance.df.unique <- performance.df

  #keep distinct values only to avoid very similar spatial folds
  performance.df.unique <- performance.df.unique %>%
    dplyr::distinct(
      dplyr::across(dplyr::all_of(paste0("testing_", metrics))),
      .keep_all = TRUE
    )

  #subsetting the spatial folds
  spatial.folds <- spatial.folds[performance.df.unique$fold.id]

  #resetting fold ID
  performance.df.unique$fold.id <-
    names(spatial.folds) <-
    seq(
      from = 1,
      to = nrow(performance.df.unique),
      by = 1
    )

  #remove columns with NA
  performance.df.unique <- performance.df.unique[, names(na.omit(colSums(performance.df.unique)))]

  #message with number of removed folds
  nrow.difference <- nrow(performance.df) - nrow(performance.df.unique)
  if(nrow.difference > 0 & verbose == TRUE){
    message(
      paste0(
        nrow.difference,
        " redundant spatial folds removed. The total number of spatial folds is ",
        nrow(performance.df.unique),
        "."
      )
    )
  }

  #overwriting performance.df with performance.df.unique
  performance.df <- performance.df.unique
  rm(performance.df.unique)

  #performance.df.long
  performance.df.long <- performance.df %>%
    tidyr::pivot_longer(
      cols = dplyr::contains(match = metrics)
    ) %>%
    tidyr::separate(
      col = name,
      sep = "_",
      into = c("evaluation.set", "metric")
    )

  #adding testing values to performance list
  for(metric.i in metrics){
    model$performance[[paste0(metric.i, ".scv")]] <- performance.df.long %>%
      dplyr::filter(
        evaluation.set == "testing",
        metric == metric.i
        ) %>%
      dplyr::pull(
      value
      )
  }

  #aggregating
  performande.df.aggregated <- performance.df.long %>%
    dplyr::group_by(evaluation.set, metric) %>%
    dplyr::summarise(
      median = median(value),
      median_absolute_deviation = stats::mad(value),
      q1 = quantile(value, 0.25, na.rm = TRUE),
      q3 = quantile(value, 0.75, na.rm = TRUE),
      mean = mean(value),
      se = standard_error(value),
      sd = sd(value),
      min = min(value),
      max = max(value)
    ) %>%
    dplyr::ungroup()

  #add results to the model
  if("evaluation" %in% names(model)){
    model$evaluation <- NULL
  }
  model$evaluation <- list()
  model$evaluation$metrics <- metrics
  model$evaluation$spatial.folds <- spatial.folds
  model$evaluation$training.fraction <- training.fraction
  model$evaluation$per.fold <- performance.df.long
  model$evaluation$aggregated <- performande.df.aggregated

  if(verbose == TRUE){
    message("Evaluation results stored in model$evaluation.")
  }

  class(model) <- c(class(model), "rf_evaluate")

  #coercing output to tibble
  if(return.tibble == TRUE){
    model$evaluation$per.fold <- tibble::as_tibble(model$evaluation$per.fold)
    model$evaluation$aggregated <- tibble::as_tibble(model$evaluation$aggregated)
  }

  if(verbose == TRUE){
    print_evaluation(model = model)
  }

  model

}
