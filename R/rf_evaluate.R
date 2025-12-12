#' @title Evaluates random forest models with spatial cross-validation
#' @description Evaluates the performance of random forest on unseen data over independent spatial folds.
#' @param model Model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param metrics Character vector, names of the performance metrics selected. The possible values are: "r.squared" (`cor(obs, pred) ^ 2`), "pseudo.r.squared" (`cor(obs, pred)`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`), and "auc" (only for binary responses with values 1 and 0). Default: `c("r.squared", "pseudo.r.squared", "rmse", "nrmse")`
#' @param distance.step Numeric, argument `distance.step` of [thinning_til_n()]. distance step used during the selection of the centers of the training folds. These fold centers are selected by thinning the data until a number of folds equal or lower than `repetitions` is reached. Its default value is 1/1000th the maximum distance within records in `xy`. Reduce it if the number of training folds is lower than expected.
#' @param distance.step.x Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the x axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the x coordinates).
#' @param distance.step.y Numeric, argument `distance.step.x` of [make_spatial_folds()]. Distance step used during the growth in the y axis of the buffers defining the training folds. Default: `NULL` (1/1000th the range of the y coordinates).
#' @param grow.testing.folds Logic. By default, this function grows contiguous training folds to keep the spatial structure of the data as intact as possible. However, when setting `grow.testing.folds = TRUE`, the argument `training.fraction` is set to `1 - training.fraction`, and the training and testing folds are switched. This option might be useful when the training data has a spatial structure that does not match well with the default behavior of the function. Default: `FALSE`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use for parallel execution. Creates a socket cluster with `parallel::makeCluster()`, runs operations in parallel with `foreach` and `%dopar%`, and stops the cluster with `parallel::clusterStop()` when the job is done. Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()`. If provided, overrides `n.cores`. When `cluster = NULL` (default value), and `model` is provided, the cluster in `model`, if any, is used instead. If this cluster is `NULL`, then the function uses `n.cores` instead. The function does not stop a provided cluster, so it should be stopped with `parallel::stopCluster()` afterwards. The cluster definition is stored in the output list under the name "cluster" so it can be passed to other functions via the `model` argument, or using the `%>%` pipe. Default: `NULL`
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
#' if(interactive()){
#'
#' #loading example data
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' #fitting random forest model
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = 0,
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #evaluation with spatial cross-validation
#' rf.model <- rf_evaluate(
#'   model = rf.model,
#'   xy = plant_richness_df[, c("x", "y")],
#'   n.cores = 1
#' )
#'
#' #checking evaluation results
#' plot_evaluation(rf.model)
#' print_evaluation(rf.model)
#' x <- get_evaluation(rf.model)
#'
#' }
#' @rdname rf_evaluate
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster
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
  metrics = c(
    "r.squared",
    "pseudo.r.squared",
    "rmse",
    "nrmse",
    "auc"
  ),
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  grow.testing.folds = FALSE,
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
) {
  #declaring variables
  i <- NULL
  metric <- NULL
  value <- NULL

  if (is.null(model)) {
    stop("The argument 'model' is empty, there is no model to evaluate")
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
  ranger.arguments$local.importance <- FALSE
  ranger.arguments$data <- NULL
  ranger.arguments$scaled.importance <- FALSE
  ranger.arguments$distance.matrix <- NULL

  if (repetitions > 1) {
    ranger.arguments$num.threads <- 1
  }

  #getting xy
  if (is.null(xy)) {
    if (is.null(model$ranger.arguments$xy)) {
      stop("The argument 'xy' is required for spatial cross-validation.")
    } else {
      xy <- model$ranger.arguments$xy
    }
  }

  if (sum(c("x", "y") %in% colnames(xy)) < 2) {
    stop("The column names of 'xy' must be 'x' and 'y'.")
  }

  if (nrow(xy) != nrow(data)) {
    stop(
      "nrow(xy) and nrow(data) (stored in model$ranger.arguments$data) must be the same."
    )
  }

  #CLUSTER SETUP
  pass.cluster <- FALSE
  if (!inherits(x = cluster, what = "cluster")) {
    if (inherits(x = model$cluster, what = "cluster")) {
      cluster <- model$cluster
      pass.cluster <- TRUE
    } else if (n.cores > 1) {
      cluster <- parallel::makeCluster(
        n.cores,
        type = "PSOCK"
      )
      pass.cluster <- FALSE
      on.exit(
        {
          foreach::registerDoSEQ()
          try(
            parallel::stopCluster(cluster),
            silent = TRUE
          )
        },
        add = TRUE
      )
    } else {
      # n.cores == 1, use sequential execution
      cluster <- NULL
      pass.cluster <- FALSE
    }
  }

  # Register backend
  if (!is.null(cluster)) {
    doParallel::registerDoParallel(cl = cluster)
  } else {
    foreach::registerDoSEQ()
  }

  #testing method argument
  metrics <- match.arg(
    arg = metrics,
    choices = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
    several.ok = TRUE
  )

  #if data is binary, "auc" is used
  is.binary <- is_binary(
    data = data,
    dependent.variable.name = dependent.variable.name
  )
  if (is.binary == TRUE & !("auc" %in% metrics)) {
    metrics <- "auc"
  }

  #checking repetitions
  repetitions <- floor(repetitions)
  if (repetitions < 5) {
    stop("Argument 'repetitions' must be an integer equal or larger than 5")
  }
  if (repetitions > nrow(xy)) {
    if (verbose == TRUE) {
      message(
        "Argument 'repetitions' larger than number of cases, setting it to the number of cases."
      )
    }
    repetitions <- nrow(xy)
  }

  #capturing user options
  user.options <- options()
  #avoid dplyr messages
  options(dplyr.summarise.inform = FALSE)
  on.exit(options <- user.options)

  #training fraction limits
  if (training.fraction < 0.1) {
    training.fraction <- 0.1
  }
  if (training.fraction > 0.9) {
    training.fraction <- 0.9
  }

  #flipping training fraction if grow.testing.folds is TRUE
  if (grow.testing.folds == TRUE) {
    training.fraction <- 1 - training.fraction
  }

  #add id to data and xy
  data$id <- xy$id <- seq(1, nrow(data))

  #thinning coordinates to get a systematic sample of reference points
  if (verbose == TRUE) {
    message("Selecting pairs of coordinates as trainnig fold origins.")
  }
  xy.reference.records <- thinning_til_n(
    xy = xy,
    n = repetitions,
    distance.step = distance.step
  )

  #generates spatial folds
  ####################################
  if (verbose == TRUE) {
    message("Generating spatial folds.")
  }

  spatial.folds <- make_spatial_folds(
    data = data,
    dependent.variable.name = dependent.variable.name,
    xy.selected = xy.reference.records,
    xy = xy,
    distance.step.x = distance.step.x,
    distance.step.y = distance.step.y,
    training.fraction = training.fraction,
    n.cores = n.cores,
    cluster = cluster
  )

  #flipping spatial folds if grow.testing.folds = TRUE
  if (grow.testing.folds == TRUE) {
    for (i in 1:length(spatial.folds)) {
      names(spatial.folds[[i]]) <- c("testing", "training")
    }
  }

  #copy of ranger arguments for training mdoels
  ranger.arguments.training <- ranger.arguments

  #loop to evaluate models
  #####################################
  evaluation.df <- foreach::foreach(
    i = seq(1, length(spatial.folds), by = 1),
    .combine = "rbind",
    .verbose = FALSE
  ) %dopar%
    {
      #separating training and testing data
      data.training <- data[data$id %in% spatial.folds[[i]]$training, ]
      data.testing <- data[data$id %in% spatial.folds[[i]]$testing, ]

      #subsetting case.weights if definec
      if (!is.null(ranger.arguments.training$case.weights)) {
        ranger.arguments.training$case.weights <- ranger.arguments$case.weights[
          spatial.folds[[i]]$training
        ]
      }

      #training model
      m.training <- spatialRF::rf(
        data = data.training,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        ranger.arguments = ranger.arguments.training,
        seed = seed,
        n.cores = 1,
        cluster = NULL,
        verbose = FALSE
      )

      #predicting over data.testing
      predicted <- stats::predict(
        object = m.training,
        data = data.testing,
        type = "response",
        num.threads = 1
      )$predictions

      #getting observed data
      observed <- data.testing[, dependent.variable.name]

      #computing evaluation scores
      out.df <- data.frame(
        fold.id = i,
        fold.center.x = xy.reference.records[i, "x"],
        fold.center.y = xy.reference.records[i, "y"],
        training.records = nrow(data.training),
        testing.records = nrow(data.testing)
      )

      if ("r.squared" %in% metrics) {
        out.df$training.r.squared = m.training$performance$r.squared
        out.df$testing.r.squared = round(cor(observed, predicted)^2, 3)
      }
      if ("pseudo.r.squared" %in% metrics) {
        out.df$training.pseudo.r.squared = m.training$performance$pseudo.r.squared
        out.df$testing.pseudo.r.squared = round(
          cor(
            observed,
            predicted
          ),
          3
        )
      }
      if ("rmse" %in% metrics) {
        out.df$training.rmse = m.training$performance$rmse
        out.df$testing.rmse = round(
          spatialRF::root_mean_squared_error(
            o = observed,
            p = predicted,
            normalization = NULL
          ),
          3
        )
        if (is.na(out.df$training.rmse)) {
          out.df$testing.rmse <- NA
        }
      }
      if ("nrmse" %in% metrics) {
        out.df$training.nrmse = m.training$performance$nrmse
        out.df$testing.nrmse = round(
          spatialRF::root_mean_squared_error(
            o = observed,
            p = predicted,
            normalization = "iq"
          ),
          3
        )
        if (is.na(out.df$training.nrmse)) {
          out.df$testing.nrmse <- NA
        }
      }
      if ("auc" %in% metrics) {
        out.df$training.auc = m.training$performance$auc
        out.df$testing.auc = round(
          spatialRF::auc(
            o = observed,
            p = predicted
          ),
          3
        )
        if (is.na(out.df$training.auc)) {
          out.df$testing.auc <- NA
        }
      }
      rownames(out.df) <- NULL

      return(out.df)
    } #end of parallelized loop

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

  #getting performance values
  r.squared <- model$performance$r.squared
  pseudo.r.squared <- model$performance$pseudo.r.squared
  rmse <- model$performance$rmse
  nrmse <- model$performance$nrmse
  auc <- model$performance$auc

  #check lengths
  if (length(r.squared) == 0) {
    r.squared <- NA
  }
  if (length(pseudo.r.squared) == 0) {
    pseudo.r.squared <- NA
  }
  if (length(rmse) == 0) {
    rmse <- NA
  }
  if (length(nrmse) == 0) {
    nrmse <- NA
  }
  if (length(auc) == 0) {
    auc <- NA
  }

  #full model
  performance.full <- data.frame(
    r.squared = r.squared,
    pseudo.r.squared = pseudo.r.squared,
    rmse = rmse,
    nrmse = nrmse,
    auc = auc,
    model = "Full"
  )
  performance.full <- performance.full[, c(metrics, "model")]

  #set colnames
  colnames(performance.training) <- colnames(performance.testing) <- colnames(
    performance.full
  ) <- c(
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
      cols = seq(1, length(metrics)),
      names_to = "metric",
      values_to = "value"
    ) %>%
    as.data.frame()

  #aggregating
  performande.df.aggregated <- performance.df.long %>%
    dplyr::group_by(model, metric) %>%
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
    dplyr::ungroup() %>%
    as.data.frame()

  #stats to NA if "Full" only once in performance.df
  if (sum("Full" %in% performance.df$model) == 1) {
    performande.df.aggregated[
      performande.df.aggregated$model == "Full",
      c(
        "median",
        "median_absolute_deviation",
        "q1",
        "q3",
        "se",
        "sd",
        "min",
        "max"
      )
    ] <- NA
  }

  #add spatial folds to the model
  if ("evaluation" %in% names(model)) {
    model$evaluation <- NULL
  }
  model$evaluation <- list()
  model$evaluation$metrics <- metrics
  model$evaluation$training.fraction <- training.fraction
  model$evaluation$spatial.folds <- spatial.folds
  model$evaluation$per.fold <- evaluation.df
  model$evaluation$per.fold.long <- performance.df.long
  model$evaluation$per.model <- performance.df
  model$evaluation$aggregated <- performande.df.aggregated

  if (verbose == TRUE) {
    message("Evaluation results stored in model$evaluation.")
  }

  class(model) <- c(class(model), "rf_evaluate")

  if (verbose == TRUE) {
    print_evaluation(model = model)
  }

  #passing cluster
  if (pass.cluster == TRUE) {
    model$cluster <- cluster
  }

  model
}
