#' @title Evaluates random forest models with spatial cross-validation
#' @description Evaluates the performance of random forest on unseen data over independent spatial folds.
#' @param model (required; model) Fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param xy (required; data frame or matrix) Must have two columns named "x" and "y" containing the geographic coordinates of the sampling locations. Default: `NULL`
#' @param repetitions (optional; integer) Number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the training data. Default: `30`
#' @param training.fraction (optional; numeric) Proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param distance.step (optional; numeric) Numeric vector of length one or two. Distance step used during the growth of the buffer containing the training cases. Must be in the same units as the coordinates in `xy`. When only one distance is provided, the same growth is applied to the x and y axes. If two distances are provided, the first one is applied to the x axis, and the second one to the y. When `NULL`, it uses 1/1000th of the range of each axis as distance. The smaller this number is, the easier is to achieve an accurate `training.fraction`, but the slower the algorithm becomes. Default: `NULL`
#' @param swap.spatial.folds (optional; logical) If true, the cases inside the rectangular buffer are used as testing set instead of training. This can help in edge cases when the data distribution is highly irregular. Default: `FALSE`
#' @param seed (optional; integer) Random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose (optional; logical) If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores (optional; integer) Number of cores used by \code{\link[ranger]{ranger}} for parallel execution (used as value for the argument `num.threads` in `ranger()`). Default: `parallel::detectCores() - 1`
#' @param cluster (optional; cluster object) Cluster definition generated with `parallel::makeCluster()` or [start_cluster()]. Overrides `n.cores`. Faster than using `n.cores` for smaller models. This function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` or [stop_cluster()] at the end of your pipeline. Default: `NULL`
#' @return A model of the class "rf_evaluate" with a new slot named "evaluation". Additionally, spatial cross-validation performance scores are added to the "performance" slot of the model.
#'
#' Objects written to `model$evaluation`:
#' \itemize{
#'   \item `spatial.folds`: list of lists with the training and testing spatial folds used during the evaluation. Each element of the list corresponds to a row in the `per.fold` data frame.
#'   \item `training.fraction`: Value of the argument `training.fraction`.
#'   \item `per.fold`: Data frame with the cross-validation results per spatial fold. It contains the numeric id of each fold, it's central coordinates, the number of training and testing cases, and the training and testing performance metrics.
#'   \item `aggregated`: Aggregated version of `per.fold` with median, median absolute deviation, quartile 1, quartile 3, mean, standard error, standard deviation, minimum, and maximum performance scores across spatial folds.
#'   \item `roc_full`, `roc_oob`, and `roc_scv`: Produced only when the response is binary (values one and zero). Lists of data frames with the in-bag (_full suffix), out-of-bag (_oob), and spatial cross-validation (_scv) confusion matrices, sensitivity and specificity across all spatial folds.
#' }
#'
#' Objects written to `model$performance`:
#' \itemize{
#'   \item
#' }
#' @details The evaluation algorithm works as follows:
#'
#' Generation of contiguous training folds:
#'
#' \itemize{
#'   \item 1. The function [thinning_til_n()] finds a set of coordinates from `xy` of size `repetitions`. These pairs of coordinates are as separated as possible, and will be used as "training fold centers".
#'   \item 2. From each training fold-center, a rectangular buffer is grown from the training fold center one step at a time until it encloses a number of records in `data` as close as possible to the `training.fraction`. The step wise growth of this buffer is controlled by the argument `distance.step`.
#'   \item 3. The indices of all records in `data` within the buffer are written to the "training" vector of the nested list.
#'   \item 4. The indices of the remaining records are written to the "testing" list.
#'   \item 5. In each repetition, a model is fitted with the training cases.
#'   \item 6. The predictions of the model in 5. are compared with the values of the response in the testing records to compute model performance metrics.
#' }
#'
#' If the response variable is continuous, the metrics used are "rsquared", "rmse", and "nrmse" (normalized rmse).
#'
#' If the response is binary (zeros and ones), the metrics used are "auc" (area under the ROC curve), "rsquared" (point-biserial correlation), and "roc" (components of the confusion matrix for different prediction thresholds).
#'
#' In any case, these metrics are identified by the suffix "_scv" (from "spatial cross-validation").
#'
#' The medians of the cross-validation performance metrics are stored in the "performance" slot of the model, while the complete results are stored in the "evaluation" slot.
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
#' @importFrom stats predict mad setNames
#' @importFrom dplyr select contains group_by summarise across
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr
rf_evaluate <- function(
  model = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  distance.step = NULL,
  swap.spatial.folds = FALSE,
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
  roc_oob <- NULL
  roc_scv <- NULL
  metrics <- NULL
  group <- NULL
  id <- NULL

  if(is.null(model)){

    stop("The argument 'model' is empty, there is no model to evaluate.")

  } else {

    #getting data and ranger arguments from the model
    data <- model$ranger_arguments$data
    dependent.variable.name <- model$ranger_arguments$dependent.variable.name
    predictor.variable.names <- model$ranger_arguments$predictor.variable.names
    ranger.arguments <- model$ranger_arguments
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
      if(is.null(model$ranger_arguments$xy)){
        stop("The argument 'xy' is required for spatial cross-validation.")
      } else {
        xy <- model$ranger_arguments$xy
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
    binary_response <- TRUE
  } else {
    binary_response <- FALSE
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

  #add id to data and xy
  data$id <- xy$id <- seq(1, nrow(data))

  #thinning coordinates to get a systematic sample of reference points

  if(verbose == TRUE){
    message("Selecting centers of training folds.")
  }

  fold.centers.df <- thinning_til_n(
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
    i = seq_len(nrow(fold.centers.df)),
    .verbose = FALSE
  ) %iterator% {

    if(is.null(seed)){
      set.seed(i)
    } else {
      set.seed(i + seed)
    }

    #generating spatial folds
    training.testing.folds <- spatialRF::make_spatial_fold(
      data = data,
      dependent.variable.name = dependent.variable.name,
      fold.center = fold.centers.df[i, ],
      xy = xy,
      distance.step = distance.step,
      training.fraction = training.fraction,
      swap.spatial.folds = swap.spatial.folds
    )

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

    #computing performance metrics
    performance.df <- data.frame(
      fold.id = i,
      fold.center.x = fold.centers.df[i, "x"],
      fold.center.y = fold.centers.df[i, "y"],
      training.records = nrow(data.training),
      testing.records = nrow(data.testing)
    )

    #rsquared
    performance.df$rsquared_full <- m.training$performance$rsquared_full
    performance.df$rsquared_oob <- m.training$performance$rsquared_oob
    if(is.null(performance.df$rsquared_full)){
      performance.df$rsquared_scv <- NULL
    } else {
      performance.df$rsquared_scv <- cor(observed, predicted) ^ 2
    }

    performance.df$rmse_full <- m.training$performance$rmse_full
    performance.df$rmse_oob <- m.training$performance$rmse_oob
    if(is.null(performance.df$rmse_full)){
      performance.df$rmse_scv <- NULL
    } else {
      performance.df$rmse_scv <- spatialRF::root_mean_squared_error(
        o = observed,
        p = predicted,
        normalization = "rmse"
      )
    }


    performance.df$nrmse_full <- m.training$performance$nrmse_full
    performance.df$nrmse_oob <- m.training$performance$nrmse_oob
    if(is.null(performance.df$nrmse_full)){
      performance.df$nrmse_scv <- NULL
    } else {
      performance.df$nrmse_scv <- spatialRF::root_mean_squared_error(
        o = observed,
        p = predicted,
        normalization = "iq"
      )
    }

    performance.df$auc_full <- m.training$performance$auc_full
    performance.df$auc_oob <- m.training$performance$auc_oob
    if(is.null(performance.df$auc_full)){
      performance.df$auc_scv <- NULL
    } else {
      performance.df$auc_scv <- spatialRF::auc(
        o = observed,
        p = predicted
      )
    }

    performance.df$biserial_rsquared_full <- m.training$performance$biserial_rsquared_full
    performance.df$biserial_rsquared_oob <- m.training$performance$biserial_rsquared_oob
    if(is.null(performance.df$biserial_rsquared_full)){
      performance.df$biserial_rsquared_scv <- NULL
    } else {
      performance.df$biserial_rsquared_scv <- stats::cor.test(
        x = observed,
        y = predicted
      )$estimate
    }

    rownames(performance.df) <- NULL

    out.list <- list()
    out.list$spatial.folds <- training.testing.folds
    out.list$performance.df <- performance.df
    out.list$roc_full <- m.training$performance$roc_full
    out.list$roc_oob <- m.training$performance$roc_oob
    out.list$roc_scv <- spatialRF::roc_curve(
      o = observed,
      p = predicted
    )

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

  #getting roc curves
  rocs_full <- lapply(
    evaluation.list,
    "[[",
    "roc_full"
  )

  rocs_oob <- lapply(
    evaluation.list,
    "[[",
    "roc_oob"
  )

  rocs_scv <- lapply(
    evaluation.list,
    "[[",
    "roc_scv"
  )

  #copy of results
  performance.df.unique <- performance.df

  #keep distinct values only to avoid very similar spatial folds
  performance.df.unique <- performance.df.unique %>%
    dplyr::distinct(
      dplyr::across(
        dplyr::all_of(
          dplyr::contains("_scv")
          )
        ),
      .keep_all = TRUE
    )

  #subsetting the spatial folds
  spatial.folds <- spatial.folds[performance.df.unique$fold.id]

  #subsetting roc curves
  rocs_full <- rocs_full[performance.df.unique$fold.id]
  rocs_oob <- rocs_oob[performance.df.unique$fold.id]
  rocs_scv <- rocs_scv[performance.df.unique$fold.id]

  #resetting fold ID
  performance.df.unique$fold.id <-
    names(spatial.folds) <-
    names(rocs_full) <-
    names(rocs_oob) <-
    names(rocs_scv) <-
    seq(
      from = 1,
      to = nrow(performance.df.unique),
      by = 1
    )

  #remove columns with NA
  # performance.df.unique <- performance.df.unique[, names(na.omit(colSums(performance.df.unique)))]

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
      cols = dplyr::contains(match = c("_full", "_oob", "_scv")),
      names_to = "metric"
    )

  #adding testing values to performance list
  scv.columns <- colnames(performance.df)[grep(pattern = "scv", x = colnames(performance.df))]
  for(scv.column.i in scv.columns){
    model$performance[[scv.column.i]] <- performance.df.long %>%
      dplyr::filter(
        metric == scv.column.i
      ) %>%
      dplyr::pull(
        value
      ) %>%
      median()
  }

  #adding median of roc curves
  if(!is.nan(rocs_scv[[1]]$sensitivity[1])){
    model$performance$roc_scv <- rocs_scv %>%
      purrr::map_dfr(
        ~stats::setNames(
          .x,
          paste0(
            "a",
            1:ncol(.x)
            )
          ),
        .id = "group"
        ) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(id = 1:dplyr::n()) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(
        dplyr::across(
          -group,
          .fns = median,
          na.rm = TRUE
          )
        ) %>%
      dplyr::select(-id) %>%
      as.data.frame()
    names(model$performance$roc_scv) <- names(rocs_scv[[1]])
  }

  #ordering performance list by name
  model$performance = model$performance[order(names(model$performance))]

  #aggregating
  performance.df.aggregated <- performance.df.long %>%
    dplyr::group_by(metric) %>%
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
  model$evaluation$spatial.folds <- spatial.folds
  model$evaluation$training.fraction <- training.fraction
  model$evaluation$per_fold <- performance.df.long
  model$evaluation$aggregated <- performance.df.aggregated
  if(!is.nan(rocs_scv[[1]]$sensitivity[1])){
    model$evaluation$roc_full <- rocs_full
    model$evaluation$roc_oob <- rocs_oob
    model$evaluation$roc_scv <- rocs_scv
  }

  if(verbose == TRUE){
    message("Full evaluation results stored in model$evaluation.")
    message("Median evaluation scores (identified by the suffix '_scv') stored in model$performance")
  }

  class(model) <- c(class(model), "rf_evaluate")

  #coercing output to tibble
  if(return.tibble == TRUE){

    model$evaluation$per_fold <- tibble::as_tibble(model$evaluation$per_fold)
    model$evaluation$aggregated <- tibble::as_tibble(model$evaluation$aggregated)

    if(!is.null(model$performance$roc_full)){

      model$evaluation$roc_full <- lapply(
        X = rocs_full,
        FUN = tibble::as_tibble
        )

      model$evaluation$roc_oob <- lapply(
        X = roc_oob,
        FUN = tibble::as_tibble
        )

      model$evaluation$roc_scv <- lapply(
        X = roc_scv,
        FUN = tibble::as_tibble
        )

      model$performance$roc_scv <- tibble::as_tibble(model$performance$roc_scv)

    }
  }

  if(verbose == TRUE){
    print_evaluation(model = model)
  }

  model

}
