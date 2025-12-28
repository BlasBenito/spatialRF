#' @title Random forest models with Moran's I test of the residuals
#' @description Fits a random forest model using \link[ranger]{ranger} and extends it with spatial diagnostics: residual autocorrelation (Moran's I) at multiple distance thresholds, performance metrics (RMSE, NRMSE via [root_mean_squared_error()]), and variable importance scores computed on scaled data (via \link[base]{scale}).
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be a column name in `data`. For binary response variables (0/1), case weights are automatically computed using [case_weights()] to balance classes. Default: `NULL`
#' @param predictor.variable.names Character vector with predictor variable names. All names must be columns in `data`. Alternatively, accepts the output of [auto_cor()] or [auto_vif()] for automated variable selection. Default: `NULL`
#' @param distance.matrix Square matrix with pairwise distances between observations in `data`. Must have the same number of rows as `data`. If `NULL`, spatial autocorrelation of residuals is not computed. Default: `NULL`
#' @param distance.thresholds Numeric vector of distance thresholds for spatial autocorrelation analysis. For each threshold, distances below that value are set to zero when computing Moran's I. If `NULL`, defaults to `seq(0, max(distance.matrix), length.out = 4)`. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates, named "x" and "y". Not used by this function but stored in the model for use by [rf_evaluate()] and [rf_tuning()]. Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments. Arguments for this function can also be passed here. The default importance method is 'permutation' instead of ranger's default 'none'. The `x`, `y`, and `formula` arguments are not supported. See \link[ranger]{ranger} help for available arguments. Default: `NULL`
#' @param scaled.importance If `TRUE`, variable importance is computed on scaled data using \link[base]{scale}, making importance scores comparable across models with different predictor units. Default: `FALSE`
#' @param seed Random seed for reproducibility. Default: `1`
#' @param verbose If `TRUE`, display messages and plots during execution. Default: `TRUE`
#' @param n.cores Number of cores for parallel execution. Default: `parallel::detectCores() - 1`
#' @param cluster Cluster object from `parallel::makeCluster()`. Not used by this function but stored in the model for use in downstream functions. Default: `NULL`
#' @return A ranger model object with additional slots:
#' \itemize{
#'   \item `ranger.arguments`: Arguments used to fit the model.
#'   \item `importance`: List with global importance data frame (predictors ranked by importance), importance plot, and local importance scores (per-observation difference in accuracy between permuted and non-permuted predictors, based on out-of-bag data).
#'   \item `performance`: Model performance metrics including R-squared (out-of-bag and standard), pseudo R-squared, RMSE, and NRMSE.
#'   \item `residuals`: Model residuals with normality diagnostics ([residuals_diagnostics()]) and spatial autocorrelation ([moran_multithreshold()]).
#' }
#' @details See \link[ranger]{ranger} documentation for additional details. The `formula` interface is supported via `ranger.arguments`, but variable interactions are not permitted. For feature engineering including interactions, see [the_feature_engineer()].
#' @examples
#'
#' data(
#'   plants_df,
#'   plants_response,
#'   plants_predictors,
#'   plants_distance
#' )
#'
#' m <- rf(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors,
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(100, 1000, 2000),
#'   ranger.arguments = list(
#'     num.trees = 50,
#'     min.node.size = 20
#'   ),
#'   verbose = FALSE,
#'   n.cores = 1
#' )
#'
#' class(m)
#' #variable importance
#' m$importance$per.variable
#' m$importance$per.variable.plot
#'
#' #model performance
#' m$performance
#'
#' #autocorrelation of residuals
#' m$residuals$autocorrelation$per.distance
#' m$residuals$autocorrelation$plot
#'
#' #model predictions
#' m$predictions$values
#'
#' #predictions for new data (using stats::predict)
#' y <- stats::predict(
#'   object = m,
#'   data = plants_df[1:5, ],
#'   type = "response"
#' )$predictions
#'
#' #alternative: pass arguments via ranger.arguments list
#' args <- list(
#'   data = plants_df,
#'   dependent.variable.name = plants_response,
#'   predictor.variable.names = plants_predictors,
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(100, 1000, 2000),
#'   num.trees = 50,
#'   min.node.size = 20,
#'   num.threads = 1
#' )
#'
#' m <- rf(
#'   ranger.arguments = args,
#'   verbose = FALSE
#' )
#' @importFrom ranger ranger
#' @rdname rf
#' @family main_models
#' @export
#' @autoglobal
rf <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  xy = NULL,
  ranger.arguments = NULL,
  scaled.importance = FALSE,
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
) {
  #giving priority to data not from ranger.arguments
  if (!is.null(data) && !is.null(ranger.arguments)) {
    ranger.arguments$data <- NULL
    ranger.arguments$dependent.variable.name <- NULL
    ranger.arguments$predictor.variable.names <- NULL
  }

  #default ranger arguments
  num.trees <- 500
  mtry <- NULL
  mtry <- NULL
  importance <- "permutation"
  write.forest <- TRUE
  probability <- FALSE
  min.node.size <- NULL
  max.depth <- NULL
  replace <- TRUE
  sample.fraction <- ifelse(replace, 1, 0.632)
  case.weights <- NULL
  class.weights <- NULL
  splitrule <- NULL
  num.random.splits <- 1
  alpha <- 0.5
  minprop <- 0.1
  split.select.weights <- NULL
  always.split.variables <- NULL
  respect.unordered.factors <- NULL
  scale.permutation.importance <- TRUE
  local.importance <- TRUE
  regularization.factor <- 1
  regularization.usedepth <- FALSE
  keep.inbag <- FALSE
  inbag <- NULL
  holdout <- FALSE
  quantreg <- FALSE
  oob.error <- TRUE
  num.threads <- n.cores
  save.memory <- FALSE
  classification <- NULL

  #putting ranger arguments in the environment
  if (!is.null(ranger.arguments)) {
    list2env(ranger.arguments, envir = environment())
  }

  #coerce to data frame if tibble
  if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
    data <- as.data.frame(data)
  }

  if (inherits(xy, "tbl_df") || inherits(xy, "tbl")) {
    xy <- as.data.frame(xy)
  }

  #predictor.variable.names comes from auto_vif or auto_cor
  if (inherits(predictor.variable.names, "variable_selection")) {
    predictor.variable.names <- predictor.variable.names$selected.variables
  } else {
    if (
      sum(predictor.variable.names %in% colnames(data)) <
        length(predictor.variable.names)
    ) {
      stop(
        paste0(
          "The predictor.variable.names ",
          paste0(
            predictor.variable.names[
              !(predictor.variable.names %in% colnames(data))
            ],
            collapse = ", "
          ),
          " are missing from 'data'"
        )
      )
    }
  }

  #checking if dependent.variable.name and predictor.variable.names are in colnames(data) and are numeric
  if (!(dependent.variable.name %in% colnames(data))) {
    stop(
      paste0(
        "The dependent.variable.name ",
        dependent.variable.name,
        " is not a column of 'data'."
      )
    )
  }

  #subset data
  data <- data[, c(dependent.variable.name, predictor.variable.names)]

  #setting up seed if available
  if (!is.null(seed)) {
    set.seed(seed)
  }

  #scaling the data if required
  if (scaled.importance) {
    data.scaled <- as.data.frame(scale(x = data))

    #check if there are NaN
    if (
      sum(apply(data.scaled, 2, is.nan)) > 0 ||
        sum(apply(data.scaled, 2, is.infinite)) > 0
    ) {
      scaled.importance <- FALSE
      warning(
        "The training data yields NaN or Inf when scaled, setting scaled.importance to FALSE."
      )
    }
  }

  #computing case weights if dependent.variable.name is binary
  is.binary <- is_binary(
    data = data,
    dependent.variable.name = dependent.variable.name
  )
  if (is.binary && is.null(case.weights)) {
    case.weights <- case_weights(
      data = data,
      dependent.variable.name = dependent.variable.name
    )
  }

  #ranger model for r-squared and predictions
  m <- ranger::ranger(
    data = data,
    dependent.variable.name = dependent.variable.name,
    num.trees = num.trees,
    mtry = mtry,
    importance = importance,
    write.forest = write.forest,
    probability = probability,
    min.node.size = min.node.size,
    max.depth = max.depth,
    replace = replace,
    sample.fraction = sample.fraction,
    case.weights = case.weights,
    class.weights = class.weights,
    splitrule = splitrule,
    num.random.splits = num.random.splits,
    alpha = alpha,
    minprop = minprop,
    split.select.weights = split.select.weights,
    always.split.variables = always.split.variables,
    respect.unordered.factors = respect.unordered.factors,
    scale.permutation.importance = scale.permutation.importance,
    local.importance = local.importance,
    regularization.factor = regularization.factor,
    regularization.usedepth = regularization.usedepth,
    keep.inbag = keep.inbag,
    inbag = inbag,
    holdout = holdout,
    quantreg = quantreg,
    oob.error = oob.error,
    num.threads = num.threads,
    save.memory = save.memory,
    verbose = verbose,
    seed = seed,
    classification = classification
  )

  #get variable importance
  variable.importance.global <- m$variable.importance
  variable.importance.local <- m$variable.importance.local

  #if scaled.importance is TRUE
  if (scaled.importance) {
    #ranger model for variable importance
    m.scaled <- ranger::ranger(
      data = data.scaled,
      dependent.variable.name = dependent.variable.name,
      num.trees = num.trees,
      mtry = mtry,
      importance = importance,
      write.forest = write.forest,
      probability = probability,
      min.node.size = min.node.size,
      max.depth = max.depth,
      replace = replace,
      sample.fraction = sample.fraction,
      case.weights = case.weights,
      class.weights = class.weights,
      splitrule = splitrule,
      num.random.splits = num.random.splits,
      alpha = alpha,
      minprop = minprop,
      split.select.weights = split.select.weights,
      always.split.variables = always.split.variables,
      respect.unordered.factors = respect.unordered.factors,
      scale.permutation.importance = FALSE,
      local.importance = local.importance,
      regularization.factor = regularization.factor,
      regularization.usedepth = regularization.usedepth,
      keep.inbag = keep.inbag,
      inbag = inbag,
      holdout = holdout,
      quantreg = quantreg,
      oob.error = oob.error,
      num.threads = num.threads,
      save.memory = save.memory,
      verbose = verbose,
      seed = seed,
      classification = classification
    )

    #overwrite variable importance
    variable.importance.global <- m.scaled$variable.importance
    variable.importance.local <- m.scaled$variable.importance.local
  }

  #adding model arguments
  m$ranger.arguments <- list(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    xy = xy,
    num.trees = num.trees,
    mtry = mtry,
    importance = importance,
    scaled.importance = scaled.importance,
    write.forest = write.forest,
    probability = probability,
    min.node.size = min.node.size,
    max.depth = max.depth,
    replace = replace,
    sample.fraction = sample.fraction,
    case.weights = case.weights,
    class.weights = class.weights,
    splitrule = splitrule,
    num.random.splits = num.random.splits,
    alpha = alpha,
    minprop = minprop,
    split.select.weights = split.select.weights,
    always.split.variables = always.split.variables,
    respect.unordered.factors = respect.unordered.factors,
    scale.permutation.importance = scale.permutation.importance,
    local.importance = local.importance,
    regularization.factor = regularization.factor,
    regularization.usedepth = regularization.usedepth,
    keep.inbag = keep.inbag,
    inbag = inbag,
    holdout = holdout,
    quantreg = quantreg,
    oob.error = oob.error,
    num.threads = num.threads,
    save.memory = save.memory,
    seed = seed,
    classification = classification
  )

  #importance slot
  if (importance == "permutation") {
    #importance slot
    m$importance <- list()

    #global importance
    #sign of the importance
    variable.importance.global.sign <- variable.importance.global
    variable.importance.global.sign[variable.importance.global.sign >= 0] <- 1
    variable.importance.global.sign[variable.importance.global.sign < 0] <- -1

    #applying sqrt
    variable.importance.global <- sqrt(abs(variable.importance.global)) *
      variable.importance.global.sign

    m$importance$per.variable <- data.frame(
      variable = names(variable.importance.global),
      importance = variable.importance.global,
      stringsAsFactors = FALSE
    )
    rownames(m$importance$per.variable) <- NULL

    m$importance$per.variable <- m$importance$per.variable[
      order(m$importance$per.variable$importance, decreasing = TRUE),
    ]

    m$importance$per.variable$importance <- round(
      m$importance$per.variable$importance, 3
    )

    m$importance$per.variable.plot <- plot_importance(
      m$importance$per.variable,
      verbose = verbose
    )

    #local importance (reconverting values from ^2 to sqrt())

    #matrix with sign of the value
    variable.importance.local.sign <- variable.importance.local
    variable.importance.local.sign[variable.importance.local.sign >= 0] <- 1
    variable.importance.local.sign[variable.importance.local.sign < 0] <- -1

    #applying sqrt
    variable.importance.local <- sqrt(abs(variable.importance.local)) *
      variable.importance.local.sign

    #saving to the slot
    m$importance$local <- variable.importance.local
  }

  #computing predictions
  predicted <- stats::predict(
    object = m,
    data = data,
    type = "response"
  )$predictions

  #saving predictions
  m$predictions <- list()
  m$predictions$values <- predicted

  #getting observed data
  observed <- data[, dependent.variable.name]

  #performance slot
  m$performance <- list()

  m$performance$r.squared.oob <- m$r.squared

  m$performance$r.squared <- stats::cor(observed, predicted)^2

  m$performance$pseudo.r.squared <- stats::cor(
    observed,
    predicted
  )

  m$performance$rmse.oob <- sqrt(m$prediction.error)

  m$performance$rmse <- root_mean_squared_error(
    o = observed,
    p = predicted,
    normalization = "rmse"
  )
  names(m$performance$rmse) <- NULL
  m$performance$nrmse <- root_mean_squared_error(
    o = observed,
    p = predicted,
    normalization = "iq"
  )
  names(m$performance$nrmse) <- NULL
  m$performance$auc <- NA

  #compute AUC
  m$performance$auc <- auc(
    o = observed,
    p = predicted
  )

  #residuals
  m$residuals$values <- observed - predicted
  m$residuals$stats <- summary(m$residuals$values)

  #compute moran I of residuals if distance.matrix is provided
  if (!is.null(distance.matrix)) {
    m$residuals$autocorrelation <- moran_multithreshold(
      x = m$residuals$values,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      verbose = verbose
    )
  }

  #normality of the residuals
  m$residuals$normality <- residuals_diagnostics(
    residuals = m$residuals$values,
    predictions = predicted
  )

  #plot of the residuals diagnostics
  m$residuals$diagnostics <- plot_residuals_diagnostics(
    m,
    verbose = verbose
  )

  #adding cluster
  if (inherits(x = cluster, what = "cluster")) {
    m$cluster <- cluster
  }

  #adding rf class
  class(m) <- c("rf", "ranger")

  if (verbose) {
    print(m)
  }

  #return model
  m
}
