# R/helpers.R
# Internal helper functions for rf() and related functions
# None of these functions are exported

# Helper 1: validate_rf_inputs() ----

#' Validate inputs for rf() function
#' @param data Data frame with response and predictors
#' @param dependent.variable.name Name of response variable
#' @param predictor.variable.names Names of predictor variables
#' @param distance.matrix Optional distance matrix
#' @return Invisible TRUE if validation passes, stops with error otherwise
#' @noRd
validate_rf_inputs <- function(
  data,
  dependent.variable.name,
  predictor.variable.names,
  distance.matrix = NULL
) {
  # Check predictor names exist in data
  missing_predictors <- predictor.variable.names[
    !predictor.variable.names %in% colnames(data)
  ]
  if (length(missing_predictors) > 0) {
    stop(
      "The predictor.variable.names ",
      paste(missing_predictors, collapse = ", "),
      " are missing from 'data'"
    )
  }

  # Check dependent variable exists
  if (!dependent.variable.name %in% colnames(data)) {
    stop(
      "The dependent.variable.name '",
      dependent.variable.name,
      "' is not a column of 'data'."
    )
  }

  # Check distance matrix dimensions if provided
  if (!is.null(distance.matrix)) {
    if (nrow(distance.matrix) != nrow(data)) {
      stop(
        "distance.matrix rows (", nrow(distance.matrix),
        ") must equal data rows (", nrow(data), ")"
      )
    }
  }

  invisible(TRUE)
}

# Helper 2: build_ranger_arguments() ----

#' Build complete ranger arguments list with defaults
#' @param data Data frame for modeling
#' @param dependent.variable.name Name of response variable
#' @param ranger.arguments User-provided ranger arguments
#' @param n.cores Number of cores for ranger threading
#' @param seed Random seed
#' @return Named list of ranger arguments ready for do.call()
#' @noRd
build_ranger_arguments <- function(
  data,
  dependent.variable.name,
  ranger.arguments = NULL,
  n.cores = NULL,
  seed = NULL
) {

  # Define all ranger defaults in one list
  defaults <- list(
    data = data,
    dependent.variable.name = dependent.variable.name,
    num.trees = 500,
    mtry = NULL,
    importance = "permutation",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    max.depth = NULL,
    replace = TRUE,
    sample.fraction = NULL,  # will be computed below
    case.weights = NULL,
    class.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = TRUE,
    local.importance = TRUE,
    regularization.factor = 1,
    regularization.usedepth = FALSE,
    keep.inbag = FALSE,
    inbag = NULL,
    holdout = FALSE,
    quantreg = FALSE,
    oob.error = TRUE,
    num.threads = NULL,  # will be computed below
    save.memory = FALSE,
    verbose = TRUE,
    seed = seed,
    classification = NULL
  )

  # Merge user arguments (user args override defaults)
  if (!is.null(ranger.arguments)) {
    # Remove spatialRF-specific args that shouldn't go to ranger
    ranger.arguments$n.cores <- NULL
    ranger.arguments$distance.matrix <- NULL
    ranger.arguments$distance.thresholds <- NULL
    ranger.arguments$xy <- NULL

    # Merge user args into defaults
    args <- utils::modifyList(defaults, ranger.arguments)
  } else {
    args <- defaults
  }

  # Handle sample.fraction logic
  if (is.null(args$sample.fraction)) {
    args$sample.fraction <- ifelse(args$replace, 1, 0.632)
  }

  # Handle num.threads logic
  if (is.null(n.cores)) {
    n.cores <- future::availableCores(omit = 1)
  }
  # Auto-disable ranger threading if future plan active
  if (!inherits(future::plan(), "sequential")) {
    n.cores <- 1
  }
  args$num.threads <- n.cores

  return(args)
}

# Helper 3: fit_ranger_model() ----

#' Fit ranger model
#' @param ranger_args Complete list of ranger arguments
#' @param case.weights Optional case weights for binary response
#' @return ranger model object
#' @noRd
fit_ranger_model <- function(
  ranger_args,
  case.weights = NULL
) {

  # Add case weights if computed for binary response
  if (!is.null(case.weights)) {
    ranger_args$case.weights <- case.weights
  }

  # Fit model
  m <- do.call(ranger::ranger, ranger_args)

  return(m)
}

# Helper 4: build_importance_slots() ----

#' Build paired dataframe + plot slots for importance data
#' @param importance_df Data frame with 'variable' and 'importance' columns
#' @param slot_prefix Prefix for slot names (e.g., "per.variable")
#' @param verbose If TRUE, display plot
#' @return List with two elements: dataframe and plot
#' @noRd
build_importance_slots <- function(
  importance_df,
  slot_prefix = "per.variable",
  verbose = TRUE
) {
  result <- list()
  result[[slot_prefix]] <- importance_df
  result[[paste0(slot_prefix, ".plot")]] <- plot_importance(
    importance_df,
    verbose = verbose
  )
  return(result)
}

# Helper 5: compute_performance() ----

#' Compute performance metrics for model
#' @param observed Observed values
#' @param predicted Predicted values
#' @param ranger_r_squared R-squared from ranger (OOB)
#' @param ranger_prediction_error Prediction error from ranger (OOB)
#' @param is.binary Whether response is binary
#' @return List of performance metrics
#' @noRd
compute_performance <- function(
  observed,
  predicted,
  ranger_r_squared,
  ranger_prediction_error,
  is.binary = FALSE
) {

  performance <- list()

  # R-squared metrics
  performance$r.squared.oob <- ranger_r_squared
  performance$r.squared <- collinear::score_r2(
    o = observed,
    p = predicted
  )
  performance$pseudo.r.squared <- abs(
    stats::cor(observed, predicted, use = "complete.obs", method = "pearson")
  )

  # Error metrics
  performance$rmse.oob <- sqrt(ranger_prediction_error)
  performance$rmse <- root_mean_squared_error(
    o = observed,
    p = predicted,
    normalization = "rmse"
  )
  performance$nrmse <- root_mean_squared_error(
    o = observed,
    p = predicted,
    normalization = "iq"
  )

  # AUC for binary responses
  performance$auc <- NA
  if (is.binary) {
    performance$auc <- collinear::score_auc(
      o = observed,
      p = predicted
    )
  }

  return(performance)
}

# Helper 6: compute_residuals_analysis() ----

#' Compute residuals and their analysis
#' @param observed Observed values
#' @param predicted Predicted values
#' @param distance.matrix Optional distance matrix for spatial autocorrelation
#' @param distance.thresholds Optional distance thresholds
#' @param verbose If TRUE, display messages and plots
#' @return List of residuals analysis components
#' @noRd
compute_residuals_analysis <- function(
  observed,
  predicted,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  verbose = TRUE
) {

  residuals_list <- list()

  # Compute residuals
  residuals_list$values <- observed - predicted
  residuals_list$stats <- summary(residuals_list$values)

  # Spatial autocorrelation (if distance matrix provided)
  if (!is.null(distance.matrix)) {
    residuals_list$autocorrelation <- moran_multithreshold(
      x = residuals_list$values,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      verbose = verbose
    )
  }

  # Normality diagnostics
  residuals_list$normality <- residuals_diagnostics(
    residuals = residuals_list$values,
    predictions = predicted
  )

  return(residuals_list)
}
