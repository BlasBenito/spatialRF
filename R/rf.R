#' @title Random forest models with Moran's I test of the residuals
#' @description Fits a random forest model using \link[ranger]{ranger} and extends it with spatial diagnostics: residual autocorrelation (Moran's I) at multiple distance thresholds, performance metrics (RMSE, NRMSE via [root_mean_squared_error()]), and variable importance scores computed on scaled data (via \link[base]{scale}).
#' @param data Data frame or sf object with response variable and predictors. If sf object: coordinates are automatically extracted from geometry. If regular data frame with coordinate columns (x/y, lon/lat, longitude/latitude): coordinates are automatically detected. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be a column name in `data`. For binary response variables (0/1), case weights are automatically computed using [collinear::case_weights()] to balance classes. Default: `NULL`
#' @param predictor.variable.names Character vector with predictor variable names. All names must be columns in `data`. Default: `NULL`
#' @param distance.matrix Square matrix with pairwise distances between observations in `data`. Must have the same number of rows as `data`. If `NULL`, spatial autocorrelation of residuals is not computed. Default: `NULL`
#' @param distance.thresholds Numeric vector of distance thresholds for spatial autocorrelation analysis. For each threshold, distances below that value are set to zero when computing Moran's I. If `NULL`, defaults to `seq(0, max(distance.matrix), length.out = 4)`. Default: `NULL`
#' @param xy (optional) Data frame with columns "x" and "y" containing coordinates. If NULL and data is sf or has detectable coordinate columns, coordinates are extracted automatically. When provided, overrides automatic extraction. Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments. Arguments for this function can also be passed here. The default importance method is 'permutation' instead of ranger's default 'none'. The `x`, `y`, and `formula` arguments are not supported. See \link[ranger]{ranger} help for available arguments. Default: `NULL`
#' @param seed Random seed for reproducibility. Default: `1`
#' @param verbose If `TRUE`, display messages and plots during execution. Default: `TRUE`
#' @param n.cores Integer. Number of threads for ranger's internal parallelization. Default: `NULL` (auto-detected as `future::availableCores(omit = 1)`). Set to 1 for debugging or when using a parallel plan for R-level parallelization
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
  seed = 1,
  verbose = TRUE,
  n.cores = NULL
) {
  # 1. Extract arguments from ranger.arguments if needed
  if (is.null(data) && !is.null(ranger.arguments$data)) {
    data <- ranger.arguments$data
  }
  if (is.null(dependent.variable.name) && !is.null(ranger.arguments$dependent.variable.name)) {
    dependent.variable.name <- ranger.arguments$dependent.variable.name
  }
  if (is.null(predictor.variable.names) && !is.null(ranger.arguments$predictor.variable.names)) {
    predictor.variable.names <- ranger.arguments$predictor.variable.names
  }

  # 2. Handle ranger.arguments priority (remove extracted values)
  if (!is.null(ranger.arguments)) {
    ranger.arguments$data <- NULL
    ranger.arguments$dependent.variable.name <- NULL
    ranger.arguments$predictor.variable.names <- NULL
  }

  # 3. Coerce tibbles to data frames
  if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
    data <- as.data.frame(data)
  }
  if (inherits(xy, "tbl_df") || inherits(xy, "tbl")) {
    xy <- as.data.frame(xy)
  }

  # 3.5. Extract coordinates from data if needed
  xy_extraction <- extract_xy_from_data(data = data, xy = xy, require_id = FALSE)
  data <- xy_extraction$data  # geometry dropped if sf
  if (!is.null(xy_extraction$xy)) {
    xy <- xy_extraction$xy
  }

  # 4. Validate inputs
  validate_rf_inputs(data, dependent.variable.name, predictor.variable.names, distance.matrix)

  # 5. Subset data to relevant columns
  data <- data[, c(dependent.variable.name, predictor.variable.names)]

  # 6. Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # 7. Compute case weights for binary response
  is.binary <- is_binary(data, dependent.variable.name)
  case.weights <- NULL
  if (is.binary) {
    case.weights <- collinear::case_weights(data[[dependent.variable.name]])
  }

  # 8. Build ranger arguments
  ranger_args <- build_ranger_arguments(
    data, dependent.variable.name, ranger.arguments, n.cores, seed
  )

  # 9. Fit ranger model
  m <- fit_ranger_model(ranger_args, case.weights)

  # 10. Add metadata slots
  m$ranger.arguments <- ranger_args
  m$ranger.arguments$predictor.variable.names <- predictor.variable.names
  if (!is.null(distance.matrix)) {
    m$distance.matrix <- distance.matrix
  }
  if (!is.null(distance.thresholds)) {
    m$distance.thresholds <- distance.thresholds
  }
  if (!is.null(xy)) {
    m$xy <- xy
  }

  # 11. Process importance
  if (ranger_args$importance == "permutation") {
    # Transform global importance (sqrt with sign preservation)
    sign_vector <- ifelse(m$variable.importance >= 0, 1, -1)
    transformed <- sqrt(abs(m$variable.importance)) * sign_vector

    per.variable.df <- data.frame(
      variable = names(transformed),
      importance = round(transformed, 3),
      stringsAsFactors = FALSE
    )
    rownames(per.variable.df) <- NULL
    per.variable.df <- per.variable.df[
      order(per.variable.df$importance, decreasing = TRUE),
    ]

    # Use helper to create importance slots with plot
    m$importance <- build_importance_slots(per.variable.df, "per.variable", verbose)

    # Add local importance
    sign_matrix <- ifelse(m$variable.importance.local >= 0, 1, -1)
    m$importance$local <- sqrt(abs(m$variable.importance.local)) * sign_matrix
  }

  # 12. Compute predictions
  predicted <- stats::predict(m, data = data, type = "response")$predictions
  m$predictions <- list(values = predicted)
  observed <- data[[dependent.variable.name]]

  # 13. Compute performance metrics
  m$performance <- compute_performance(
    observed, predicted, m$r.squared, m$prediction.error, is.binary
  )
  # Remove names from rmse and nrmse for backwards compatibility
  names(m$performance$rmse) <- NULL
  names(m$performance$nrmse) <- NULL

  # 14. Compute residuals analysis
  m$residuals <- compute_residuals_analysis(
    observed, predicted, distance.matrix, distance.thresholds, verbose
  )

  # 15. Plot residuals diagnostics
  m$residuals$diagnostics <- plot_residuals_diagnostics(m, verbose = verbose)

  # 16. Set class and print
  class(m) <- c("rf", "ranger")
  if (verbose) {
    print(m)
  }

  # Return model
  m
}
