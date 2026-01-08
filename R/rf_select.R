#' @title Select optimal predictor subset via spatial cross-validation
#' @description Identifies the optimal subset of predictors that maximizes model transferability by combining multicollinearity filtering, predictor ranking via leave-one-out spatial cross-validation, and forward greedy selection. Returns a model with fewer predictors, better out-of-sample performance, and reduced multicollinearity.
#' @param model Model fitted with [rf()]. Does not work with models from [rf_spatial()] or [rf_repeat()]. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function attempts to extract coordinates from the model. Default: `NULL`
#' @param collinear_threshold Numeric between 0 and 1. R-squared threshold for multicollinearity filtering. Predictors with pairwise R² above this threshold are considered collinear. Converted internally to VIF threshold for [collinear::collinear()]. Default: `0.75` (moderate filtering, VIF ≈ 2.3)
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param metric Character, name of the performance metric to use for ranking and selection. Possible values: "r.squared" (`cor(obs, pred) ^ 2`), "pseudo.r.squared" (`cor(obs, pred)`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`), and "auc" (only for binary responses with values 1 and 0). Default: `"r.squared"`
#' @param distance.step Numeric. Distance step used for (1) selection of fold centers via [thinning_til_n()], and (2) proportional buffer growth in [make_spatial_folds()]. Default: `NULL` (auto-calculated)
#' @param distance.step.x `r lifecycle::badge("deprecated")` Use distance.step instead.
#' @param distance.step.y `r lifecycle::badge("deprecated")` Use distance.step instead.
#' @param weight.r.squared Numeric between 0 and 1, weight of training R-squared in the optimization score. Higher values prioritize in-sample performance. Default: `0.75`
#' @param weight.penalization.n.predictors Numeric between 0 and 1, weight of the parsimony penalty in the optimization score. Higher values favor models with fewer predictors. Default: `0.25`
#' @param seed Integer, random seed to facilitate reproducibility. Default: `1`
#' @param verbose Logical. If `TRUE`, messages and plots generated during execution are displayed. Default: `TRUE`
#' @param n.cores Integer. Number of threads for ranger's internal parallelization. Default: `NULL` (auto-detected: if user has set a parallel plan via `future::plan()`, defaults to 1; otherwise defaults to `future::availableCores(omit = 1)`). When using a parallel plan, setting n.cores > 1 may cause oversubscription.
#' @return The input model or a new model with optimized predictors, with a new `selection` slot containing:
#' \itemize{
#'   \item `method`: Character string describing the selection method used
#'   \item `collinear_threshold`: The R² threshold used for collinearity filtering
#'   \item `metric`: The performance metric used for optimization
#'   \item `predictors`: List with predictor names at each stage (original, after_collinearity, ranked, selected, final)
#'   \item `ranking`: Data frame with predictor importance scores from leave-one-out CV
#'   \item `optimization`: Data frame tracking performance metrics across forward selection iterations
#'   \item `optimization.plot`: ggplot object visualizing the optimization trajectory
#'   \item `comparison`: Data frame comparing original vs selected model performance
#'   \item `used_selected`: Logical indicating whether the selected model was returned (TRUE) or the original model was kept (FALSE)
#' }
#' @details
#' This function implements a three-step pipeline for predictor selection:
#'
#' **Step 1: Multicollinearity filtering**
#'
#' Removes redundant predictors using [collinear::collinear()] with VIF-based filtering. The `collinear_threshold` parameter is specified as R² (more intuitive) and converted internally to VIF threshold.
#'
#' **Step 2: Ranking by transferability**
#'
#' Ranks remaining predictors using a leave-one-out approach with spatial cross-validation. For each predictor, fits a model without that predictor, evaluates it via spatial CV, and computes importance as the median performance difference across folds. Only predictors with positive contribution are retained for selection.
#'
#' **Step 3: Forward greedy selection**
#'
#' Sequentially adds ranked predictors one-by-one, evaluating each candidate set via spatial CV. Computes an optimization score balancing testing performance, training R², and parsimony. Selects the predictor subset that maximizes this score.
#'
#' **Step 4: Model comparison**
#'
#' Compares the selected model against the original via spatial CV. If the selected model outperforms the original, returns it; otherwise returns the original model with selection diagnostics stored in the `$selection` slot.
#'
#' **Computational cost:**
#'
#' This function is computationally expensive. For P predictors after collinearity filtering, it fits approximately 2P models, each evaluated with `repetitions` spatial folds. Parallelization via [future::plan()] is strongly recommended. Expected runtime for 10 predictors with 30 folds: ~10 minutes (sequential), ~4 minutes (4 workers).
#'
#' **When to use:**
#'
#' Use `rf_select()` when you want to optimize predictor selection for model transferability. This is most valuable when:
#' \itemize{
#'   \item You have many potentially redundant predictors
#'   \item Model parsimony is important
#'   \item Out-of-sample performance matters more than in-sample fit
#'   \item You can afford the computational cost
#' }
#'
#' For diagnostic analysis of predictor importance without refitting, use [rf_importance()] instead.
#'
#' @examples
#' \dontrun{
#' data(plants_rf, plants_xy)
#'
#' # Enable parallel execution for speed
#' future::plan(future::multisession, workers = 4)
#'
#' # Select optimal predictor subset
#' m_selected <- rf_select(
#'   model = plants_rf,
#'   xy = plants_xy,
#'   collinear_threshold = 0.75,
#'   repetitions = 30,
#'   metric = "r.squared",
#'   verbose = TRUE
#' )
#'
#' # Check if selection improved the model
#' m_selected$selection$comparison
#'
#' # View selected predictors
#' m_selected$selection$predictors$selected
#'
#' # Examine optimization trajectory
#' m_selected$selection$optimization.plot
#'
#' # Reset parallel plan
#' future::plan(future::sequential)
#' }
#' @rdname rf_select
#' @family model_workflow
#' @export
#' @autoglobal
rf_select <- function(
  model = NULL,
  xy = NULL,
  collinear_threshold = 0.75,
  repetitions = 30,
  training.fraction = 0.75,
  metric = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  weight.r.squared = 0.75,
  weight.penalization.n.predictors = 0.25,
  seed = 1,
  verbose = TRUE,
  n.cores = NULL
) {

  # Validate metric argument
  metric <- match.arg(
    arg = metric,
    choices = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
    several.ok = FALSE
  )

  # Handle deprecated arguments
  if (!is.null(distance.step.x) || !is.null(distance.step.y)) {
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "rf_select(distance.step.x)",
      with = "rf_select(distance.step)"
    )
    lifecycle::deprecate_warn(
      when = "1.3.0",
      what = "rf_select(distance.step.y)",
      with = "rf_select(distance.step)"
    )
  }

  # Validate model is provided
  if (is.null(model)) {
    stop("Argument 'model' is required. Please provide a model fitted with rf().")
  }

  # Check model is not spatial
  if (inherits(model, "rf_spatial") || !is.null(model$spatial)) {
    stop(
      "rf_select() only works with non-spatial models fitted with rf().\n",
      "The input model appears to contain spatial predictors.\n",
      "Please use a model fitted with rf() instead."
    )
  }

  # Extract data from model
  if (is.null(model$ranger.arguments)) {
    stop("Model does not contain ranger.arguments. Was it fitted with rf()?")
  }

  data <- model$ranger.arguments$data
  dependent.variable.name <- model$ranger.arguments$dependent.variable.name
  predictor.variable.names <- model$ranger.arguments$predictor.variable.names

  if (is.null(data) || is.null(dependent.variable.name) || is.null(predictor.variable.names)) {
    stop("Model is missing required data or variable names. Was it fitted with rf()?")
  }

  # Drop geometry if sf
  data <- drop_geometry_if_sf(data)

  # Validate minimum predictors
  if (length(predictor.variable.names) < 2) {
    stop("rf_select() requires at least 2 predictors for meaningful selection.")
  }

  # Handle xy coordinates
  if (is.null(xy)) {
    # Try to extract from model
    if (!is.null(model$ranger.arguments$xy)) {
      xy <- model$ranger.arguments$xy
    } else if (all(c("x", "y") %in% colnames(data))) {
      xy <- data[, c("x", "y")]
    } else {
      stop(
        "Argument 'xy' is required for spatial cross-validation.\n",
        "Please provide coordinates as a data frame with columns 'x' and 'y'."
      )
    }
  }

  # Validate xy
  if (!all(c("x", "y") %in% colnames(xy))) {
    stop("xy must have columns named 'x' and 'y'.")
  }
  if (nrow(xy) != nrow(data)) {
    stop("Number of rows in xy (", nrow(xy), ") must equal number of rows in data (", nrow(data), ").")
  }

  # Validate collinear_threshold
  if (!is.numeric(collinear_threshold) || collinear_threshold <= 0 || collinear_threshold >= 1) {
    stop("collinear_threshold must be numeric between 0 and 1.")
  }

  # Validate repetitions
  if (repetitions < 5) {
    stop("repetitions must be at least 5.")
  }
  if (repetitions > nrow(data)) {
    stop("repetitions (", repetitions, ") cannot exceed number of rows in data (", nrow(data), ").")
  }

  # Validate training.fraction
  if (training.fraction < 0.5 || training.fraction > 0.9) {
    stop("training.fraction must be between 0.5 and 0.9.")
  }

  # Setup ranger arguments
  ranger.arguments <- model$ranger.arguments
  ranger.arguments$data <- NULL
  ranger.arguments$dependent.variable.name <- NULL
  ranger.arguments$predictor.variable.names <- NULL
  ranger.arguments$importance <- "none"
  ranger.arguments$local.importance <- FALSE

  # STEP 1: COLLINEARITY FILTERING
  if (verbose) {
    message("\n=== Step 1/3: Filtering collinear predictors ===")
  }

  predictor_data <- data[, predictor.variable.names, drop = FALSE]

  # Convert R² threshold to VIF
  max_vif <- 1 / (1 - collinear_threshold^2)

  # Call collinear::collinear()
  filtered <- collinear::collinear(
    df = predictor_data,
    max_vif = max_vif
  )

  predictors_after_collinearity <- filtered$result$selection

  if (length(predictors_after_collinearity) == 0) {
    stop(
      "All predictors removed by collinearity filtering.\n",
      "Try reducing collinear_threshold (current: ", collinear_threshold, ")."
    )
  }

  if (length(predictors_after_collinearity) < 2) {
    warning(
      "Only ", length(predictors_after_collinearity), " predictor(s) remain after collinearity filtering.\n",
      "Selection requires at least 2 predictors. Returning original model with diagnostics."
    )
    return(fallback_result_rf_select(
      model = model,
      reason = "insufficient_predictors",
      predictors_original = predictor.variable.names,
      predictors_after_collinearity = predictors_after_collinearity,
      collinear_threshold = collinear_threshold,
      metric = metric
    ))
  }

  if (verbose) {
    n_removed <- length(predictor.variable.names) - length(predictors_after_collinearity)
    message("Removed ", n_removed, " collinear predictor(s)")
    message("Remaining: ", paste(predictors_after_collinearity, collapse = ", "))
  }

  # STEP 2: RANKING BY TRANSFERABILITY
  if (verbose) {
    message("\n=== Step 2/3: Ranking predictors by contribution to transferability ===")
  }

  # Fit and evaluate full model with filtered predictors
  model_filtered <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictors_after_collinearity,
    distance.matrix = model$ranger.arguments$distance.matrix,
    distance.thresholds = model$ranger.arguments$distance.thresholds,
    xy = xy,
    ranger.arguments = ranger.arguments,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores
  )

  model_filtered <- rf_evaluate(
    model = model_filtered,
    xy = xy,
    repetitions = repetitions,
    training.fraction = training.fraction,
    distance.step = distance.step,
    distance.step.x = distance.step.x,
    distance.step.y = distance.step.y,
    metrics = metric,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores
  )

  # Extract baseline performance
  testing_cols <- grep(
    "testing.",
    colnames(model_filtered$evaluation$per.fold),
    fixed = TRUE,
    value = TRUE
  )
  evaluation.df <- model_filtered$evaluation$per.fold[,
    c("fold.id", testing_cols),
    drop = FALSE
  ]
  evaluation.df <- evaluation.df[,
    colnames(evaluation.df) != "testing.records",
    drop = FALSE
  ]
  colnames(evaluation.df)[2] <- "with"

  # Setup progressor for leave-one-out
  p <- progressr::progressor(along = seq_len(length(predictors_after_collinearity)))

  if (verbose) {
    message("Evaluating leave-one-out models (", length(predictors_after_collinearity), " predictors)...")
  }

  # Parallel leave-one-out evaluation
  evaluation.list <- future.apply::future_lapply(
    X = predictors_after_collinearity,
    FUN = function(predictor.i) {
      # Remove predictor.i
      predictors.i <- predictors_after_collinearity[
        predictors_after_collinearity != predictor.i
      ]

      # Fit and evaluate model without predictor.i
      model.i <- rf(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictors.i,
        distance.matrix = model$ranger.arguments$distance.matrix,
        distance.thresholds = model$ranger.arguments$distance.thresholds,
        xy = xy,
        ranger.arguments = ranger.arguments,
        seed = seed,
        verbose = FALSE,
        n.cores = n.cores
      )

      model.i <- rf_evaluate(
        model = model.i,
        xy = xy,
        repetitions = repetitions,
        training.fraction = training.fraction,
        distance.step = distance.step,
        distance.step.x = distance.step.x,
        distance.step.y = distance.step.y,
        metrics = metric,
        seed = seed,
        verbose = FALSE,
        n.cores = n.cores
      )

      # Extract performance without predictor.i
      testing_cols_i <- grep("testing.", colnames(model.i$evaluation$per.fold), fixed = TRUE, value = TRUE)
      testing_cols_i <- testing_cols_i[testing_cols_i != "testing.records"]

      eval_df_i <- data.frame(
        variable = predictor.i,
        fold.id = model.i$evaluation$per.fold$fold.id,
        without = model.i$evaluation$per.fold[, testing_cols_i[1]]
      )

      p()  # Signal progress

      return(eval_df_i)
    },
    future.seed = TRUE
  )

  # Combine and compute importance scores
  importance.per.repetition <- do.call(rbind, evaluation.list)
  importance.per.repetition <- merge(
    importance.per.repetition,
    evaluation.df,
    by = "fold.id"
  )

  # Compute median importance per variable
  importance.per.variable <- aggregate_importance_rf_select(importance.per.repetition)

  # Sort by importance (descending)
  importance.per.variable <- importance.per.variable[
    order(importance.per.variable$importance, decreasing = TRUE),
  ]

  # Filter out predictors with negative or zero contribution
  importance.per.variable <- importance.per.variable[
    importance.per.variable$importance > 0,
  ]

  if (nrow(importance.per.variable) == 0) {
    warning(
      "No predictors have positive contribution to transferability.\n",
      "Returning original model with diagnostics."
    )
    return(fallback_result_rf_select(
      model = model,
      reason = "no_positive_importance",
      predictors_original = predictor.variable.names,
      predictors_after_collinearity = predictors_after_collinearity,
      collinear_threshold = collinear_threshold,
      metric = metric
    ))
  }

  ranking <- importance.per.variable$variable

  if (verbose) {
    message("Ranked predictors: ", paste(ranking, collapse = ", "))
  }

  # STEP 3: FORWARD GREEDY SELECTION
  if (verbose) {
    message("\n=== Step 3/3: Forward selection of optimal predictor subset ===")
  }

  # Setup progressor
  p <- progressr::progressor(along = seq_len(length(ranking)))

  if (verbose) {
    message("Evaluating predictor subsets (1 to ", length(ranking), " predictors)...")
  }

  # Parallel forward selection
  optimization.list <- future.apply::future_lapply(
    X = seq_len(length(ranking)),
    FUN = function(n.predictors) {
      # Select first n predictors from ranking
      predictors.n <- ranking[1:n.predictors]

      # Fit model with these predictors
      model.n <- rf(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictors.n,
        distance.matrix = model$ranger.arguments$distance.matrix,
        distance.thresholds = model$ranger.arguments$distance.thresholds,
        xy = xy,
        ranger.arguments = ranger.arguments,
        seed = seed + n.predictors,
        verbose = FALSE,
        n.cores = n.cores
      )

      # Evaluate via spatial CV
      model.n <- rf_evaluate(
        model = model.n,
        xy = xy,
        repetitions = repetitions,
        training.fraction = training.fraction,
        distance.step = distance.step,
        distance.step.x = distance.step.x,
        distance.step.y = distance.step.y,
        metrics = metric,
        seed = seed + n.predictors,
        verbose = FALSE,
        n.cores = n.cores
      )

      # Extract aggregated testing performance
      testing_performance <- model.n$evaluation$aggregated[
        model.n$evaluation$aggregated$model == "Testing" &
        model.n$evaluation$aggregated$metric == metric,
        "median"
      ]

      # Extract training R²
      training_r2 <- model.n$performance$r.squared

      # Compute penalization (linear with number of predictors)
      penalization <- n.predictors / length(ranking)

      out.df <- data.frame(
        n.predictors = n.predictors,
        predictor.added = predictors.n[n.predictors],
        testing.performance = testing_performance,
        training.r.squared = training_r2,
        penalization = penalization
      )

      p()  # Signal progress

      return(out.df)
    },
    future.seed = TRUE
  )

  # Combine results
  optimization.df <- do.call(rbind, optimization.list)

  # Add predictor names column (cumulative)
  optimization.df$predictors <- sapply(
    optimization.df$n.predictors,
    function(n) paste(ranking[1:n], collapse = ", ")
  )

  # Compute optimization score
  optimization.df$optimization <- compute_optimization_score_rf_select(
    testing_performance = optimization.df$testing.performance,
    r_squared = optimization.df$training.r.squared,
    penalization = optimization.df$penalization,
    weight.r.squared = weight.r.squared,
    weight.penalization = weight.penalization.n.predictors,
    metric = metric
  )

  # Select optimal number of predictors
  optimal.index <- which.max(optimization.df$optimization)
  selected_predictors <- ranking[1:optimal.index]

  optimization.df$selected <- FALSE
  optimization.df$selected[1:optimal.index] <- TRUE

  if (verbose) {
    message("Optimal subset: ", length(selected_predictors), " predictor(s)")
    message("Selected: ", paste(selected_predictors, collapse = ", "))
  }

  # STEP 4: COMPARE AND RETURN
  if (verbose) {
    message("\n=== Step 4/4: Comparing models ===")
  }

  # Fit final model with selected predictors
  model_selected <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = selected_predictors,
    distance.matrix = model$ranger.arguments$distance.matrix,
    distance.thresholds = model$ranger.arguments$distance.thresholds,
    xy = xy,
    ranger.arguments = ranger.arguments,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores
  )

  # Evaluate selected model
  model_selected <- rf_evaluate(
    model = model_selected,
    xy = xy,
    repetitions = repetitions,
    training.fraction = training.fraction,
    distance.step = distance.step,
    distance.step.x = distance.step.x,
    distance.step.y = distance.step.y,
    metrics = metric,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores
  )

  # Extract testing performance
  selected_testing_performance <- model_selected$evaluation$aggregated[
    model_selected$evaluation$aggregated$model == "Testing" &
    model_selected$evaluation$aggregated$metric == metric,
    "median"
  ]

  # Evaluate original model if not already done
  if (is.null(model$evaluation)) {
    model <- rf_evaluate(
      model = model,
      xy = xy,
      repetitions = repetitions,
      training.fraction = training.fraction,
      distance.step = distance.step,
      distance.step.x = distance.step.x,
      distance.step.y = distance.step.y,
      metrics = metric,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores
    )
  }

  # Extract original model testing performance
  original_testing_performance <- model$evaluation$aggregated[
    model$evaluation$aggregated$model == "Testing" &
    model$evaluation$aggregated$metric == metric,
    "median"
  ]

  # Decide whether to return selected or original
  if (metric %in% c("r.squared", "pseudo.r.squared", "auc")) {
    # Higher is better
    improvement <- selected_testing_performance - original_testing_performance
    use_selected <- improvement > 0
  } else {
    # rmse, nrmse: lower is better
    improvement <- original_testing_performance - selected_testing_performance
    use_selected <- improvement > 0
  }

  # Create optimization plot (custom for predictor selection)
  optimization.plot <- ggplot2::ggplot(data = optimization.df) +
    ggplot2::aes(
      x = n.predictors,
      y = testing.performance,
      color = optimization
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_color_gradientn(
      colors = grDevices::hcl.colors(100, palette = "Zissou 1", rev = FALSE)
    ) +
    ggplot2::geom_point(
      data = optimization.df[optimization.df$selected, ],
      ggplot2::aes(
        x = n.predictors,
        y = testing.performance
      ),
      colour = "black",
      size = 5,
      shape = 1,
      alpha = 0.5
    ) +
    ggplot2::labs(
      x = "Number of predictors",
      y = paste("Testing", metric),
      color = "Optimization\nscore",
      title = "Forward selection of optimal predictor subset"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # Build selection slot
  selection <- list(
    method = "forward.greedy.cv",
    collinear_threshold = collinear_threshold,
    metric = metric,
    predictors = list(
      original = predictor.variable.names,
      after_collinearity = predictors_after_collinearity,
      ranked = ranking,
      selected = selected_predictors,
      final = if(use_selected) selected_predictors else predictor.variable.names
    ),
    ranking = importance.per.variable,
    optimization = optimization.df,
    optimization.plot = optimization.plot,
    comparison = data.frame(
      model = c("Original", "Selected"),
      n_predictors = c(
        length(predictor.variable.names),
        length(selected_predictors)
      ),
      testing_performance = c(
        original_testing_performance,
        selected_testing_performance
      ),
      improvement = c(0, improvement),
      used = c(!use_selected, use_selected)
    ),
    used_selected = use_selected
  )

  # Choose which model to return
  if (use_selected) {
    final_model <- model_selected
  } else {
    final_model <- model
  }

  final_model$selection <- selection
  class(final_model) <- c(class(final_model), "rf_select")

  if (verbose) {
    print_selection_summary_rf_select(final_model)
    print(final_model$selection$optimization.plot)
  }

  return(final_model)
}


#' @keywords internal
#' @autoglobal
aggregate_importance_rf_select <- function(importance.per.repetition) {
  # Input: data frame with columns: variable, fold.id, with, without
  # Output: data frame with columns: variable, importance, importance.mad

  do.call(rbind, lapply(
    split(importance.per.repetition, importance.per.repetition$variable),
    function(grp) {
      with_median <- stats::median(grp$with)
      without_median <- stats::median(grp$without)
      importance_val <- with_median - without_median

      data.frame(
        variable = grp$variable[1],
        importance = importance_val,
        importance.mad = stats::mad(grp$with - grp$without),
        stringsAsFactors = FALSE
      )
    }
  ))
}


#' @keywords internal
#' @autoglobal
compute_optimization_score_rf_select <- function(
  testing_performance,
  r_squared,
  penalization,
  weight.r.squared,
  weight.penalization,
  metric
) {
  # Determine if higher is better
  maximize <- metric %in% c("r.squared", "pseudo.r.squared", "auc")

  # Rescale components to 0-1
  if (maximize) {
    perf_scaled <- rescale_vector(testing_performance)
  } else {
    # For RMSE/NRMSE, lower is better, so invert
    perf_scaled <- rescale_vector(1 / testing_performance)
  }

  r2_scaled <- rescale_vector(r_squared)
  pen_scaled <- rescale_vector(penalization)

  # Compute optimization score
  # Higher testing performance + higher R² - more predictors
  score <- perf_scaled +
           (weight.r.squared * r2_scaled) -
           (weight.penalization * pen_scaled)

  rescale_vector(score)
}


#' @keywords internal
#' @autoglobal
print_selection_summary_rf_select <- function(model) {
  sel <- model$selection

  message("\n=== Predictor Selection Summary ===")
  message("Original predictors: ", length(sel$predictors$original))
  message("After collinearity filtering: ", length(sel$predictors$after_collinearity))

  removed <- setdiff(sel$predictors$original, sel$predictors$after_collinearity)
  if (length(removed) > 0) {
    message("  Removed: ", paste(removed, collapse = ", "))
  }

  message("\nSelected predictors: ", length(sel$predictors$selected))
  message("  ", paste(sel$predictors$selected, collapse = ", "))

  message("\nPerformance comparison:")
  print(sel$comparison)

  if (sel$used_selected) {
    message("\nSelected model has better transferability. Returning selected model.")
  } else {
    message("\nSelected model did not improve transferability. Returning original model with selection diagnostics.")
  }
}


#' @keywords internal
#' @autoglobal
fallback_result_rf_select <- function(
  model,
  reason = "unknown",
  predictors_original = NULL,
  predictors_after_collinearity = NULL,
  collinear_threshold = NULL,
  metric = NULL
) {
  # Returns original model with empty selection slot
  message_text <- switch(
    reason,
    "no_positive_importance" = "No predictors showed positive contribution to transferability",
    "insufficient_predictors" = "Insufficient predictors remaining after collinearity filtering",
    "Unknown failure"
  )

  model$selection <- list(
    method = "forward.greedy.cv",
    status = "failed",
    reason = reason,
    message = message_text,
    collinear_threshold = collinear_threshold,
    metric = metric,
    predictors = list(
      original = predictors_original,
      after_collinearity = predictors_after_collinearity,
      ranked = character(0),
      selected = character(0),
      final = predictors_original
    )
  )

  class(model) <- c(class(model), "rf_select")

  message("\n", message_text)
  message("Returning original model with diagnostics in $selection slot.")

  return(model)
}
