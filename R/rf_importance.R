#' @title Contribution of each predictor to model transferability
#' @description Evaluates the contribution of the predictors to model transferability via spatial cross-validation. The function returns the median increase or decrease in a given evaluation metric (R2, pseudo R2, RMSE, nRMSE, or AUC) when a variable is introduced in a model, by comparing and evaluating via spatial cross-validation models with and without the given variable. This function was devised to provide importance scores that would be less sensitive to spatial autocorrelation than those computed internally by random forest on the out-of-bag data. This function is experimental.
#' @param model Model fitted with [rf()] and/or [rf_spatial()]. The function doesn't work with models fitted with [rf_repeat()]. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param metric Character, nams of the performance metric to use. The possible values are: "r.squared" (`cor(obs, pred) ^ 2`), "pseudo.r.squared" (`cor(obs, pred)`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`), and "auc" (only for binary responses with values 1 and 0). Default: `"r.squared"`
#' @param distance.step Numeric. Distance step used for (1) selection of fold centers via [thinning_til_n()], and (2) proportional buffer growth in [make_spatial_folds()]. For fold centers, reduces data until `repetitions` or fewer folds remain. For buffer growth, applies to the longer axis of the coordinate bounding box with proportional calculation for the shorter axis. Default: `NULL` (1/1000th the maximum distance for thinning; 1/1000th the range per axis for buffer growth). Reduce if fold count is lower than expected.
#' @param distance.step.x `r lifecycle::badge("deprecated")` Use distance.step instead, which automatically calculates proportional step sizes.
#' @param distance.step.y `r lifecycle::badge("deprecated")` Use distance.step instead, which automatically calculates proportional step sizes.
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `grDevices::hcl.colors(100)`). Default: `grDevices::hcl.colors(100, palette = "Zissou 1", rev = FALSE, alpha = 0.8)`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"white"`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer. Number of threads for ranger's internal parallelization. Default: `NULL` (auto-detected: if user has set a parallel plan via `future::plan()`, defaults to 1; otherwise defaults to `future::availableCores(omit = 1)`). Set to 1 for debugging. Note: When using a parallel plan (multiple workers), setting n.cores > 1 may cause oversubscription. The function will warn if this occurs.
#' @return The input model with new data in its "importance" slot. The new importance scores are included in the data frame `model$importance$per.variable`, under the column names "importance.cv" (median contribution to transferability over spatial cross-validation repetitions), "importance.cv.mad" (median absolute deviation of the performance scores over spatial cross-validation repetitions), "importance.cv.percent" ("importance.cv" expressed as a percent, taking the full model's performance as baseline), and "importance.cv.mad" (median absolute deviation of "importance.cv"). The plot is stored as "cv.per.variable.plot".
#' @examples
#'
#' if(interactive()){
#'   data(plants_rf)
#'
#'   m_importance <- rf_importance(
#'     model = plants_rf,
#'     repetitions = 5
#'   )
#' }
#'
#' @rdname rf_importance
#' @family model_workflow
#' @export
#' @autoglobal
rf_importance <- function(
  model = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  metric = c(
    "r.squared",
    "pseudo.r.squared",
    "rmse",
    "nrmse",
    "auc"
  ),
  distance.step = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  fill.color = grDevices::hcl.colors(
    n = 100,
    palette = "Zissou 1",
    rev = FALSE,
    alpha = 1
  ),
  line.color = "white",
  seed = 1,
  verbose = TRUE,
  n.cores = NULL
) {
  #testing method argument
  metric <- match.arg(
    arg = metric,
    choices = c("r.squared", "pseudo.r.squared", "rmse", "nrmse", "auc"),
    several.ok = FALSE
  )

  #evaluating the full model
  if (verbose) {
    message("Evaluating the full model with spatial cross-validation.")
  }

  #evaluating the full model
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

  #getting the evaluation.df
  testing_cols <- grep(
    "testing.",
    colnames(model$evaluation$per.fold),
    fixed = TRUE,
    value = TRUE
  )
  evaluation.df <- model$evaluation$per.fold[,
    c("fold.id", testing_cols),
    drop = FALSE
  ]
  evaluation.df <- evaluation.df[,
    colnames(evaluation.df) != "testing.records",
    drop = FALSE
  ]
  colnames(evaluation.df)[2] <- "with"

  #getting training data
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

  #getting xy
  if (is.null(xy)) {
    # Try extracting from data if it's sf or has coordinate columns
    if (is_sf(data)) {
      xy <- sf_to_xy(data, add_id = TRUE)
    } else {
      # Try extracting from data frame columns
      xy_extracted <- extract_xy_from_data_frame(data)
      if (!is.null(xy_extracted)) {
        xy <- xy_extracted
        xy$id <- seq_len(nrow(xy))
      }
    }
    # If still NULL, fall back to model$xy
    if (is.null(xy) && !is.null(model$xy)) {
      xy <- model$xy
    }
    # If still NULL, error
    if (is.null(xy)) {
      stop("The argument 'xy' is required for spatial cross-validation.")
    }
  }

  # Drop geometry before processing
  data <- drop_geometry_if_sf(data)

  if (sum(c("x", "y") %in% colnames(xy)) < 2) {
    stop("The column names of 'xy' must be 'x' and 'y'.")
  }

  if (nrow(xy) != nrow(data)) {
    stop(
      "nrow(xy) and nrow(data) (stored in model$ranger.arguments$data) must be the same."
    )
  }

  #list to store evaluation dataframes
  evaluation.list <- list()

  #evaluating the full model
  if (verbose) {
    message("Evaluating models fitted without each predictor.")
  }

  #iterating across predictors
  for (predictor.i in predictor.variable.names) {
    #copy of data
    data.i <- data

    #removing predictor.i
    predictor.variable.names.i <- predictor.variable.names[
      predictor.variable.names != predictor.i
    ]

    #fitting model without predictor.i
    model.i <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      xy = xy,
      ranger.arguments = ranger.arguments,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores
    ) |>
      rf_evaluate(
        repetitions = repetitions,
        xy = xy,
        training.fraction = training.fraction,
        distance.step = distance.step,
        distance.step.x = distance.step.x,
        distance.step.y = distance.step.y,
        metrics = metric,
        seed = seed,
        verbose = FALSE
      )

    #getting evaluation data frame
    eval_df_i <- model.i$evaluation$per.fold[,
      colnames(model.i$evaluation$per.fold) != "testing.records",
      drop = FALSE
    ]

    testing_col <- grep(
      "testing.",
      colnames(eval_df_i),
      fixed = TRUE,
      value = TRUE
    )
    colnames(eval_df_i)[colnames(eval_df_i) == testing_col] <- "without"

    eval_df_i <- merge(
      eval_df_i,
      evaluation.df,
      by = "fold.id",
      all.x = TRUE
    )

    eval_df_i <- cbind(
      variable = predictor.i,
      eval_df_i,
      stringsAsFactors = FALSE
    )

    training_cols <- grep(
      "training.",
      colnames(eval_df_i),
      fixed = TRUE,
      value = TRUE
    )
    eval_df_i <- eval_df_i[,
      !colnames(eval_df_i) %in% training_cols,
      drop = FALSE
    ]

    evaluation.list[[predictor.i]] <- eval_df_i
  }

  #putting together the evaluation data frames
  importance.per.repetition <- do.call(
    "rbind",
    evaluation.list
  )
  rownames(importance.per.repetition) <- NULL

  #summary of differences
  importance.per.variable <- do.call(
    rbind,
    lapply(
      split(importance.per.repetition, importance.per.repetition$variable),
      function(grp) {
        with_median <- median(grp$with)
        without_median <- median(grp$without)
        importance_val <- with_median - without_median

        data.frame(
          variable = grp$variable[1],
          with = with_median,
          without = without_median,
          importance = importance_val,
          importance.mad = round(stats::mad(grp$with - grp$without), 3),
          importance.percent = round(importance_val * 100 / with_median, 1),
          importance.percent.mad = round(
            stats::mad(
              (grp$with * 100 / grp$with[1]) - (grp$without * 100 / grp$with[1])
            ),
            1
          ),
          stringsAsFactors = FALSE
        )
      }
    )
  )
  rownames(importance.per.variable) <- NULL

  importance.per.variable <- importance.per.variable[
    order(importance.per.variable$importance, decreasing = TRUE),
  ]

  importance.per.variable <- importance.per.variable[, c(
    "variable",
    "with",
    "without",
    "importance",
    "importance.mad",
    "importance.percent",
    "importance.percent.mad"
  )]

  #pretty metric name
  if (metric == "r.squared") {
    metric.pretty <- "R-squared"
  }
  if (metric == "pseudo.r.squared") {
    metric.pretty <- "pseudo R-squared"
  }
  if (metric == "rmse") {
    metric.pretty <- "RMSE"
  }
  if (metric == "nrmse") {
    metric.pretty <- "normalized RMSE"
  }
  if (metric == "auc") {
    metric.pretty <- "AUC"
  }

  importance.per.variable.plot <- ggplot2::ggplot(
    data = importance.per.variable
  ) +
    ggplot2::aes(
      x = importance,
      y = stats::reorder(
        variable,
        importance,
        FUN = max
      ),
      fill = importance
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        xmin = importance - importance.mad,
        xmax = importance + importance.mad,
        color = importance
      ),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      size = 4,
      shape = 21,
      color = line.color
    ) +
    ggplot2::scale_x_continuous(
      sec.axis = ggplot2::sec_axis(
        ~ . * 100 / importance.per.variable$with[1],
        name = "Percentage"
      )
    ) +
    ggplot2::scale_fill_gradientn(colors = fill.color) +
    ggplot2::scale_color_gradientn(colors = fill.color) +
    ggplot2::ylab("") +
    ggplot2::xlab(paste0(metric.pretty)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(
      paste0(
        "Median contribution to model transferability\n    across ",
        repetitions,
        " cross-validation repetitions"
      )
    )

  #adding to the model's importance slot
  model$importance$per.variable$importance.cv <- NULL
  model$importance$per.variable$importance.cv.mad <- NULL
  model$importance$per.variable$importance.cv.percent <- NULL
  model$importance$per.variable$importance.cv.percent.mad <- NULL

  if ("importance.oob" %in% colnames(model$importance$per.variable)) {
    model$importance$per.variable$importance <- model$importance$per.variable$importance.oob
    model$importance$per.variable$importance.oob <- NULL
  }

  model$importance$per.variable <- merge(
    model$importance$per.variable,
    importance.per.variable,
    by = "variable",
    all.x = TRUE
  )

  colnames(model$importance$per.variable)[
    colnames(model$importance$per.variable) == "importance.x"
  ] <- "importance.oob"
  colnames(model$importance$per.variable)[
    colnames(model$importance$per.variable) == "importance.y"
  ] <- "importance.cv"
  colnames(model$importance$per.variable)[
    colnames(model$importance$per.variable) == "importance.mad"
  ] <- "importance.cv.mad"
  colnames(model$importance$per.variable)[
    colnames(model$importance$per.variable) == "importance.percent"
  ] <- "importance.cv.percent"
  colnames(model$importance$per.variable)[
    colnames(model$importance$per.variable) == "importance.percent.mad"
  ] <- "importance.cv.percent.mad"

  model$importance$per.variable <- model$importance$per.variable[,
    !colnames(model$importance$per.variable) %in% c("with", "without"),
    drop = FALSE
  ]

  #changing names
  model$importance$oob.per.variable.plot <- model$importance$per.variable.plot
  model$importance$per.variable.plot <- NULL
  model$importance$cv.per.variable.plot <- importance.per.variable.plot

  if (verbose) {
    message("Importance scores stored in model$importance$per.variable.")
    message("Importance plot stored in model$importance$cv.per.variable.plot.")
  }

  if (verbose) {
    print(importance.per.variable.plot)
  }

  model
}
