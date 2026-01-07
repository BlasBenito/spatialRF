#' @title Prepares variable importance objects for spatial models
#' @description Prepares variable importance data frames and plots for models fitted with [rf_spatial()].
#' @param model An importance data frame with spatial predictors, or a model fitted with [rf_spatial()].
#' @return A list with importance data frames in different formats depending on whether the model was fitted with [rf()] or [rf_repeat()].
#' @examples
#'
#' data(plants_rf_spatial)
#'
#' prepare_importance_spatial(plants_rf_spatial) |>
#'   head()
#'
#' @rdname prepare_importance_spatial
#' @family utilities
#' @export
#' @autoglobal
prepare_importance_spatial <- function(model) {
  #getting importance df
  if (!inherits(model, "rf_spatial")) {
    stop("This function requires a model fitted with rf_spatial()")
  }
  full.importance.df <- model$importance$per.variable

  #spatial predictors only
  spatial.predictors <- full.importance.df[
    grepl("spatial_predictor", full.importance.df$variable, fixed = TRUE),
  ]
  spatial.predictors$variable <- "spatial_predictors"

  #non-spatial predictors
  non.spatial.predictors <- full.importance.df[
    !grepl("spatial_predictor", full.importance.df$variable, fixed = TRUE),
  ]

  #joining for plot
  importance.plot.df <- rbind(
    spatial.predictors,
    non.spatial.predictors
  )

  #aggregating spatial predictors
  #min, max, median and mean of the spatial predictors
  spatial.predictors.stats <- data.frame(
    variable = c(
      "spatial_predictors (max)",
      "spatial_predictors (min)",
      "spatial_predictors (median)",
      "spatial_predictors (quantile 0.25)",
      "spatial_predictors (quantile 0.75)"
    ),
    importance = c(
      max(spatial.predictors$importance),
      min(spatial.predictors$importance),
      stats::median(spatial.predictors$importance),
      stats::quantile(spatial.predictors$importance, probs = 0.25),
      stats::quantile(spatial.predictors$importance, probs = 0.75)
    )
  )

  #formatting importance.df
  importance.df <- rbind(
    non.spatial.predictors,
    spatial.predictors.stats
  )
  importance.df <- importance.df[order(importance.df$importance, decreasing = TRUE), ]

  #preparing out list using build_importance_slots helper
  # Start with per.variable slots (reusing helper)
  out.list <- build_importance_slots(
    model$importance$per.variable,
    slot_prefix = "per.variable",
    verbose = FALSE
  )

  # Add spatial.predictors slots
  spatial_slots <- build_importance_slots(
    importance.plot.df,
    slot_prefix = "spatial.predictors",
    verbose = FALSE
  )
  out.list <- c(out.list, spatial_slots)

  # Add spatial.predictors.stats slots
  stats_slots <- build_importance_slots(
    importance.df,
    slot_prefix = "spatial.predictors.stats",
    verbose = FALSE
  )
  out.list <- c(out.list, stats_slots)

  #returning the list
  out.list
}
