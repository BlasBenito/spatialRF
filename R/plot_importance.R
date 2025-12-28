#' @title Visualize variable importance scores
#' @description Creates a visualization of variable importance scores from models fitted with [rf()], [rf_repeat()], or [rf_spatial()]. For single-run models ([rf()], [rf_spatial()]), displays points ordered by importance. For repeated models ([rf_repeat()]), displays violin plots showing the distribution of importance scores across model repetitions.
#' @param model Model fitted with [rf()], [rf_repeat()], or [rf_spatial()]. Alternatively, a data frame with variable importance scores (for internal use only).
#' @param fill.color Character vector of colors or a function generating a color palette. Accepts hexadecimal codes (e.g., `c("#440154FF", "#21908CFF", "#FDE725FF")`) or palette functions (e.g., `viridis::viridis(100)`). For single-run models, creates a continuous gradient. For repeated models, assigns discrete colors to variables. Default: `viridis::viridis(100, option = "F", direction = -1, alpha = 1, end = 0.9)`.
#' @param line.color Character string specifying the color of point borders (single-run models) or violin plot outlines (repeated models). Default: `"white"`.
#' @param verbose Logical. If `TRUE`, prints the plot to the graphics device. Default: `TRUE`.
#' @return ggplot object that can be further customized or saved. The plot displays variable importance on the x-axis and variable names on the y-axis, ordered by importance (highest at top).
#' @details
#' This function creates different visualizations depending on the model type:
#'
#' **Single-run models** ([rf()], [rf_spatial()] without repetitions):
#' \itemize{
#'   \item Displays points showing the importance value for each variable
#'   \item Variables ordered top-to-bottom by importance (most important at top)
#'   \item Point color represents importance magnitude using a continuous gradient
#' }
#'
#' **Repeated models** ([rf_repeat()], [rf_spatial()] with repetitions):
#' \itemize{
#'   \item Displays violin plots showing the distribution of importance across repetitions
#'   \item Variables ordered top-to-bottom by median importance (most important at top)
#'   \item The median line within each violin shows the center of the distribution
#'   \item Width of violin reflects the density of importance values at each level
#'   \item Each variable receives a distinct fill color
#' }
#'
#' **Importance metric:**
#'
#' The x-axis shows permutation importance, which measures the increase in prediction error when a variable's values are randomly shuffled. Higher values indicate more important variables. Importance is computed on out-of-bag (OOB) samples, providing an unbiased estimate of variable contribution.
#'
#' **Spatial predictors:**
#'
#' In [rf_spatial()] models, all spatial predictors (MEMs or PCA factors) are grouped into a single category labeled "spatial_predictors" to simplify comparison with non-spatial predictors.
#'
#' **Note on violin plots:**
#'
#' Violin plots display kernel density estimates. The median line shown is the median of the density estimate, which may differ slightly from the actual data median. However, variables are always ordered by the true median importance to ensure accurate ranking.
#'
#' **Cross-validated importance:**
#'
#' This function does not plot results from [rf_importance()]. For cross-validated importance plots, access `model$importance$cv.per.variable.plot` after running [rf_importance()].
#' @seealso [print_importance()], [get_importance()], [rf_importance()]
#' @examples
#'
#' if(interactive()){
#'
#' data(plants_rf, plants_rf_spatial)
#'
#' # Plot importance from Random Forest model
#' plot_importance(plants_rf)
#'
#' # Plot importance from Spatial Random Forest model
#' plot_importance(plants_rf_spatial)
#'
#' }
#'
#'
#' @rdname plot_importance
#' @family visualization
#' @export
#' @autoglobal
plot_importance <- function(
  model,
  fill.color = viridis::viridis(
    100,
    option = "F",
    direction = -1,
    alpha = 1,
    end = 0.9
  ),
  line.color = "white",
  verbose = TRUE
) {
  #if x is not a data frame
  if (!is.data.frame(model)) {
    #importance from rf
    if (
      inherits(model, "rf") &&
        !inherits(model, "rf_spatial") &&
        !inherits(model, "rf_repeat")
    ) {
      x <- model$importance$per.variable

      if ("importance.oob" %in% colnames(x)) {
        x <- x |>
          dplyr::select(-importance) |>
          dplyr::rename(
            importance = importance.oob
          )
      }
    }

    #importance from rf_repeat
    if (inherits(model, "rf_repeat") && !inherits(model, "rf_spatial")) {
      x <- model$importance$per.repetition
    }

    #importance from rf_spatial and rf
    if (inherits(model, "rf_spatial")) {
      x <- model$importance$spatial.predictors
    }
  } else {
    x <- model
  }

  #find duplicates in "variable"
  variable.duplicated <- duplicated(x$variable)

  #no duplicates, rf
  if (sum(variable.duplicated) == 0) {
    p <- ggplot2::ggplot(data = x) +
      ggplot2::aes(
        x = importance,
        y = stats::reorder(
          variable,
          importance,
          FUN = max
        ),
        fill = importance
      ) +
      ggplot2::geom_point(
        size = 4,
        shape = 21,
        color = line.color
      ) +
      ggplot2::scale_fill_gradientn(colors = fill.color) +
      ggplot2::ylab("") +
      ggplot2::xlab("Mean error increase when permuted") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle(
        "Permutation importance\ncomputed on the out-of-bag data"
      )
  } else {
    #no "spatial_predictors" in variable, rf_repeat
    if (!("spatial_predictors" %in% x$variable)) {
      #adapting palette
      n.variables <- length(unique(x$variable))
      if (length(fill.color) != 1) {
        if (length(fill.color) > length(n.variables)) {
          fill.colors.function <- grDevices::colorRampPalette(
            fill.color,
            alpha = TRUE
          )
          fill.color <- fill.colors.function(n.variables)
        }
      }

      p <- ggplot2::ggplot(data = x) +
        ggplot2::aes(
          x = importance,
          y = stats::reorder(
            variable,
            importance,
            FUN = stats::median
          ),
          fill = stats::reorder(
            variable,
            importance,
            FUN = stats::median
          )
        ) +
        ggplot2::geom_violin(
          quantiles = 0.5,
          color = line.color,
          scale = "width"
        ) +
        ggplot2::scale_fill_manual(values = fill.color) +
        ggplot2::ylab("") +
        ggplot2::xlab("Increase in error when permuted") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle(
          "Permutation importance\ncomputed on the out-of-bag data"
        )
    }

    #spatial_predictors, rf_spatial
    if ("spatial_predictors" %in% x$variable) {
      #if no predictors duplicated, rf_spatial rf
      if (
        sum(duplicated(x$variable[x$variable != "spatial_predictors"])) == 0
      ) {
        p <- ggplot2::ggplot(data = x) +
          ggplot2::aes(
            x = importance,
            y = stats::reorder(
              variable,
              importance,
              FUN = max
            ),
            fill = importance
          ) +
          ggplot2::geom_point(
            size = 4,
            shape = 21,
            color = line.color
          ) +
          ggplot2::scale_fill_gradientn(colors = fill.color) +
          ggplot2::ylab("") +
          ggplot2::xlab("Increase in error when permuted") +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "none") +
          ggplot2::ggtitle(
            "Permutation importance\ncomputed on the out-of-bag data"
          )

        #rf_spatial rf_repeat
      } else {
        #adapting palette
        n.variables <- length(unique(x$variable))
        if (length(fill.color) != 1) {
          if (length(fill.color) > length(n.variables)) {
            fill.colors.function <- grDevices::colorRampPalette(
              fill.color,
              alpha = TRUE
            )
            fill.color <- fill.colors.function(n.variables)
          }
        }

        p <- ggplot2::ggplot(data = x) +
          ggplot2::aes(
            x = importance,
            y = stats::reorder(
              variable,
              importance,
              FUN = stats::median
            ),
            fill = stats::reorder(
              variable,
              importance,
              FUN = stats::median
            )
          ) +
          ggplot2::geom_violin(
            draw_quantiles = 0.5,
            color = line.color,
            scale = "width"
          ) +
          ggplot2::scale_fill_manual(values = fill.color) +
          ggplot2::ylab("") +
          ggplot2::xlab("Increase in error when permuted") +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "none") +
          ggplot2::ggtitle(
            "Permutation importance\ncomputed on the out-of-bag data"
          )
      }
    }
  }

  p
}
