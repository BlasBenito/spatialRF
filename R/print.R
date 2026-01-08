#' @title Custom print method for random forest models
#' @description Custom print method for models fitted with [rf()], [rf_repeat()], and [rf_spatial()].
#' @param x A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param ... Additional arguments for print methods.
#' @return Prints model details to the console.
#' @seealso [print_evaluation()], [print_importance()], [print_moran()], [print_performance()]
#' @examples
#'
#' data(plants_rf)
#'
#' print(plants_rf)
#'
#' #or
#' plants_rf
#'
#' @rdname print
#' @family model_info
#' @export
#' @autoglobal
print.rf <- function(x, ...) {
  #getting model features

  cat("+ Model type\n")
  cat(
    "  - Fitted with:         ",
    class(x)[2],
    "()\n",
    sep = ""
  )
  if (inherits(x, "rf_repeat")) {
    cat(
      "  - Repetitions:         ",
      x$ranger.arguments$repetitions,
      "\n",
      sep = ""
    )
  }
  if (inherits(x, "rf_spatial")) {
    cat(
      "  - rf_spatial() method: ",
      x$spatial$method,
      "\n",
      sep = ""
    )
  }
  if (inherits(x, "rf_interactions")) {
    cat(
      "  - Interactions selected: ",
      nrow(x$interactions$selected),
      "\n",
      sep = ""
    )
  }
  cat(
    "  - Response variable:     ",
    x$ranger.arguments$dependent.variable.name,
    "\n",
    sep = ""
  )

  cat("\n")
  cat(
    "+ Hyperparameters\n"
  )
  cat(
    "  - Type:                  ",
    x$treetype,
    "\n",
    sep = ""
  )
  cat(
    "  - Number of trees:       ",
    x$num.trees,
    "\n",
    sep = ""
  )
  cat(
    "  - Sample size:           ",
    x$num.samples,
    "\n",
    sep = ""
  )
  cat(
    "  - Number of predictors:  ",
    x$num.independent.variables,
    "\n",
    sep = ""
  )
  cat(
    "  - Mtry:                  ",
    x$mtry,
    "\n",
    sep = ""
  )
  cat(
    "  - Minimum node size:     ",
    x$min.node.size,
    "\n",
    sep = ""
  )

  print_performance(x)

  cat("\n+ Residuals \n")
  cat("  + Stats: \n")
  cat(
    "    - Minimum:      ",
    x$residuals$stats[1],
    "\n"
  )
  cat(
    "    - 1st Quartile: ",
    x$residuals$stats[2],
    "\n"
  )
  cat(
    "    - Meidan:       ",
    x$residuals$stats[3],
    "\n"
  )
  cat(
    "    - Mean:         ",
    x$residuals$stats[4],
    "\n"
  )
  cat(
    "    - 3rd Quartile: ",
    x$residuals$stats[5],
    "\n"
  )
  cat(
    "    - Maximum:      ",
    x$residuals$stats[6],
    "\n"
  )

  cat("  + Normality: \n")
  cat(
    "    - Shapiro-Wilks W:",
    round(x$residuals$normality$shapiro.w, 3),
    "\n"
  )
  cat("    - p-value:        ", round(x$residuals$normality$p.value, 4), "\n")
  cat("    - Interpretation: ", x$residuals$normality$interpretation, "\n")

  if ("autocorrelation" %in% names(x$residuals)) {
    cat("  - Spatial autocorrelation: \n")
    print_moran(x)
  }

  if ("variable.importance" %in% names(x)) {
    cat("\n")
    cat("- Importance: \n")
    print_importance(x)
  }

  if (inherits(x, "rf_interactions")) {
    cat("\n+ Interactions\n")
    cat("  - Number selected:  ", nrow(x$interactions$selected), "\n")
    cat("  - Names:\n")
    n_show <- min(5, nrow(x$interactions$selected))
    for (i in seq_len(n_show)) {
      int_name <- x$interactions$selected$interaction.name[i]
      int_imp <- x$interactions$selected$interaction.importance[i]
      int_gain <- x$interactions$selected$interaction.metric.gain[i]
      cat(sprintf(
        "    %d. %s (imp: %.1f, gain: %.3f)\n",
        i, int_name, int_imp, int_gain
      ))
    }
    if (nrow(x$interactions$selected) > n_show) {
      cat("    ... and", nrow(x$interactions$selected) - n_show, "more\n")
    }
  }

  if (inherits(x, "rf_evaluate")) {
    print_evaluation(x)
  }
}
