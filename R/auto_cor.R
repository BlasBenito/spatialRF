#' @title Multicollinearity reduction via Pearson correlation
#' @description Filters predictors using sequential evaluation of pairwise correlations. Predictors are ranked by user preference (or column order) and evaluated sequentially. Each candidate is added to the selected pool only if its maximum absolute correlation with already-selected predictors does not exceed the threshold.
#' @param x Data frame with predictors, or a `variable_selection` object from [auto_vif()]. Default: `NULL`.
#' @param preference.order Character vector specifying variable preference order. Does not need to include all variables in `x`. If `NULL`, column order is used. Default: `NULL`.
#' @param cor.threshold Numeric between 0 and 1 (recommended: 0.5 to 0.9). Maximum allowed absolute Pearson correlation between selected variables. Default: `0.50`
#' @param verbose Logical. If `TRUE`, prints messages about operations and removed variables. Default: `TRUE`
#' @return List with class `variable_selection` containing:
#' \itemize{
#'   \item `cor`: Correlation matrix of selected variables (only if 2+ variables selected).
#'   \item `selected.variables`: Character vector of selected variable names.
#'   \item `selected.variables.df`: Data frame containing selected variables.
#' }
#' @details The algorithm follows these steps:
#' \enumerate{
#'   \item Rank predictors by `preference.order` (or use column order if NULL).
#'   \item Initialize selection pool with first predictor.
#'   \item For each remaining candidate:
#'   \itemize{
#'     \item Compute maximum absolute correlation with selected predictors.
#'     \item If max correlation equal or lower than `cor.threshold`, add to selected pool.
#'     \item Otherwise, skip candidate.
#'   }
#'   \item Return selected predictors.
#' }
#'
#' \strong{Data cleaning}: Variables in `preference.order` not found in `colnames(x)` are silently removed. Non-numeric columns are removed with a warning. Rows with NA values are removed via [na.omit()]. Zero-variance columns trigger a warning but are not removed.
#'
#' This function can be chained with [auto_vif()] through pipes (see examples).
#' @examples
# auto_cor ----
#' data(
#'   plants_df,
#'   plants_predictors
#' )
#'
#' y <- auto_cor(
#'   x = plants_df[, plants_predictors]
#' )
#'
#' y$selected.variables
#' y$cor
#' head(y$selected.variables.df)
#' @seealso [auto_vif()]
#' @rdname auto_cor
#' @family preprocessing
#' @export
#' @autoglobal
auto_cor <- function(
  x = NULL,
  preference.order = NULL,
  cor.threshold = 0.50,
  verbose = TRUE
) {
  if (inherits(x, "variable_selection")) {
    x <- x$selected.variables.df
  }

  #coerce to data frame if tibble
  if (inherits(x, "tbl_df") || inherits(x, "tbl")) {
    x <- as.data.frame(x)
  }

  #removing NA
  x <- na.omit(x)

  #handle edge case: no columns
  if (ncol(x) == 0) {
    output.list <- list()
    output.list$selected.variables <- character(0)
    output.list$selected.variables.df <- x
    class(output.list) <- "variable_selection"
    return(output.list)
  }

  #handle edge case: single column (nothing to correlate)
  if (ncol(x) == 1) {
    output.list <- list()
    output.list$selected.variables <- colnames(x)
    output.list$selected.variables.df <- x
    class(output.list) <- "variable_selection"
    if (verbose) {
      message("[auto_cor()]: Only one variable, nothing to filter.")
    }
    return(output.list)
  }

  #finding and removing non-numeric columns
  non.numeric.columns <- colnames(x)[!sapply(x, is.numeric)]
  if (length(non.numeric.columns) > 0) {
    warning(
      "These columns are non-numeric and will be removed: ",
      paste(
        non.numeric.columns,
        collapse = ", "
      )
    )
    x <- x[, !(colnames(x) %in% non.numeric.columns)]
  }

  #finding zero variance columns
  zero.variance.columns <- colnames(x)[round(apply(x, 2, var), 4) == 0]
  if (length(zero.variance.columns) > 0) {
    warning(
      "These columns have zero variance and might cause issues: ",
      paste(
        zero.variance.columns,
        collapse = ", "
      )
    )
  }

  #step 1: establish ranking order
  if (!is.null(preference.order)) {
    preference.order <- preference.order[preference.order %in% colnames(x)]
    not.in.preference.order <- setdiff(colnames(x), preference.order)
    preference.order <- c(preference.order, not.in.preference.order)
  } else {
    preference.order <- colnames(x)
  }

  #step 2: compute full correlation matrix
  x.cor <- stats::cor(x)

  #step 3: initialize with first predictor
  selected.variables <- preference.order[1]

  #step 4: sequential evaluation
  for (i in 2:length(preference.order)) {
    candidate <- preference.order[i]

    #correlation gate: max absolute correlation with selected predictors
    max.cor <- max(abs(x.cor[candidate, selected.variables]))

    if (max.cor <= cor.threshold) {
      selected.variables <- c(selected.variables, candidate)
    }
  }

  #identify removed variables
  removed.vars <- setdiff(colnames(x), selected.variables)

  #message
  if (verbose) {
    if (length(removed.vars) != 0) {
      message(
        paste0(
          "[auto_cor()]: Removed variables: ",
          paste0(
            removed.vars,
            collapse = ", "
          )
        )
      )
    } else {
      message("[auto_cor()]: Variables are not collinear.")
    }
  }

  #build selected variables dataframe
  selected.variables.df <- x[, selected.variables, drop = FALSE]

  #return output
  output.list <- list()
  if (length(selected.variables) > 1) {
    output.list$cor <- round(stats::cor(selected.variables.df), 2)
  }
  output.list$selected.variables <- selected.variables
  output.list$selected.variables.df <- selected.variables.df

  class(output.list) <- "variable_selection"

  output.list
}
