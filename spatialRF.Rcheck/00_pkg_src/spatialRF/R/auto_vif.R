#' @title Multicollinearity reduction via Variance Inflation Factor
#' @description Filters predictors using sequential evaluation of variance inflation factors. Predictors are ranked by user preference (or column order) and evaluated sequentially. Each candidate is added to the selected pool only if the maximum VIF of all predictors (candidate plus already-selected) does not exceed the threshold.
#' @param x Data frame with predictors, or a `variable_selection` object from [auto_cor()]. Default: `NULL`.
#' @param preference.order Character vector specifying variable preference order. Does not need to include all variables in `x`. If `NULL`, column order is used. Default: `NULL`.
#' @param vif.threshold Numeric (recommended: 2.5 to 10). Maximum allowed VIF among selected variables. Higher values allow more collinearity. Default: `5`.
#' @param verbose Logical. If `TRUE`, prints messages about operations and removed variables. Default: `TRUE`
#' @return List with class `variable_selection` containing:
#' \itemize{
#'   \item `vif`: Data frame with selected variable names and their VIF scores.
#'   \item `selected.variables`: Character vector of selected variable names.
#'   \item `selected.variables.df`: Data frame containing selected variables.
#' }
#' @details The algorithm follows these steps:
#' \enumerate{
#'   \item Rank predictors by `preference.order` (or use column order if NULL).
#'   \item Initialize selection pool with first predictor.
#'   \item For each remaining candidate:
#'   \itemize{
#'     \item Compute VIF for candidate plus all selected predictors.
#'     \item If max VIF equal or lower than `vif.threshold`, add candidate to selected pool.
#'     \item Otherwise, skip candidate.
#'   }
#'   \item Return selected predictors with their VIF values.
#' }
#'
#' \strong{Data cleaning}: Variables in `preference.order` not found in `colnames(x)` are silently removed. Non-numeric columns are removed with a warning. Rows with NA values are removed via [na.omit()]. Zero-variance columns trigger a warning but are not removed.
#'
#' This function can be chained with [auto_cor()] through pipes (see examples).
#' @seealso [auto_cor()]
#' @examples
#' data(
#'   plants_df,
#'   plants_predictors
#' )
#'
#' y <- auto_vif(
#'   x = plants_df[, plants_predictors]
#' )
#'
#' y$selected.variables
#' y$vif
#' head(y$selected.variables.df)
#'
#' @rdname auto_vif
#' @family preprocessing
#' @importFrom magrittr `%>%`
#' @importFrom stats cor
#' @export
auto_vif <- function(
  x = NULL,
  preference.order = NULL,
  vif.threshold = 5,
  verbose = TRUE
) {
  if (inherits(x, "variable_selection") == TRUE) {
    x <- x$selected.variables.df
  }

  #coercing to data frame
  #coerce to data frame if tibble
  if (inherits(x, "tbl_df") | inherits(x, "tbl")) {
    x <- as.data.frame(x)
  }

  #removing non-numeric and zero variance columns
  #removing NA
  x <- na.omit(x)

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

  #step 2: initialize with first predictor
  selected.variables <- preference.order[1]

  #step 3: sequential evaluation
  for (i in 2:length(preference.order)) {
    candidate <- preference.order[i]

    #vif gate: compute VIF for candidate + selected
    vif.df <- .vif_to_df(x[, c(selected.variables, candidate)])

    if (max(vif.df$vif) <= vif.threshold) {
      selected.variables <- c(selected.variables, candidate)
    }
  }

  #step 4: build output
  vif.df <- .vif_to_df(x[, selected.variables])
  selected.variables.df <- x[, selected.variables, drop = FALSE]

  #prepare output list
  output.list <- list()
  output.list$vif <- vif.df
  output.list$selected.variables <- selected.variables
  output.list$selected.variables.df <- selected.variables.df

  #message
  if (verbose == TRUE) {
    removed.vars <- setdiff(colnames(x), output.list$selected.variables)
    if (length(removed.vars) != 0) {
      message(
        paste0(
          "[auto_vif()]: Removed variables: ",
          paste0(
            removed.vars,
            collapse = ", "
          )
        )
      )
    } else {
      message("[auto_vif()]: Variables are not collinear.")
    }
  }

  #adding class
  class(output.list) <- "variable_selection"

  #returning output
  output.list
}


#' @title Convert VIF values to data frame
#' @description Computes variance inflation factors for all variables in a data frame and returns them in a tidy format, sorted by VIF in descending order.
#' @param x Data frame with numeric predictors for which to compute VIF values.
#' @return Data frame with two columns: `variable` (character, variable names) and `vif` (numeric, VIF scores), sorted by VIF in descending order.
#' @family utilities
#' @export
.vif_to_df <- function(x) {
  #defining global variable
  vif <- NULL

  #turns vif output into tidy df
  df <-
    data.frame(
      diag(solve(cor(x))),
      stringsAsFactors = FALSE
    ) %>%
    dplyr::rename(vif = 1) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::arrange(dplyr::desc(vif))

  df
}
