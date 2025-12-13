#' @title Multicollinearity reduction via Variance Inflation Factor
#'
#' @description Filters predictors using sequential evaluation of variance inflation factors. Predictors are ranked by user preference (or column order), and evaluated sequentially. Each candidate is added to the selected pool only if the maximum VIF of all predictors (candidate plus already-selected) does not exceed the threshold. \strong{Warning}: variables in `preference.order` not in `colnames(x)`, and non-numeric columns are removed silently from `x` and `preference.order`. The same happens with rows having NA values ([na.omit()] is applied). The function issues a warning if zero-variance columns are found.
#' @usage
#' auto_vif(
#'   x = NULL,
#'   preference.order = NULL,
#'   vif.threshold = 5,
#'   verbose = TRUE
#' )
#' @param x A data frame with predictors or the result of [auto_cor()]. Default: `NULL`.
#' @param preference.order a character vector with columns names of x ordered by the user preference, Default: `NULL`.
#' @param vif.threshold Numeric between 2.5 and 10 defining the selection threshold for the VIF analysis. Higher numbers result in a more relaxed variable selection. Default: 5.
#' @param verbose Logical. if `TRUE`, describes the function operations to the user. Default:: `TRUE`
#' @return List with three slots:
#' \itemize{
#'   \item `vif`: data frame with the names of the selected variables and their respective VIF scores.
#'   \item `selected.variables`: character vector with the names of the selected variables.
#'   \item `selected.variables.df`: data frame with the selected variables.
#'  }
#' @details The algorithm follows these steps:
#' \enumerate{
#'   \item Rank predictors by `preference.order` (or use column order if NULL)
#'   \item Initialize selection pool with first predictor
#'   \item For each remaining candidate:
#'   \itemize{
#'     \item Compute VIF for candidate plus all selected predictors
#'     \item If max VIF ≤ `vif.threshold`, add candidate to selected pool
#'     \item Otherwise, skip candidate
#'   }
#'   \item Return selected predictors with their VIF values
#' }
#' Can be chained together with [auto_cor()] through pipes, see the examples below.
#' @seealso [auto_cor()]
#' @examples
#'
#'#loading data
#'data(plants_df)
#'
#'#on a data frame
#'out <- auto_vif(x = plants_df[, 5:21])
#'
#'#getting out the vif data frame
#'out$vif
#'
#'#getting the names of the selected variables
#'out$selected.variables
#'
#'#getting the data frame of selected variables
#'out$selected.variables.df
#'
#'#on the result of auto_cor
#'out <- auto_cor(x = plants_df[, 5:21])
#'out <- auto_vif(x = out)
#'
#'#with pipes
#'out <- plants_df[, 5:21] %>%
#'  auto_cor() %>%
#'  auto_vif()
#'
#' @rdname auto_vif
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
