#' @title Variance Inflation Factor of a data frame
#' @description Computes the variance inflation factor (VIF) of the colums in a data frame. \strong{Warning}: variables in `preference.order` not in `colnames(x)`, and non-numeric columns are removed silently from `x` and `preference.order`. The same happens with rows having NA values ([na.omit()] is applied). The function issues a warning if zero-variance columns are found.
#' @param x Data frame with numeric columns, typically containing a set of model predictors.
#' @return A data frame with two columns having the name of the variables in 'x' and their respective VIF values.
#' @seealso [auto_vif()], [auto_cor()]
#' @examples
#'
#'  data(plants_df)
#'
#'  vif(plants_df[, 5:21])
#'
#' @rdname vif
#' @importFrom tibble rownames_to_column
#' @export
vif <- function(x) {
  if (!is.data.frame(x)) {
    stop("x must be a data frame with numeric columns")
  }

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

  out <- x %>%
    na.omit() %>%
    as.matrix() %>%
    cor() %>%
    solve() %>%
    diag() %>%
    sort(decreasing = TRUE) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variable")

  colnames(out)[2] <- "vif"

  out$vif <- round(out$vif, 3)

  return(out)
}
