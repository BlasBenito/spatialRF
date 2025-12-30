#' @title Check if variable is binary with values 0 and 1
#' @description Tests whether a variable contains only the values 0 and 1.
#' @param data Data frame containing the variable to check.
#' @param dependent.variable.name Character string with the name of the variable to test. Must be a column name in `data`.
#' @return Logical. `TRUE` if the variable contains exactly two unique values (0 and 1), `FALSE` otherwise.
#' @details
#' This function is used internally by spatialRF to determine whether to apply classification-specific methods. The function returns `FALSE` if:
#' \itemize{
#'   \item The variable has more than two unique values
#'   \item The variable has only one unique value (constant)
#'   \item The unique values are not exactly 0 and 1 (e.g., 1 and 2, or TRUE and FALSE)
#' }
#'
#' Missing values (NA) are ignored when determining unique values.
#' @examples
#' # Binary variable (returns TRUE)
#' is_binary(
#'   data = data.frame(response = c(0, 0, 0, 1, 1)),
#'   dependent.variable.name = "response"
#' )
#'
#' # Non-binary variable (returns FALSE)
#' is_binary(
#'   data = data.frame(response = c(1, 2, 3, 4, 5)),
#'   dependent.variable.name = "response"
#' )
#'
#' # Binary but wrong values (returns FALSE)
#' is_binary(
#'   data = data.frame(response = c(1, 1, 2, 2)),
#'   dependent.variable.name = "response"
#' )
#'
#' @rdname is_binary
#' @family preprocessing
#' @export
#' @autoglobal
is_binary <- function(
  data = NULL,
  dependent.variable.name = NULL
) {
  if (
    sort(unique(data[, dependent.variable.name]))[1] == 0 &&
      sort(unique(data[, dependent.variable.name]))[2] == 1 &&
      length(unique(data[, dependent.variable.name])) == 2
  ) {
    binary.data <- TRUE
  } else {
    binary.data <- FALSE
  }
  binary.data
}
