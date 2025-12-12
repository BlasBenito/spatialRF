#' @title Checks if dependent variable is binary with values 1 and 0
#' @description Returns `TRUE` if `dependent.variable.name` is a binary variable with the values 1 and 0.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @return Logical.
#' @examples
#'
#'  #dummy data frame
#'  data <- data.frame(
#'    response = c(0, 0, 0, 1, 1)
#'  )
#'
#'  #checking if response is binary
#'  is_binary(
#'    data = data,
#'    dependent.variable.name = "response"
#'  )
#'
#' @rdname is_binary
#' @export
is_binary <- function(
  data = NULL,
  dependent.variable.name = NULL
) {
  if (
    sort(unique(data[, dependent.variable.name]))[1] == 0 &
      sort(unique(data[, dependent.variable.name]))[2] == 1 &
      length(unique(data[, dependent.variable.name])) == 2
  ) {
    binary.data <- TRUE
  } else {
    binary.data <- FALSE
  }
  binary.data
}
