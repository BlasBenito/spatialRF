#' @title Generates case weights for binary data
#' @description When the data is binary, setting the `ranager` argument `case.weights` helps to minimize the issues produced by class imbalance. This function takes a binary response variable and returns a vector of weights populated with the values `1/#zeros` and `1/#ones`. It is used internally by the function [rf()].
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @return A vector with a length equal to `nrow(data)` with the respective weights of the cases.
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  data <- data.frame(
#'    response = c(0, 0, 0, 1, 1)
#'  )
#'
#'  case_weights(
#'    data = data,
#'    dependent.variable.name = "response"
#'  )
#'
#'  }
#' }
#' @rdname case_weights
#' @export
case_weights <- function(
  data = NULL,
  dependent.variable.name = NULL
){

  #counting number of ones and zeros
  n <- nrow(data)
  n.1 <- sum(data[, dependent.variable.name])
  n.0 <- n - n.1

  #computing weights
  weight.1 <- 1/n.1
  weight.0 <- 1/n.0

  #vector of weights
  case.weights <- rep(NA, n)
  case.weights[data[, dependent.variable.name] == 1] <- weight.1
  case.weights[data[, dependent.variable.name] == 0] <- weight.0

  #return case weights
  case.weights

}
