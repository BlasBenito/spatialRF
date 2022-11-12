#' @title Generates case weights for binary responses
#' @description When the data is binary, setting the `ranager` argument `case.weights` helps to minimize the issues produced by class imbalance. This function takes a binary response variable and returns a vector of weights populated with the values `1/#zeros` and `1/#ones`. It is used internally by the function [rf()].
#' @param x Numeric vector with values 1 and 0 representing the response column to train a binary-response model. Default: `NULL`
#' @param case.weights Numeric vector with case weights. Only for internal use within the package. Default: `NULL`
#' @return A vector with a length equal to `x` with case weights.
#' @examples
#' if(interactive()){
#'
#'  case_weights(
#'    x = c(0, 0, 0, 1, 1)
#'  )
#'
#'  }
#' @rdname case_weights
#' @export
case_weights <- function(
    x = NULL,
    case.weights = NULL
){

  binary <- is_binary_response(x)

  if(is.null(x) | !binary){
    return(case.weights)
  }

  #if the user did not provide case weights
  if(is.null(case.weights)){

    #computing case weights
    #counting number of ones and zeros
    n <- length(x)
    n.1 <- sum(x)
    n.0 <- n - n.1

    #computing weights
    weight.1 <- 1/n.1
    weight.0 <- 1/n.0

    #vector of weights
    case.weights <- rep(NA, n)
    case.weights[x == 1] <- weight.1
    case.weights[x == 0] <- weight.0

    return(case.weights)

  } else {

    if(length(case.weights) == length(x)){

      return(case.weights)

    } else {
      stop("The length of the argument 'case.weights' must match the number of rows of 'data'.")
    }

  }

}

