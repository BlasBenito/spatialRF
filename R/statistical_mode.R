#' @title Statistical mode of a vector
#' @description Computes the mode of a numeric or character vector
#' @param x Numeric or character vector.
#' @return Statistical mode of `x`.
#' @examples
#' if(interactive()){
#'
#'  statistical_mode(c(10, 9, 10, 8))
#'
#' }
#' @rdname statistical_mode
#' @export
statistical_mode <- function(x){

  x.unique <- unique(x)

  x.unique[which.max(tabulate(match(x, x.unique)))]

}
