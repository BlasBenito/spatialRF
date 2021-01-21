#' @title statistical_mode
#' @description computes the mode of a numeric or character vector
#' @param x numeric or character vector
#' @return statistical mode of x
#' @examples
#' \dontrun{
#' if(interactive()){
#'  out <- statistical_mode(c(rep(10, 10), rep(9, 9)))
#'  out
#'  }
#' }
#' @rdname statistical_mode
#' @export
statistical_mode <- function(x){
  x.unique <- unique(x)
  x.unique[which.max(tabulate(match(x, x.unique)))]
  x.unique <- unlist(x.unique[1])
  x.unique
}
