#' @title Principal Components Analysis
#' @description Extracts all factors of a principal component analysis of a matrix or data frame. Just a convenient wrapper for [prcomp].
#' @usage
#' pca(
#'   x = NULL,
#'   colnames.prefix = "pca_factor"
#' )
#' @param x numeric matrix or data frame, Default: NULL
#' @param colnames.prefix character, name prefix for the output columns, Default: 'pca_factor'
#' @return A data frame with the PCA factors of `x`.
#' @details Columns in `x` with zero variance are removed before computing the PCA.
#' @seealso [pca_multithreshold()]
#' @examples
#' if(interactive()){
#'
#'  #load example distance matrix
#'  data(distance_matrix)
#'
#'  #PCA of the distance matrix
#'  out <- pca(x = distance_matrix)
#'  out
#'
#' }
#' @rdname pca
#' @importFrom stats prcomp var
#' @export
pca <- function(
  x = NULL,
  colnames.prefix = "pca_factor"
){

  if(inherits(x, "tbl_df") | inherits(x, "tbl")){
    x <- as.data.frame(x)
  }

  #removing columns with zero variance
  x <- x[ , which(apply(x, 2, var) != 0)]

  #computing pca of distance matrix
  x.pca <- prcomp(x, scale. = TRUE)

  #getting pca factors
  x.pca.factors <- as.data.frame(x.pca$x)
  colnames(x.pca.factors) <- paste(
    colnames.prefix,
    seq(1, ncol(x.pca.factors)
        ),
    sep = "_"
    )

  #returning output
  x.pca.factors

}
