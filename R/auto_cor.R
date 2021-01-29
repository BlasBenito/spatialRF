#' @title auto_cor
#' @description Reduces the bivariate correlation in a data frame of predictors
#' @param x A data frame with predictors. Default: `NULL`.
#' @param preference.order Character vector indicating the user's order of preference to keep variables. Default: `NULL`.
#' @param cor.threshold Numeric between 0 and 1, maximum Pearson correlation between any pair of the selected variables. Default: 0.75
#' @return List with two slots:
#' \itemize{
#'   \item **cor**: Correlation matrix of the selected variables.
#'   \item **selected.variables**: Character vector with the names of the selected variables.
#' }
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(plant_richness_df)
#'  out <- auto_cor(x = plant_richness_df[, 4:21])
#'  out$selected.variables
#'  }
#' }
#' @rdname auto_cor
#' @export
auto_cor <- function(
  x = NULL,
  preference.order = NULL,
  cor.threshold = 0.75
){

  #removing na
  x <- na.omit(x)

  #compute correlation matrix of x
  x.cor <- abs(cor(x))

  #diagonals to zero
  diag(x.cor) <- 0

  #reorder by preference
  if(!is.null(preference.order)){
    x.cor <- x.cor[preference.order, preference.order]
  }

  #iterate through columns
  i <- 1
  while(i <= ncol(x.cor)){

    #select variables with cor below cor.threshold
    vars.to.keep <- names(which(x.cor[, i] <= cor.threshold))

    #subsetting
    x.cor <- x.cor[vars.to.keep, vars.to.keep]

    #adding 1 to the index
    i <- i + 1

  }

  #return output
  output.list <- list()
  output.list$cor <- x.cor
  output.list$selected.variables <- colnames(x.cor)
  output.list

}
