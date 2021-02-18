#' @title Multicollinearity reduction via Pearson correlation
#' @description Computes the correlation matrix among a set of predictors, orders the correlation matrix according to a user-defined preference order, and removes variables one by one, taking into account the preference order, until the remaining ones are below a given Pearson correlation threshold. \strong{Warning}: variables in `preference.order` not in `colnames(x)`, columns with zero variance, and non-numeric columns are removed silently from `x` and `preference.order`. The same happens with rows having NA values ([na.omit()] is applied).
#' @param x A data frame with predictors, or the result of [auto_vif()] Default: `NULL`.
#' @param preference.order Character vector indicating the user's order of preference to keep variables. Doesn't need to contain If not provided, variables in `x` are prioritised by their column order. Default: `NULL`.
#' @param cor.threshold Numeric between 0 and 1, with recommended values between 0.5 and 0.9. Maximum Pearson correlation between any pair of the selected variables. Default: `0.75`
#' @param verbose Logical. if `TRUE`, describes the function operations to the user. Default:: `TRUE`
#' @return List with three slots:
#' \itemize{
#'   \item `cor`: correlation matrix of the selected variables.
#'   \item `selected.variables`: character vector with the names of the selected variables.
#'   \item `selected.variables.df`: data frame with the selected variables.
#' }
#' @details Can be chained together with [auto_vif()] through pipes, see the examples below.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(plant_richness_df)
#'
#'  #on a data frame
#'  out <- auto_cor(x = plant_richness_df[, 4:21])
#'  out$selected.variables
#'
#'  #on the result of auto_vif
#'  out <- auto_vif(x = plant_richness_df[, 5:20])
#'  out <- auto_cor(x = out)
#'
#'  #with pipes (cor and vif thresholds are arbitrary)
#'  out <- plant_richness_df[, 5:20] %>%
#'  auto_vif(vif.threshold = 2.5) %>%
#'  auto_cor(cor.threshold = 0.4)
#'
#'  }
#' }
#' @seealso [auto_vif()]
#' @rdname auto_cor
#' @export
auto_cor <- function(
  x = NULL,
  preference.order = NULL,
  cor.threshold = 0.75,
  verbose = TRUE
){

  if(inherits(x, "variable_selection")){
    x <- x$selected.variables.df
  }

  #removing columns with zero variance
  x <- na.omit(x)
  x <- x[sapply(x, is.numeric)]
  x <- x[ , which(apply(x, 2, var) != 0)]


  #compute correlation matrix of x
  x.cor <- abs(cor(x))

  #diagonals to zero
  diag(x.cor) <- 0

  #completing preference order
  if(!is.null(preference.order)){
    #subset preference.order to colnames(x)
    preference.order <- preference.order[preference.order %in% colnames(x)]
    #if there are variables not in preference.order, add them in any order
    if(length(preference.order) < ncol(x)){
      not.in.preference.order <- colnames(x)[!(colnames(x) %in% preference.order)]
      preference.order <- c(preference.order, not.in.preference.order)
    }
    #organize the matrix according to preference.order
    x.cor <- x.cor[preference.order, preference.order]
  }


  #iterating through columns
  for(i in ncol(x.cor):1){

    #compute max
    x.cor.max <- apply(x.cor, 2, FUN = max)

    #remove i column if max > cor.threshold
    if(x.cor.max[i] > cor.threshold){

      #identify column name
      variable.to.remove <- names(x.cor.max[i])

      #remove it from x.cor
      x.cor <- x.cor[
        rownames(x.cor) != variable.to.remove,
        rownames(x.cor) != variable.to.remove
      ]
    }

  }

  #message
  if(verbose == TRUE){
    removed.vars <- setdiff(colnames(x), colnames(x.cor))
    message(
      paste0(
        "[auto_cor()]: Removed variables: ",
        paste0(
          removed.vars,
          collapse = ", "
        )
      )
    )
  }

  #return output
  output.list <- list()
  output.list$cor <- round(cor(x[, colnames(x.cor)]), 3)
  output.list$selected.variables <- colnames(x.cor)
  output.list$selected.variables.df <- x[, colnames(x.cor)]

  class(output.list) <- "variable_selection"

  output.list

}
