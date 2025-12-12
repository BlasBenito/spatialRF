#' @title Multicollinearity reduction via Pearson correlation
#' @description Computes the correlation matrix among a set of predictors, orders the correlation matrix according to a user-defined preference order, and removes variables one by one, taking into account the preference order, until the remaining ones are below a given Pearson correlation threshold. \strong{Warning}: variables in `preference.order` not in `colnames(x)`, and non-numeric columns are removed silently from `x` and `preference.order`. The same happens with rows having NA values ([na.omit()] is applied). The function issues a warning if zero-variance columns are found.
#' @param x A data frame with predictors, or the result of [auto_vif()] Default: `NULL`.
#' @param preference.order Character vector indicating the user's order of preference to keep variables. Doesn't need to contain If not provided, variables in `x` are prioritised by their column order. Default: `NULL`.
#' @param cor.threshold Numeric between 0 and 1, with recommended values between 0.5 and 0.9. Maximum Pearson correlation between any pair of the selected variables. Default: `0.50`
#' @param verbose Logical. if `TRUE`, describes the function operations to the user. Default:: `TRUE`
#' @return List with three slots:
#' \itemize{
#'   \item `cor`: correlation matrix of the selected variables.
#'   \item `selected.variables`: character vector with the names of the selected variables.
#'   \item `selected.variables.df`: data frame with the selected variables.
#' }
#' @details Can be chained together with [auto_vif()] through pipes, see the examples below.
#' @examples
#' if(interactive()){
#'
#'  #load data
#'  data(plant_richness_df)
#'
#'  #on a data frame
#'  out <- auto_cor(x = plant_richness_df[, 5:21])
#'
#'  #getting the correlation matrix
#'  out$cor
#'
#'  #getting the names of the selected variables
#'  out$selected.variables
#'
#'  #getting the data frame of selected variables
#'  out$selected.variables.df
#'
#'  #on the result of auto_vif
#'  out <- auto_vif(x = plant_richness_df[, 5:21])
#'  out <- auto_cor(x = out)
#'
#'  #with pipes
#'  out <- plant_richness_df[, 5:21] %>%
#'  auto_vif() %>%
#'  auto_cor()
#'
#' }
#' @seealso [auto_vif()]
#' @rdname auto_cor
#' @export
auto_cor <- function(
  x = NULL,
  preference.order = NULL,
  cor.threshold = 0.50,
  verbose = TRUE
){

  if(inherits(x, "variable_selection")){
    x <- x$selected.variables.df
  }

  #coerce to data frame if tibble
  if(inherits(x, "tbl_df") | inherits(x, "tbl")){
    x <- as.data.frame(x)
  }

  #removing NA
  x <- na.omit(x)

  #handle edge case: no columns
  if (ncol(x) == 0) {
    output.list <- list()
    output.list$selected.variables <- character(0)
    output.list$selected.variables.df <- x
    class(output.list) <- "variable_selection"
    return(output.list)
  }

  #handle edge case: single column (nothing to correlate)
  if (ncol(x) == 1) {
    output.list <- list()
    output.list$selected.variables <- colnames(x)
    output.list$selected.variables.df <- x
    class(output.list) <- "variable_selection"
    if (verbose == TRUE) {
      message("[auto_cor()]: Only one variable, nothing to filter.")
    }
    return(output.list)
  }

  #finding and removing non-numeric columns
  non.numeric.columns <- colnames(x)[!sapply(x, is.numeric)]
  if(length(non.numeric.columns) > 0){
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
  if(length(zero.variance.columns) > 0){
    warning(
      "These columns have zero variance and might cause issues: ",
      paste(
        zero.variance.columns,
        collapse = ", "
      )
    )
  }


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

  } else {

    preference.order <- colnames(x)

  }

  #vector to store variables to remove
  removed.vars <- vector()

  #iterating through columns
  for(i in seq(ncol(x.cor), 1)){

    #compute max
    x.cor.max <- apply(x.cor, 2, FUN = max)

    #remove i column if max > cor.threshold
    if(x.cor.max[i] > cor.threshold){

      #identify column name
      variable.to.remove <- names(x.cor.max[i])

      #adding it to removed.vars
      removed.vars <- c(removed.vars, variable.to.remove)

      #remove it from x.cor
      x.cor <- x.cor[
        rownames(x.cor) != variable.to.remove,
        rownames(x.cor) != variable.to.remove
      ]

    }

    if(is.null(dim(x.cor))){
      break
    }

  }

  #message
  if(verbose == TRUE){
    if(length(removed.vars) != 0){
      message(
        paste0(
          "[auto_cor()]: Removed variables: ",
          paste0(
            removed.vars,
            collapse = ", "
          )
        )
      )
    } else {
      message("[auto_cor()]: Variables are not collinear.")
    }
  }

  #selected variables
  selected.variables <- setdiff(colnames(x), removed.vars)
  selected.variables.df <- x[, selected.variables, drop = FALSE]

  #return output
  output.list <- list()
  if(length(selected.variables) > 1){
    output.list$cor <- round(cor(selected.variables.df), 2)
  }
  output.list$selected.variables <- selected.variables
  output.list$selected.variables.df <- selected.variables.df

  class(output.list) <- "variable_selection"

  output.list

}
