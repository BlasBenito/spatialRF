#' @title Multicollinearity reduction via Pearson correlation
#' @description Computes the correlation matrix among a set of predictors, orders the correlation matrix according to a user-defined preference order, and removes variables one by one, taking into account the preference order, until the remaining ones are below a given Pearson correlation threshold. \strong{Warning}: variables in `preference.order` not in `colnames(x)`, and non-numeric columns are removed silently from `x` and `preference.order`. The same happens with rows having NA values ([na.omit()] is applied). The function issues a warning if zero-variance columns are found.
#' @param x A data frame with predictors, or the result of [auto_vif()] Default: `NULL`.
#' @param preference.order Character vector indicating the user's order of preference to keep variables. Predictors not included in this argument are ranked by the sum of their correlation with other variables (variables with higher sums receive lower ranks and have therefore lower preference order). Default: `NULL`.
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
#' if(interactive()){
#'
#'  #loading example data
#'  data(
#'   ecoregions_df,
#'   ecoregions_predvar_names
#'   )
#'
#'  #on a data frame
#'  out <- auto_cor(x = ecoregions_df[, ecoregions_predvar_names])
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
#'  out <- auto_vif(x = ecoregions_df[, ecoregions_predvar_names])
#'  out <- auto_cor(x = out)
#'
#'  #with pipes
#'  out <- ecoregions_df[, ecoregions_predvar_names] %>%
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
  cor.threshold = 0.75,
  verbose = TRUE
){

  if(cor.threshold < 0){
    cor.threshold <- 0
    if(verbose == TRUE){
      message("cor.threshold is negative, setting it to 0.")
    }
  }

  if(cor.threshold > 1){
    cor.threshold <- 1
    if(verbose == TRUE){
      message("cor.threshold is larger than 1, setting it to 1 (this will not work well anyway!).")
    }
  }

  if(inherits(x, "variable_selection")){
    x <- x$selected.variables.df
  }

  #coerce to data frame if tibble
  if(inherits(x, "tbl_df") | inherits(x, "tbl")){
    x <- as.data.frame(x)
  }

  #removing NA
  x <- na.omit(x)

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
    x <- x[, !(colnames(x) %in% non.numeric.columns), drop = FALSE]
  }

  #finding zero variance columns
  zero.variance.columns <- colnames(x)[round(apply(x, 2, var), 6) == 0]
  if(length(zero.variance.columns) > 0){
    warning(
      "These columns have almost zero variance and might cause issues: ",
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

  #auto preference order
  preference.order.auto <- colSums(x.cor) %>%
    sort() %>%
    names()

  #if there is no preference order
  if(is.null(preference.order)){

    preference.order <- preference.order.auto

  } else {

    #subset preference.order to colnames(x)
    preference.order <- preference.order[preference.order %in% colnames(x)]

    #if there are variables not in preference.order, add them in the order of preference.order.auto
    if(length(preference.order) < ncol(x)){

      not.in.preference.order <- colnames(x)[!(colnames(x) %in% preference.order)]
      preference.order <- c(preference.order, preference.order.auto[preference.order.auto %in% not.in.preference.order])

    }

  }

  #organize the matrix according to preference.order
  x.cor <- x.cor[preference.order, preference.order]

  #vector to store variables to remove
  removed.vars <- vector()

  #iterating through columns
  for(i in seq(from = ncol(x.cor), to = 1)){

    #find max correlation in x.cor
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
          "[auto_cor()]: Removed variables: \n",
          paste0(
            removed.vars,
            collapse = ", \n"
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

  if(verbose == TRUE){
    message(
      paste0(
        "The selected variables are:\n",
        paste(selected.variables, collapse = "\n")
      )
    )
  }

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
