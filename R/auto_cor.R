#' @title Multicollinearity reduction via Pearson correlation
#' @description Computes the correlation matrix among a set of predictors, orders the correlation matrix according to a user-defined preference order, and removes variables one by one, taking into account the preference order, until the remaining ones are below a given Pearson correlation threshold. \strong{Warning}: variables in `preference.order` not in `colnames(x)`, and non-numeric columns are removed silently from `x` and `preference.order`. The same happens with rows having NA values ([na.omit()] is applied). The function issues a message if zero-variance columns are found.
#' @param data A data frame with predictors. Default: `NULL`.
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. If `NULL`, all the columns in data are used, and non-numeric ones discarded. Default: `NULL`
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
#'   ecoregions_predictor_variable_names
#'   )
#'
#'  #on a data frame
#'  out <- auto_cor(
#'    data = ecoregions_df,
#'    predictor.variable.names = ecoregions_predictor_variable_names
#'  )
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
#'  #with preference order (fiver first in ecoregions_predictor_variable_names)
#'  out <- auto_cor(
#'    data = ecoregions_df,
#'    predictor.variable.names = ecoregions_predictor_variable_names,
#'    preference.order = ecoregions_predictor_variable_names[1:5],
#'  )
#'
#'  #with pipes
#'  out <- auto_cor(
#'    data = ecoregions_df,
#'    predictor.variable.names = ecoregions_predictor_variable_names
#'  ) %>%
#'  auto_vif()
#'
#' }
#' @seealso [auto_vif()]
#' @rdname auto_cor
#' @export
auto_cor <- function(
    data = NULL,
    predictor.variable.names = NULL,
    preference.order = NULL,
    cor.threshold = 0.75,
    verbose = TRUE
){

  #subsetting data
  if(!is.null(predictor.variable.names)){
    predictor.variable.names <- intersect(
      x = predictor.variable.names,
      y = colnames(data)
    )
    data <- data[, predictor.variable.names]
  }

  if(is.null(cor.threshold)){
    cor.threshold <- 1
  }

  if(cor.threshold < 0){
    cor.threshold <- 0
    if(verbose == TRUE){
      message("cor.threshold is negative, setting it to 0.")
    }
  }

  if(cor.threshold > 1){
    cor.threshold <- 1
    if(verbose == TRUE){
      message("cor.threshold is larger than 1, setting it to 1 (this will select all variables!).")
    }
  }

  #coerce to data frame if tibble
  if(inherits(data, "tbl_df") | inherits(data, "tbl")){
    data <- as.data.frame(data)
  }

  #removing NA
  data <- na.omit(data)

  #finding and removing non-numeric columns
  non.numeric.columns <- colnames(data)[!sapply(data, is.numeric)]
  if(length(non.numeric.columns) > 0){
    if(verbose == TRUE){
      message(
        "These columns are non-numeric and will be removed: ",
        paste(
          non.numeric.columns,
          collapse = ", "
        )
      )
    }
    data <- data[, !(colnames(data) %in% non.numeric.columns), drop = FALSE]
  }

  #finding zero variance columns
  zero.variance.columns <- colnames(data)[round(apply(data, 2, var), 6) == 0]
  if(length(zero.variance.columns) > 0){
    if(verbose == TRUE){
      message(
        "These columns have almost zero variance and might cause issues: ",
        paste(
          zero.variance.columns,
          collapse = ", "
        )
      )
    }
  }


  #compute correlation matrix of x
  data.cor <- abs(cor(data))

  #diagonals to zero
  diag(data.cor) <- 0

  #auto preference order
  preference.order.auto <- colSums(data.cor) %>%
    sort() %>%
    names()

  #if there is no preference order
  if(is.null(preference.order)){

    preference.order <- preference.order.auto

  } else {

    #subset preference.order to colnames(x)
    preference.order <- preference.order[preference.order %in% colnames(data)]

    #if there are variables not in preference.order, add them in the order of preference.order.auto
    if(length(preference.order) < ncol(data)){

      not.in.preference.order <- colnames(data)[!(colnames(data) %in% preference.order)]
      preference.order <- c(preference.order, preference.order.auto[preference.order.auto %in% not.in.preference.order])

    }

  }

  #organize the matrix according to preference.order
  data.cor <- data.cor[preference.order, preference.order]

  #vector to store variables to remove
  removed.vars <- vector()

  #iterating through columns
  for(i in seq(from = ncol(data.cor), to = 1)){

    #find max correlation in data.cor
    data.cor.max <- apply(data.cor, 2, FUN = max)

    #remove i column if max > cor.threshold
    if(data.cor.max[i] > cor.threshold){

      #identify column name
      variable.to.remove <- names(data.cor.max[i])

      #adding it to removed.vars
      removed.vars <- c(removed.vars, variable.to.remove)

      #remove it from data.cor
      data.cor <- data.cor[
        rownames(data.cor) != variable.to.remove,
        rownames(data.cor) != variable.to.remove
      ]

    }

    if(is.null(dim(data.cor))){
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
  selected.variables <- preference.order[preference.order %in% setdiff(colnames(data), removed.vars)]
  selected.variables.df <- data[, selected.variables, drop = FALSE]

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
