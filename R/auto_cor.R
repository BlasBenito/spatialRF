#' @title Multicollinearity reduction via Pearson correlation
#' @description Iterative reduction of multicollinearity via Pearson bivariate correlation.
#'
#' The function `auto_cor()` applies a recursive algorithm to remove variables with a Pearson correlation with another variable higher than a given threshold (defined by the argument `max.cor`).  When two variables are correlated above this threshold, the one with the highest sum of R-squared with all the other variables is removed.
#'
#' The function `auto_vif()` allows the user to define preference selection order via the argument `preference.order`. If `preference.order` is `"c("a", "b")`, they are correlated above `max.cor`, there there are other variables in `data`, and the sum of R-squared with all other variables of `"a"` is higher than the sum of `"b"`, then `"b"` is removed anyway.
#'
#' The argument `preference.order` allows the user to "protect" variables that might be interesting or even required for the given analysis.
#'
#' If `preference.order` is not provided, then the predictors are ranked from lower to higher sum of R-squared with the other preodictors, and removed one by one until the maximum R-squared of the correlation matrix is lower than `max.cor`.
#'
#' If there are categorical variables named in `predictor.variable.names` and `dependent.variable.name` is provided, then the function applies [fe_target_encoding()] with the method "mean" to transform the categorical variables into numeric. If a categorical variable is selected, then its original categorical values are returned.
#'
#' @param data (required; data frame or tibble) A data frame with predictors. Default: `NULL`.
#' @param dependent.variable.name (optional; character string) Name of the dependent variable. Only required when there are categorical variables within `predictor.variable.names`. Default: `NULL`
#' @param predictor.variable.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. If `NULL`, all the columns in data except `dependent.variable.name` are used. Default: `NULL`
#' @param preference.order  (optional; character vector) Character vector indicating the user's order of preference to keep variables. Predictors not included in this argument are ranked by the sum of their correlation with other variables (variables with higher sums receive lower ranks and have therefore lower preference order). Default: `NULL`.
#' @param max.cor (optional; numeric) Numeric between 0 and 1, with recommended values between 0.5 and 0.9. Maximum Pearson correlation between any pair of the selected variables. Default: `0.75`
#' @param verbose (optional, logical) Logical. if `TRUE`, describes the function operations to the user. Default:: `TRUE`
#' @return Character vector with predictor variable names.
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#'  data(
#'   ecoregions_df,
#'   ecoregions_numeric_predictors
#'   )
#'
#'  #on a data frame
#'  out <- auto_cor(
#'    data = ecoregions_df,
#'    predictor.variable.names = ecoregions_numeric_predictors
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
#'  #with preference order (fiver first in ecoregions_numeric_predictors)
#'  out <- auto_cor(
#'    data = ecoregions_df,
#'    predictor.variable.names = ecoregions_numeric_predictors,
#'    preference.order = ecoregions_numeric_predictors[1:5],
#'  )
#'
#'  #with pipes
#'  out <- auto_cor(
#'    data = ecoregions_df,
#'    predictor.variable.names = ecoregions_numeric_predictors
#'  ) %>%
#'  auto_vif()
#'
#' }
#' @seealso [auto_vif()]
#' @rdname auto_cor
#' @export
auto_cor <- function(
    predictor.variable.names = NULL,
    data = NULL,
    dependent.variable.name = NULL,
    preference.order = NULL,
    max.cor = 0.75,
    verbose = TRUE
){

  if(is.null(data)){
    stop("Argument 'data' is required.")
  }

  #returning all variables
  if((is.null(max.cor)) | (max.cor > 1)){
    if(verbose == TRUE){
      message("max.cor is NULL or larger than 1, returning all predictor's names.")
    }
    return(predictor.variable.names)
  }

  if(max.cor < 0){
    max.cor <- 0
    if(verbose == TRUE){
      message("max.cor is negative, setting it to 0.")
    }
  }

  #dropping geometry if sf
  if("sf" %in% class(data)){
    data <- sf::st_drop_geometry(data)
  }

  #setting predictor.variable.names
  if(!is.null(predictor.variable.names)){
    predictor.variable.names <- intersect(
      x = predictor.variable.names,
      y = colnames(data)
    )
  } else {
    predictor.variable.names <- setdiff(
      x = colnames(data),
      y = dependent.variable.name
    )
  }


  #coerce categorical to numeric with target encoding
  if(!is.null(dependent.variable.name) && dependent.variable.name %in% colnames(data)){

    data <- fe_target_encoding(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      methods = "mean",
      replace = TRUE,
      verbose = verbose
    )

  }

  #subset for correlation analysis
  data <- data[, predictor.variable.names]

  #finding zero variance columns
  zero.variance.columns <- colnames(data)[round(apply(data, 2, var), 6) == 0]
  if(length(zero.variance.columns) > 0){
    if(verbose == TRUE){
      message(
        "These columns have almost zero variance and will be ignored: ",
        paste(
          zero.variance.columns,
          collapse = ", "
        )
      )
    }
  }

  #remove zero variance columns
  if(length(zero.variance.columns) > 0){

    predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero.variance.columns)]

    #subset for correlation analysis
    data <- data[, predictor.variable.names]

  }

  #compute correlation matrix of x
  data.cor <- cor(
    x = data,
    use = "complete.obs"
  ) %>%
    abs()

  #diagonals to zero
  diag(data.cor) <- 0

  #auto preference order
  #variables with lower sum of cor with others go higher
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

  #organize the correlation matrix according to preference.order
  data.cor <- data.cor[preference.order, preference.order]

  #iterating through columns
  for(i in seq(from = ncol(data.cor), to = 1)){

    #remove i column if max > max.cor
    if(max(data.cor[, i]) > max.cor){

      #remove it from data.cor
      data.cor <- data.cor[-i, -i]

    }

    if(is.null(dim(data.cor))){
      break
    }

  }

  removed.vars <- setdiff(
    x = colnames(data),
    y = colnames(data.cor)
  )

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
      message("[auto_cor()]: Variables are not collinear, noting to do here.")
    }
  }

  #selected variables
  selected.variables <- preference.order[preference.order %in% setdiff(predictor.variable.names, removed.vars)]

  if(verbose == TRUE){
    message(
      paste0(
        "The selected variables are:\n",
        paste(selected.variables, collapse = "\n")
      )
    )
  }

  #return output
  selected.variables

}
