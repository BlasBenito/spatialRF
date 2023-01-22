#' Scaling and centering
#'
#' @description Convenient wrapper of the function [scale()] to standardize a given set of numeric columns in a training data frame via centering and/or scaling.
#'
#' Centering:
#'
#' If `center = TRUE` (default value), each element in a numeric column `a` is centered by computing `a - mean(a)`. This operation centers the values of `a` to zero, generating negative values.
#'
#' Scaling:
#'
#' If `center = TRUE`, the data in column `a` is centered via `a - mean(a)` and then is divided by the standard deviation of `a`, as in `(a - mean(a))/sd(a)`.
#'
#' If `center = FALSE`, the column `a` is divided by its root-mean-square using the expression `a/(sqrt(sum(a^2)/(length(a)-1)))`.
#'
#' What columns are standardized?
#'
#' If only `data` is provided, then all numeric columns in `data` are scaled.
#'
#' If `data` and `dependent.variable.name` are provided, then `dependent.variable.name` is scaled if it is numeric and continuous (binary dependent variables with values 0 and 1 are not scaled). This is useful to compare the importance scores of models fitted with response variables with different ranges and/or units.
#'
#' If `data` and `predictor.variable.names` are provided, then all numeric variables in `predictor.variable.names` are scaled.
#'
#' @param data (required, data frame, tibble, or sf data frame) Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name (optional; character string) Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names (optional; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param center (optional, logical) If `TRUE`, the selected numeric columns are centered by subtracting their respective means. Default: `TRUE`
#' @param scale (optional, logical) If `TRUE`, the data is scaled. If `center = TRUE`, the scaling is performed using the expression `(a - mean(a))/sd(a)` If `center = FALSE`, then a numeric column `a` is scaled using the expression `a/(sqrt(sum(a^2)/(length(a)-1)))`

#' @param verbose (optional, logical) If `TRUE`, the function prints message indicating what columns have been scaled. Default: `TRUE`
#'
#' @return Input data frame with scaled numeric response and predictors.
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_all_predictors,
#'   ecoregions_continuous_response
#' )
#'
#' data.scaled <- fe_scale(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_continuous_response,
#'   predictor.variable.names = ecoregions_all_predictors
#'   )
#'
#' }
#'
fe_scale <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    scale = TRUE,
    center = TRUE,
    verbose = TRUE
){

  if(is.null(data)){
    stop("Argument 'data' must be provided")
  }

  if(!("data.frame" %in% class(data))){
    stop("Argument 'data' must be of class 'data.frame'.")
  }

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #columns to scale
  cols <- c(dependent.variable.name, predictor.variable.names)
  if(is.null(cols)){
    cols <- colnames(data)
  } else {
    cols <- intersect(cols, colnames(data))
  }

  #find numeric predictors
  cols <- lapply(
    X = data[, cols],
    FUN = is.numeric
  ) %>%
    unlist()

  #subset numerics
  cols <- names(cols)[cols]

  if(length(cols) == 0){

    message("No numeric columns to scale in 'data', returning the original input.")

  } else {

    #message
    if(verbose == TRUE){
      message(
        "Scaling columns:\n",
        paste0(cols, collapse = "\n")
      )
    }

    #scale
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(cols),
          base::scale,
          scale = scale,
          center = center
        ),
        dplyr::across(
          tidyselect::all_of(cols),
          as.vector
        )
      ) %>%
      as.data.frame()


    #return tibble
    if(return.tibble == TRUE){
      data <- tibble::as_tibble(data)
    }

  }

  data

}
