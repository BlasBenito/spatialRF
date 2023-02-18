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
#' If `data` and `response.name` are provided, then `response.name` is scaled if it is numeric and continuous (binary dependent variables with values 0 and 1 are not scaled). This is useful to compare the importance scores of models fitted with response variables with different ranges and/or units.
#'
#' If `data` and `predictors.names` are provided, then all numeric variables in `predictors.names` are scaled.
#'
#' @param data (required, data frame, tibble, or sf data frame) Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param response.name (required if `predictors.names = NULL`; character string) Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictors.names (required if `response.name = NULL`; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
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
#'   response.name = ecoregions_continuous_response,
#'   predictors.names = ecoregions_all_predictors
#'   )
#'
#' }
#'
fe_scale <- function(
    data = NULL,
    response.name = NULL,
    predictors.names = NULL,
    scale = TRUE,
    center = TRUE,
    verbose = TRUE
){

  #checking data
  ##############
  data <- check_data(
    data = data,
    verbose = verbose
  )

  predictors.names <- check_predictors_names(
    predictors.names = predictors.names,
    data = data,
    numeric.only = FALSE,
    is.required = FALSE,
    verbose = verbose
  )

  response.name <- check_response_name(
    response.name = response.name,
    data = data,
    is.required = FALSE
  )

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #columns to scale
  columns.to.scale <- c(response.name, predictors.names)

  #find numeric predictors
  columns.to.scale <- numeric_columns(
    data = data,
    columns = columns.to.scale
  )

  if(length(columns.to.scale) == 0){

    message("No numeric columns to scale in 'data', returning the original input.")

  } else {

    #message
    if(verbose == TRUE){
      message(
        "Scaling columns:\n",
        paste0(columns.to.scale, collapse = "\n")
      )
    }

    #scale
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(columns.to.scale),
          base::scale,
          scale = scale,
          center = center
        ),
        dplyr::across(
          tidyselect::all_of(columns.to.scale),
          as.vector
        )
      )

  }

  #return tibble
  if(return.tibble == FALSE){
    data <- as.data.frame(data)
  }

  data

}
