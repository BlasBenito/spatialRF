#' Scale model predictors
#'
#' @description Applies the `scale()` function to the numeric predictors of a training data frame.
#'
#' @param data (required if `model` is `NULL`; data frame or tibble) Data frame with a response variable and a set of predictors. If `data` is a tibble, all data frames in the output model are coerced to tibble. Default: `NULL`
#' @param predictor.variable.names (required if `model` is `NULL`; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Optionally, the result of [auto_cor()] or [auto_vif()]. Default: `NULL`
#'
#' @return Input data frame with scaled numeric predictors.
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' data(
#' ecoregions_df,
#' ecoregions_all_predictors
#' )
#'
#' data.scaled <- fe_scale_predictors(
#'   data = ecoregions_df,
#'   predictor.variable.names = ecoregions_all_predictors
#'   )
#'
#' }
#'
fe_scale_predictors <- function(
    data,
    predictor.variable.names
){

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #find numeric predictors
  numeric.predictors <- lapply(
    X = data[, predictor.variable.names],
    FUN = is.numeric
  ) %>%
    unlist()

  numeric.predictors <- predictor.variable.names[numeric.predictors]

  #scale
  data <- data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(numeric.predictors),
        scale
      )
    ) %>%
    as.data.frame()

  #check scaling


  #return tibble
  if(return.tibble == TRUE){
    data <- tibble::as_tibble(data)
  }

  data

}


#' Scale model predictors
#'
#' @description Applies the `scale()` function to the numeric predictors of a training data frame.
#'
#' @param data (required if `model` is `NULL`; data frame or tibble) Data frame with a response variable and a set of predictors. If `data` is a tibble, all data frames in the output model are coerced to tibble. Default: `NULL`
#' @param dependent.variable.name (required if `model` is `NULL`; character string) Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names (required if `model` is `NULL`; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Optionally, the result of [auto_cor()] or [auto_vif()]. Default: `NULL`
#'
#' @return Input data frame with scaled numeric response and predictors.
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' data(
#' ecoregions_df,
#' ecoregions_all_predictors,
#' ecoregions_continuous_response
#' )
#'
#' data.scaled <- fe_scale_all(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_continuous_response,
#'   predictor.variable.names = ecoregions_all_predictors
#'   )
#'
#' }
#'
fe_scale_all <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL
){

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #find numeric predictors
  numeric.variables <- lapply(
    X = data[, c(
      dependent.variable.name,
      predictor.variable.names
      )],
    FUN = is.numeric
  ) %>%
    unlist()

  numeric.variables <- predictor.variable.names[numeric.variables]

  #scale
  data <- data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(numeric.variables),
        scale
      )
    ) %>%
    as.data.frame()


  #return tibble
  if(return.tibble == TRUE){
    data <- tibble::as_tibble(data)
  }

  data

}
