#' Scaling to a new range
#'
#' @description Re-scales all indicated variables to a new range.
#'
#' @param data (required, data frame, tibble, or sf data frame) Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param response.name (optional; character string) Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictors.names (optional; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param new.min (optional, numeric) New minimum value. Default: `0`
#' @param new.max (optional, numeric) New maximum value. Default: `1`
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
#' data.rescaled <- fe_rescale(
#'   data = ecoregions_df,
#'   response.name = ecoregions_continuous_response,
#'   predictors.names = ecoregions_all_predictors,
#'   new.min = 0,
#'   new.max = 100
#'   )
#'
#' }
#'
fe_rescale <- function(
    data = NULL,
    response.name = NULL,
    predictors.names = NULL,
    new.min = 0,
    new.max = 1,
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
  if(is.null(columns.to.scale)){
    columns.to.scale <- colnames(data)
  } else {
    columns.to.scale <- intersect(columns.to.scale, colnames(data))
  }

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
          rescale_vector,
          new.min = new.min,
          new.max = new.max
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
