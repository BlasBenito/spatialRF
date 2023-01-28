#' Scaling to a new range
#'
#' @description Re-scales all indicated variables to a new range.
#'
#' @param data (required, data frame, tibble, or sf data frame) Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name (optional; character string) Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names (optional; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param new.min New minimum value. Default: `0`
#' @param new.max New maximum value. Default: `1`
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
#'   dependent.variable.name = ecoregions_continuous_response,
#'   predictor.variable.names = ecoregions_all_predictors,
#'   new.min = 0,
#'   new.max = 100
#'   )
#'
#' }
#'
fe_rescale <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    new.min = 0,
    new.max = 1,
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
          rescale_vector,
          new.min = new.min,
          new.max = new.max
        ),
        dplyr::across(
          tidyselect::all_of(cols),
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
