#' Target encoding of character and factor variables
#'
#' @description Converts character and factor variables to numeric. For each character or factor column, it replaces each value with the corresponding mean of the response column (as defined by the argument `dependent.variable.name`). For example, if the response column has the values 1, 2, 3, and 4, and the character column has the values "a", "a", "b", and "b", then "a" is replaced with 1.5, and "b" is replaced by 3.5.
#'
#' Target encoding facilitates using any kind of character or factor variable as numeric.
#'
#' @param data (required; data frame, tibble, or sf) A training data frame. Default: `NULL`
#' @param dependent.variable.name (required; character string) Name of the response. Must be a column name of `data`. Default: `NULL`
#' @param predictor.variable.names (required; character vector). Names of all the predictors in `data`.  Default: `NULL`
#'
#' @return A list with two slots:
#' \itemize{
#'   \item `data`: Input data frame, but with target-encoded character or factor columns.
#'   \item `encoding_maps`: List with slots named after the variables in `predictor.variable.names` that have been target encoded. Each slot contains a data frame with the old and new values of each target-encoded variable.
#' }
#'
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' x <- target_encoding(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_continuous_response,
#'   predictor.variable.names = ecoregions_all_predictors
#'   )
#'
#' x$data
#'
#' x$encoding_maps
#'
#' }
target_encoding <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL
){

  #avoid check complaints
  . <- NULL

  #CHECK INPUT ARGUMENTS

  if(is.null(data)){
    stop("Argument 'data' must be provided.")
  }

  if(is.null(dependent.variable.name)){
    stop("Argument 'dependent.variable.name' must be provided.")
  }

  if(is.null(predictor.variable.names)){
    stop("Argument 'predictor.variable.names' must be provided.")
  }

  if(!(dependent.variable.name %in% colnames(data))){
    stop("Argument 'dependent.variable.name' must be a column of 'data'")
  }

  if(sum(predictor.variable.names %in% colnames(data)) < length(predictor.variable.names)){

    stop(
      paste0(
        "The predictor.variable.names ",
        paste0(
          predictor.variable.names[!(predictor.variable.names %in% colnames(data))],
          collapse = ", "
        ),
        " are missing from 'data'"
      )
    )
  }

  if(is.numeric(
    dplyr::pull(
      data,
      dependent.variable.name
    )
  ) == FALSE){
    stop("The column ", dependent.variable.name, " must be numeric.")
  }

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #return data if all predictors are numeric
  data.numeric <- unlist(
    lapply(
      X = data[, predictor.variable.names],
      FUN = is.numeric
    )
  )


  if(
    sum(data.numeric) == length(predictor.variable.names)
  ){
    message("All predictors are numeric already, nothing to do.")
    return(data)
  }

  #factors to characters
  data <- rapply(
    object = data,
    f = as.character,
    classes = "factor",
    how = "replace"
  )

  #indices of the character columns
  character.variables <- predictor.variable.names[unlist(
    lapply(
      X = data[, predictor.variable.names],
      FUN = is.character
    )
  )]

  #replace NA with "nodata"
  data.character <- data[, character.variables] %>%
    replace(
      list = is.na(.),
      values = "nodata"
    )

  #iterating over character variables to aggregate
  maps <- list()

  for(character.variable in character.variables){

    #aggregate
    df.map <- stats::aggregate(
      x = dplyr::pull(
        data,
        dependent.variable.name
        ),
      by = list(dplyr::pull(
        data.character,
        character.variable
        )
        ),
      FUN = mean
    )
    names(df.map) <- c(character.variable, "target_encoding")

    #merge
    data <- merge(
      x = data,
      y = df.map,
      by = character.variable
    )

    #remove character variable
    data[, character.variable] <- NULL

    #rename encoded variable
    colnames(data)[colnames(data) == "target_encoding"] <- character.variable

    #add to maps
    colnames(df.map) <- c("old_value", "new_value")

    if(return.tibble == TRUE){
      df.map <- tibble::as_tibble(df.map)
    }

    maps[[character.variable]] <- df.map

  }

  if(return.tibble == TRUE){
    data <- tibble::as_tibble(data)
  }

  #preparing output object
  out <- list(
    data = data,
    encoding_maps = maps
  )

  #return output
  out

}
