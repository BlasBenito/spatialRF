#' Target encoding of character and factor variables
#'
#' @description Converts character and factor variables to numeric using the method known as "greedy target encoding". For each character or factor column, it replaces each value with the corresponding mean of the response column (as defined by the argument `dependent.variable.name`). For example, if the response column has the values 1, 2, 3, and 4, and the character column has the values "a", "a", "b", and "b", then "a" is replaced with 1.5, and "b" is replaced by 3.5.
#'
#' Target encoding facilitates using any kind of character or factor variable as numeric.
#'
#' @param data (required; data frame, tibble, or sf) A training data frame. Default: `NULL`
#' @param dependent.variable.name (required; character string) Name of the response. Must be a column name of `data`. Default: `NULL`
#' @param predictor.variable.names (required; character vector). Names of all the predictors in `data`.  Default: `NULL`
#' @param method (optional; character string). Name of the target encoding method. The ones available are:
#' \itemize{
#'   \item `mean`: uses the mean value of the response over the group. This option may lead to leakage if no `noise` is applied.
#'   \item `rnorm`: uses `rnorm()` to generate values taken from a normal distribution with the mean and standard deviation of the response over the group. Aggressive option against leakage.
#' }
#' @param seed (optional; integer) Random seed to facilitate reproducibility. If set to a given number, the returned model is always the same. Default: `1`
#' @param noise (optional; numeric) Numeric in the range 0-1. Noise to add to the encoding to reduce data leakage. Expressed as a quantile of `dependent.variable.name`. If `noise = 0.1`, a random number between `min(dependent.variable.name)` and `quantile(dependent.variable.name, probs = 0.1)` will be added to the encoding. This option only applies to `method = "mean"`. Default: `0`.
#' @param verbose (optional; logical) If TRUE, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#'
#' @return
#' If no target encoding is needed because all predictors are numeric, the function returns `data`. Otherwise it returns a list of the class "target_encoding" with the slots:
#' \itemize{
#'   \item `data`: Input data frame, but with target-encoded character or factor columns.
#'   \item `leakage_test`: Data frame with the results of a linear model between the target-encoded variable and the response aimed to identify potential data leakage. It contains the following columns:
#'   \itemize{
#'     \item `variable`: name of the target-encoded variable.
#'     \item `r_squared`: R-squared resulting from `cor.test()` on the target-encoded variable and the response.
#'     \item `interpretation`: Interpretation of the test, with the values "Leakage", "Likely leakage", "Unlikely leakage", and "No leakage". If you find concerning results, you may either increase the value of the `noise` argument (if `method = "mean"`), or select the "rnorm" method.
#'   }
#'   \item `encoding_map`: List with slots named after the variables in `predictor.variable.names` that have been target encoded. Each slot contains a data frame with the old and new values of each target-encoded variable.
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
#' x$encoding_map
#'
#' }
#' @rdname target_encoding
target_encoding <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    method = "mean",
    seed = 1,
    noise = 0,
    verbose = TRUE
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

    if(verbose == TRUE){
      message("All predictors are numeric, nothing to do. Returning the input data frame.")
    }

    return(data)

  }

  #copy of data
  data.copy <- data

  #factors to characters
  data <- rapply(
    object = data,
    f = as.character,
    classes = "factor",
    how = "replace"
  )

  #find names of character variables
  character.variables <- predictor.variable.names[unlist(
    lapply(
      X = data[, predictor.variable.names],
      FUN = is.character
    )
  )]

  if(verbose == TRUE){
    message(
      "Encoding the variables:\n",
      paste0(
        character.variables,
        collapse = "\n"
      )
    )
  }

  #ENCODING METHODS
  ##################################

  if(method == "mean"){

    data <- target_encoding_mean(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      noise = noise,
      seed = seed
    )

  }

  if(method == "rnorm"){

    data <- target_encoding_rnorm(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      seed = seed
    )

  }

  #ENCODING MAPS AND CORRELATION WITH RESPONSE
  #####################################
  encoding.maps <- list()
  leakage.df <- data.frame(
    variable = character.variables,
    r_squared = NA,
    interpretation = NA
  )

  for(character.variable in character.variables){

    #encoding map
    encoding.maps[[character.variable]] <- data.frame(
      old_value = dplyr::pull(
        data.copy,
        character.variable
      ),
      new_value = dplyr::pull(
        data,
        character.variable
      )
    )

    if(return.tibble == TRUE){
      encoding.maps[[character.variable]] <- tibble::as_tibble(encoding.maps[[character.variable]])
    }


    #data for linear models
    y <- dplyr::pull(
      data,
      dependent.variable.name
    )

    x <- dplyr::pull(
      data,
      character.variable
    )

    #correlation test
    cor.test.result <- stats::cor.test(
      x,
      y
    )

    #write results to leakage.df
    leakage.df[
      leakage.df$variable == character.variable,
      "r_squared"
    ] <- round(cor.test.result$estimate, 3)

  }

  #add interpretation
  leakage.df <- dplyr::mutate(
    leakage.df,
    interpretation = dplyr::case_when(

      r_squared >= 0.9 ~ "Leakage",
      r_squared < 0.9 & r_squared >= 0.75 ~ "Likely leakage",
      r_squared < 0.75 & r_squared >= 0.25 ~ "Unlikely leakage",
      r_squared < 0.25 ~ "No leakage"
    )
  )

  if(return.tibble == TRUE){
    data <- tibble::as_tibble(data)
    leakage.df <- tibble::as_tibble(leakage.df)
  }

  #preparing output object
  out <- list(
    data = data,
    leakage_test = leakage.df,
    encoding_map = encoding.maps
  )

  class(out) <- "target_encoding"

  #message with output
  if(verbose == TRUE){
    message(
      "Leakage test:\n\n",
      paste0(
        utils::capture.output(leakage.df),
        collapse = "\n"
      ),
      "\n\nr_squared: correlation between the target-encoded variable and the response."
    )
  }

  #return output
  out

}

#' @rdname target_encoding
#' @export
target_encoding_mean <- function(
    data,
    dependent.variable.name,
    predictor.variable.names,
    noise = 0,
    seed = 1
){

  #noise bounds
  if(noise < 0){
    noise <- 0
  }
  if(noise > 1){
    noise <- 1
  }

  #find names of character variables
  character.variables <- predictor.variable.names[unlist(
    lapply(
      X = data[, predictor.variable.names],
      FUN = is.character
    )
  )]

  #iterating over character variables
  for(character.variable in character.variables){

    #aggregate by groups
    df.map <- stats::aggregate(
      x = dplyr::pull(
        data,
        dependent.variable.name
      ),
      by = list(
        dplyr::pull(
          data,
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
    data <- dplyr::select(
      data,
      -{{character.variable}}
    )

    #rename encoded variable
    colnames(data)[colnames(data) == "target_encoding"] <- character.variable

    #add noise, if any
    if(noise > 0){

      #finding min and max noise limits
      y <- dplyr::pull(
        data,
        character.variable
      )

      #generate noise
      set.seed(seed)

      if(is_binary_response(
        x = dplyr::pull(
          data,
          dependent.variable.name
        )
      ) == TRUE){

        noise.vector <- stats::runif(
          n = length(y),
          min = 0,
          max = noise
        )

      } else {

        noise.vector <- stats::runif(
          n = length(y),
          min = min(y),
          max = stats::quantile(
            x = y,
            probs = noise
          )
        )

      }


      #add noise
      data[, character.variable] <- data[, character.variable] + noise.vector

    }

  }

  data

}

#' @rdname target_encoding
#' @export
target_encoding_rnorm <- function(
    data,
    dependent.variable.name,
    predictor.variable.names,
    seed = 1
){

  #noise bounds
  if(noise < 0){
    noise <- 0
  }
  if(noise > 1){
    noise <- 1
  }

  #find names of character variables
  character.variables <- predictor.variable.names[unlist(
    lapply(
      X = data[, predictor.variable.names],
      FUN = is.character
    )
  )]

  #iterating over character variables
  for(character.variable in character.variables){

    set.seed(seed)

    #iterate over groups to encode variable
    for(group.i in unique(dplyr::pull(
      data,
      character.variable
    ))){

      group.i.response <- data[
        data[, character.variable] == group.i,
        dependent.variable.name
        ]

      data[
        data[, character.variable] == group.i,
        character.variable
      ] <- stats::rnorm(
        n = length(group.i.response),
        mean = mean(group.i.response),
        sd = sd(group.i.response)
      )

    } #end of iteration over groups

  } #end of iteration over variables

  data

}
