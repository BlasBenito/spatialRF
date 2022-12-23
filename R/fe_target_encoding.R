#' Target encoding of character and factor variables
#'
#' @description Target encoding involves replacing the values of categorical variables with continuous values that are indicative of the response variable. There are several approaches to target encoding, including calculating the mean target value for each level of the categorical variable. Target encoding can be useful for improving the performance of machine learning models, but it can also introduce bias if the encoded values are not representative of the true underlying relationship between the categorical variables and the target.
#'
#' This function identifies categorical variables in the input data frame, transforms them using a set of methods defined by the user, and returns the input data frame with the newly encoded variables.
#'
#' The target encoding methods implemented in this function are:
#'
#' \itemize{
#'   \item `rank`: returns the order of the group as a integer, being the 1 the rank of the group with the lower mean of of the response variable. This method accepts the  `noise` argument, which adds white noise to the result to increase data variability and reduce leakage. The variables returned by this method are named with the suffix "__encoded_rank". This method is implemented in the function [fe_target_encoding_rank()].
#'   \item `mean`: uses the mean value of the response over each group in the categorical variable. This option accepts `noise`. The variables returned by this method are named with the suffix "__encoded_mean".  This method is implemented in the function [fe_target_encoding_mean()].
#'   \item `rnorm`: This method computes the mean and standard deviation of the response for each group of the categorical variable, and uses [rnorm()] to generate values taken from a normal distribution. The argument `sd.width` is used as a multiplier of the standard deviation to reduce the range of values produced by [rnorm()] for each group of the categorical predictor. The variables returned by this method are named with the suffix "__encoded_rnorm".  This method is implemented in the function [fe_target_encoding_rnorm()].
#'   \item `loo`: This is the leave-one-out method. Each categorical value is replaced with the mean of the response variable across the other cases within the same group. The variables returned by this method are named with the suffix "__encoded_loo". This method is implemented in the function [fe_target_encoding_loo()].
#' }
#'
#' The methods "mean" and "rank" support the `noise` argument. Values larger than zero in this argument add white noise to the target-encoded variables using `stats::rnorm()` via the function [fe_target_encoding_noise()]). The `noise` argument represents a fraction of the average differences between groups of the target-encoded variable. For example, if noise = 0.25 and the target-encoded variable has the unique values c(1, 2, 3), as it could be the case when using the "rank" method, then the average between-groups difference would be 1, and the range of the noise added to each row would go between 0 and 0.25
#'
#' The method "rnorm" has the argument `sd.width`, which multiplies the standard deviation argument of the `rnorm()` function to limit the spread of the encoded values between groups.
#'
#' @param data (required; data frame, tibble, or sf) A training data frame. Default: `NULL`
#' @param dependent.variable.name (required; character string) Name of the response. Must be a column name of `data`. Default: `NULL`
#' @param predictor.variable.names (required; character vector) Names of all the predictors in `data`. Only character and factor predictors are processed, but all are returned in the "data" slot of the function's output.  Default: `NULL`
#' @param methods (optional; character string). Name of the target encoding methods. Default: `c("mean", "rank", "rnorm", "loo")`
#' @param seed (optional; integer) Random seed to facilitate reproducibility. Default: `1`
#' @param noise (optional; numeric vector) Only in methods "mean" and "rank". Numeric vector with noise values in the range 0-1. Default: `0`.
#' @param sd.width (optional; numeric vector) Only for the method "rnorm". Numeric vector with multiplicators of the standard deviation of each group in the categorical variable, in the range 0.01-1. Default: `0.1`
#' @param verbose (optional; logical) If TRUE, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#'
#' @return
#' If no target encoding is needed because all predictors are numeric, the function returns `data`. Otherwise it returns a list of the class "fe_target_encoding" with the slots:
#' \itemize{
#'   \item `data`: Input data frame, but with target-encoded character or factor columns.
#'   \item `leakage_test`: Data frame with the results of a linear model between the target-encoded variable and the response aimed to identify potential data leakage. It contains the following columns:
#'   \itemize{
#'     \item `encoded_predictor`: name of the target-encoded variable.
#'     \item `correlation_with_response`: R-squared resulting from `cor.test()` on the target-encoded variable and the response.
#'     \item `interpretation`: Interpretation of the test, with the values "Leakage", "Likely leakage", "Unlikely leakage", and "No leakage". If you find concerning results, you may either increase the value of the `noise` argument (if `method = "mean"`), or select the "rnorm" method.
#'   }
#' }
#'
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_continuous_response,
#'   ecoregions_all_predictors
#'   )
#'
#' #the dataframe ecoregions_df contains two categorical variables
#' unique(ecoregions_df$dominant_landcover)
#' unique(ecoregions_df$primary_productivity)
#'
#' #applying all methods for a continuous response
#' output <- fe_target_encoding(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_continuous_response,
#'   predictor.variable.names = ecoregions_all_predictors,
#'   methods = c(
#'     "mean",
#'     "rank",
#'     "rnorm",
#'     "loo"
#'   ),
#'   sd.width = c(0.01, 0.1, 1),
#'   noise = c(0, 1)
#' )
#'
#' #the output has several objects
#' names(output)
#'
#' #names of the encoded predictors
#' output$encoded_predictors
#'
#' #the data with the original and the encoded predictors
#' colnames(output$data)
#'
#' #a leakage test assessing the correlation between the response and the encoded predictors
#' output$leakage_test
#'
#' #plotting the transformations of "primary_productivity"
#' tidyr::pivot_longer(
#'   data = output$data,
#'   cols = dplyr::all_of(
#'     grep(
#'       pattern = "primary_productivity",
#'       x = output$encoded_predictors,
#'       value = TRUE)
#'     )
#' ) %>%
#'   dplyr::select(
#'     plant_richness,
#'     primary_productivity,
#'     name,
#'     value
#'   ) %>%
#'   ggplot2::ggplot() +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = value,
#'     color = primary_productivity
#'   ) +
#'   ggplot2::facet_wrap(~name, scales = "free_y") +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(
#'     x = "Response values",
#'     y = "Encoded values",
#'     color = "Original\ngroups"
#'   )
#'
#' }
#' @importFrom rlang :=
#' @importFrom sf st_drop_geometry
#' @rdname fe_target_encoding
fe_target_encoding <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    methods = c(
      "mean",
      "rank",
      "rnorm",
      "loo"
      ),
    sd.width = 0.1,
    seed = 1,
    noise = 0,
    verbose = TRUE
){

  # data for development
  # data = ecoregions_df
  # dependent.variable.name = ecoregions_continuous_response
  # predictor.variable.names = ecoregions_all_predictors
  # methods = c(
  #   "mean",
  #   "rank",
  #   "rnorm",
  #   "loo"
  # )
  # sd.width = c(0.1, 0.2, 0.3)
  # seed = 1
  # noise = c(0, 0.1, 0.2)
  # verbose = TRUE

  #testing method argument
  methods <- match.arg(
    arg = methods,
    choices = c(
      "rank",
      "mean",
      "rnorm",
      "loo"
    ),
    several.ok = TRUE
  )

  #avoid check complaints
  . <- NULL
  new_value <- NULL
  r_squared <- NULL
  encoded_predictor <- NULL
  interpretation <- NULL
  correlation_with_response <- NULL

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

  if(is.numeric(data[[dependent.variable.name]]
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
      X = dplyr::select(
        data,
        dplyr::all_of(predictor.variable.names)
        ),
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

  #factors to characters
  data <- rapply(
    object = data,
    f = as.character,
    classes = c(
      "factor",
      "ordered",
      "logical"
    ),
    how = "replace"
  )

  #find names of character variables
  categorical.predictors <- predictor.variable.names[unlist(
    lapply(
      X = data[, predictor.variable.names, drop = FALSE],
      FUN = is.character
    )
  )]

  if(verbose == TRUE){
    message(
      "Encoding the variables:\n",
      paste0(
        categorical.predictors,
        collapse = "\n"
      ),
      "\n"
    )
  }

  #original column names
  original.column.names <- colnames(data)

  #iterating over categorical variables
  for(categorical.predictor in categorical.predictors){

      #method "mean"
      if("mean" %in% methods){

        for(noise.i in noise){

        data <- fe_target_encoding_mean(
          data = data,
          dependent.variable.name = dependent.variable.name,
          categorical.variable.name = categorical.predictor,
          noise = noise.i,
          seed = seed,
          verbose = verbose
        )

        }

      }

      if("rank" %in% methods){

        for(noise.i in noise){

        data <- fe_target_encoding_rank(
          data = data,
          dependent.variable.name = dependent.variable.name,
          categorical.variable.name = categorical.predictor,
          noise = noise.i,
          seed = seed
        )

        }

      }


    if("rnorm" %in% methods){

      for(sd.width.i in sd.width){

        data <- fe_target_encoding_rnorm(
          data = data,
          dependent.variable.name = dependent.variable.name,
          categorical.variable.name = categorical.predictor,
          sd.width = sd.width.i,
          seed = seed,
          verbose = verbose
        )

      }

    }

    if("loo" %in% methods){

      data <- fe_target_encoding_loo(
        data = data,
        dependent.variable.name = dependent.variable.name,
        categorical.variable.name = categorical.predictor
      )

    }

  } #end of iteration over predictors

  #new variables
  encoded.predictors <- setdiff(
    x = colnames(data),
    y = original.column.names
  )


  leakage.df <- lapply(
    X = sf::st_drop_geometry(data[, encoded.predictors]),
    FUN = function(x){
      stats::cor.test(
        x,
        data[[dependent.variable.name]]
      )$estimate
    }
  ) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(
      r_squared = "."
    ) %>%
    dplyr::mutate(
      r_squared = round(r_squared, 2),
      encoded_predictor = encoded.predictors,
      interpretation = dplyr::case_when(

        r_squared >= 0.9 ~ "Leakage",
        r_squared < 0.9 & r_squared >= 0.75 ~ "Likely leakage",
        r_squared < 0.75 & r_squared >= 0.25 ~ "Unlikely leakage",
        r_squared < 0.25 ~ "No leakage"
      )
    ) %>%
    dplyr::transmute(
      encoded_predictor,
      correlation_with_response = r_squared,
      interpretation
    ) %>%
    dplyr::arrange(
      correlation_with_response
    )

  rownames(leakage.df) <- seq_len(nrow(leakage.df))


  #to tibble
  if(return.tibble == TRUE){
    data <- tibble::as_tibble(data)
    leakage.df <- tibble::as_tibble(leakage.df)
  } else {
    if(!("sf" %in% class(data))){
      data <- as.data.frame(data)
    }
    leakage.df <- as.data.frame(leakage.df)
  }

  #preparing output object
  out <- list(
    encoded_predictors = encoded.predictors,
    data = data,
    leakage_test = leakage.df
  )

  class(out) <- "fe_target_encoding"

  #message with output
  if(verbose == TRUE){
    message(
      "Leakage test for method:\n\n",
      paste0(
        utils::capture.output(as.data.frame(leakage.df)),
        collapse = "\n"
      ),
      "\n\nr_squared: correlation between the target-encoded variable and the response.\n"
    )
  }

  #return output
  out

}


