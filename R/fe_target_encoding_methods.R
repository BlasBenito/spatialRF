#' Target encoding of character and factor variables
#'
#' @description Converts character and factor variables to numeric using the method known as "greedy target encoding". For each character or factor column, it replaces each value with the corresponding mean of the response column (as defined by the argument `dependent.variable.name`). For example, if the response column has the values 1, 2, 3, and 4, and the character column has the values "a", "a", "b", and "b", then "a" is replaced with 1.5, and "b" is replaced by 3.5.
#'
#' Target encoding facilitates using any kind of character or factor variable as numeric.
#'
#' @param data (required; data frame, tibble, or sf) A training data frame. Default: `NULL`
#' @param dependent.variable.name (required; character string) Name of the response. Must be a column name of `data`. Default: `NULL`
#' @param predictor.variable.names (required; character vector). Names of all the predictors in `data`. Only character and factor predictors are processed, but all are returned in the "data" slot of the function's output.  Default: `NULL`
#' @param method (optional; character string). Name of the target encoding method. The ones available are:
#' \itemize{
#'   \item `rank` (default method): returns the order of the group as a integer if `noise = 0`, being the 1 the rank of the group with the lower mean of `dependent.variable.name`. This option accepts `noise`.
#'   \item `mean`: uses the mean value of the response over the group. This option accepts `noise`.

#'   \item `rnorm`: uses `rnorm()` to generate values taken from a normal distribution with the mean and standard deviation of the response over the group. This option does not accept `noise`.
#'   \item `leave-one-out` or `loo`: within each group, each character string is replaced with the mean of `dependent.variable.name` over the other cases of the same group.
#' }
#' @param seed (optional; integer) Random seed to facilitate reproducibility. If set to a given number, the returned model is always the same. Default: `1`
#' @param noise (optional; numeric) Only in methods "mean" and "rank". Numeric in the range 0-1. White noise (generated with `stats::rnorm()`) to add to a target-encoded variable to increase diversity and reduce data leakage. Represents a fraction of the average between-groups difference of a target-encoded variable. For example, if noise = 0.25 and the target-encoded variable has the unique values c(1, 2, 3), as it could be the case when using the "rank" method, then the average between-groups difference would be 1, and the range of the noise added to each row would go between 0 and 0.25. Default: `0`.
#' @param sd.width (optional; numeric) Only in method "rnorm". Numeric in the range 0.01-1 representing the width of the actual per-group standard deviation to use. For example, if the standard deviation of the values of the response for a given group in a character variable is 1.2, and sd.wdith = 0.5, then the standard deviation used in `rnorm()` is 0.6 instead. Smaller numbers help reduce the width of the random values assigned to each group. Default: `0.5` (half the standard deviation of the response for each group).
#' @param verbose (optional; logical) If TRUE, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#'
#' @return
#' If no target encoding is needed because all predictors are numeric, the function returns `data`. Otherwise it returns a list of the class "fe_target_encoding" with the slots:
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
#'
#' }
#' @rdname target_encoding_methods
#' @export
fe_target_encoding_mean <- function(
    data,
    dependent.variable.name,
    categorical.variable.name,
    noise = 0,
    seed = 1,
    verbose = TRUE
){

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_mean"
  )

  #aggregate by groups
  df.map <- tapply(
    X = data[[dependent.variable.name]],
    INDEX = data[[categorical.variable.name]],
    FUN = mean,
    na.rm = TRUE
  )
  df.map <- data.frame(names(df.map), df.map)
  names(df.map) <- c(
    categorical.variable.name,
    categorical.variable.new.name
  )

  #join
  data <- dplyr::inner_join(
    x = data,
    y = df.map,
    by = categorical.variable.name
  )

  #add noise if any
  data <- fe_target_encoding_noise(
    data = data,
    categorical.variable.name = categorical.variable.new.name,
    noise = noise,
    seed = seed
  )

  if(verbose == TRUE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  data

}


#' @rdname target_encoding_methods
#' @export
fe_target_encoding_rnorm <- function(
    data,
    dependent.variable.name,
    categorical.variable.name,
    sd.width = 0.1,
    seed = 1,
    verbose = TRUE
){

  if(sd.width <= 0){
    sd.width <- 0.0001
  }
  if(sd.width > 1){
    sd.width <- 1
  }

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_rnorm"
  )

  set.seed(seed)
  data <- data %>%
    dplyr::group_by(.data[[categorical.variable.name]]) %>%
    dplyr::mutate(
      !!categorical.variable.new.name := stats::rnorm(
        n = dplyr::n(),
        mean = mean(
          get(dependent.variable.name),
          na.rm = TRUE
        ),
        sd = sd(
          get(dependent.variable.name),
          na.rm = TRUE
        ) * sd.width
      )
    ) %>%
    dplyr::ungroup()

  if(verbose == TRUE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  data

}


#' @rdname target_encoding_methods
#' @export
fe_target_encoding_rank <- function(
    data,
    dependent.variable.name,
    categorical.variable.name,
    noise = 0,
    seed = 1,
    verbose = TRUE
){

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_rank"
  )

  #aggregate by groups
  df.map <- tapply(
    X = data[[dependent.variable.name]],
    INDEX = data[[categorical.variable.name]],
    FUN = mean,
    na.rm = TRUE
  ) %>%
    sort()

  df.map <- data.frame(
    names(df.map),
    1:length(df.map)
  )
  names(df.map) <- c(categorical.variable.name, categorical.variable.new.name)

  #merge
  data <- dplyr::inner_join(
    x = data,
    y = df.map,
    by = categorical.variable.name
  )

  #add noise if any
  data <- fe_target_encoding_noise(
    data = data,
    categorical.variable.name = categorical.variable.new.name,
    noise = noise,
    seed = seed
  )

  if(verbose == TRUE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  data

}

#' @rdname target_encoding_methods
#' @export
fe_target_encoding_loo <- function(
    data,
    dependent.variable.name,
    categorical.variable.name,
    verbose = TRUE
){

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_loo"
  )

  #leave one out
  #by group, sum all cases of the response, subtract the value of the current row, and divide by n-1
  data <- data %>%
    dplyr::group_by_at(categorical.variable.name) %>%
    dplyr::mutate(
      !!categorical.variable.new.name := (
        sum(
          get(dependent.variable.name),
          na.rm = TRUE
        ) -
          get(dependent.variable.name)
      ) /
        (dplyr::n() - 1)
    )

  if(verbose == TRUE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  data

}

