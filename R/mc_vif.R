#' @title Variance Inflation Factor
#'
#' @description
#'
#' Computes the Variance Inflation Factor of all variables in a training data frame.
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all the remaining variables of the input data set as predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity.
#'
#' Please note that near-zero variance columns are ignored by this function. Use [mc_auto_vif()] to remove them.
#'
#' @param data (required; data frame or tibble) A data frame with predictors. Default: `NULL`.
#' @param predictors.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param verbose (optional, logical) Set to `FALSE` to silence messages. Default: `TRUE`
#'
#' @return Data frame with the columns "variable" containing the predictor name, and "vif" containing the variance inflation factor of the given variable.
#' @rdname mc_vif
#' @export
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_continuous_response,
#'   ecoregions_all_predictors
#' )
#'
#' df <- mc_vif(
#'       data = ecoregions_df,
#'       predictors.names = ecoregions_all_predictors
#' )
#'
#' }
mc_vif <- function(
    data = NULL,
    predictors.names = NULL,
    verbose = TRUE
){

  vif <- NULL

  data <- check_data(
    data = data,
    drop.geometry = TRUE,
    verbose = verbose
  )

  predictors.names <- check_predictors_names(
    predictors.names = predictors.names,
    data = data,
    numeric.only = TRUE,
    na.allowed = TRUE,
    zero.variance.allowed = FALSE,
    is.required = TRUE,
    verbose = verbose
  )

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #subset for correlation analysis
  data <- data[, predictors.names]

  #stop if not enough data
  if(ncol(data) == 1){
    stop("There are not enough predictors to perform the analysis.")
  }

  #check cor
  data.cor <- mc_cor(
    data = data,
    predictors.names = predictors.names
  )

  if(max(data.cor$cor) >= 0.99){
    stop("The maximum correlation between a pair of predictors is > 0.99. The VIF computation will fail. Please use the output of 'mc_cor_auto()' as input for the argument 'predictors.names'.")
  }

  #vif data frame
  vif.df <- data.frame(
    diag(
      solve(
        cor(
          x = data,
          use = "complete.obs"
        ),
        tol = 0
      )
    ),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::rename(vif = 1) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::mutate(vif = round(vif, 3)) %>%
    dplyr::arrange(vif)

  if(return.tibble == TRUE){
    vif.df <- tibble::as_tibble(vif.df)
  } else {
    vif.df <- as.data.frame(vif.df)
  }

  vif.df

}





