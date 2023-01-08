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
#' If there are categorical variables named in `predictor.variable.names` and `dependent.variable.name` is provided, then the function applies [fe_target_encoding()] with the method "mean" to transform the categorical variables into numeric. If a categorical variable is selected, then its original categorical values are returned.
#'
#' Please note that near-zero variance columns are ignored by this function. Use [mc_auto_vif()] to remove them.
#'
#' @param data (required; data frame or tibble) A data frame with predictors. Default: `NULL`.
#' @param dependent.variable.name (optional; character string) Name of the dependent variable. Only required when there are categorical variables within `predictor.variable.names`. Default: `NULL`
#' @param predictor.variable.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. If `NULL`, all the columns in data except `dependent.variable.name` are used. Default: `NULL`
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
#'       predictor.variable.names = ecoregions_all_predictors,
#'       dependent.variable.name = ecoregions_continuous_response
#' )
#'
#' }
mc_vif <- function(
    data = NULL,
    predictor.variable.names = NULL,
    dependent.variable.name = NULL
){

  vif <- NULL

  if(is.null(data)){
    stop("Argument 'data' is required.")
  }

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #dropping geometry if sf
  if("sf" %in% class(data)){
    data <- sf::st_drop_geometry(data)
  }

  #setting predictor.variable.names
  if(!is.null(predictor.variable.names)){
    predictor.variable.names <- intersect(
      x = predictor.variable.names,
      y = colnames(data)
    )
  } else {

    if(!is.null(dependent.variable.name)){
      predictor.variable.names <- colnames(data)
    } else {
      predictor.variable.names <- setdiff(
        x = colnames(data),
        y = dependent.variable.name
      )
    }

  }

  #coerce categorical to numeric with target encoding
  if(!is.null(dependent.variable.name) && dependent.variable.name %in% colnames(data)){

    data <- fe_target_encoding(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      methods = "mean",
      replace = TRUE,
      verbose = FALSE
    )

  }

  #subset for correlation analysis
  data <- data[, predictor.variable.names]

  #finding zero variance columns
  zero.variance.columns <- colnames(data)[round(apply(data, 2, var), 6) == 0]


  #remove zero variance columns
  if(length(zero.variance.columns) > 0){

    predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero.variance.columns)]

    #subset for correlation analysis
    data <- data[, predictor.variable.names]

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





