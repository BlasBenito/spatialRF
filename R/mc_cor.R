#' Pairwise Pearson correlation
#'
#' @description Returns the pairwise Pearson correlation between all pairs of predictors in a training dataset.
#'
#' If there are categorical variables named in `predictors.names` and `response.name` is provided, then the function applies [fe_target_encoding()] with the method "mean" to transform the categorical variables into numeric. If a categorical variable is selected, then its original categorical values are returned.
#'
#' Please note that near-zero variance columns are ignored by this function. Use [mc_auto_vif()] to remove them.
#'
#' @param data (required; data frame or tibble) A data frame with predictors. Default: `NULL`.
#' @param response.name (optional; character string) Name of the dependent variable. Only required when there are categorical variables within `predictors.names`. If not provided, non-numeric columns are ignored. Default: `NULL`
#' @param predictors.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. If `NULL`, all the columns in data except `response.name` are used. Default: `NULL`
#'
#' @return Data frame with the columns "a" and "b" containing the names of pairs of predictors, and "cor" containing Pearson correlation between each pair of predictors.
#' @rdname mc_cor
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
#' df <- mc_cor(
#'       data = ecoregions_df,
#'       predictors.names = ecoregions_all_predictors,
#'       response.name = ecoregions_continuous_response
#' )
#'
#' }
mc_cor <- function(
    data = NULL,
    predictors.names = NULL,
    response.name = NULL
){

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

  #setting predictors.names
  if(is.null(predictors.names)){

    #from data
    predictors.names <- colnames(data)

  } else {

    #ensuring they are in data
    predictors.names <- intersect(
      x = predictors.names,
      y = colnames(data)
    )

  }

  #response.name
  if(
    is.null(response.name) |
    (!is.null(response.name) && !(response.name %in% colnames(data)))
  ){

    #take numerics only
    predictors.names <- colnames(data[, predictors.names])[sapply(data[, predictors.names], is.numeric)]

  } else {

    #coerce categorical to numeric
    data <- fe_target_encoding(
      data = data,
      response.name = response.name,
      predictors.names = predictors.names,
      methods = "mean",
      replace = TRUE,
      verbose = FALSE
    )

  }

  #subset for correlation analysis
  data <- data[, predictors.names]

  #finding zero variance columns
  zero.variance.columns <- colnames(data)[round(apply(data, 2, var), 6) == 0]

  #remove zero variance columns
  if(length(zero.variance.columns) > 0){

    predictors.names <- predictors.names[!(predictors.names %in% zero.variance.columns)]

    #subset for correlation analysis
    data <- data[, predictors.names]

  }

  #stop if not enough data
  if(ncol(data) == 1){
    stop("There are not enough predictors to perform the analysis.")
  }

  #correlation matrix
  cor.matrix <- cor(
    x = data,
    use = "complete.obs"
  ) %>%
    abs() %>%
    round(3)

  #selecting upper triangle only
  cor.matrix.upper.tri <- upper.tri(cor.matrix)

  #not upper tri to na
  cor.matrix[!cor.matrix.upper.tri] <- NA

  #to long format
  cor.df <- cor.matrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "a") %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(predictors.names),
      names_to = "b",
      values_to = "cor"
    ) %>%
    na.omit() %>%
    dplyr::arrange(
      dplyr::desc(cor)
    )

  if(return.tibble == TRUE){
    cor.df <- tibble::as_tibble(cor.df)
  } else {
    cor.df <- as.data.frame(cor.df)
  }

  #returning output
  cor.df

}
