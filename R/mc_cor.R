#' Pairwise Pearson correlation
#'
#' @description Returns the pairwise Pearson correlation between all pairs of predictors in a training dataset.
#'
#' If there are categorical variables named in `predictor.variable.names` and `dependent.variable.name` is provided, then the function applies [fe_target_encoding()] with the method "mean" to transform the categorical variables into numeric. If a categorical variable is selected, then its original categorical values are returned.
#'
#' @param data (required; data frame or tibble) A data frame with predictors. Default: `NULL`.
#' @param dependent.variable.name (optional; character string) Name of the dependent variable. Only required when there are categorical variables within `predictor.variable.names`. Default: `NULL`
#' @param predictor.variable.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. If `NULL`, all the columns in data except `dependent.variable.name` are used. Default: `NULL`
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
#'       predictor.variable.names = ecoregions_all_predictors,
#'       dependent.variable.name = ecoregions_continuous_response
#' )
#'
#' }
mc_cor <- function(
    data = NULL,
    predictor.variable.names = NULL,
    dependent.variable.name = NULL
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
      verbose = verbose
    )

  }

  #subset for correlation analysis
  data <- data[, predictor.variable.names]

  #finding zero variance columns
  zero.variance.columns <- colnames(data)[round(apply(data, 2, var), 6) == 0]
  if(length(zero.variance.columns) > 0){
    if(verbose == TRUE){
      message(
        "These columns have almost zero variance and will be ignored: ",
        paste(
          zero.variance.columns,
          collapse = ", "
        )
      )
    }
  }

  #remove zero variance columns
  if(length(zero.variance.columns) > 0){

    predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero.variance.columns)]

    #subset for correlation analysis
    data <- data[, predictor.variable.names]

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
      cols = dplyr::all_of(predictor.variable.names),
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
