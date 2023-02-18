#' Pairwise Pearson correlation
#'
#' @description Returns the pairwise Pearson correlation between all pairs of predictors in a training dataset.
#'
#' Please note that near-zero variance columns are ignored by this function. Use [mc_auto_vif()] to remove them.
#'
#' @param data (required; data frame or tibble) A data frame with predictors. Default: `NULL`.
#' @param predictors.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param verbose (optional, logical) Set to `FALSE` to silence messages. Default: `TRUE`
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
#'       predictors.names = ecoregions_all_predictors
#' )
#'
#' }
mc_cor <- function(
    data = NULL,
    predictors.names = NULL,
    verbose = TRUE
){

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
