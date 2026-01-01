#' @title Remove redundant spatial predictors
#' @description Removes spatial predictors that are highly correlated with other spatial predictors or with non-spatial predictors. Particularly useful when using multiple distance thresholds that produce correlated spatial predictors.
#' @param data Data frame containing the predictor variables. Default: `NULL`.
#' @param predictor.variable.names Character vector of non-spatial predictor names. Must match column names in `data`. Can also be a `variable_selection` object. Default: `NULL`.
#' @param spatial.predictors.df Data frame of spatial predictors (e.g., from [mem_multithreshold()]). Default: `NULL`.
#' @param cor.threshold Numeric between 0 and 1 (recommended: 0.5 to 0.75). Maximum allowed absolute Pearson correlation. Default: `0.50`.
#' @return Data frame containing only spatial predictors with correlations below `cor.threshold` (both among themselves and with non-spatial predictors).
#' @details
#' Filtering is performed in two steps:
#' \enumerate{
#'   \item Remove spatial predictors correlated with each other (using [collinear::cor_select()])
#'   \item Remove spatial predictors correlated with non-spatial predictors
#' }
#' This two-step process ensures the retained spatial predictors are independent of both each other and the environmental predictors, improving model interpretability and reducing multicollinearity.
#' @examples
#' data(
#'   plants_df,
#'   plants_predictors,
#'   plants_distance
#' )
#'
#' # Generate spatial predictors using multiple distance thresholds
#' mem.df <- mem_multithreshold(
#'   distance.matrix = plants_distance,
#'   distance.thresholds = c(0, 1000)
#' )
#'
#' # Filter spatial predictors to remove redundancy
#' # Removes spatial predictors correlated > 0.50 with each other
#' # or with environmental predictors
#' spatial.predictors.filtered <- filter_spatial_predictors(
#'   data = plants_df,
#'   predictor.variable.names = plants_predictors,
#'   spatial.predictors.df = mem.df,
#'   cor.threshold = 0.50
#' )
#'
#' # Check dimensions
#' ncol(mem.df)  # original number
#' ncol(spatial.predictors.filtered)  # after filtering
#'
#' @rdname filter_spatial_predictors
#' @family spatial_analysis
#' @export
#' @autoglobal
filter_spatial_predictors <- function(
  data = NULL,
  predictor.variable.names = NULL,
  spatial.predictors.df = NULL,
  cor.threshold = 0.50
) {
  #filtering spatial predictors by pair-wise correlation
  selected_internal <- collinear::cor_select(
    df = spatial.predictors.df,
    predictors = colnames(spatial.predictors.df),
    preference_order = colnames(spatial.predictors.df),
    max_cor = cor.threshold,
    quiet = TRUE
  )

  spatial.predictors.df <- spatial.predictors.df[,
    selected_internal,
    drop = FALSE
  ]

  #filtering spatial predictors by correlation with non-spatial ones
  #identifying numeric predictors
  non.spatial.numeric.predictors <- collinear::identify_numeric_variables(
    df = data,
    predictors = predictor.variable.names,
    quiet = TRUE
  )$valid

  #correlation between spatial and non-spatial predictors
  cor.predictors <- stats::cor(
    x = data[, non.spatial.numeric.predictors, drop = FALSE],
    y = spatial.predictors.df,
    use = "pairwise.complete.obs",
    method = "pearson"
  ) |>
    abs()

  #max correlation of the spatial predictors
  max.cor.spatial.predictors <- apply(
    X = cor.predictors,
    MARGIN = 2,
    FUN = max
  )

  #selected spatial predictors
  selected_external <- names(max.cor.spatial.predictors[
    max.cor.spatial.predictors < cor.threshold
  ])

  #subsetting spatial.predictors.df
  spatial.predictors.df <- spatial.predictors.df[,
    selected_external,
    drop = FALSE
  ]

  #returning result
  spatial.predictors.df
}
