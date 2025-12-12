#' @title Removes redundant spatial predictors
#' @description Removes spatial predictors that are pair-wise correlated with other spatial predictors (which happens when there are several close distance thresholds), and spatial predictors correlated with non-spatial predictors.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param spatial.predictors.df Data frame of spatial predictors.
#' @param cor.threshold Numeric between 0 and 1, maximum Pearson correlation between any pair of the selected variables. Default: `0.50`
#' @return A data frame with non-redundant spatial predictors.
#' @examples
#' if(interactive()){
#'
#' #loading data
#' data("distance_matrix")
#' data("plant_richness_df")
#'
#' #computing Moran's Eigenvector Maps
#' spatial.predictors.df <- mem_multithreshold(
#'   distance_matrix = distance_matrix,
#'   distance.thresholds = c(0, 1000)
#'   )
#'
#' #filtering spatial predictors
#' spatial.predictors.df <- filter_spatial_predictors(
#'   data = plant_richness_df,
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   spatial.predictors.df = spatial.predictors.df,
#'   cor.threshold = 0.50
#'  )
#'
#'
#' }
#' @rdname filter_spatial_predictors
#' @export
filter_spatial_predictors <- function(
  data = NULL,
  predictor.variable.names = NULL,
  spatial.predictors.df = NULL,
  cor.threshold = 0.50
) {
  #predictor.variable.names comes from auto_vif or auto_cor
  if (!is.null(predictor.variable.names)) {
    if (inherits(predictor.variable.names, "variable_selection")) {
      predictor.variable.names <- predictor.variable.names$selected.variables
    }
  }

  #filtering spatial predictors by pair-wise correlation
  spatial.predictors.df <- auto_cor(
    x = spatial.predictors.df,
    preference.order = colnames(spatial.predictors.df),
    cor.threshold = cor.threshold,
    verbose = FALSE
  )$selected.variables.df

  #handle edge case: no spatial predictors remain after filtering
  if (ncol(spatial.predictors.df) == 0) {
    return(spatial.predictors.df)
  }

  #filtering spatial predictors by correlation with non-spatial ones

  #generating df of non-spatial predictors
  non.spatial.predictors.df <- data[, predictor.variable.names, drop = FALSE]

  #correlation between spatial and non-spatial predictors
  cor.predictors <- cor(
    non.spatial.predictors.df,
    spatial.predictors.df
  )

  #max correlation of the spatial predictors
  max.cor.spatial.predictors <- apply(cor.predictors, 2, FUN = max)

  #selected spatial predictors
  selected.spatial.predictors <- names(max.cor.spatial.predictors[
    max.cor.spatial.predictors < cor.threshold
  ])

  #subsetting spatial.predictors.df
  spatial.predictors.df <- spatial.predictors.df[,
    selected.spatial.predictors,
    drop = FALSE
  ]

  #returning result
  spatial.predictors.df
}
