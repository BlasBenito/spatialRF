#' @title Optimization equation to select spatial predictors
#' @description Optimizes the selection of spatial predictors using two different methods: "moran.i", and "p.value".
#' @param x Optimization data frame generated internally by [select_spatial_predictors_sequential()] or [select_spatial_predictors_recursive()]. Default: `NULL`
#' @param weight.performance Numeric between 0 and 1, weight of R-squared in the optimization process. Default: `NULL`
#' @param weight.penalization.n.predictors Numeric between 0 and 1, weight of the penalization on the number of added spatial predictors. Default: `NULL`
#' @param optimization.method Character, one of "moran.i", and "p.value". Default: `"moran.i"`
#' @return A numeric vector with the optimization criteria.
#' @details The method "moran.i" tries to maximize `1 - Moran's` I while taking into account the R-squared of the model and a penalization on the number of introduced spatial predictors through the expression
#'
#'
#' (1 - Moran's I) + w1 * rsquared - w2 * penalization
#'
#' The method "p.value" uses a binary version of the p-values of Moran's I (1 if >= 0.05, 0 otherwise), and uses the expression
#'
#'
#' max(1 - Moran's I, binary p-value) + w1 * rsquared - w2 * penalization
#'
#' The "moran.i" method generally selects more spatial predictors than the "p.value" method.
#' @seealso [select_spatial_predictors_recursive()], [select_spatial_predictors_sequential()]
#' @rdname internal
#' @keywords internal
#' @export
optimization_function <- function(
    x = NULL,
    weight.performance = NULL,
    weight.penalization.n.predictors = NULL,
    optimization.method = "moran.i"
){

  #using Moran's I and its p-value
  if(optimization.method == "p.value"){

    optimization <- rescale_vector(
      pmax(
        rescale_vector(1 - x$moran.i),
        x$p_value_binary
      ) +
        (weight.performance * rescale_vector(x$performance)) -
        (weight.penalization.n.predictors * rescale_vector(x$penalization.per.variable))
    )

  }

  #Using only Moran's I
  if(optimization.method == "moran.i"){

    optimization <- rescale_vector(rescale_vector(1 - x$moran.i) +
      (weight.performance * rescale_vector(x$performance)) -
      (weight.penalization.n.predictors * rescale_vector(x$penalization.per.variable)))

  }

  optimization

}


#' @title Removes redundant spatial predictors
#' @description Removes spatial predictors that are pair-wise correlated with other spatial predictors (which happens when there are several close distance thresholds), and spatial predictors correlated with non-spatial predictors.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param spatial.predictors.df Data frame of spatial predictors.
#' @param max.cor Numeric between 0 and 1, maximum Pearson correlation between any pair of the selected variables. Default: `0.50`
#' @return A data frame with non-redundant spatial predictors.
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_numeric_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#' #computing Moran's Eigenvector Maps
#' spatial.predictors.df <- mem_multithreshold(
#'   distance.matrix = ecoregions_distance_matrix,
#'   distance.thresholds = c(0, 1000)
#'   )
#'
#' #filtering spatial predictors
#' spatial.predictors.df <- filter_spatial_predictors(
#'   data = ecoregions_df,
#'   predictor.variable.names = ecoregions_numeric_predictors,
#'   spatial.predictors.df = spatial.predictors.df,
#'   max.cor = 0.50
#'  )
#'
#'  spatial.predictors.df
#'
#'
#' }
#' @rdname internal
#' @keywords internal
#' @export
filter_spatial_predictors <- function(
    data = NULL,
    predictor.variable.names = NULL,
    spatial.predictors.df = NULL,
    max.cor = 0.50
){

  #predictor.variable.names comes from auto_vif or auto_cor
  if(!is.null(predictor.variable.names)){
    if(inherits(predictor.variable.names, "variable_selection")){
      predictor.variable.names <- predictor.variable.names$selected.variables
    }
  }

  #filtering spatial predictors by pair-wise correlation
  spatial.predictors.selection <- auto_cor(
    data = spatial.predictors.df,
    max.cor = max.cor,
    verbose = FALSE
  )


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
  selected.spatial.predictors <- names(max.cor.spatial.predictors[max.cor.spatial.predictors < max.cor])

  #subsetting spatial.predictors.df
  spatial.predictors.df <- spatial.predictors.df[, selected.spatial.predictors, drop = FALSE]

  #returning result
  spatial.predictors.df

}


#' @title Default distance thresholds to generate spatial predictors
#' @description Generates four distance thresholds, from 0 to max(distance.matrix)/2.
#' @param distance.matrix Distance matrix. Default: `NULL`.
#' @return A numeric vector with distance thresholds.
#' @examples
#' if(interactive()){
#'
#'  #loading example distance matrix
#'  data(ecoregions_distance_matrix)
#'
#'  #computing set of default distance thresholds
#'  default_distance_thresholds(ecoregions_distance_matrix)
#'
#'  }
#' @rdname internal
#' @keywords internal
#' @export
default_distance_thresholds <- function(
    distance.matrix = NULL
){

  #stopping if no distance matrix
  if(is.null(distance.matrix)){
    stop("The argument 'distance.matrix' is missing.")
  }

  distance.thresholds <- floor(
    seq(
      0,
      max(distance.matrix, na.rm = TRUE)/2,
      length.out = 4
    )
  )

  distance.thresholds

}





#' @rdname internal
#' @keywords internal
#' @export
is_binary_response <- function(
    x
){

  #unique values
  unique.values <- sort(unique(x))

  #checking equality
  if(
    length(unique.values) == 2 &&
    all(unique.values == c(0, 1)) == TRUE
  ){
    return(TRUE)
  } else {
    return(FALSE)
  }


}


#' @title Prepares variable importance objects for spatial models
#' @description Prepares variable importance data frames and plots for models fitted with [rf_spatial()].
#' @param model An importance data frame with spatial predictors, or a model fitted with [rf_spatial()].
#' @return A list with importance data frames in different formats depending on whether the model was fitted with [rf()] or [rf_repeat()].
#' @examples
#' if(interactive()){
#'
#'#loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_numeric_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#'  #fittind spatial model
#'  model <- rf_spatial(
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_continuous_response,
#'    predictor.variable.names = ecoregions_numeric_predictors,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds =  0,
#'    n.cores = 1
#'  )
#'
#'  #preparing the importance data frame
#'  importance <- prepare_importance_spatial(model)
#'  names(importance)
#'
#' }
#' @rdname internal
#' @keywords internal
#' @export
prepare_importance_spatial <- function(model){

  importance <- NULL
  variable <- NULL

  #getting importance df
  if(inherits(model, "rf_spatial") == FALSE){
    stop("This function requires a model fitted with rf_spatial()")
  }
  importance.df <- model$importance$global

  #spatial predictors only
  spatial.predictors <- importance.df[grepl(
    "spatial_predictor",
    importance.df$variable
  ),]
  spatial.predictors$variable <- "spatial_predictors"

  #non-spatial predictors
  non.spatial.predictors <- importance.df[!grepl(
    "spatial_predictor",
    importance.df$variable
  ),]

  #joining for plot
  importance.plot.df <- rbind(
    spatial.predictors,
    non.spatial.predictors
  )

  #aggregating spatial predictors
  #min, max, median and mean of the spatial predictors
  spatial.predictors.stats <- data.frame(
    variable = c(
      "spatial_predictors (max)",
      "spatial_predictors (min)",
      "spatial_predictors (median)",
      "spatial_predictors (quantile 0.25)",
      "spatial_predictors (quantile 0.75)"
    ),
    importance = c(
      max(spatial.predictors$importance),
      min(spatial.predictors$importance),
      median(spatial.predictors$importance),
      quantile(spatial.predictors$importanc, probs = 0.25),
      quantile(spatial.predictors$importanc, probs = 0.75)
    )
  )

  #formatting importance.df
  importance.df <- rbind(
    non.spatial.predictors,
    spatial.predictors.stats
  ) %>%
    dplyr::arrange(dplyr::desc(importance))

  #preparing out list
  out.list <- list()

  #common slots
  out.list$per.variable <- model$importance$global
  out.list$per.variable.plot <- plot_importance(
    model$importance$global,
    verbose = FALSE
  )
  out.list$spatial_predictors <- importance.plot.df
  out.list$spatial_predictors.plot <- plot_importance(
    importance.plot.df,
    verbose = FALSE
  )
  out.list$spatial_predictors.stats <- importance.df
  out.list$spatial_predictors_stats_plot <- plot_importance(
    importance.df,
    verbose = FALSE
  )

  #returning the list
  out.list

}
