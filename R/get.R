#' @title Functions to retrieve data from a fitted model.
#' @description All `get_xxx()` functions retrieve useful information from a fitted model.
#' @param model A model fitted with [rf_evaluate()].
#' @return
#' Functions to get permutation importance scores from any model type (fitted with `spatialRF`):
#' \itemize{
#'   \item `get_importance()`: data frame with permutation importance scores for each model predictor. If the model too many spatial predictors, then the function returns the statistics of importance of the model predictors (the one stored in `model$importance$spatial_predictors.stats`).
#'   \item `get_importance_local()` data frame with the permutation error on the out-of-bag data of each predictor on each location.
#' }
#'
#' Functions to get residuals and results of residuals autocorrelation tests.
#' \itemize{
#'   \item `get_moran()`: data frame with the results of the Moran's I test on the model residuals for the distances introduced in the argument `distance.thresholds`.
#'   \item `get_residuals()`: numeric vector with the model residuals in the same order as training data frame introduced via the argument `data`.
#' }
#'
#' Functions to get evaluation objects from models fitted with [rf_evaluate()]:
#' \itemize{
#'   \item `get_evaluation_aggregated()`: data frame with statistics (median, median absolute deviation, first quartile, third quartile, mean, standard error, standard deviation, minimum, and maximum) of training and testing performance scores from a model fitted with [rf_evaluate()].
#'   \item `get_evaluation()`: data frame with one row of performance scores for every pair of training and testing folds.
#'   \item `get_evaluation_folds()`: list with training and testing folds used by [rf_evaluate()].
#' }
#'
#' Functions to get objects related with the model performance and predictions
#' \itemize{
#'   \item `get_performance()`: data frame with performance metrics that can be either computed from the out-of-bag data (rsquared_oob and rmse_oob) or by comparing the response variable with the predictions for all cases (rsquared, rmse, nrmse). For models fitted with [rf_repeat()], the median and median absolute deviation of the performances are shown.
#'   \item `get_predictions()`: numeric vector of model predictions or median of model predictions if the model was fitted with [rf_repeat()].
#' }
#'
#' Function to get the data frame resulting from [rf_jackknife()].
#' \itemize{
#'   \item `get_jackknife()`: data frame with jackknife results (model performance computed via spatial cross-validation for different metrics comparing models fitted with all predictors, all predictors except one, and that one predictor alone).
#' }
#'
#' Function to get the spatial predictors used by [rf_spatial()] to fit a model.
#' \itemize{
#'   \item `get_spatial_predictors()`: data frame with the spatial predictors used to fit the model.
#' }
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_numeric_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#'  #fitting a random forest model
#'  model <- rf(
#'    data = ecoregions_df,
#'    response.name = ecoregions_continuous_response,
#'    predictors.names = ecoregions_numeric_predictors,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = c(0, 100, 1000),
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
#'
#'  #getting permutation importance data frame
#'  get_importance(model = model)
#'
#'  #local permutation importance
#'  get_importance_local(model = model)
#'
#'  #model residuals
#'  get_residuals(model = model)
#'
#'  #Moran's I test of the model residuals
#'  get_moran(model = model)
#'
#'  #get model performance
#'  get_performance(model = model)
#'
#'  #get model predictions
#'  get_predictions(model = model)
#'
#' #evaluating the model with spatial cross-validation
#' model <- rf_evaluate(
#'   model = model,
#'   xy = ecoregions_df[, c("x", "y")],
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #getting evaluation results aggregated over spatial folds
#' get_evaluation_aggregated(model = model)
#'
#' #get evaluation scores per spatial fold
#' get_evaluation(model = model)
#'
#' #get list with indices of training and testing cases
#' #on each evaluation iteration
#' get_evaluation_folds(model = model)
#'
#'
#' #fitting a spatial model
#' model <- rf_spatial(model = model)
#'
#' #data frame of spatial predictors used to fit the model
#' get_spatial_predictors(model = model)
#'
#' }
#' @rdname get
#' @export
get_evaluation_aggregated <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  #stop if no evaluation slot
  if(inherits(model, "rf_evaluate") == FALSE | !("evaluation" %in% names(model))){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

}


#' @rdname get
#' @export
get_evaluation <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  #stop if no evaluation slot
  if(inherits(model, "rf_evaluate") == FALSE | !("evaluation" %in% names(model))){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  model$evaluation$per_fold

}


#' @rdname get
#' @export
get_evaluation_folds <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  #stop if no evaluation slot
  if(inherits(model, "rf_evaluate") == FALSE | !("evaluation" %in% names(model))){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  model$evaluation$spatial.folds

}


#' @rdname get
#' @export
get_importance <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  #declaring variables
  importance <- NULL

  #importance from rf
  if((inherits(model, "rf") & !inherits(model, "rf_spatial")) | (inherits(model, "rf_repeat") & !inherits(model, "rf_spatial"))){
    x <- model$importance$global
  }

  #importance from rf_repeat
  if(inherits(model, "rf_spatial")){

    if(!is.null(model$ranger_arguments$repetitions)){
      repetitions <- model$ranger_arguments$repetitions
    } else {
      repetitions <- 1
    }

    #count non-spatial predictors
    length.non.spatial.predictors <- sum(model$importance$spatial_predictors$variable != "spatial_predictors") / repetitions

    length.spatial.predictors <- sum(model$importance$spatial_predictors$variable == "spatial_predictors") / repetitions

    #get spatial.predictor.stats if too many spatial predictors
    if(length.spatial.predictors >= length.non.spatial.predictors){
      x <- model$importance$spatial_predictors.stats
    } else {
      x <- model$importance$global
    }
  }

  if(is.null(x)){
    stop("This model doesn't have a 'variable.importance' slot")
  }

  #arranging
  x <- dplyr::arrange(x, dplyr::desc(importance))

  #return importance
  x

}


#' @rdname get
#' @export
get_importance_local <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  model$importance$local

}


#' @rdname get
#' @export
get_moran <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  model$residuals$autocorrelation$per_distance

}


#' @rdname get
#' @export
get_residuals <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  model$residuals$values

}


#' @rdname get
#' @export
get_performance <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  if(inherits(model, "rf_repeat")){

    x.median <- sapply(model$performance, FUN = median)
    x.mad <- sapply(model$performance, FUN = mad)

  } else {

    x.median <- unlist(model$performance)
    x.mad <- NA

  }

  out.df <- data.frame(
    metric = names(x.median),
    median = x.median,
    median_absolute_deviation = x.mad
  )

  if(inherits(model , "rf_repeat") == FALSE){
    colnames(out.df)[2] <- "value"
  }

  rownames(out.df) <- NULL

  out.df <- out.df[,colSums(is.na(out.df)) < nrow(out.df)]

  out.df <- na.omit(out.df)

  out.df

}


#' @rdname get
#' @export
get_predictions <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  if(inherits(model , "rf_repeat") == TRUE){
    return(model$predictions$values.per.repetition)
  } else {
    return(model$predictions$values)
  }

}

#' @rdname get
#' @export
get_jackknife <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  if(!("jackknife" %in% names(model))){
    stop("There is no 'jackknife' object in this model.")
  }

  df.list <- lapply(
    model$jackknife,
    "[[",
    "df"
  )

  out.df <- Reduce(function(x, y) merge(x, y, all=TRUE), df.list)

  out.df

}

#' @rdname get
#' @export
get_select <- function(model){

  if(inherits(model, "rf") == FALSE){
    stop("This is not an 'rf' object.")
  }

  if(!("selection" %in% names(model))){
    stop("There is no 'selection' object in this model.")
  }

  model$selection$df

}

#' @rdname get
#' @export
get_spatial_predictors <- function(model){

  if(!inherits(model, "rf_spatial")){
    stop("This function only works on models fitted with 'rf_spatial()'")
  }

  model$spatial$spatial_predictors

}
