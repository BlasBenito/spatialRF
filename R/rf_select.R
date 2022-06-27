#' @title Selects uncorrelated predictors for Random Forest models
#'
#' @description Combines the functions [rf_repeat()], [auto_cor()], and [auto_vif()] to select a set of uncorrelated predictors ranked by their univariate effect on the response variable. For each predictor in `predictor.variable.names`, a univariate model is fitted `repetitions` times with [rf_repeat()], and its out-of-bag RMSE is stored. All variables are then ranked by their RMSE (from minimum to maximum), and their rankings are used as `preference.order` in the functions [auto_cor()], and [auto_vif()]. The output of this function is a list of the class "variable_selection" that can be used as input for the argument `predictor.variable.names` in most modelling functions of this package.
#'
#' Please, take in mind that this experimental function is not intended to help you increase sheer model performance, but to reduce multicollinearity to increase the interpretability of the variable importance scores.
#'
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param repetitions Integer, number of random forest models to fit. Default: `10`
#' @param vif.threshold Numeric between 2.5 and 10 defining the selection threshold for the VIF analysis. Higher numbers result in a more relaxed variable selection. Default: 5.
#' @param cor.threshold Numeric between 0 and 1, with recommended values between 0.5 and 0.9. Maximum Pearson correlation between any pair of the selected variables. Default: `0.50`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical, ff `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores used by \code{\link[ranger]{ranger}} for parallel execution (used as value for the argument `num.threads` in `ranger()`). Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()` or \code{\link{make_cluster}}. Only advisable if you need to spread a large number of repetitions over the nodes of a large cluster when working with large data. If provided, overrides `n.cores`. The function does not stop a cluster, please remember to shut it down with [stop_cluster()] or `parallel::stopCluster(cl = cluster_name)` at the end of your pipeline. Default: `NULL`
#'
#' @return List of class "variable_selection" with five slots:
#' \itemize{
#'   \item `univariate.importance`: a data frame with the names of the predictors and their univariate out-of-bag RMSE.
#'   \item `cor`: correlation matrix of the selected variables.
#'   \item `vif`: data frame with the names of the selected variables and their respective VIF scores.
#'   \item `selected.variables`: character vector with the names of the selected variables.
#'   \item `selected.variables.df`: data frame with the selected variables.
#'  }

#'
#' @examples
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predvar_names,
#'   ecoregions_depvar_name
#' )
#'
#' #variable selection
#' variable.selection <- rf_select(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_depvar_name,
#'   predictor.variable.names = ecoregions_predvar_names,
#'   repetitions = 10,
#'   cor.threshold = 0.50,
#'   vif.threshold = 5,
#'   seed = 1,
#'   n.cores = 1,
#'   verbose = TRUE
#' )
#'
#' #fitting a random forest model with the selected variables
#' rf.model <- rf(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_depvar_name,
#'   predictor.variable.names = variable.selection,
#'   distance.matrix = ecoregions_distance_matrix,
#'   distance.thresholds = 0,
#'   n.cores = 1
#' )
#'
#' @rdname rf_select
#' @export
rf_select <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    ranger.arguments = NULL,
    repetitions = 10,
    vif.threshold = 5,
    cor.threshold = 0.50,
    seed = 1,
    verbose = TRUE,
    n.cores = parallel::detectCores() - 1,
    cluster = NULL
){

  #declaring variables
  rmse <- NULL

  #fitting one model per predictor and retrieving oob rmse
  i <- NULL
  univariate.models <- foreach::foreach(
    i = predictor.variable.names,
    .verbose = FALSE
  ) %do% {

    #generating training data
    training.df <- data.frame(
      y = data[, dependent.variable.name],
      x1 = data[, i],
      x2 = data[, i],
      x3 = data[, i]
    )

    #fitting model
    model.i <- rf_repeat(
      data = training.df,
      dependent.variable.name = "y",
      predictor.variable.names = c("x1", "x2", "x3"),
      ranger.arguments = ranger.arguments,
      keep.models = FALSE,
      repetitions = repetitions,
      n.cores = n.cores,
      cluster = cluster,
      verbose = FALSE
    )

    #capturing median r.squared
    return(mean(model.i$performance$rmse.oob))

  }

  #preference order data frame
  preference.order.df <- data.frame(
    variable = predictor.variable.names,
    rmse = unlist(univariate.models)
  ) %>%
    dplyr::arrange(rmse)

  #filtering by bivariate correlation and vif
  out.cor <- auto_cor(
    x = data[, predictor.variable.names],
    preference.order = preference.order.df$variable,
    cor.threshold = cor.threshold,
    verbose = verbose
  )

  out.vif <- auto_vif(
    x = out.cor,
    preference.order = preference.order.df$variable,
    vif.threshold = vif.threshold,
    verbose = verbose
  )

  #composing output
  out.list <- list(
    univariate.importance = preference.order.df,
    cor = round(cor(data[, out.vif$selected.variables]), 2),
    vif = out.vif$vif,
    selected.variables = out.vif$selected.variables,
    selected.variables.df = out.vif$selected.variables.df
  )

  class(out.list) <- class(out.vif)

  out.list

}
