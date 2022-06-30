#' @title Selects predictors for Random Forest models
#'
#' @description Combines the functions[auto_cor()], and [auto_vif()] to select a set of uncorrelated predictors. These predictors can either be ranked by their univariate effect on the response variable (a.k.a "auto mode"), or prioritized according to the user's preferences via the `preference.order` argument. In the "auto mode", a univariate random forest model is fitted for each predictor in `predictor.variable.names`, and its out-of-bag RMSE is stored. All variables are then ranked by their RMSE (from minimum to maximum), and their rankings are used as `preference.order` in the functions [auto_cor()], and [auto_vif()]. If the user provides the `preference.order` argument, a redundant variable is removed when there is a similar one with a higher priority.
#'
#' When the argument `jackknife` is set to `TRUE`, the function fits a random forest with all uncorrelated variables, and random forest models without each variable  to compute the out-of-bag RMSE gained or lost when each variable is removed from the model. Finally, a model fitted with all the variables that decrease performance when removed from the model is compared with a model fitted with all the uncorrelated variables. The model with better performance is then returned.
#'
#' Setting `jackknife = TRUE` is recommended when your goal is obtaining the minimum set of variables with the better model performance.
#'
#' The output of this function is a model of the class "rf" fitted with the selected variables, that can be used as input in the argument `model` of most modelling functions of this package.
#'
#'
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param preference.order Character vector indicating the user's order of preference to keep variables. Predictors not included in this argument are ranked at random (with rank scores below those predictors in `preference.order`). If not provided, the predictors are ranked by their univariate RMSE on the out-of-bag data. Default: `NULL`.
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param vif.threshold Numeric between 2.5 and 10 defining the selection threshold for the VIF analysis. Higher numbers result in a more relaxed variable selection. Lower values increase the number of predictors returned. Set this argument to 0 if you desire to disable the VIF filtering. Default: 5.
#' @param cor.threshold Numeric between 0 and 1, with recommended values between 0.5 and 0.9. Maximum Pearson correlation between any pair of the selected variables. Higher values increase the number of predictors returned. Set this argument to 1 if you desire to disable the bivariate-correlation filtering. Default: `0.50`
#' @param jackknife Logical. If `TRUE`, the function fits a full model with all the selected variables, and one model without each variable, and compares their respective performances. Only variables that decrease performance when removed from the model are kept in the final selection. Default: `FALSE`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical, ff `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores used by \code{\link[ranger]{ranger}} for parallel execution of individual random forest models (used as value for the argument `num.threads` in `ranger()`). Default: `parallel::detectCores() - 1`
#'
#' @return A model of the class "rf" fitted with the selected variables by the function [rf()] with a slot named `variable.selection`. This slot contains the following elements:
#' \itemize{
#'   \item `univariate.importance`: a data frame with the names of the predictors and their univariate out-of-bag RMSE. This slot will be `NA` if the argument `preference.order` is provided.
#'   \item `jackknife.result`: a data frame with the results of the jackknife when `jackknife = TRUE`, or `NA` otherwise.
#'   \item `cor`: correlation matrix of the selected variables.
#'   \item `vif`: data frame with the names of the selected variables and their respective VIF scores.
#'   \item `selected.variables`: character vector with the names of the selected variables.
#'  }
#'
#'  This model can be used as input for other modelling functions such as [rf()], [rf_repeat()], [rf_tuning()], [rf_spatial()], [rf_evaluate()], and [rf_importance()].
#'
#' @examples
#'
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predvar_names,
#'   ecoregions_depvar_name
#' )
#'
#' #variable selection without preference order
#' rf.selection <- rf_select(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_depvar_name,
#'   predictor.variable.names = ecoregions_predvar_names,
#'   cor.threshold = 0.75,
#'   vif.threshold = 5,
#'   n.cores = 1
#' )
#'
#' rf.selection$variable.selection$selected.variables
#'
#' #re-fitting the model with different hyperparameters
#' rf.selection.custom <- rf(
#'   model = rf.selection,
#'   ranger.arguments = list(
#'     num.trees = 5000,
#'     min.node.size = 10,
#'     mtry = 3
#'     )
#'  )
#'
#' #other methods to select variables
#'
#' #variable selection prioritizing a set of variables
#' rf.selection <- rf_select(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_depvar_name,
#'   predictor.variable.names = ecoregions_predvar_names,
#'   preference.order = c(
#'     "climate_bio1_average",
#'     "climate_bio12_average",
#'     "fragmentation_cohesion"
#'     ),
#'   n.cores = 1
#' )
#'
#' #variable selection with jackknife and preference order
#' variable.selection <- rf_select(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_depvar_name,
#'   predictor.variable.names = ecoregions_predvar_names,
#'   preference.order = c(
#'     "climate_bio1_average",
#'     "climate_bio12_average",
#'     "fragmentation_cohesion"
#'     ),
#'   jackknife = TRUE,
#'   n.cores = 1
#' )
#'
#' #variable selection with jackknife without preference order
#' variable.selection <- rf_select(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_depvar_name,
#'   predictor.variable.names = ecoregions_predvar_names,
#'   jackknife = TRUE,
#'   n.cores = 1
#' )
#'
#'}
#'
#' @importFrom dplyr pull
#'
#' @rdname rf_select
#' @export
rf_select <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    preference.order = NULL,
    ranger.arguments = NULL,
    vif.threshold = 5,
    cor.threshold = 0.75,
    jackknife = FALSE,
    seed = 1,
    verbose = TRUE,
    n.cores = parallel::detectCores() - 1
){

  #declaring variables
  rmse <- NULL
  oob.rmse.change.without.variable <- NULL
  variable <- NULL
  i <- NULL
  oob.rmse.with.variable <- NULL
  oob.rmse.without.variable <- NULL

  if(verbose == TRUE){
    message(
      "Ranking predictors by their univariate effect on the response variable."
    )
  }

  #auto preference order
  if(is.null(preference.order)){

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
      model.i <- spatialRF::rf(
        data = training.df,
        dependent.variable.name = "y",
        predictor.variable.names = c("x1", "x2", "x3"),
        ranger.arguments = ranger.arguments,
        seed = seed,
        n.cores = n.cores,
        verbose = FALSE
      )

      #capturing median r.squared
      return(model.i$performance$rmse.oob)

    }

    #preference order data frame
    preference.order.df <- data.frame(
      variable = predictor.variable.names,
      rmse = unlist(univariate.models)
    ) %>%
      dplyr::arrange(rmse)

    preference.order <- preference.order.df$variable

  }

  #REDUCING MULTICOLLINEARITY

  if(verbose == TRUE){
    message(
      "Reducing multicollinearity."
    )
  }


  #filtering by bivariate correlation
  out.cor <- auto_cor(
    x = data[, predictor.variable.names],
    preference.order = preference.order,
    cor.threshold = cor.threshold,
    verbose = FALSE
  )

  #filtering by VIF
  out.vif <- auto_vif(
    x = out.cor,
    preference.order = preference.order,
    vif.threshold = vif.threshold,
    verbose = FALSE
  )

  #selected variables
  selected.variables <- out.vif$selected.variables

  #removing variables with negative contribution
  if(jackknife == TRUE){

    if(verbose == TRUE){
      message(
        "Performing jackknife to remove preditors with negative contribution to model performance."
      )
    }

    #creating a data frame to store results
    partial.contribution.df <- data.frame(
      variable = selected.variables,
      oob.rmse.with.variable = NA,
      oob.rmse.without.variable = NA,
      oob.rmse.change.without.variable = NA
    )

    #fitting the model with all predictors
    rf.all.variables <- spatialRF::rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = selected.variables,
      ranger.arguments = ranger.arguments,
      seed = seed,
      n.cores = n.cores,
      verbose = FALSE
    )

    #fitting a model without each one of the predictors
    univariate.models <- foreach::foreach(
      i = selected.variables,
      .verbose = FALSE
    ) %do% {

      #fitting model
      model.i <- spatialRF::rf(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = selected.variables[!(selected.variables %in% i)],
        ranger.arguments = ranger.arguments,
        seed = seed,
        n.cores = n.cores,
        verbose = FALSE
      )

      #capturing median r.squared
      return(model.i$performance$rmse.oob)

    }

    #saving results
    partial.contribution.df <- partial.contribution.df %>%
      dplyr::mutate(
        oob.rmse.with.variable = rf.all.variables$performance$rmse.oob,
        oob.rmse.without.variable = unlist(univariate.models),
        oob.rmse.change.without.variable = oob.rmse.with.variable - oob.rmse.without.variable
      ) %>%
      dplyr::arrange(oob.rmse.change.without.variable)

    #selecting variables with positive importance
    selected.variables.jackknife <- partial.contribution.df %>%
      dplyr::filter(oob.rmse.change.without.variable < 0) %>%
      dplyr::pull(variable)

    #fitting model with the selected variables
    rf.variables.jackknife <- spatialRF::rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = selected.variables.jackknife,
      ranger.arguments = ranger.arguments,
      seed = seed,
      n.cores = n.cores,
      verbose = FALSE
    )

    #if the model after the jackknife is better, we keep these variables
    if(rf.variables.jackknife$performance$rmse.oob < rf.all.variables$performance$rmse.oob){

      #new selected variables
      selected.variables <- selected.variables.jackknife

      #output model
      output.model <- rf.variables.jackknife
      rm(rf.variables.jackknife)

    } else {

      #alternative output model
      output.model <- rf.all.variables
      rm(rf.all.variables)

    }

  }

  #printing out the selected variables
  if(verbose == TRUE){
    message(
      paste0(
        "\nThe selected variables are:\n\n",
        paste(selected.variables, collapse = "\n"),
        "\n"
      )
    )
  }

  #creating dummy preference.order.df
  if(exists("preference.order.df") == FALSE){
    preference.order.df <- NA
  }

  if(exists("partial.contribution.df") == FALSE){
    partial.contribution.df <- NA
  }

  #fitting output model if it does not exist
  if(exists("output.model") == FALSE){

    if(verbose == TRUE){
      message(
        "Fitting output model."
      )
    }

    output.model <- spatialRF::rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = selected.variables,
      ranger.arguments = ranger.arguments,
      seed = seed,
      n.cores = n.cores,
      verbose = FALSE
    )

  }

  if(verbose == TRUE){
    message(
      "Job done, you will find the results of the variable selection in the slot 'variable.selection' of the output model."
    )
  }

  #adding new slot to the output model
  output.model$variable.selection <- list(
    univariate.importance = preference.order.df,
    jackknife.result = partial.contribution.df,
    cor = round(cor(data[, selected.variables]), 2),
    vif = out.vif$vif,
    selected.variables = selected.variables
  )

  output.model

}
