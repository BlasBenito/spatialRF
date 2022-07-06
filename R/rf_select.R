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
#' @param model A model fitted with [rf()] or [rf_spatial()]. If provided, the data and ranger arguments are taken directly from the model definition (stored in `model$ranger.arguments`). Default: `NULL`
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector with neighborhood distances. All distances in the distance matrix below each value in `dustance.thresholds` are set to 0 for the computation of Moran's I. If `NULL`, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: `NULL`
#' @param xy (optional) Data frame or matrix with two columns containing coordinates and named "x" and "y". It is not used by this function, but it is stored in the slot `ranger.arguments$xy` of the model, so it can be used by [rf_evaluate()] and [rf_tuning()]. Default: `NULL`
#' @param preference.order Character vector indicating the user's order of preference to keep variables. Predictors not included in this argument are ranked at random (with rank scores below those predictors in `preference.order`). If not provided, the predictors are ranked by their univariate RMSE on the out-of-bag data. Default: `NULL`.
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param vif.threshold Numeric between 2.5 and 10 defining the selection threshold for the VIF analysis. Higher numbers result in a more relaxed variable selection. Lower values increase the number of predictors returned. Set this argument to 0 if you desire to disable the VIF filtering. Default: 5.
#' @param cor.threshold Numeric between 0 and 1, with recommended values between 0.5 and 0.9. Maximum Pearson correlation between any pair of the selected variables. Higher values increase the number of predictors returned. Set this argument to 1 if you desire to disable the bivariate-correlation filtering. Default: `0.50`
#' @param repetitions Integer. Number of times to repeat random forest models with different random seeds (see the `repetitions` argument of [rf_repeat()]). Values higher than 10 will yield more robust results but at a higher computational cost. Default: `1`
#' @param jackknife Logical. If `TRUE`, the function fits a full model with all the selected variables, and one model without each variable, and compares their respective performances. Only variables that decrease performance when removed from the model are kept in the final selection. Default: `FALSE`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical, ff `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores used by \code{\link[ranger]{ranger}} for parallel execution of individual random forest models (used as value for the argument `num.threads` in `ranger()`). Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()` or \code{\link{make_cluster}}. If provided, overrides `n.cores`. The function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` or `spatialRF::stop_cluster()` at the end of your pipeline. Default: `NULL`
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
#'#loading example data
#'data(
#'  ecoregions_df,
#'  ecoregions_distance_matrix,
#'  ecoregions_predvar_names,
#'  ecoregions_depvar_name
#')
#'
#'#automatic variable selection
#'rf.selection <- rf_select(
#'  data = ecoregions_df,
#'  dependent.variable.name = ecoregions_depvar_name,
#'  predictor.variable.names = ecoregions_predvar_names,
#'  xy = ecoregions_df[, c("x", "y")],
#'  n.cores = 1
#')
#'
#'#the result is a model!
#'plot_importance(rf.selection)
#'print_performance(rf.selection)
#'
#'#you can use this model as input for other functions
#'rf.selection <- rf_evaluate(
#'  model = rf.selection
#')

#'#or you can connect it with other modelling functions using the %>% pìpe
#'rf.selection <- rf_select(
#'  data = ecoregions_df,
#'  dependent.variable.name = ecoregions_depvar_name,
#'  predictor.variable.names = ecoregions_predvar_names,
#'  xy = ecoregions_df[, c("x", "y")],
#' n.cores = 1
#') %>%
#'  rf_evaluate()

#'#example of complete pipeline (this will take a while to execute)
#'cl <- make_cluster()
#'
#'rf.selection <- rf_select(
#'  data = ecoregions_df,
#'  dependent.variable.name = ecoregions_depvar_name,
#'  predictor.variable.names = ecoregions_predvar_names,
#'  distance.matrix = ecoregions_distance_matrix,
#'  xy = ecoregions_df[, c("x", "y")],
#'  n.cores = 1
#') %>%
#'  rf_tuning(cluster = cl) %>%
#'  rf_spatial(cluster = cl) %>%
#'  rf_evaluate(cluster = cl) %>%
#'  rf_importance(cluster = cl)
#'
#'#automatic variable selection with jackknife
#'rf.selection <- rf_select(
#'  data = ecoregions_df,
#'  dependent.variable.name = ecoregions_depvar_name,
#'  predictor.variable.names = ecoregions_predvar_names,
#'  jackknife = TRUE,
#'  n.cores = 1
#')
#'
#'#variable selection with preference order
#'rf.selection <- rf_select(
#'  data = ecoregions_df,
#'  dependent.variable.name = ecoregions_depvar_name,
#'  predictor.variable.names = ecoregions_predvar_names,
#' preference.order = c(
#'   "climate_bio5_average",
#'   "climate_hypervolume",
#'   "human_population_density"
#' ),
#' n.cores = 1
#')
#'
#'#variable selection with preference order and jackknife
#'rf.selection <- rf_select(
#'  data = ecoregions_df,
#'  dependent.variable.name = ecoregions_depvar_name,
#'  predictor.variable.names = ecoregions_predvar_names,
#'  preference.order = c(
#'    "climate_bio5_average",
#'    "climate_hypervolume",
#'    "human_population_density"
#'  ),
#'  jackknife = TRUE,
#'  n.cores = 1
#')
#'
#'#preference order, jackknife, and repetitions
#'#for more robust estimates of importance but higher computational cost
#'rf.selection <- rf_select(
#'  data = ecoregions_df,
#'  dependent.variable.name = ecoregions_depvar_name,
#'  predictor.variable.names = ecoregions_predvar_names,
#'  preference.order = c(
#'    "climate_bio5_average",
#'    "climate_hypervolume",
#'    "human_population_density"
#'  ),
#' jackknife = TRUE,
#'  repetitions = 10,
#' n.cores = 1
#')
#'
#'}
#'
#' @importFrom dplyr pull
#'
#' @rdname rf_select
#' @export
rf_select <- function(
    model = NULL,
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    preference.order = NULL,
    distance.matrix = NULL,
    distance.thresholds = NULL,
    xy = NULL,
    ranger.arguments = NULL,
    vif.threshold = 5,
    cor.threshold = 0.75,
    repetitions = 1,
    jackknife = FALSE,
    seed = 1,
    verbose = TRUE,
    n.cores = parallel::detectCores() - 1,
    cluster = NULL
){

  #declaring variables
  rmse <- NULL
  oob.rmse.shift.without.variable <- NULL
  variable <- NULL
  i <- NULL
  oob.rmse.full.model <- NULL
  oob.rmse.model.without.variable <- NULL

  #HANDLING ARGUMENTS
  ######################################

  #if model is provided
  if(!is.null(model)){

    #stopping if model is not of the right class
    if(!("rf" %in% class(model))){
      stop("The argument 'model' is not of the class 'rf'.")
    }

    #RULE 1: if ranger.arguments is not provided
    if(is.null(ranger.arguments)){

      #overriding input arguments
      data <- NULL
      dependent.variable.name <- NULL
      predictor.variable.names <- NULL
      distance.matrix <- NULL
      distance.thresholds <- NULL
      xy <- NULL

      #writing the model's ranger.arguments to the environment
      ranger.arguments <- model$ranger.arguments

      #writing arguments to the function environment
      list2env(model$ranger.arguments, envir=environment())

    } else {

      #RULE 2:
      #input arguments in model$ranger.arguments take precedence

      ranger.arguments$data <- NULL
      ranger.arguments$dependent.variable.name <- NULL
      ranger.arguments$predictor.variable.names <- NULL
      ranger.arguments$distance.matrix <- NULL
      ranger.arguments$distance.thresholds <- NULL
      ranger.arguments$xy <- NULL

      #writing arguments to the function environment
      list2env(model$ranger.arguments, envir=environment())
      list2env(ranger.arguments, envir=environment())


    }

  } else {
    #RULE 3:

    if(!is.null(ranger.arguments)){

      if(is.null(data)){
        data <- model$ranger.arguments$data
      }

      if(is.null(dependent.variable.name)){
        dependent.variable.name <- model$ranger.arguments$dependent.variable.name
      }

      if(is.null(predictor.variable.names)){
        predictor.variable.names <- model$ranger.arguments$predictor.variable.names
      }

      if(is.null(distance.matrix)){
        distance.matrix <- model$ranger.arguments$distance.matrix
      }

      if(is.null(distance.thresholds)){
        distance.thresholds <- model$ranger.arguments$distance.thresholds
      }

      if(is.null(xy)){
        xy <- model$ranger.arguments$xy
      }

      if(is.null(cluster)){
        cluster <- model$ranger.arguments$cluster
      }

      #writing ranger.arguments to the function environment
      list2env(ranger.arguments, envir=environment())

    }

  }

  #coerce to data frame if tibble
  if(inherits(data, "tbl_df") | inherits(data, "tbl")){
    data <- as.data.frame(data)
  }

  if(inherits(xy, "tbl_df") | inherits(xy, "tbl")){
    xy <- as.data.frame(xy)
  }

  ##########################
  #END OF HANDLING ARGUMENTS


  #HANDLING PARALLELIZATION
  ##########################

  #HANDLING PARALLELIZATION
  ##########################
  if("cluster" %in% class(cluster)){

    #registering cluster
    doParallel::registerDoParallel(cl = cluster)

    #parallel iterator
    `%iterator%` <- foreach::`%dopar%`

    #restricting the number of cores
    n.cores <- 1
    ranger.arguments$num.threads <- 1

  } else {

    #sequential iterator
    `%iterator%` <- foreach::`%do%`

  }

  ##########################
  #END OF HANDLING PARALLELIZATION

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
    ) %iterator% {

      #generating training data
      training.df <- data.frame(
        y = data[, dependent.variable.name],
        x1 = data[, i],
        x2 = data[, i],
        x3 = data[, i]
      )

      #fitting model
      model.i <- spatialRF::rf_repeat(
        data = training.df,
        dependent.variable.name = "y",
        predictor.variable.names = c("x1", "x2", "x3"),
        ranger.arguments = ranger.arguments,
        repetitions = repetitions,
        seed = seed,
        n.cores = n.cores,
        verbose = FALSE
      )

      #capturing median r.squared
      return(median(model.i$performance$rmse.oob))

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

    if(length(selected.variables) >= 3){

      if(verbose == TRUE){
        message(
          "Performing jackknife to remove predictors with negative contribution to model performance."
        )
      }

      #creating a data frame to store results
      jackknife.df <- data.frame(
        variable = selected.variables,
        oob.rmse.full.model = NA,
        oob.rmse.model.without.variable = NA,
        oob.rmse.shift.without.variable = NA
      )

      #fitting the model with all predictors
      rf.full.model <- spatialRF::rf_repeat(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = selected.variables,
        ranger.arguments = ranger.arguments,
        repetitions = repetitions,
        seed = seed,
        n.cores = n.cores,
        verbose = FALSE
      )

      #fitting a model without each one of the predictors
      jackknife.models <- foreach::foreach(
        i = selected.variables,
        .verbose = FALSE
      ) %do% {

        #fitting model
        model.i <- spatialRF::rf_repeat(
          data = data,
          dependent.variable.name = dependent.variable.name,
          predictor.variable.names = selected.variables[!(selected.variables %in% i)],
          ranger.arguments = ranger.arguments,
          repetitions = repetitions,
          seed = seed,
          n.cores = n.cores,
          verbose = FALSE
        )

        #capturing median r.squared
        return(median(model.i$performance$rmse.oob))

      }

      #filling jackknife.df
      jackknife.df <- jackknife.df %>%
        dplyr::mutate(
          oob.rmse.full.model = median(rf.full.model$performance$rmse.oob),
          oob.rmse.model.without.variable = unlist(jackknife.models),
          oob.rmse.shift.without.variable = oob.rmse.model.without.variable - oob.rmse.full.model
        ) %>%
        dplyr::arrange(dplyr::desc(oob.rmse.shift.without.variable))

      #fitting models with decreasing number of variables
      jackknife.models <- foreach::foreach(
        i = seq(from = nrow(jackknife.df), to = 4, by = -1),
        .verbose = FALSE
      ) %iterator% {

        #fitting model
        model.i <- spatialRF::rf_repeat(
          data = data,
          dependent.variable.name = dependent.variable.name,
          predictor.variable.names = jackknife.df$variable[1:i],
          ranger.arguments = ranger.arguments,
          repetitions = repetitions,
          seed = seed,
          n.cores = n.cores,
          verbose = FALSE
        )

        #capturing median r.squared
        return(median(model.i$performance$rmse.oob))

      }

      #adding the data to jackknife.df
      jackknife.df$oob.rmse.including.this.variable[4:nrow(jackknife.df)] <- unlist(jackknife.models)

      #subset of best variables
      selected.variables <- jackknife.df$variable[1:which.min(jackknife.df$oob.rmse.including.this.variable)[1]]


    } else {

      if(verbose == TRUE){
        message("The number of selected variables is equal or lower than 3, the jackknife procedure cannot be performed."
        )

      }

    }

  } #end of jackknife

  #fitting model with the selected variables
  if("cluster" %in% class(cluster)){
    n.cores <- length(cluster)
  }

  m <- spatialRF::rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = selected.variables,
    ranger.arguments = ranger.arguments,
    xy = xy,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores,
    cluster = cluster
  )

  #adding the number of repetitions to the model
  m$ranger.arguments$repetitions <- repetitions
  m$ranger.arguments$xy <- xy
  m$ranger.arguments$distance.matrix <- distance.matrix
  m$ranger.arguments$distance.thresholds <- distance.thresholds

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

  if(exists("jackknife.df") == FALSE){
    jackknife.df <- NA
  }

  if(verbose == TRUE){
    message(
      "Job done, you will find the variable selection results in the slot 'variable.selection' of the output model."
    )
  }

  #adding cluster to model
  if(!is.null(cluster) & "cluster" %in% class(cluster)){
    m$ranger.arguments$cluster <- cluster
  }

  #adding new slot to the output model
  m$variable.selection <- list(
    univariate.importance = preference.order.df,
    jackknife.result = jackknife.df,
    cor = round(cor(data[, selected.variables]), 2),
    vif = out.vif$vif,
    selected.variables = selected.variables
  )

  m

}
