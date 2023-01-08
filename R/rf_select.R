#' @title Selects set of predictors with maximum predictive performance for Random Forest models
#'
#' @description If the argument model does not have a `"jackknife"` slot, then the function runs [rf_jackknife()]. The function removes collinear predictors, and ranks the remaining ones using the jackknife results. Then, these predictors are added one by one to the model, until a maximum performance is achieved.
#'
#' The output of this function is a model of the class "rf" fitted with the selected variables, that can be used as input in the argument `model` of most modelling functions of this package. This model has a slot named `"selection"` with the selection results.
#'
#' @param model Model fitted with [rf()] and/or [rf_spatial()]. The function doesn't work with models fitted with [rf_repeat()]. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param max.vif Numeric between 2.5 and 10 defining the selection threshold for the VIF analysis. Higher numbers result in a more relaxed variable selection. Lower values increase the number of predictors returned. If `NULL`, VIF analysis to reduce multicollinearity is disabled. Default: `5`.
#' @param max.cor Numeric between 0 and 1, with recommended values between 0.5 and 0.9. Maximum Pearson correlation between any pair of the selected variables. Higher values increase the number of predictors returned. If `NULL`, bivariate filtering is disabled. Default: `0.75`
#' @param distance.step (optional; numeric) Numeric vector of length one or two. Distance step used during the growth of the buffer containing the training cases. Must be in the same units as the coordinates in `xy`. When only one distance is provided, the same growth is applied to the x and y axes. If two distances are provided, the first one is applied to the x axis, and the second one to the y. When `NULL`, it uses 1/1000th of the range of each axis as distance. The smaller this number is, the easier is to achieve an accurate `training.fraction`, but the slower the algorithm becomes. Default: `NULL`
#' @param fill.color Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", direction = -1, alpha = 0.8, end = 0.9)`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `1`.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores used by \code{\link[ranger]{ranger}} for parallel execution (used as value for the argument `num.threads` in `ranger()`). Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()` or \code{\link{start_cluster}}. Faster than using `n.cores` for smaller models. If provided, overrides `n.cores`. The function does not stop a cluster, please remember to shut it down with `parallel::stopCluster(cl = cluster_name)` or \code{\link{stop_cluster}} at the end of your pipeline. Default: `NULL`
#'
#' @return A model of the class "rf" fitted with the selected variables by the function [rf()] with a slot named `selection`. This slot contains the following elements:
#' \itemize{
#'   \item `selection.plot`: a ggplot with the selection criteria.
#'   \item `selection.df`: a data frame with the data used for the selection plot.
#'   \item `cor`: correlation matrix of the selected variables.
#'   \item `vif`: data frame with the names of the selected variables and their respective VIF scores.
#'   \item `selected.variables`: character vector with the names of the selected variables.
#'  }
#'
#'  This model can be used as input for other modelling functions such as [rf()], [rf_repeat()], [rf_tuning()], [rf_spatial()], [rf_evaluate()], and [rf_jackknife()].
#'
#' @examples
#'
#' if(interactive()){
#'
#'#loading example data
#'data(
#'  ecoregions_df,
#'  ecoregions_distance_matrix,
#'  ecoregions_numeric_predictors,
#'  ecoregions_continuous_response
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
    xy = NULL,
    repetitions = 30,
    training.fraction = 0.75,
    max.vif = 5,
    max.cor = 0.75,
    distance.step = NULL,
    fill.color = viridis::viridis(
      5,
      option = "F",
      direction = 1,
      alpha = 1,
      end = 0.9
    ),
    seed = 1,
    verbose = TRUE,
    n.cores = parallel::detectCores() - 1,
    cluster = NULL
){

  . <- NULL
  i <- NULL
  value <- NULL
  variable <- NULL
  inclusion_order <- NULL
  selected <- NULL

  #terminating if there is no model
  if(is.null(model)){
    stop("The argument 'model' is empty, there is no model to work with.")
  }

  #stopping if model is not of the right class
  if(!("rf" %in% class(model))){
    stop("The argument 'model' is not of the class 'rf'.")
  }

  #testing method argument
  metrics <- match.arg(
    arg = metrics,
    choices = c(
      "rsquared",
      "rmse",
      "nrmse",
      "auc"
    ),
    several.ok = TRUE
  )

  #getting data and ranger arguments from the model
  data <- model$ranger_arguments$data
  dependent.variable.name <- model$ranger_arguments$dependent.variable.name
  predictor.variable.names <- model$ranger_arguments$predictor.variable.names
  distance.matrix <- model$ranger_arguments$distance.matrix
  distance.thresholds <- model$ranger_arguments$distance.thresholds
  ranger.arguments <- model$ranger_arguments
  ranger.arguments$data <- NULL
  ranger.arguments$dependent.variable.name <- NULL
  ranger.arguments$predictor.variable.names <- NULL

  #if data is binary, "auc" is added
  if(is_binary_response(
    x = data[[dependent.variable.name]]
  )){
    metrics <- c(metrics, "auc")
  } else {
    metrics <- metrics[metrics != "auc"]
  }
  metrics <- unique(metrics)

  #run jackknife if the model doesn't have the slot
  if(!("jackknife" %in% names(model))){

    #evaluating the full model
    if(verbose == TRUE){
      message("Running rf_jackknife() to rank predictors.")
    }

    model <- rf_jackknife(
      model = model,
      xy = xy,
      repetitions = repetitions,
      training.fraction = training.fraction,
      distance.step = distance.step,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores,
      cluster = cluster
    )

  }

  #rewriting metrics to the ones available in the jackknife
  metrics <- names(model$jackknife)

  #getting the ranking data frame from the jackknife
  jackknife_df <- do.call(
    "cbind",
    lapply(
      model$jackknife,
      "[[",
      "df"
    )
  ) %>%
    dplyr::rename(
      variable = 1
    ) %>%
    dplyr::select(
      -dplyr::contains(".variable"),
      -dplyr::contains("_only_with")
    )

  #removing lowercase from colnames
  names(jackknife_df) <- gsub(
    pattern = "_",
    replacement = ".",
    x = names(jackknife_df)
  )

  #computing differences without - all
  for(metric in metrics){

    without.column <- paste0(
      metric,
      ".",
      metric,
      ".without"
      )

    all.column <- paste0(
      metric,
      ".",
      metric,
      ".all"
      )

    if(metric %in% c("rsquared", "auc")){

      jackknife_df[, paste0(metric, ".loss")] <-  as.vector(
        scale(
          jackknife_df[, without.column] - jackknife_df[, all.column]
          )
        )

    } else {

      jackknife_df[, paste0(metric, ".loss")] <-  as.vector(
        scale(
          jackknife_df[, all.column] - jackknife_df[, without.column]
          )
        )

    }

    jackknife_df[, without.column] <- NULL
    jackknife_df[, all.column] <- NULL

  }

  #summing scaled columns to obtain rank
  jackknife_df <- jackknife_df %>%
    dplyr::mutate(
      rank = rowSums(
        dplyr::select_if(., is.numeric),
        na.rm = TRUE
        )
      ) %>%
    dplyr::arrange(rank)

  #getting preference order
  preference.order <- jackknife_df$variable

  #REDUCING MULTICOLLINEARITY

  if(verbose == TRUE & (!is.null(max.cor) | !is.null(max.vif))){
    message(
      "Reducing multicollinearity."
    )
  }


  #filtering by bivariate correlation
  out.cor <- mc_auto_cor(
    data = data,
    predictor.variable.names = predictor.variable.names,
    preference.order = preference.order,
    max.cor = max.cor,
    verbose = FALSE
  )

  #filtering by VIF
  out.vif <- mc_auto_vif(
    data = out.cor,
    preference.order = preference.order,
    max.vif = max.vif,
    verbose = FALSE
  )

  #selected variables
  selected.variables <- out.vif$selected.variables

  #HANDLING PARALLELIZATION
  ##########################
  if("cluster" %in% class(cluster)){

    #registering cluster
    doParallel::registerDoParallel(cl = cluster)

    #parallel iterator
    `%iterator%` <- foreach::`%dopar%`

    #in-loop cores and ranger arguments
    in.loop.n.cores <- 1
    in.loop.ranger.arguments <- ranger.arguments
    in.loop.ranger.arguments$num.threads <- 1
    in.loop.ranger.arguments$importance <- "none"
    in.loop.ranger.arguments$local.importance <- FALSE
    in.loop.ranger.arguments$scaled.importance <- FALSE
    in.loop.ranger.arguments$distance.matrix <- NULL

  } else {

    #sequential iterator
    `%iterator%` <- foreach::`%do%`

    #in-loop cores and ranger arguments
    in.loop.n.cores <- n.cores
    in.loop.ranger.arguments <- ranger.arguments

  }

  if(verbose == TRUE){
    message(
      "Fitting models with different subsets of predictors."
    )
  }


  ##########################
  #END OF HANDLING PARALLELIZATION

  #fitting models with decreasing number of variables
  sequential.models.df <- foreach::foreach(
    i = seq(from = length(selected.variables), to = 1, by = -1),
    .verbose = FALSE,
    .combine = rbind,
    .packages = c("spatialRF", "magrittr", "dplyr")
  ) %iterator% {


    if(i == 1){



      #univariate model if i == 1
      variable.i <- selected.variables[1]
      model.i <- spatialRF::rf(
        data = data.frame(
          y = data[[dependent.variable.name]],
          x1 = data[[variable.i]],
          x2 = data[[variable.i]],
          x3 = data[[variable.i]]
        ),
        dependent.variable.name = "y",
        predictor.variable.names = c("x1", "x2", "x3"),
        ranger.arguments = in.loop.ranger.arguments,
        seed = seed,
        n.cores = in.loop.n.cores,
        verbose = FALSE
      )

    } else {

      #multivariate model otherwise
      model.i <- spatialRF::rf(
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = selected.variables[1:i],
        ranger.arguments = in.loop.ranger.arguments,
        seed = seed,
        n.cores = in.loop.n.cores,
        verbose = FALSE
      )

    }

    #evaluating model
    model.i <- rf_evaluate(
        model = model.i,
        repetitions = repetitions,
        xy = xy,
        training.fraction = training.fraction,
        distance.step = distance.step,
        seed = seed,
        verbose = FALSE,
        n.cores = in.loop.n.cores,
        cluster = NULL
      )

    #getting the evaluation.df
    evaluation.df.i <- model.i$evaluation$aggregated %>%
      dplyr::filter(
        model == "Testing"
      ) %>%
      dplyr::select(
        metric,
        median
      ) %>%
      t() %>%
      as.data.frame()

    colnames(evaluation.df.i) <- evaluation.df.i[1, ]
    rownames(evaluation.df.i) <- NULL
    evaluation.df.i <- evaluation.df.i[2 , , drop = FALSE]
    evaluation.df.i <- lapply(
      X = evaluation.df.i,
      FUN = as.numeric
    ) %>%
      as.data.frame()

    return(evaluation.df.i)

  } #end of parallelized loop

  #adding variable to sequential.models.df
  sequential.models.df$variable <- selected.variables[seq(from = length(selected.variables), to = 1, by = -1)]
  sequential.models.df <- sequential.models.df[, c("variable", metrics)]

  #computing combined column
  sequential.models.df.combined <- sequential.models.df

  #computing differences without - all
  for(metric in metrics){

    if(metric %in% c("rsquared", "auc")){

      sequential.models.df.combined[, metric] <- as.vector(
        scale(
          1 - sequential.models.df.combined[, metric]
        )
      )

    } else {

      sequential.models.df.combined[, metric] <- as.vector(
        scale(
          sequential.models.df.combined[, metric]
        )
      )

    }

  }

  #summing scaled columns to obtain rank
  sequential.models.df.combined <- sequential.models.df.combined %>%
    dplyr::mutate(
      combined = rowSums(
        dplyr::select_if(., is.numeric),
        na.rm = TRUE
      )
    )

  sequential.models.df$combined <- sequential.models.df.combined$combined

  rm(sequential.models.df.combined)

  #adding combined to metrics
  metrics <- c(metrics, "combined")

  #all metrics and colors
  all.metrics <- c(
    "rsquared",
    "auc",
    "rmse",
    "nrmse"
  )

  #naming colors
  names(fill.color) <- c(all.metrics[all.metrics %in% metrics], "combined")
  fill.color <- fill.color[!is.na(names(fill.color))]

  #sequential models plot
  sequential.models.df.long <- sequential.models.df %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(metrics),
      names_to = "metric",
      values_to = "value"
    )

  #finding last predictor
  last.predictor <- sequential.models.df.long %>%
    dplyr::filter(metric == "combined") %>%
    dplyr::slice(which.min(value)) %>%
    dplyr::pull(variable)

  #subsetting selected variables to the last predictor
  selected.variables <- selected.variables[1:which(selected.variables == last.predictor)]

  #renaming factors and ordering
  sequential.models.df.renamed <- sequential.models.df %>%
    dplyr::mutate(
      variable = ifelse(
        !(variable %in% selected.variables),
        paste(variable, "*"),
        variable
      )
    )

  #long format for plotting
  sequential.models.df.long <- sequential.models.df.long %>%
    dplyr::mutate(
      variable = ifelse(
        !(variable %in% selected.variables),
        paste(variable, "*"),
        variable
      )
    ) %>%
    dplyr::mutate(
      variable = factor(
        variable,
        levels = sequential.models.df.renamed$variable
      ),
      metric = factor(
        metric,
        levels = names(fill.color)
      )
    )

  #starting the plot
  sequential.models.plot <-
    ggplot2::ggplot(data = sequential.models.df.long) +
    ggplot2::facet_wrap(~metric, ncol = length(metrics), scales = "free_x") +
    ggplot2::aes(
      x = value,
      y = variable,
      group = metric,
      color = metric
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::scale_color_manual(values = fill.color) +
    ggplot2::theme_bw() +
    ggplot2::guides(color = "none")

  #removing factors
  sequential.models.df.long <- dplyr::mutate_if(
    sequential.models.df.long,
    is.factor,
    as.character
  )

  sequential.models.plot <- sequential.models.plot +
    ggplot2::geom_hline(
      yintercept = last.predictor,
      color = fill.color[names(fill.color) == "combined"],
      size = 2,
      alpha = 0.5
    ) +
    ggplot2::labs(
      y = "",
      x = "Model performance",
      title = "Selection criteria",
      subtitle = "Note: adding predictors marked with * decreases model performance."
    )

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
    cluster = NULL
  )

  #adding the number of repetitions to the model
  m$ranger_arguments$xy <- xy
  m$ranger_arguments$distance.matrix <- distance.matrix
  m$ranger_arguments$distance.thresholds <- distance.thresholds

  if(verbose == TRUE){

    message(
      "Selected variables:\n",
      "-------------------\n",
      paste0(
        selected.variables,
        collapse = ",\n"
      )
    )

    message(
      "\nJob done! The variable selection plot is in 'model$selection$plot', and the selection data frame in 'model$selection$df'.\n\n"
    )

    print(sequential.models.plot)

    message(
      "Interpretation:\n",
      "---------------\n\n",
      " - All performance scores are computed via spatial cross-validation.\n\n",
      " - The performance score of each variable is computed from a model including such variable and all the ones above it.\n\n",
      " - The 'combined' performance score is the average of all the other performance metrics.\n\n",
      " - The horizontal thick line identifies the last variable of the set of predictors that maximizes model performance.\n"
    )
  }

  #adding cluster to model
  if(!is.null(cluster) & "cluster" %in% class(cluster)){
    m$ranger_arguments$cluster <- cluster
  }

  #adding insertion order to sequential.models.df
  sequential.models.df <- sequential.models.df %>%
    dplyr::mutate(
      inclusion_order = nrow(sequential.models.df):1,
      selected = ifelse(
        variable %in% selected.variables,
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::relocate(
      inclusion_order,
      .after = variable
    ) %>%
    dplyr::relocate(
      selected,
      .after = inclusion_order
    ) %>%
    dplyr::arrange(
      inclusion_order
    )

  #adding new slot to the output model
  m$selection <- list(
    variables = selected.variables,
    plot = sequential.models.plot,
    df = sequential.models.df,
    cor = round(cor(data[, selected.variables]), 2),
    vif = out.vif$vif
  )

  m


}
