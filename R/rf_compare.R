#' @title Compares the performance of several models on independent data
#' @description Uses [rf_evaluate()] to compare the performance of several models based on the same pairs of coordinates on independent spatial folds.
#' @param models Named list with models based on the same pairs of coordinates. Example: `models = list(a = model.a, b = model.b)`. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". The same `xy` is used for all models, that's why all models must be based on the same pairs of coordinates. If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the number of records to be used in model training. Default: `0.8`
#' @param metrics Character vector, names of the performance metrics selected. The possible values are: "r.squared" (`cor(obs, pred) ^ 2`), "pseudo.r.squared" (`cor(obs, pred)`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`). Default: `c("r.squared", "pseudo.r.squared", "rmse", "nrmse")`
#' @param notch Logical, if `TRUE`, boxplot notches are plotted. Default: `TRUE`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same.
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use. Default = `parallel::detectCores() - 1`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Character, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `"11000"`
#' @return A list with three slots: `comparison.df`, a data frame with one performance value per spatial fold, metric, and model; `spatial.folds`, a list with the indices of the training and testing records for each evaluation repetition; `plot` a boxplot of `comparison.df`.
#' @examples
#' \donttest{
#' if(interactive()){
#'
#'  data(distance_matrix)
#'  data(plant_richness_df)
#'
#'  rf.model <- rf(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 1000, 2000)
#'  )
#'
#'  rf.spatial <- rf_spatial(model = rf.model)
#'
#'comparison <- rf_compare(
#'  models = list(
#'    `Non spatial` = rf.model,
#'    Spatial = rf.spatial
#'  ),
#'  xy = plant_richness_df[, c("x", "y")],
#'  metrics = c("r.squared", "rmse")
#')
#'
#'  }
#' }
#' @seealso [rf_evaluate()]
#' @rdname rf_compare
#' @export
rf_compare <- function(
  models = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.8,
  metrics = c("r.squared", "pseudo.r.squared", "rmse", "nrmse"),
  notch = TRUE,
  seed = NULL,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
){

  #declaring variables
  model <- NULL
  value <- NULL
  model.name <- NULL
  metric <- NULL

  #capturing user options
  user.options <- options()
  #avoid dplyr messages
  options(dplyr.summarise.inform = FALSE)
  on.exit(options <- user.options)

  #testing method argument
  metrics <- match.arg(
    arg = metrics,
    choices = c("r.squared", "pseudo.r.squared", "rmse", "nrmse"),
    several.ok = TRUE
  )

  #missing input
  if(is.null(models)){
    stop("There are no models to compare in 'models'.")
  }
  if(is.null(xy)){
    stop("Case coordinates 'xy' is missing.")
  }

  #list to store evaluation outputs
  evaluation.list <- list()

  #iterating through models
  for(model.i in names(models)){

    #evaluating model
    models[[model.i]] <- rf_evaluate(
      model = models[[model.i]],
      xy = xy,
      repetitions = repetitions,
      training.fraction = training.fraction,
      metrics = metrics,
      seed = seed,
      verbose = FALSE,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #getting evaluation data frame
    evaluation.df.i <- models[[model.i]]$evaluation$per.fold.long
    evaluation.df.i$model.name <- model.i

    #adding it to the evaluation list
    evaluation.list[[model.i]] <- evaluation.df.i

  }

  #binding data frames
  evaluation.df <- do.call("rbind", evaluation.list)
  rownames(evaluation.df) <- NULL

  #remove non testing models
  evaluation.df <- dplyr::filter(
    evaluation.df,
    model == "Testing"
  ) %>%
    dplyr::select(
      metric,
      value,
      model.name
    ) %>%
    dplyr::rename(
      model = model.name
    )

  #df to plot
  x <- evaluation.df
  x[x$metric == "r.squared", "metric"] <- "R squared"
  x[x$metric == "pseudo.r.squared", "metric"] <- "pseudo R squared"
  x[x$metric == "rmse", "metric"] <- "RMSE"
  x[x$metric == "nrmse", "metric"] <- "NRMSE"
  x[x$metric == "auc", "metric"] <- "AUC"

  #plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_violin(
      data = x,
      ggplot2::aes(
        group = model,
        y = model,
        x = value,
        fill = model
      ),
      draw_quantiles = 0.5
    ) +
    ggplot2::facet_wrap(
      "metric",
      scales = "free",
      drop = TRUE,
      ncol = 1
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_fill_viridis_d(end = 0.9, alpha = 0.5) +
    ggplot2::labs(fill = "Model") +
    ggplot2::ggtitle(
      paste0(
        "Evaluation results on ",
        repetitions,
        " spatial folds."
      )
    )

  #out list
  out.list <- list()
  out.list$comparison.df <- evaluation.df
  out.list$spatial.folds <- models[[model.i]]$evaluation$spatial.folds
  out.list$plot <- p

  if(verbose == TRUE){
    suppressMessages(print(p))

  }

  out.list

}
