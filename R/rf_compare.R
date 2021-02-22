#' @title Compares the performance of several models on independent data
#' @description Uses [rf_evaluate()] to compare the performance of several models based on the same pairs of coordinates on independent spatial folds.
#' @param models Named list with models based on the same pairs of coordinates. Example: `models = list(a = model.a, b = model.b)`. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". The same `xy` is used for all models, that's why all models must be based on the same pairs of coordinates. If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the number of records to be used in model training. Default: `0.8`
#' @param metrics Character vector, names of the performance metrics selected. The possible values are: "r.squared" (`cor(obs, pred) ^ 2`), "pseudo.r.squared" (`cor(obs, pred)`), "rmse" (`sqrt(sum((obs - pred)^2)/length(obs))`), "nrmse" (`rmse/(quantile(obs, 0.75) - quantile(obs, 0.25))`). Default: `c("r.squared", "pseudo.r.squared", "rmse", "nrmse")`
#' @param notch Logical, if `TRUE`, boxplot notches are plotted. Default: `TRUE`
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `11000`
#' @return A list with three slots: `df.long`, with the performance metrics for each model across repetitions; `df.aggregated`, with aggregated stats per model and performance metrics; `plot`, with a boxplot built from `df.long`.
#' @examples
#' \donttest{
#' if(interactive()){
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
#'  comparison <- rf_compare(
#'    a = rf.model,
#'    b = rf.spatial,
#'    a.name = "Non spatial",
#'    b.name = "Spatial",
#'    xy = plant_richness_df[, c("x", "y")],
#'    )
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
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = 11000
){

  #declaring variables
  model <- NULL
  value <- NULL
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
      verbose = FALSE,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

    #getting evaluation data frame
    evaluation.df.i <- models[[model.i]]$evaluation$per.fold.long
    evaluation.df.i$model <- model.i

    #adding it to the evaluation list
    evaluation.list[[model.i]] <- evaluation.df.i

  }

  #binding data frames
  evaluation.df <- do.call("rbind", evaluation.list)
  rownames(evaluation.df) <- NULL

  #df to plot
  x <- evaluation.df
  x[x$metric == "r.squared", "metric"] <- "R squared"
  x[x$metric == "pseudo.r.squared", "metric"] <- "pseudo R squared"
  x[x$metric == "rmse", "metric"] <- "RMSE"
  x[x$metric == "nrmse", "metric"] <- "NRMSE"

  #plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = x,
      ggplot2::aes(
        group = model,
        y = model,
        x = value,
        fill = model
      ),
      notch = notch,
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
    ggplot2::scale_fill_viridis_d(end = 0.5, alpha = 0.75) +
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
  out.list$plot <- p

  if(verbose == TRUE){
    suppressMessages(print(p))

  }

  out.list

}
