#' @title Compares performance of two models on independent data
#' @description Uses [rf_evaluate()] to compare the performance of two models on independent spatial folds.
#' @param a Model fitted with [rf()], [rf_tuning()], [rf_repeat()], or [rf_spatial()]. Default: NULL
#' @param b Model fitted with [rf()], [rf_tuning()], [rf_repeat()], or [rf_spatial()]., Default: NULL
#' @param a.name Character, name of model `a`, Default: `'Model A'`
#' @param b.name Character, name of model `b`, Default: `'Model B'`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y", or an sf file with geometry class `sfc_POINT` (see [plant_richness_sf]). If `NULL`, the function will throw an error. Default: `NULL`
#' @param repetitions Integer, must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the number of records to be used in model training. Default: `0.8`
#' @param notch Logical, if `TRUE`, boxplot notches are plotted. Default: `TRUE`
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `11000`
#' @return A list with three slots: `df.long`, with the performance metrics for each model across repetitions; `df.aggregated`, with aggregated stats per model and performance matric; `plot`, with a boxplot built from `df.long`.
#' @examples
#' \dontrun{
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
  a = NULL,
  b = NULL,
  a.name = "Model A",
  b.name = "Model B",
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.8,
  notch = FALSE,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #declaring variables
  model <- NULL
  value <- NULL
  metric <- NULL

  #capturing user options
  user.options <- options()
  options(dplyr.summarise.inform = FALSE)
  on.exit(options <- user.options)

  #missing input
  if(is.null(a)){
    stop("Model 'a' is missing.")
  }
  if(is.null(b)){
    stop("Model 'b' is missing.")
  }
  if(is.null(xy)){
    stop("Case coordinates 'xy' is missing.")
  }

  #evaluating models
  a.evaluation <- rf_evaluate(
    model = a,
    xy = xy,
    repetitions = repetitions,
    training.fraction = training.fraction,
    verbose = FALSE,
    n.cores = n.cores,
    cluster.ips = cluster.ips,
    cluster.cores = cluster.cores,
    cluster.user = cluster.user,
    cluster.port = cluster.port
  )

  b.evaluation <- rf_evaluate(
    model = b,
    xy = xy,
    repetitions = repetitions,
    training.fraction = training.fraction,
    verbose = FALSE,
    n.cores = n.cores,
    cluster.ips = cluster.ips,
    cluster.cores = cluster.cores,
    cluster.user = cluster.user,
    cluster.port = cluster.port
  )

  #getting evaluation objects
  a.evaluation <- a.evaluation$evaluation$per.model %>%
    dplyr::filter(model == "Testing") %>%
    dplyr::mutate(model = a.name) %>%
    tidyr::pivot_longer(
      cols = 1:4,
      names_to = "metric",
      values_to = "value"
    ) %>%
    as.data.frame()

  b.evaluation <- b.evaluation$evaluation$per.model %>%
    dplyr::filter(model == "Testing") %>%
    dplyr::mutate(model = b.name) %>%
    tidyr::pivot_longer(
      cols = 1:4,
      names_to = "metric",
      values_to = "value"
    ) %>%
    as.data.frame()

  comparison <- rbind(
    a.evaluation,
    b.evaluation
  )

  comparison$metric <- factor(
    comparison$metric,
    levels = c("r.squared", "pseudo.r.squared", "rmse", "nrmse"),
    labels = c("R squared", "pseudo R squared" , "RMSE","NRMSE")
  )

  p <- ggplot2::ggplot() +
    ggplot2::facet_wrap(
      "metric",
      scales = "free_x",
      ncol = 1) +
    ggplot2::geom_boxplot(
      data = comparison,
      ggplot2::aes(
        x = value,
        y = model,
        group = model,
        fill = model
      ),
      notch = notch
    ) +
    ggplot2::scale_fill_viridis_d(end = 0.8, alpha = 0.75) +
    ggplot2::theme_bw() +
    ggplot2::ylab("") +
    ggplot2::theme(legend.position = "none")

  comparison.aggregated <- comparison %>%
    dplyr::group_by(model, metric) %>%
    dplyr::summarise(
      mean = round(mean(value), 3),
      median = round(median(value), 3),
      standard_deviation = round(sd(value), 3),
      standard_error = round(standard_error(value), 3),
      minimum = round(min(value), 3),
      maximum = round(max(value), 3)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(metric) %>%
    as.data.frame()

  #out list
  out.list <- list()
  out.list$df.long <- comparison
  out.list$df.aggregated <- comparison.aggregated
  out.list$plot <- p

  if(verbose == TRUE){
    suppressMessages(print(p))

  }

  out.list


}
