rf_compare <- function(
  a = NULL,
  b = NULL,
  a.name = "Model A",
  b.name = "Model B",
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.8,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

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
      notch = TRUE
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
