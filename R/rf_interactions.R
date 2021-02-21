#' @title Suggest variable interactions for random forest models
#' @description Suggests candidate variable interactions by selecting the variables above a given importance threshold (given by the argument `importance.threshold`, or the median importance if not provided) from a model and combining them in pairs through multiplication (`a * b`).
#'
#' For each variable interaction, a model including all the predictors plus the interaction is fitted, and it's R squared is compared with the R squared of the model without interactions. This model without interactions can either be provided through the argument `model`, or is fitted on the fly with [rf_repeat()] if the user provides the data.
#'
#' Users should not use the suggested variable interactions hastily. Most likely, only one or a few of the suggested interactions may make sense from a domain expertise standpoint.
#' @param model A model fitted with [rf()]. If used, the data is taken directly from the model definition and no other data-related arguments are required. Default: `NULL`
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param importance.threshold Value of variable importance from `model` used as threshold to select variables to generate candidate interactions. Default: Median of the variable importance in `model`.
#' @param repetitions Integer, number of random forest models to fit in order to assess the importance of the interaction. Default: `10`
#' @param verbose Logical If `TRUE`, messages and plots generated during the execution of the function are displayed, Default: `TRUE`
#' @param n.cores Integer, number of cores to use during computations. If `NULL`, all cores but one are used, unless a cluster is used. Default = `NULL`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine.
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `11000`
#' @return A list with four slots: `screening`, with the complete screening results; `selected`, with the names and the R squared improvement produced by each variable interaction; `columns`, data frame with the interactions computed from the training data, and `plot`, with the list of plots of the selected interactions versus the response variable. The output list can be plotted all at once with `patchwork::wrap_plots(p)` or `cowplot::plot_grid(plotlist = p)`, or one by one by extracting each plot from the list.
#' @examples
#' \donttest{
#' if(interactive()){
#'  data(plant_richness_df)
#'
#'  interactions <- rf_interactions(
#'    data = plant_richness_df,
#'    dependent.variable.name = "richness_species_vascular",
#'    predictor.variable.names = colnames(plant_richness_df)[5:21],
#'    verbose = TRUE
#'  )
#'
#'  interactions$screening
#'  interactions$selected
#'  interactions$columns
#'
#' }
#' }
#' @importFrom utils combn
#' @importFrom foreach %do%
#' @rdname rf_interactions
#' @export
rf_interactions <- function(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  ranger.arguments = NULL,
  importance.threshold = NULL,
  repetitions = 10,
  verbose = TRUE,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = 11000
  ){

  #declaring variables
  variable <- NULL
  interaction.r.squared.gain <- NULL
  y <- NULL

  #fitting model if absent
  if(is.null(model)){

    #subsetting data
    # data <- data[, c(
    #   dependent.variable.name,
    #   predictor.variable.names
    # )]

    #fitting model
    model <- rf_repeat(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      ranger.arguments = ranger.arguments,
      scaled.importance = FALSE,
      repetitions = repetitions,
      verbose = FALSE,
      n.cores = n.cores,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

  }

  #getting model arguments
  ranger.arguments <- model$ranger.arguments
  data <- ranger.arguments$data
  dependent.variable.name <- ranger.arguments$dependent.variable.name
  predictor.variable.names <- ranger.arguments$predictor.variable.names
  scaled.importance <- ranger.arguments$scaled.importance
  importance <- "permutation"
  local.importance <- FALSE

  #select variables to test
  if(is.null(importance.threshold)){
    importance.threshold <- quantile(model$variable.importance$per.variable$importance, 0.50)
  }
  variables.to.test <- model$variable.importance$per.variable[model$variable.importance$per.variable$importance >= importance.threshold, "variable"]

  #remove spatial_predictors
  if(inherits(model, "rf_spatial")){
    variables.to.test <- variables.to.test[!grepl('spatial_predictor', variables.to.test)]
  }

  #pairs of variables
  variables.pairs <- as.data.frame(t(utils::combn(variables.to.test, 2)))

  #ranger.arguments.i
  ranger.arguments.i <- ranger.arguments

  if(verbose == TRUE){
    message(paste0("Testing ", nrow(variables.pairs), " candidate interactions."))
  }

  #setup of parallel execution
  if(is.null(n.cores)){

    n.cores <- parallel::detectCores() - 1
    `%dopar%` <- foreach::`%dopar%`

  } else {

    #only one core, no cluster
    if(n.cores == 1){

      #replaces dopar (parallel) by do (serial)
      `%dopar%` <- foreach::`%do%`
      on.exit(`%dopar%` <- foreach::`%dopar%`)

    } else {

      `%dopar%` <- foreach::`%dopar%`

    }

  }

  #local cluster
  if(is.null(cluster.ips) & n.cores > 1){

    if(.Platform$OS.type == "windows"){
      temp.cluster <- parallel::makeCluster(
        n.cores,
        type = "PSOCK"
      )
    } else {
      temp.cluster <- parallel::makeCluster(
        n.cores,
        type = "FORK"
      )
    }

    #register cluster and close on exit
    doParallel::registerDoParallel(cl = temp.cluster)
    on.exit(parallel::stopCluster(cl = temp.cluster))

  }

  #beowulf cluster
  if(!is.null(cluster.ips)){


    #cluster port
    Sys.setenv(R_PARALLEL_PORT = cluster.port)

    #preparing the cluster specification
    cluster.spec <- cluster_specification(
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user
    )

    #cluster setup
    if(verbose == TRUE){
      outfile <- ""
    } else {
        outfile <- NULL
      }
    temp.cluster <- parallel::makeCluster(
      master = cluster.ips[1],
      spec = cluster.spec,
      port = cluster.port,
      outfile = outfile,
      homogeneous = TRUE
    )

    #register cluster and close on exit
    doParallel::registerDoParallel(cl = temp.cluster)
    on.exit(parallel::stopCluster(cl = temp.cluster))

  }

  #testing interactions
  i <- NULL
  interaction.screening <- foreach::foreach(
    i = 1:nrow(variables.pairs),
    .combine = "rbind"
  ) %dopar% {

    #get pair
    pair.i <- c(variables.pairs[i, 1], variables.pairs[i, 2])
    pair.i.name <- paste(pair.i, collapse = "_X_")

    #prepare data.i
    data.i <- data.frame(
      data,
      interaction = data[, pair.i[1]] * data[, pair.i[2]]
    )
    colnames(data.i)[ncol(data.i)] <- pair.i.name

    #prepare predictor.variable.names.i
    predictor.variable.names.i <- c(
      predictor.variable.names,
      pair.i.name
    )

    #fitting model
    model.i <- spatialRF::rf_repeat(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      ranger.arguments = ranger.arguments.i,
      scaled.importance = FALSE,
      verbose = FALSE,
      repetitions = repetitions,
      n.cores = 1
    )

    #importance data frames
    model.i.importance <- model.i$variable.importance$per.variable

    #gathering results
    out.df <- data.frame(
      interaction.name = pair.i.name,
      interaction.importance = round((model.i.importance[model.i.importance$variable == pair.i.name, "importance"] * 100) / max(model.i.importance$importance), 3),
      interaction.r.squared.gain = mean(model.i$performance$r.squared) - mean(model$performance$r.squared),
      variable.a.name = pair.i[1],
      variable.b.name = pair.i[2]
    )

    return(out.df)

  }#end of parallelized loop

  #adding column of selected interactions
  interaction.screening$selected <- ifelse(
    interaction.screening$interaction.r.squared.gain > 0 &
    interaction.screening$interaction.importance > 50,
    TRUE,
    FALSE
  )

  #compute order
  interaction.screening$order <- (interaction.screening$interaction.importance / 100) + interaction.screening$interaction.r.squared.gain

  #arrange by gain
  interaction.screening <- dplyr::arrange(
    interaction.screening,
    dplyr::desc(order)
    )

  #remove order
  interaction.screening$order <- NULL

  #selected only
  interaction.screening.selected <- interaction.screening[interaction.screening$selected == TRUE, ]
  interaction.screening.selected <- interaction.screening.selected[, c(
    "interaction.name",
    "interaction.importance",
    "interaction.r.squared.gain",
    "variable.a.name",
    "variable.b.name"
    )]

  if(nrow(interaction.screening) == 0){
    stop("There are no variable interactions to suggest for this model.")
  }

  if(verbose == TRUE){
    message(
      paste0(
        nrow(
          interaction.screening.selected), " potential interactions identified."
        )
      )
  }

  #preparing data frame of interactions
  interaction.df <- data.frame(
    dummy.column = rep(NA, nrow(data))
  )
  for(i in 1:nrow(interaction.screening.selected)){
    interaction.df[, interaction.screening.selected[i, "interaction.name"]] <- data[, interaction.screening.selected[i, "variable.a.name"]] * data[, interaction.screening.selected[i, "variable.b.name"]]
  }
  interaction.df$dummy.column <- NULL

  #removing variable names
  interaction.screening.selected$variable.a.name <- NULL
  interaction.screening.selected$variable.b.name <- NULL

  #printing suggested interactions
  if(verbose == TRUE){

    x <- interaction.screening.selected
    colnames(x) <- c("Interaction", "Importance (% of max)", "R2 improvement")

    x.hux <- huxtable::hux(x) %>%
      huxtable::set_bold(
        row = 1,
        col = huxtable::everywhere,
        value = TRUE
      ) %>%
      huxtable::set_all_borders(TRUE)
    huxtable::number_format(x.hux)[2:nrow(x), 2] <- 1
    huxtable::number_format(x.hux)[2:nrow(x), 3] <- 3
    huxtable::print_screen(x.hux, colnames = FALSE)

  }

  #plot interactions
  plot.list <- list()
  for(variable in names(interaction.df)){

    #create plot data frame
    plot.df <- data.frame(
      y = data[, dependent.variable.name],
      x = interaction.df[, variable]
    )

    #save plot
    plot.list[[variable]] <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = plot.df,
        ggplot2::aes(
          x = x,
          y = y
        ),
        alpha = 0.5
      ) +
      ggplot2::xlab(variable) +
      ggplot2::ylab(dependent.variable.name) +
      ggplot2::ggtitle(
        paste0(
          "R2 gain: ",
          round(interaction.screening.selected[interaction.screening.selected$interaction.name == variable, "interaction.r.squared.gain"], 3),
          "; Importance (%): ",
          round(interaction.screening.selected[interaction.screening.selected$interaction.name == variable, "interaction.importance"], 1)
        )
      ) +
      ggplot2::theme_bw() +
      ggplot2::coord_cartesian(
        xlim = c(min(plot.df$x), quantile(plot.df$x, 0.95)),
        ylim = c(min(plot.df$y), quantile(plot.df$y, 0.95))
      )

  }

  #plot list of plots
  if(length(plot.list) == 1){
    plot.list.out <- plot.list[[1]]
  }
  if(length(plot.list) == 2){
    plot.list.out <- patchwork::wrap_plots(plot.list, ncol = 2)
  }
  if(length(plot.list) > 2){
    plot.list.out <- patchwork::wrap_plots(plot.list, ncol = 3)
  }

  if(verbose == TRUE){
    print(plot.list.out)
  }

  #preparing out list
  out.list <- list()
  out.list$screening <- interaction.screening
  out.list$selected <- interaction.screening.selected
  out.list$columns <- interaction.df
  out.list$plot <- plot.list

  out.list

}
