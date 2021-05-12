#' @title Suggest variable interactions for random forest models
#' @description Suggests candidate variable interactions for random forest models. For a pair of predictors `a` and `b`, interactions are build via multiplication (`a * b`), and by extracting the first factor of a principal component analysis performed with [pca()], after rescaling `a` and `b` between 1 and 100. Interactions based on multiplication are named `a..x..b`, and interactions based on PCA factors are named `a..pca..b`.
#'
#'Candidate variables `a` and `b` are selected from those predictors in `predictor.variable.names` with a variable importance above `importance.threshold` (set by default to the median of the importance scores).
#'
#' For each variable interaction, a model including all the predictors plus the interaction is fitted, and it's R squared is compared with the R squared of the model without interactions.
#'
#'From all the potential interactions screened, only those with a positive increase in the oub-of-bag R squared of the model, a variable importance above the median, and a maximum correlation among themselves and with the predictors in `predictor.variable.names` not higher than `cor.threshold` (set to 0.5 by default) are selected. Such a restrictive set of rules ensures that the selected interactions can be used right away for modeling purposes without increasing model complexity unnecessarily. However, the suggested variable interactions should not be used hastily. Most likely, only one or a few of the suggested interactions may make sense from a domain expertise standpoint.
#'
#'The function returns the criteria used to select the interactions, and the data required to use these interactions a modeling workflow.
#'
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables, or object of class `"variable_selection"` produced by [auto_vif()] and/or [auto_cor()]. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param importance.threshold Value of variable importance from `model` used as threshold to select variables to generate candidate interactions. Default: Quantile 0.75 of the variable importance in `model`.
#' @param cor.threshold Numeric, maximum Pearson correlation between any pair of the selected interactions, and between any interaction and the predictors in `predictor.variable.names`. Default: `0.50`
#' @param point.color Colors of the plotted points. Can be a single color name (e.g. "red4"), a character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F")`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `NULL`
#' @param verbose Logical If `TRUE`, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#' @param n.cores Integer, number of cores to use. Default: `parallel::detectCores() - 1`
#' @param cluster.ips Character vector with the IPs of the machines in a cluster. The machine with the first IP will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed. Default: `NULL`
#' @param cluster.cores Numeric integer vector, number of cores to use on each machine. Default: `NULL`
#' @param cluster.user Character string, name of the user (should be the same throughout machines). Defaults to the current system user.
#' @param cluster.port Integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: `11000`
#' @return A list with seven slots:
#' \itemize{
#'   \item `screening`: Data frame with selection scores of all the interactions considered.
#'   \item `selected`: Data frame with selection scores of the selected interactions.
#'   \item `df`: Data frame with the computed interactions.
#'   \item `plot`: List of plots of the selected interactions versus the response variable. The output list can be plotted all at once with `patchwork::wrap_plots(p)` or `cowplot::plot_grid(plotlist = p)`, or one by one by extracting each plot from the list.
#'   \item `data`: Data frame with the response variable, the predictors, and the selected interactions, ready to be used as `data` argument in the package functions.
#'   \item `dependent.variable.name`: Character, name of the response.
#'   \item `predictor.variable.names`: Character vector with the names of the predictors and the selected interactions.
#' }
#'
#'
#' @examples
#' \donttest{
#' if(interactive()){
#'
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
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  ranger.arguments = NULL,
  importance.threshold = NULL,
  cor.threshold = 0.50,
  point.color = viridis::viridis(
    100,
    option = "F"
    ),
  seed = NULL,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = "11000"
  ){

  #declaring variables
  variable <- NULL
  y <- NULL

  #predictor.variable.names comes from auto_vif or auto_cor
  if(inherits(predictor.variable.names, "variable_selection")){
    predictor.variable.names <- predictor.variable.names$selected.variables
  }

  #fitting model
  model <- rf_repeat(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    ranger.arguments = ranger.arguments,
    repetitions = 10,
    seed = seed,
    verbose = FALSE
  )

  #r squared
  model.r.squared <- median(model$performance$r.squared.oob)

  #ranger.arguments.i
  ranger.arguments.i <- ranger.arguments
  ranger.arguments.i$data <- NULL
  ranger.arguments.i$dependent.variable.name <- NULL
  ranger.arguments.i$predictor.variable.names <- NULL
  ranger.arguments.i$num.trees <- 1000

  #select variables to test
  if(is.null(importance.threshold)){
    importance.threshold <- quantile(
      x = model$importance$per.variable$importance,
      probs = 0.75
      )
  }
  variables.to.test <- model$importance$per.variable[
    model$importance$per.variable$importance >= importance.threshold,
    "variable"
    ]

  #pairs of variables
  variables.pairs <- as.data.frame(
    t(
      utils::combn(
        variables.to.test,
        2
        )
      ),
    stringsAsFactors = FALSE
    )

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

    #creating the cluster
    temp.cluster <- parallel::makeCluster(
      n.cores,
      type = "PSOCK"
    )

    #register cluster and close on exit
    doParallel::registerDoParallel(cl = temp.cluster)

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
      if(.Platform$OS.type == "windows"){
        outfile <- "nul:"
      } else {
        outfile <- "/dev/null"
      }
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
  interaction.screening.1 <- foreach::foreach(
    i = seq(1, nrow(variables.pairs)),
    .combine = "rbind",
    .verbose = FALSE
  ) %dopar% {

    #get pair
    pair.i <- c(variables.pairs[i, 1], variables.pairs[i, 2])
    pair.i.name <- paste0(
      pair.i,
      collapse = "..x.."
      )

    #get interaction values
    pair.i.1 <- spatialRF::rescale_vector(
      x = data[, pair.i[1]],
      new.min = 1,
      new.max = 100
      )
    pair.i.2 <- spatialRF::rescale_vector(
      x = data[, pair.i[2]],
      new.min = 1,
      new.max = 100
      )

    #prepare data.i
    data.i <- data.frame(
      data,
      interaction = pair.i.1 * pair.i.2
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
      repetitions = 10,
      seed = seed,
      verbose = FALSE,
      n.cores = 1
    )

    #importance data frames
    model.i.importance <- model.i$importance$per.variable

    #computing max correlation with predictors
    cor.out <- cor(data.i[, predictor.variable.names.i])
    diag(cor.out) <- NA
    max.cor <- max(cor.out[pair.i.name, ], na.rm = TRUE)

    #gathering results
    out.df <- data.frame(
      interaction.name = pair.i.name,
      interaction.importance = round((model.i.importance[model.i.importance$variable == pair.i.name, "importance"] * 100) / max(model.i.importance$importance), 3),
      interaction.r.squared.gain = median(model.i$performance$r.squared.oob) - model.r.squared,
      max.cor.with.predictors = max.cor,
      variable.a.name = pair.i[1],
      variable.b.name = pair.i[2],
      stringsAsFactors = FALSE
    )

    return(out.df)

  }#end of parallelized loop

  interaction.screening.2 <- foreach::foreach(
    i = seq(1, nrow(variables.pairs)),
    .combine = "rbind",
    .verbose = FALSE
  ) %dopar% {

    #get pair
    pair.i <- c(variables.pairs[i, 1], variables.pairs[i, 2])
    pair.i.name <- paste0(
      pair.i[1],
      "..pca..",
      pair.i[2]
      )

    #get interaction values
    pair.i.1 <- spatialRF::rescale_vector(
      x = data[, pair.i[1]],
      new.min = 1,
      new.max = 100
    )
    pair.i.2 <- spatialRF::rescale_vector(
      x = data[, pair.i[2]],
      new.min = 1,
      new.max = 100
    )

    #prepare data.i
    data.i <- data.frame(
      data,
      interaction = spatialRF::pca(
        x = data.frame(
          pair.i.1,
          pair.i.2
        )
      )$pca_factor_1
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
      repetitions = 10,
      seed = seed,
      verbose = FALSE,
      n.cores = 1
    )

    #importance data frames
    model.i.importance <- model.i$importance$per.variable

    #computing max correlation with predictors
    cor.out <- cor(data.i[, predictor.variable.names.i])
    diag(cor.out) <- NA
    max.cor <- max(cor.out[pair.i.name, ], na.rm = TRUE)

    #gathering results
    out.df <- data.frame(
      interaction.name = pair.i.name,
      interaction.importance = round((model.i.importance[model.i.importance$variable == pair.i.name, "importance"] * 100) / max(model.i.importance$importance), 3),
      interaction.r.squared.gain = median(model.i$performance$r.squared.oob) - model.r.squared,
      max.cor.with.predictors = max.cor,
      variable.a.name = pair.i[1],
      variable.b.name = pair.i[2],
      stringsAsFactors = FALSE
    )

    return(out.df)

  }#end of parallelized loop

  interaction.screening <- rbind(
    interaction.screening.1,
    interaction.screening.2
  )

  #stopping cluster
  if(exists("temp.cluster")){
    parallel::stopCluster(cl = temp.cluster)
  }

  if(nrow(interaction.screening) == 0){
    message("No promising interactions found. \n")
    stop()
  }

  #adding column of selected interactions
  interaction.screening$selected <- ifelse(
    interaction.screening$interaction.r.squared.gain > 0 &
    interaction.screening$max.cor.with.predictors < cor.threshold,
    TRUE,
    FALSE
  )

  if(sum(interaction.screening$selected) == 0){
    message("No promising interactions found. \n")
    return(NA)
  }

  #compute order
  interaction.screening$order <-
    spatialRF::rescale_vector(interaction.screening$interaction.importance) +
    spatialRF::rescale_vector(interaction.screening$interaction.r.squared.gain) -
    spatialRF::rescale_vector(1 - interaction.screening$max.cor.with.predictors)

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
    "max.cor.with.predictors",
    "variable.a.name",
    "variable.b.name"
    )]


  #preparing data frame of interactions
  interaction.df <- data.frame(
    dummy.column = rep(NA, nrow(data))
  )
  for(i in seq(1, nrow(interaction.screening.selected))){

    #get interaction values
    pair.i.1 <- rescale_vector(
      x = data[, interaction.screening.selected[i, "variable.a.name"]],
      new.min = 1,
      new.max = 100
      )
    pair.i.2 <- rescale_vector(
      x = data[, interaction.screening.selected[i, "variable.b.name"]],
      new.min = 1,
      new.max = 100
      )
    pair.i.name <- interaction.screening.selected[i, "interaction.name"]

    #find what type of interaction
    if(grepl("..pca..", pair.i.name, fixed = TRUE) == TRUE){
      interaction.df[, interaction.screening.selected[i, "interaction.name"]] <- spatialRF::pca(
        x = data.frame(
          pair.i.1,
          pair.i.2
        )
      )$pca_factor_1
    } else {
      interaction.df[, interaction.screening.selected[i, "interaction.name"]] <- pair.i.1 * pair.i.2
    }
  }
  interaction.df$dummy.column <- NULL

  #removing variable names
  interaction.screening.selected$variable.a.name <- NULL
  interaction.screening.selected$variable.b.name <- NULL

  #filtering out interactions by correlation among themselves
  if(nrow(interaction.screening.selected) > 1){

    interaction.selection <- auto_cor(
      x = interaction.df,
      preference.order = interaction.screening.selected$interaction.name,
      cor.threshold = cor.threshold,
      verbose = FALSE
    )

    interaction.screening.selected <- interaction.screening.selected[
      interaction.screening.selected$interaction.name %in% interaction.selection$selected.variables,
      ]

    interaction.df <- interaction.df[, interaction.selection$selected.variables]

  }

  if(verbose == TRUE){
    message(
      paste0(
        nrow(interaction.screening.selected),
        " potential interactions identified."
      )
    )
  }


  #printing suggested interactions
  if(verbose == TRUE){

    x <- interaction.screening.selected
    colnames(x) <- c("Interaction", "Importance (% of max)", "R-squared improvement", "Max cor with predictors")

    x.hux <- huxtable::hux(x) %>%
      huxtable::set_bold(
        row = 1,
        col = huxtable::everywhere,
        value = TRUE
      ) %>%
      huxtable::set_all_borders(TRUE)
    huxtable::number_format(x.hux)[2:nrow(x), 2] <- 1
    huxtable::number_format(x.hux)[2:nrow(x), 3] <- 3
    huxtable::number_format(x.hux)[2:nrow(x), 4] <- 2
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
          y = y,
          color = y
        )
      ) +
      ggplot2::scale_color_gradientn(colors = point.color) +
      ggplot2::xlab(variable) +
      ggplot2::ylab(dependent.variable.name) +
      ggplot2::ggtitle(
        paste0(
          "+R2: ",
          round(
            interaction.screening.selected[
            interaction.screening.selected$interaction.name == variable,
            "interaction.r.squared.gain"
            ],
            3
            ),
          "; Imp. (%): ",
          round(
            interaction.screening.selected[
            interaction.screening.selected$interaction.name == variable,
            "interaction.importance"
            ],
            0
            ),
          "; Max cor: ",
          round(
            interaction.screening.selected[
              interaction.screening.selected$interaction.name == variable,
              "max.cor.with.predictors"],
            2
            )
        )
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
      )

  }

  #plot list of plots
  if(length(plot.list) == 1){
    plot.list.out <- plot.list[[1]]
  }
  if(length(plot.list) == 2){
    plot.list.out <- patchwork::wrap_plots(plot.list)
  }
  if(length(plot.list) > 2){
    plot.list.out <- patchwork::wrap_plots(plot.list)
  }

  if(verbose == TRUE){
    print(plot.list.out)
  }

  #generating training df
  training.df <- cbind(
    data,
    interaction.df
  )

  #preparing out list
  out.list <- list()
  out.list$screening <- interaction.screening
  out.list$selected <- interaction.screening.selected
  out.list$df <- interaction.df
  out.list$plot <- plot.list
  out.list$data <- training.df
  out.list$dependent.variable.name <- dependent.variable.name
  out.list$predictor.variable.names <- c(
    predictor.variable.names,
    colnames(interaction.df)
  )

  out.list

}
