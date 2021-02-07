#' @title suggest_interactions
#' @description Takes a model fitted with [rf()], [rf_repeat()], or [rf_spatial()] to find variable interactions improving the R squared of the model.
#' @param model (optional) a model produced by [rf]. If used, the arguments `data`, `dependent.variable.name`, `predictor.variable.names`, `distance.matrix`, `distance.thresholds`, `ranger.arguments`, `trees.per.variable`, and `scaled.importance` are taken directly from the model definition. Default: `NULL`
#' @param importance.threshold Value of variable importance from `model` used as threshold to select variables to generate candidate interactions. Default: Median of the variable importance in `model`.
#' @param n.cores number of cores to use to compute repetitions. If NULL, all cores but one are used, unless a cluster is used.
#' @param cluster.ips character vector, IPs of the machines in the cluster. The first machine will be considered the main node of the cluster, and will generally be the machine on which the R code is being executed.
#' @param cluster.cores numeric integer vector, number of cores on each machine.
#' @param cluster.user character string, name of the user (should be the same throughout machines), Defaults to the current system user. Default: user name of the current session.
#' @param cluster.port integer, port used by the machines in the cluster to communicate. The firewall in all computers must allow traffic from and to such port. Default: 11000.
#' @return A list with two slots: *selected*, with the names and the R squared improvement produced by each variable interaction; *df*, data frame with the interactions computed from the data in `model$ranger.arguments` after scaling it with [scale_robust()]. Variable interactions are computed as `x * y` on the scaled data.
#' @details This function suggests variable interactions that could improve a model by adding them one at a time to the original `model` and assessing their contribution to the model's R squared. Only interactions with a positive contribution are suggested. We advise the users not to use variable interactions blindly. Most likely, only one or a few of the suggested interactions may make sense from a domain expertise standpoint.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(plant_richness_df)
#' data(distance_matrix)
#'
#' rf.model <- rf(
#'  data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = c(0, 1000, 2000),
#'   verbose = FALSE
#' )
#'
#' interactions <- suggest_interactions(model = rf.model)
#' interactions$selected
#' interactions$df
#'  }
#' }
#' @importFrom utils combn
#' @rdname suggest_interactions
#' @export
suggest_interactions <- function(
  model = NULL,
  importance.threshold = NULL,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = Sys.info()[["user"]],
  cluster.port = 11000
  ){

  #declaring variables
  variable <- NULL
  interaction.r.squared.gain <- NULL

  if(is.null(model)){
    stop("Argument 'model' must be a model fitted with rf(), rf_repeat(), or rf_spatial()")
  }

  #getting ranger.arguments
  ranger.arguments <- model$ranger.arguments
  ranger.arguments$num.threads <- 1 #one core within the parallel loop
  data <- ranger.arguments$data
  dependent.variable.name <- ranger.arguments$dependent.variable.name
  predictor.variable.names <- ranger.arguments$predictor.variable.names
  distance.matrix = ranger.arguments$distance.matrix
  distance.thresholds <- ranger.arguments$distance.thresholds
  trees.per.variable <- ranger.arguments$trees.per.variable
  scaled.importance <- ranger.arguments$scaled.importance
  seed <- NULL
  importance <- "permutation"

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

  #INITIALIZING CLUSTER
  if(is.null(cluster.port)){
    cluster.port <- Sys.getenv("R_PARALLEL_PORT")
  }

  #preparing cluster for stand alone machine
  if(is.null(cluster.ips) == TRUE){

    #number of available cores
    if(is.null(n.cores)){
      n.cores <- parallel::detectCores() - 1
    }
    if(n.cores == 1){
      if(is.null(ranger.arguments)){
        ranger.arguments <- list()
      }
      ranger.arguments$num.threads <- 1
    }
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

    #preparing beowulf cluster
  } else {

    #preparing the cluster specification
    cluster.spec <- cluster_specification(
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user
    )

    #setting parallel port
    Sys.setenv(R_PARALLEL_PORT = cluster.port)

    #cluster setup
    temp.cluster <- parallel::makeCluster(
      master = cluster.ips[1],
      spec = cluster.spec,
      port = cluster.port,
      outfile = "",
      homogeneous = TRUE
    )

  }
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  #ranger.arguments.i
  ranger.arguments.i <- ranger.arguments

  #getting data and normalizing it
  data.scaled <- scale_robust(
    x = data
  )

  #PARALLELIZED LOOP
  i <- NULL
  interaction.screening <- foreach::foreach(
    i = 1:nrow(variables.pairs),
    .combine = "rbind"
  ) %dopar% {

    #get pair
    pair.i <- c(variables.pairs[i, 1], variables.pairs[i, 2])
    pair.i.name <- paste(pair.i, collapse = ":")

    #prepare data.i
    ranger.arguments.i$data <- data.frame(
      data.scaled,
      interaction = data.scaled[, pair.i[1]] * data.scaled[, pair.i[2]]
    )
    colnames(ranger.arguments.i$data)[ncol(ranger.arguments.i$data)] <- pair.i.name

    #prepare predictor.variable.names.i
    ranger.arguments.i$predictor.variable.names <- c(
      predictor.variable.names,
      pair.i.name
    )

    #fitting model
    model.i <- rf(
      ranger.arguments = ranger.arguments.i,
      scaled.importance = FALSE,
      verbose = FALSE,
      seed = i
    )

    #importance data frames
    model.i.importance <- model.i$variable.importance$per.variable %>%
      dplyr::filter(variable %in% c(pair.i.name, pair.i))

    #gathering results
    out.df <- data.frame(
      interaction.name = pair.i.name,
      interaction.importance = round(model.i.importance[model.i.importance$variable == pair.i.name, "importance"], 3),
      interaction.r.squared.gain = mean(model.i$performance$r.squared) - mean(model$performance$r.squared),
      variable.1.name = pair.i[1],
      variable.1.importance = round(model.i.importance[model.i.importance$variable == pair.i[1], "importance"], 3),
      variable.2.name = pair.i[2],
      variable.2.importance = round(model.i.importance[model.i.importance$variable == pair.i[2], "importance"], 3)
    )

    return(out.df)

  }

  #adding column of selected interactions
  interaction.screening$selected <- ifelse(
    interaction.screening$interaction.r.squared.gain > 0 &
    interaction.screening$interaction.importance > importance.threshold,
    TRUE,
    FALSE
  )

  #select interactions
  interaction.screening <- interaction.screening[interaction.screening$selected == TRUE, ] %>%
    dplyr::arrange(dplyr::desc(interaction.r.squared.gain))

  if(nrow(interaction.screening) == 0){
    stop("There are no variable interactions to suggest for this model.")
  }

  #remove extra column
  interaction.screening$selected <- NULL

  #preparing data frame of interactions
  interaction.df <- data.frame(
    dummy.column = rep(NA, nrow(data.scaled))
  )
  for(i in 1:nrow(interaction.screening)){
    interaction.df[, interaction.screening[i, "interaction.name"]] <- data.scaled[, interaction.screening[i, "variable.1.name"]] * data.scaled[, interaction.screening[i, "variable.2.name"]]
  }
  interaction.df$dummy.column <- NULL

  #preparing out list
  out.list <- list()
  out.list$selected <- interaction.screening
  out.list$df <- interaction.df

  out.list

}
