#' @param xy
rf_evaluate <- function(
  model,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.6,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #getting data from the model
  data <- model$ranger.arguments$data
  dependent.variable.name <- model$ranger.arguments$dependent.variable.name
  predictor.variable.names <- model$ranger.arguments$predictor.variable.names
  ranger.arguments <- model$ranger.arguments

  #preparing xy
  #if null, stop
  if(is.null(xy)){
    stop("Argument 'xy' requires a matrix or data frame with longitude-latitude coordinates with columns named 'x' and 'y'.")
  } else {
    if(inherits(xy, "sf")){
      xy <- sf_points_to_xy(xy)
    }
    if(sum(colnames(xy) == c("x", "y")) < 2){
      stop("The column names of 'xy' must be 'x' and 'y'.")
    }
  }

  #check nrow of xy and data
  if(nrow(xy) != nrow(data)){
    stop("nrow(xy) and nrow(data) (stored in model$ranger.arguments$data) must be the same.")
  }

  #add id to data and xy
  data$id <- xy$id <- 1:nrow(data)

  #thinning coordinates to get a more systematic sample of reference points
  if(repetitions < nrow(xy)){
    xy.reference.records <- thinning_til_n(
      xy = xy,
      n = repetitions
    )
  } else {
    xy.reference.records <- xy
  }

  #computing distance step for spatial folds
  distance.step <- min(dist(xy)) / 2

  #generates spatial folds
  ####################################
  spatial.folds <- make_spatial_folds(
    xy.selected = xy.reference.records,
    xy = xy,
    distance.step = distance.step,
    training.fraction = training.fraction,
    n.cores = parallel::detectCores() - 1
  )

  #INITIALIZING CLUSTER

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
      port = Sys.getenv("R_PARALLEL_PORT"),
      outfile = "",
      homogeneous = TRUE
    )

  }
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  #setting importance = "none" in ranger.arguments
  ranger.arguments$importance <- "none"
  ranger.arguments$data <- NULL
  ranger.arguments$scaled.importance <- FALSE
  ranger.arguments$distance.matrix <- NULL

  #loop to evaluate models
  #####################################
  evaluation.df <- foreach::foreach(
    i = 1:length(spatial.folds),
    .combine = "rbind",
    .packages = c(
      "ranger",
      "magrittr"
    ),
    .export = c(
      "root_mean_squared_error",
      "rescale_vector",
      "scale_robust",
      "root_mean_squared_error"
    )
  ) %dopar% {

    #separating training and testing data
    data.training <- data[data$id %in% spatial.folds[[i]]$training, ]
    data.testing <- data[data$id %in% spatial.folds[[i]]$testing, ]

    #training model
    m.training <- rf(
      data = data.training,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      ranger.arguments = ranger.arguments,
      verbose = FALSE
    )

    #predicting over data.testing
    predicted <- predict(
      object = m.training,
      data = data.testing,
      type = "response"
    )$predictions
    observed <- data.testing[, dependent.variable.name]

    #computing evaluation scores
    out.df <- data.frame(
      reference.record.id = xy.reference.records[i, "id"],
      reference.record.x = xy.reference.records[i, "x"],
      reference.record.y = xy.reference.records[i, "y"],
      training.records = nrow(data.training),
      testing.records = nrow(data.testing),
      training.r.squared = m.training$performance$r.squared,
      testing.r.squared = round(1 - ((sum((predicted - observed)^2))/(sum((mean(observed) - observed)^2))), 3),
      training.pseudo.r.squared = m.training$performance$pseudo.r.squared,
      testing.pseudo.r.squared = round(cor(
        observed,
        predicted
      ), 3),
      training.rmse = m.training$performance$rmse,
      testing.rmse = round(root_mean_squared_error(
        o = observed,
        p = predicted,
        normalization = NULL
      ), 3),
      training.nrmse = m.training$performance$nrmse,
      testing.nrmse = round(root_mean_squared_error(
        o = observed,
        p = predicted,
        normalization = "iq"
      ), 3)
    )
    rownames(out.df) <- NULL

    return(out.df)

  }#end of parallelized loop

  #prepare data for plotting and reporting

  #add spatial folds to the model
  model$evaluation <- list()
  model$evaluation$spatial.folds <- spatial.folds
  model$evaluation$df <- evaluation.df

  #TODO evaluation plot


}
