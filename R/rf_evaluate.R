
rf_evaluate <- function(
  model,
  x.column, #character, column name if available, or coordinates.
  y.column, #character, column name if available, or coordinates.
  sf.points = NULL,
  iterations = 30,
  training.fraction = 0.6,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  rf <- get("rf", envir = .GlobalEnv)
  root_mean_squared_error <- get("root_mean_squared_error", envir = .GlobalEnv)
  rescale_vector <- get("rescale_vector", envir = .GlobalEnv)
  multiscale_moran <- get("multiscale_moran", envir = .GlobalEnv)
  moran <- get("moran", envir = .GlobalEnv)
  make_spatial_fold <- get("make_spatial_fold", envir = .GlobalEnv)

  #getting data from the model
  data <- model$ranger.arguments$data
  dependent.variable.name <- model$ranger.arguments$dependent.variable.name
  predictor.variable.names <- model$ranger.arguments$predictor.variable.names
  ranger.arguments <- model$ranger.arguments

  #TODO check that the model is a ranger model and has a data and ranger.arguments slots

  #TODO check if x.column and y.column are vectors or characters.
  #if vectors, length must be nrow(model$data).
  #if characters, they must be in colnames(model$data)

  #process sf.points to get xy
  if(!is.null(sf.points) &
     inherits(sf.points, c("sf", "data.frame")) &
     inherits(sf::st_geometry(sf.points),"sfc_POINT")){
    if(nrow(sf.points) != nrow(model$data)){
      stop("sf.points doesn't has as many rows as data$model.")
    } else {
      #get xy columns
      xy <- as.data.frame(
        do.call(
          "rbind",
          sf::st_geometry(sf.points)
        )
      )
      colnames(xy) <- names
    }
  }

  #add id to data and xy
  data$id <- xy$id <- 1:nrow(data)

  #thinning coordinates to get a more systematic sample of reference points
  if(iterations < nrow(xy)){
    xy.reference.records <- thinning_til_n(
      xy = xy,
      n = iterations
    )
  } else {
    xy.reference.records <- xy
  }


  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

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

  #setting importance = "none" in ranger.arguments
  ranger.arguments$importance <- "none"
  ranger.arguments$data <- NULL

  #prepare cluster
  #preparing cluster for stand alone machine
  if(is.null(cluster.ips) == TRUE){

    #number of available cores
    n.cores <- parallel::detectCores() - 1
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
      ips = cluster.ips,
      cores = cluster.cores,
      user = cluster.user
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
      "multiscale_moran",
      "moran",
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
      scaled.importance = FALSE
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
      intrinsic.r.squared = m.training$r.squared,
      extrinsic.r.squared = 1 - ((sum((predicted - observed)^2))/(sum((mean(observed) - observed)^2))),
      instrinsic.pseudo.r.squared = m.training$pseudo.r.squared,
      extrinsic.pseudo.r.squared = cor(
        observed,
        predicted
      ),
      intrinsic.rmse = m.training$rmse,
      extrinsic.rmse = root_mean_squared_error(
        o = observed,
        p = predicted,
        type = NULL
      ),
      intrinsic.nrmse = m.training$nrmse,
      extrinsic.nrmse = root_mean_squared_error(
        o = observed,
        p = predicted,
        type = "iq"
      )
    )
    rownames(out.df) <- NULL

    return(out.df)

  }#end of parallelized loop

  #add spatial folds to the model
  model$evaluation <- list()
  model$evaluation$spatial.folds <- spatial.folds
  model$evaluation$df <- evaluation.df

  #TODO evaluation plot


}
