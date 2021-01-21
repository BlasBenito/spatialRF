

#note about methods
#define a unique "method" argument with options such as "hengl", "hengl.sequential", "hengl.optimized" (using columns of distance matrix instead of pca), "mem.sequential", "pca.sequential", and "pca.optimized".
rf_spatial <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  method = c(
    "hengl", #all distance matrix columns as predictors
    "hengl.sequential", #distance matrix columns added in order of effect.
    "hengl.optimized", #distance matrix columns added maximizing their joint effect.
    "mem.sequential", #pca factors added in order of their Moran's I.
    "moran.reduction.sequential", #pca factors added in order of effect
    "moran.reduction.optimized" #pca factors added maximizing their joint effect.
  ),
  iterations = 1,
  trees.per.variable = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000,
  verbose = TRUE,
  ranger.arguments = list(
    formula = NULL,
    mtry = NULL,
    importance = "permutation",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    max.depth = NULL,
    replace = TRUE,
    case.weights = NULL,
    class.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = TRUE,
    local.importance = FALSE,
    regularization.factor = 1,
    regularization.usedepth = FALSE,
    keep.inbag = FALSE,
    inbag = NULL,
    holdout = FALSE,
    quantreg = FALSE,
    oob.error = TRUE,
    num.threads = parallel::detectCores() - 1,
    save.memory = FALSE,
    verbose = TRUE,
    seed = NULL,
    classification = NULL,
    x = NULL,
    y = NULL,
    sample.fraction = 1
  )
){

  #testing method argument
  method <- match.arg(method)

  #initializing "importance"
  if(!is.null(ranger.arguments$importance)){
    importance <- ranger.arguments$importance
  } else {
    importance <- "none"
  }

  #FITTING NON-SPATIAL MODEL
  #######################################################
  if(iterations == 1){

    m.non.spatial <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments
    )

  } else {

    m.non.spatial <- rf_repeat(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      iterations = iterations,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      trees.per.variable = trees.per.variable,
      ranger.arguments = ranger.arguments
    )

  }

  #extracting autocorrelation of the residuals
  m.spatial.correlation.residuals <- m.non.spatial$spatial.correlation.residuals$df %>%
    dplyr::arrange(desc(moran.i)) %>%
    dplyr::filter(interpretation == "Positive spatial correlation")

  #if residuals are not autocorrelated, return original model
  if(nrow(m.spatial.correlation.residuals) == 0){

    if(verbose == TRUE){
      message("Residuals are not spatially correlated, this model is good to go!")
      print(m.non.spatial$spatial.correlation.residuals$plot)
    }

    return(m.non.spatial)

  }# END OF FITTING NON-SPATIAL MODEL


  #GENERATING SPATIAL PREDICTORS
  #########################################################

  #names of spatial_structure columns
  spatial.predictors.names <- paste0("spatial_predictor_", 1:ncol(distance.matrix))

  #HENGL
  if(method %in% c(
    "hengl",
    "hengl.sequential",
    "hengl.optimized"
  )
  ){

    #change name of distance matrix
    spatial.predictors.df <- distance.matrix
    if(verbose == TRUE){
      message("Using distance matrix columns as spatial predictors")
    }
    colnames(spatial.predictors.df) <- rownames(spatial.predictors.df) <- paste0("spatial_predictor_", 1:ncol(distance.matrix))

  }

  #PCA
  if(method %in% c(
    "mem.sequential",
    "moran.reduction.sequential",
    "moran.reduction.optimized"
  )
  ){

    #computing pca factors for pca methods
    spatial.predictors.df <- pca_distance_matrix(
      distance.matrix = distance.matrix,
      distance.threshold =  m.spatial.correlation.residuals$distance.threshold
    )
    if(verbose == TRUE){
      message("Using PCA factors of the distance matrix as spatial predictors")
    }

  }


  #RANKING SPATIAL PREDICTORS
  ###########################################################

  #SELECTING RANKING METHOD
  if(method %in% "hengl"){
    ranking.method <- NULL
  }
  if(method %in% c("mem.sequential")){
    ranking.method <- "mem"
    if(verbose == TRUE){
      message("Ranking spatial predictors by their Moran's I.")
    }
  }
  if(method %in% c("hengl.sequential", "hengl.optimized", "moran.reduction.sequential", "moran.reduction.optimized")){
    ranking.method <- "moran.i.reduction"
    if(verbose == TRUE){
      message("Ranking spatial predictors by how they reduce the Moran's I of the model residuals.")
    }
  }
  if(method %in% c("hengl.sequential", "hengl.optimized")){
    multicollinearity.filter <- "cor"
  } else {
    multicollinearity.filter <- "vif"
  }

  #creating fast version of ranger.arguments
  ranger.arguments.fast <- ranger.arguments
  ranger.arguments.fast$importance <- "none"
  ranger.arguments.fast$scale.permutation.importance <- FALSE
  ranger.arguments.fast$keep.inbag <- FALSE
  ranger.arguments.fast$local.importance <- FALSE

  #RANKING SPATIAL PREDICTORS (if method is not "hengl")
  ######################################################
  if(!is.null(ranking.method)){

    #ranking spatial predictors
    ranking.spatial.predictors <- rank_spatial_predictors(
      ranking.method = ranking.method,
      spatial.predictors.df = spatial.predictors.df,
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      reference.moran.i = m.non.spatial$spatial.correlation.residuals$max.moran,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port,
      multicollinearity.filter = multicollinearity.filter
    )

  }

  #SEQUENTIAL SELECTION OF SPATIAL PREDICTORS
  if(method %in% c("hengl.sequential", "mem.sequential", "moran.reduction.sequential")){

    if(verbose == TRUE){
      message("Selecting spatial predictors in the order of the ranking (sequentially)")
    }

    spatial.predictors.selected <- select_spatial_predictors_sequential(
      spatial.predictors.df = spatial.predictors.df,
      spatial.predictors.ranking = ranking.spatial.predictors,
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )$best.spatial.predictors

  }


  #OPTIMIZED SELECTION OF SPATIAL PREDICTORS
  if(method %in% c("hengl.optimized", "moran.reduction.optimized")){

    if(verbose == TRUE){
      message("Selecting spatial predictors by optimizing their joint effect in the model")
    }

    spatial.predictors.selected <- select_spatial_predictors_optimized(
      spatial.predictors.df = spatial.predictors.df,
      spatial.predictors.rank = ranking.spatial.predictors,
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      cluster.ips = NULL,
      cluster.cores = NULL,
      cluster.user = NULL,
      cluster.port = NULL
    )$best.spatial.predictors

  }

  #FITTING SPATIAL MODEL
  ######################

  #subsetting spatial predictors
  spatial.predictors.df <- spatial.predictors.df[, spatial.predictors.selected]

  #prepare data with best pca factors
  data.spatial <- data.frame(
    data,
    spatial.predictors.df
  )
  colnames(data.spatial) <- c(
    colnames(data),
    spatial.predictors.selected
  )

  #prepare predictor variable names
  predictor.variable.names.spatial <- c(
    predictor.variable.names,
    spatial.predictors.selected
  )

  #fitting a single spatial model
  if(iterations == 1){

    #fitting model
    m.spatial <- rf(
      data = data.spatial,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.spatial,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments
    )

    #preparing before and after plot.df
    after.df <- m.spatial$spatial.correlation.residuals$df
    after.df$model <- "Spatial"
    before.df <- m.non.spatial$spatial.correlation.residuals$df
    before.df$model <- "Non-spatial"
    plot.df <- rbind(before.df, after.df)
    m.spatial$spatial.correlation.residuals$df <- plot.df

    #preparing variable importance plot
    if(importance != "none"){
      importance.df <- m.spatial$variable.importance$df
      spatial.predictors.df <- importance.df[grepl(
        "spatial_predictor",
        importance.df$variable
      ),]
      spatial.predictors.df$variable <- "spatial_predictors"
      predictors.df <- importance.df[!grepl(
        "spatial_predictor",
        importance.df$variable
      ),]
      importance.df <- rbind(
        spatial.predictors.df,
        predictors.df
      )
      importance.plot <- ggplot2::ggplot(data = importance.df) +
        ggplot2::aes(
          x = importance,
          y = reorder(
            variable,
            importance,
            FUN = max
          )
        ) +
        ggplot2::geom_point(alpha = 0.5, size = 4) +
        ggplot2::ylab("")

      m.spatial$variable.importance$df <- importance.df
      m.spatial$variable.importance$plot <- importance.plot
    }

  } else {

    m.spatial <- rf_repeat(
      data = data.spatial,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.spatial,
      iterations = iterations,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      trees.per.variable = trees.per.variable,
      ranger.arguments = ranger.arguments
    )

    #preparing before and after plot.df
    after.df <- m.spatial$spatial.correlation.residuals$df.long
    after.df$model <- "Spatial"
    before.df <- m.non.spatial$spatial.correlation.residuals$df.long
    before.df$model <- "Non-spatial"
    plot.df <- rbind(before.df, after.df)
    m.spatial$spatial.correlation.residuals$df.long <- plot.df

    #preparing importance plot
    if(importance != "none"){
      importance.df <- m.spatial$variable.importance$df.long
      spatial.structure.df <- importance.df[grepl(
        "spatial_structure",
        importance.df$variable
      ),]
      spatial.structure.df$variable <- "spatial_structure"
      predictors.df <- importance.df[!grepl(
        "spatial_structure",
        importance.df$variable
      ),]

      #plot
      importance.df <- rbind(
        spatial.structure.df,
        predictors.df
      )
      importance.plot <- ggplot2::ggplot(data = importance.df) +
        ggplot2::aes(
          x = importance,
          y = reorder(
            variable,
            importance,
            FUN = mean
          )
        ) +
        ggplot2::geom_boxplot() +
        ggplot2::ylab("")

      m.spatial$variable.importance$df <- importance.df
      m.spatial$variable.importance$plot <- importance.plot
    }

  }

  #plot Moran's I spatial vs non-spatial
  m.spatial$spatial.correlation.residuals$plot <- ggplot2::ggplot(data = plot.df) +
    ggplot2::aes(
      x = distance.threshold,
      y = moran.i,
      color = model
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      col = "gray10",
      size = 0.7,
      linetype = "dashed"
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = c("red4", "blue4")) +
    ggplot2::xlab("Distance thresholds") +
    ggplot2::ylab("Moran's I of residuals") +
    ggplot2::ggtitle("Spatial vs. non-spatial model") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(color = "Model")

  #printing Moran's I of residuals
  print(m.spatial$spatial.correlation.residuals$plot)

  #accuracy comparison
  comparison.df <- data.frame(
    r.squared = round(c(m.non.spatial$r.squared, m.spatial$r.squared), 3),
    rmse = round(c(m.non.spatial$rmse, m.spatial$rmse), 3),
    nrmse = round(c(m.non.spatial$nrmse, m.spatial$nrmse), 3)
  )
  rownames(comparison.df) <- c("Non-spatial model", "Spatial model")
  message("Accuracy comparison")
  print(comparison.df)

  #adding data to the model
  m.spatial$performance.comparison <- comparison.df

  #adding spatial method and predictors to the model
  m.spatial$spatial.predictors <- list()
  m.spatial$spatial.predictors$method <- method
  m.spatial$spatial.predictors$predictors.names <- spatial.predictors.selected

  #return output
  m.spatial

}

rf_repeat <- function(data = NULL,
                      dependent.variable.name = NULL,
                      predictor.variable.names = NULL,
                      distance.matrix = NULL,
                      distance.thresholds = NULL,
                      iterations = 10,
                      keep.models = FALSE,
                      trees.per.variable = NULL,
                      scaled.importance = TRUE,
                      cluster.ips = NULL,
                      cluster.cores = NULL,
                      cluster.user = NULL,
                      cluster.port = 11000,
                      ranger.arguments = list(
                        formula = NULL,
                        mtry = NULL,
                        importance = "permutation",
                        write.forest = TRUE,
                        probability = FALSE,
                        min.node.size = NULL,
                        max.depth = NULL,
                        replace = TRUE,
                        case.weights = NULL,
                        class.weights = NULL,
                        splitrule = NULL,
                        num.random.splits = 1,
                        alpha = 0.5,
                        minprop = 0.1,
                        split.select.weights = NULL,
                        always.split.variables = NULL,
                        respect.unordered.factors = NULL,
                        scale.permutation.importance = TRUE,
                        local.importance = FALSE,
                        regularization.factor = 1,
                        regularization.usedepth = FALSE,
                        keep.inbag = FALSE,
                        inbag = NULL,
                        holdout = FALSE,
                        quantreg = FALSE,
                        oob.error = TRUE,
                        num.threads = parallel::detectCores() - 1,
                        save.memory = FALSE,
                        verbose = TRUE,
                        classification = NULL,
                        x = NULL,
                        y = NULL,
                        sample.fraction = 1
                      )
){

  #initializes local.importance
  if(is.null(ranger.arguments$local.importance)){
    local.importance <- FALSE
  } else {
    local.importance <- ranger.arguments$local.importance
  }

  #initializes local.importance
  if(!is.null(ranger.arguments$trees.per.variable)){
    trees.per.variable <- ranger.arguments$trees.per.variable
  }

  #SCALING DATA
  if(scaled.importance == TRUE){

    #applying robust scaling to the data
    data.scaled <- quantable::robustscale(
      data = data[, c(
        dependent.variable.name,
        predictor.variable.names
      )],
      dim = 2,
      center = TRUE,
      scale = TRUE,
      preserveScale = FALSE
    )$data

    #if scaling fails, use regular scaling
    if(
      sum(is.nan(data.scaled[, 1])) > 0 |
      sum(is.infinite(data.scaled[, 1])) > 0
    ){
      data.scaled <- as.data.frame(scale(data))
    }

  }

  #INITIALIZING CLUSTER
  #importing functions from the global environment
  rf <- get("rf", envir = .GlobalEnv)
  root_mean_squared_error <- get("root_mean_squared_error", envir = .GlobalEnv)
  rescale_vector <- get("rescale_vector", envir = .GlobalEnv)
  multiscale_moran <- get("multiscale_moran", envir = .GlobalEnv)
  moran <- get("moran", envir = .GlobalEnv)

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
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  #PARALLELIZED LOOP
  repeated.models <- foreach::foreach(
    i = 1:iterations,
    .packages = c(
      "ranger",
      "magrittr",
      "quantable"
    ),
    .export = c(
      "rescale_vector",
      "root_mean_squared_error",
      "rescale_vector",
      "multiscale_moran",
      "moran"
    )
  ) %dopar% {

    #model on raw data
    m.i <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      white.noise = white.noise,
      autocorrelated.noise = autocorrelated.noise,
      trees.per.variable = trees.per.variable,
      seed = i,
      ranger.arguments = ranger.arguments
    )

    #model on scaled data
    if(scaled.importance == TRUE){

      m.i.scaled <- rf(
        data = data.scaled,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        distance.matrix = NULL,
        white.noise = white.noise,
        autocorrelated.noise = autocorrelated.noise,
        trees.per.variable = trees.per.variable,
        seed = i,
        ranger.arguments = ranger.arguments
      )

    }

    #gathering results
    out <- list()
    out$predictions <- m.i$predictions
    if(local.importance == TRUE){
      out$variable.importance.local <- m.i.scaled$variable.importance.local
    }
    if(scaled.importance == TRUE){
      out$variable.importance <- m.i.scaled$variable.importance$vector
    } else {
      out$variable.importance <- m.i$variable.importance$vector
    }
    out$prediction.error <- m.i$prediction.error
    out$r.squared <- m.i$r.squared
    out$pseudo.r.squared <- m.i$pseudo.r.squared
    out$rmse <- m.i$rmse
    out$nrmse <- m.i$nrmse
    out$residuals <- m.i$residuals
    out$spatial.correlation.residuals <- m.i$spatial.correlation.residuals
    if(keep.models == TRUE){
      out$model <- m.i
    }

    return(out)

  }#end of parallelized loop

  #fitting model to allow plotting partial dependence curves
  m.curves <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance.matrix,
    distance.thresholds = distance.thresholds,
    white.noise = white.noise,
    autocorrelated.noise = autocorrelated.noise,
    trees.per.variable = trees.per.variable,
    seed = seed,
    ranger.arguments = ranger.arguments
  )

  #PARSING OUTPUT OF PARALLELIZED LOOP

  #names of iterations columns
  iteration.columns <- paste("iteration", 1:iterations, sep = "_")

  #gathering predictions
  predictions.by.iteration <- as.data.frame(
    do.call(
      "cbind",
      lapply(
        repeated.models,
        "[[",
        "predictions"
      )
    )
  )
  colnames(predictions.by.iteration) <- iteration.columns
  predictions.mean <- data.frame(
    prediction_mean = rowMeans(predictions.by.iteration),
    standard_deviation = apply(predictions.by.iteration, 1, sd)
  )
  m.curves$predictions <- NULL
  m.curves$predictions$df.wide <- predictions.by.iteration
  m.curves$predictions$df <- predictions.mean

  #gathering variable.importance.local
  if(local.importance == TRUE){
    m.curves$variable.importance.local <- as.data.frame(
      apply(
        simplify2array(
          lapply(
            repeated.models,
            "[[",
            "variable.importance.local"
          )
        ),
        1:2,
        mean
      )
    )
  }


  #gathering variable.importance
  m.curves$variable.importance <- NULL

  #wide format
  variable.importance.df.wide <- as.data.frame(
    do.call(
      "cbind",
      lapply(
        repeated.models,
        "[[",
        "variable.importance"
      )
    )
  )
  colnames(variable.importance.df.wide) <- iteration.columns
  variable.importance.df.wide <- data.frame(
    variable = rownames(variable.importance.df.wide),
    variable.importance.df.wide,
    row.names = NULL
  )

  #mean
  variable.importance.mean <- data.frame(
    variable = variable.importance.df.wide$variable,
    importance = rowMeans(variable.importance.df.wide[, tidyselect::all_of(iteration.columns)]),
    standard_deviation = apply(variable.importance.df.wide[, tidyselect::all_of(iteration.columns)], 1, sd),
    row.names = NULL
  ) %>%
    dplyr::arrange(desc(importance)) %>%
    as.data.frame()

  variable.importance.df.long <- tidyr::pivot_longer(
    data = variable.importance.df.wide,
    cols = tidyselect::all_of(iteration.columns),
    names_to = "iteration",
    values_to = "importance"
  ) %>%
    as.data.frame()

  variable.importance.plot <- ggplot2::ggplot(data = variable.importance.df.long) +
    ggplot2::aes(y = reorder(
      variable,
      importance,
      FUN = max),
      x = importance
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::ylab("") +
    ggplot2::xlab("Importance score") +
    ggplot2::ggtitle(paste("Response variable: ", dependent.variable.name, sep = ""))

  m.curves$variable.importance <- list()
  m.curves$variable.importance$df <- variable.importance.mean
  m.curves$variable.importance$df.wide <- variable.importance.df.wide
  m.curves$variable.importance$df.long <- variable.importance.df.long
  m.curves$variable.importance$plot <- variable.importance.plot

  #gathering prediction.error
  m.curves$prediction.error <- unlist(
    lapply(
      repeated.models,
      "[[",
      "prediction.error"
    )
  )

  #gathering r.squared
  m.curves$r.squared <- unlist(
    lapply(
      repeated.models,
      "[[",
      "r.squared"
    )
  )

  #gathering pseudo R squared
  m.curves$pseudo.r.squared <- unlist(
    lapply(
      repeated.models,
      "[[",
      "pseudo.r.squared"
    )
  )

  #gathering rmse
  m.curves$rmse <- unlist(
    lapply(
      repeated.models,
      "[[",
      "rmse"
    )
  )

  #gathering nrmse
  m.curves$nrmse <- unlist(
    lapply(
      repeated.models,
      "[[",
      "nrmse"
    )
  )
  names(m.curves$nrmse) <- NULL

  #gathering spatial.correlation.residuals
  spatial.correlation.residuals.by.iteration <- do.call(
    "rbind",
    lapply(
      lapply(
        repeated.models,
        "[[",
        "spatial.correlation.residuals"
      ),
      "[[",
      1
    )
  ) %>%
    dplyr::arrange(distance.threshold)
  spatial.correlation.residuals.by.iteration$iteration <- rep(1:iterations, length(unique(spatial.correlation.residuals.by.iteration$distance.threshold)))

  spatial.correlation.residuals.mean <- spatial.correlation.residuals.by.iteration %>%
    dplyr::group_by(distance.threshold) %>%
    dplyr::summarise(
      moran.i = mean(moran.i),
      p.value = mean(moran.p),
      interpretation = statistical_mode(interpretation)
    ) %>%
    as.data.frame()

  m.curves$spatial.correlation.residuals <- list()
  m.curves$spatial.correlation.residuals$df <- spatial.correlation.residuals.mean
  m.curves$spatial.correlation.residuals$df.long <- spatial.correlation.residuals.by.iteration
  m.curves$spatial.correlation.residuals$plot <- ggplot2::ggplot(data = spatial.correlation.residuals.by.iteration) +
    ggplot2::aes(
      x = distance.threshold,
      y = moran.i,
      group = iteration
    ) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "red4") +
    ggplot2::xlab("Distance threshold") +
    ggplot2::ylab("Moran's I") +
    ggplot2::ggtitle("Multiscale Moran's I")

  m.curves$spatial.correlation.residuals$max.moran <-  mean(
    unlist(
      lapply(
        lapply(
          repeated.models,
          "[[",
          "spatial.correlation.residuals"
        ),
        "[[",
        3
      )
    )
  )

  m.curves$spatial.correlation.residuals$max.moran.distance.threshold <- statistical_mode(
    unlist(
      lapply(
        lapply(
          repeated.models,
          "[[",
          "spatial.correlation.residuals"
        ),
        "[[",
        4
      )
    )
  )

  #gathering residuals
  residuals <- as.data.frame(do.call("cbind",         lapply(
    repeated.models,
    "[[",
    "residuals"
  )))
  colnames(residuals) <- iteration.columns

  residuals.mean <- data.frame(
    residuals_mean = rowMeans(residuals),
    standard_deviation = apply(residuals, 1, sd),
    row.names = NULL
  )

  m.curves$residuals <- NULL
  m.curves$residuals$df <- residuals.mean
  m.curves$residuals$df.long <- residuals
  m.curves$residuals$stats <- summary(residuals.mean$residuals_mean)

  #gathering models
  if(keep.models == TRUE){
    m.curves$models <- repeated.models
  }

  #return m.curves
  m.curves


}

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
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  #computing distance step for spatial folds
  distance.step <- min(dist(xy)) / 2

  #loop to generate spatial folds
  ####################################
  spatial.folds <- foreach::foreach(
    i = 1:nrow(xy.reference.records)
  ) %dopar% {

    spatial.fold.i <- make_spatial_fold(
      xy.i = xy.reference.records[i, ],
      xy = xy,
      distance.step = distance.step,
      training.fraction = training.fraction
    )

    return(spatial.fold.i)

  }

  #setting importance = "none" in ranger.arguments
  ranger.arguments$importance <- "none"
  ranger.arguments$data <- NULL


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



#NEED TO ADAPT IT TO THE OUTPUT OF make_spatial_fold
#plots a spatial fold
# plot_spatial_fold <- function(
#   spatial.folds,
#   i,
#   legend.position
# ){
#
#   if(i > length(spatial.folds)){
#     i <- length(spatial.folds)
#   }
#
#   #getting the data
#   t <- spatial.folds[[i]]$training[, c("x", "y", "presence")]
#   p <- spatial.folds[[i]]$evaluation.presences[, c("x", "y", "presence")]
#   a <- spatial.folds[[i]]$evaluation.absences[, c("x", "y", "presence")]
#
#   #group name
#   t[t$presence == 1, "presence"] <- "Training presence"
#   t[t$presence == 0, "presence"] <- "Training background"
#   p$presence <- "Evaluation presence"
#   a$presence <- "Evaluation absence"
#
#   #together
#   plot.df <- rbind(p, a, t)
#
#   #ordering factors
#   plot.df$Group <- factor(
#     plot.df$presence,
#     levels = c(
#       "Evaluation presence",
#       "Evaluation absence",
#       "Training presence",
#       "Training background"
#     )
#   )
#
#   #plot
#   p <- ggplot2::ggplot(plot.df) +
#     ggplot2::aes(
#       x,
#       y,
#       color = Group,
#       size = Group,
#       alpha = Group) +
#     ggplot2::geom_point() +
#     ggplot2::scale_color_viridis_d() +
#     ggplot2::scale_size_manual(
#       values = c(1, 1, 1, 0.5)
#     ) +
#     ggplot2::scale_alpha_manual(
#       values = c(1, 1, 1, 0.1)
#     ) +
#     ggplot2::xlab("") +
#     ggplot2::ylab("") +
#     ggplot2::theme(
#       axis.text = element_blank(),
#       legend.position = legend.position) +
#     ggplot2::ggtitle(paste("Fold", i, sep = " "))
#
#   return(p)
#
# }



make_spatial_fold <- function(
  xy.i,
  xy,
  distance.step = distance.step,
  training.fraction = training.fraction
){

  #getting details of xy.i
  xy.i.id <- xy.i[1, "id"]
  xy.i.x <- xy.i[1, "x"]
  xy.i.y <- xy.i[1, "y"]

  #number of records to select
  records.to.select <- floor(training.fraction * nrow(xy))

  #generating first buffer
  old.buffer.x.min <- xy.i.x - distance.step
  old.buffer.x.max <- xy.i.x + distance.step
  old.buffer.y.min <- xy.i.y - distance.step
  old.buffer.y.max <- xy.i.y + distance.step

  #select first batch of presences
  records.selected <- xy[
    xy$x >= old.buffer.x.min &
      xy$x <= old.buffer.x.max &
      xy$y >= old.buffer.y.min &
      xy$y <= old.buffer.y.max, ]

  #growing buffer
  while(nrow(records.selected) < records.to.select){

    #new buffer
    new.buffer.x.min <- old.buffer.x.min - distance.step
    new.buffer.x.max <- old.buffer.x.max + distance.step
    new.buffer.y.min <- old.buffer.y.min - distance.step
    new.buffer.y.max <- old.buffer.y.max + distance.step

    #number of selected presences
    records.selected <- xy[
      xy$x >= new.buffer.x.min &
        xy$x <= new.buffer.x.max &
        xy$y >= new.buffer.y.min &
        xy$y <= new.buffer.y.max, ]

    #resetting old.buffer
    old.buffer.x.min <- new.buffer.x.min
    old.buffer.x.max <- new.buffer.x.max
    old.buffer.y.min <- new.buffer.y.min
    old.buffer.y.max <- new.buffer.y.max

  }

  #out list
  out.list <- list()
  out.list$training <- records.selected$id
  out.list$testing <- setdiff(xy$id, records.selected$id)

  out.list

}



sf_points_to_xy <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}




.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
objects_in_ram <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}


select_spatial_predictors_optimized <- function(
  spatial.predictors.df,
  spatial.predictors.rank,
  data,
  dependent.variable.name,
  predictor.variable.names,
  distance.matrix,
  distance.thresholds,
  ranger.arguments,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #initializing data for loop
  spatial.predictors.rank.i <- spatial.predictors.rank
  spatial.predictors.candidates.i <- spatial.predictors.rank$ranking

  #copy of data
  data.i <- data
  predictor.variable.names.i <- predictor.variable.names

  #vectors to build optimization.df
  optimization.index <- vector()
  optimization.spatial.predictors.name <- vector()
  optimization.moran.i <- vector()
  optimization.r.squared <- vector()
  optimization.sum <- vector()
  i <- 0

  #iterating
  while(length(spatial.predictors.candidates.i) > 0){

    i <- i + 1

    #subset spatial.predictors
    spatial.predictors.df.i <- spatial.predictors.df[, spatial.predictors.candidates.i]

    #getting name of last spatial predictor to fix an NA at the end of the loop
    last.spatial.predictors.name <- colnames(spatial.predictors.df.i)[ncol(spatial.predictors.df.i)]


    #add the first factor to data
    data.i <- data.frame(
      data.i,
      spatial.predictors.df[, spatial.predictors.candidates.i[1]]
    )
    colnames(data.i)[ncol(data.i)] <- spatial.predictors.candidates.i[1]


    #remove used column from spatial.predictors.df
    if(!is.null(ncol(spatial.predictors.df.i))){
      spatial.predictors.df.i <- spatial.predictors.df.i[, colnames(spatial.predictors.df.i) != spatial.predictors.candidates.i[1]]
    } else {
      break
    }

    #new predictor.variable.names
    predictor.variable.names.i <- c(
      predictor.variable.names.i,
      spatial.predictors.candidates.i[1]
    )

    #reference moran I
    reference.moran.i <- spatial.predictors.rank.i$ranking.criteria[spatial.predictors.rank.i$ranking.criteria$spatial.predictors.name == spatial.predictors.candidates.i[1], "model.moran.i"]

    #rank pca factors
    spatial.predictors.rank.i <- rank_spatial_predictors(
      spatial.predictors.df = spatial.predictors.df.i,
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      reference.moran.i = reference.moran.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port,
      multicollinearity.filter = "none"
    )

    #redo pca.factors.candidates.i
    spatial.predictors.candidates.i <- spatial.predictors.rank.i$ranking

    #fixing NA in last pca.factor.candidate
    if(is.na(spatial.predictors.candidates.i[1])){
      spatial.predictors.candidates.i[1] <- last.spatial.predictors.name
    }

    #gathering data for optimization df.
    optimization.index[i] <- i
    optimization.spatial.predictors.name[i] <- spatial.predictors.candidates.i[1]
    optimization.moran.i[i] <- spatial.predictors.rank.i$ranking.criteria[1, "model.moran.i"]
    optimization.r.squared[i] <- spatial.predictors.rank.i$ranking.criteria[1, "model.r.squared"]
    optimization.sum <- (1 - optimization.moran.i[i]) + optimization.r.squared[i]

  }#end of while loop

  optimization.df <- data.frame(
    spatial.predictors.index = optimization.index,
    spatial.predictors.name = optimization.spatial.predictors.name,
    moran.i = optimization.moran.i,
    r.squared = optimization.r.squared,
    sum = (1 - optimization.moran.i) + optimization.r.squared
  ) %>%
    dplyr::arrange(desc(sum))

  #get index pca factor with optimized r-squared and moran.i
  optimized.index <- optimization.df[1, "spatial.predictors.index"]

  #prepare vector with best factor names
  best.spatial.predictors <- optimization.df[optimization.df$spatial.predictors.index %in% 1:optimized.index, "spatial.predictors.name"]

  #output list
  out.list <- list()
  out.list$optimization <- optimization.df
  out.list$best.spatial.predictors <- best.spatial.predictors

  #plot
  x11()
  par(mfrow = c(2, 1))
  par(mar = c(1.5,4,2,2))
  plot(
    optimization.df$spatial.predictors.index,
    optimization.df$r.squared,
    xlab = "",
    ylab = "R-squared",
    xaxt = 'n'
  )
  par(mar = c(4,4,0,2))
  plot(
    optimization.df$spatial.predictors.index,
    optimization.df$moran.i,
    xlab = "PCA factors added",
    ylab = "Moran's I"
  )

  #return output
  out.list

}


select_spatial_predictors_sequential <- function(
  spatial.predictors.df,
  spatial.predictors.ranking,
  data,
  dependent.variable.name,
  predictor.variable.names,
  distance.matrix,
  distance.thresholds,
  ranger.arguments,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){

  #getting spatial.predictors.rank
  spatial.predictors.ranking <- spatial.predictors.ranking$ranking

  #importing functions from the global environment
  rf <- get("rf", envir = .GlobalEnv)
  root_mean_squared_error <- get("root_mean_squared_error", envir = .GlobalEnv)
  rescale_vector <- get("rescale_vector", envir = .GlobalEnv)
  multiscale_moran <- get("multiscale_moran", envir = .GlobalEnv)
  moran <- get("moran", envir = .GlobalEnv)

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
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  optimization.df <- foreach::foreach(
    spatial.predictors.i = 1:length(spatial.predictors.ranking),
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
      "scale_robust"
    )
  ) %dopar% {

    #pca factor names
    spatial.predictors.selected.names.i <- spatial.predictors.ranking[1:spatial.predictors.i]

    #add pca factor to training data
    data.i <- data.frame(
      data,
      spatial.predictors.df[, spatial.predictors.selected.names.i]
    )
    colnames(data.i)[(ncol(data)+1):ncol(data.i)] <- spatial.predictors.selected.names.i

    #new predictor.variable.names
    predictor.variable.names.i <- c(
      predictor.variable.names,
      spatial.predictors.selected.names.i
    )

    #fitting model i
    m.i <- rf(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      seed = spatial.predictors.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      trees.per.variable = trees.per.variable,
      ranger.arguments = ranger.arguments
    )

    #output.df
    out.df <- data.frame(
      spatial.predictor.index = spatial.predictors.i,
      moran.i = m.i$spatial.correlation.residuals$max.moran,
      r.squared = m.i$r.squared,
      sum = (1 - m.i$spatial.correlation.residuals$max.moran) + m.i$r.squared
    )

    return(out.df)

  }#end of parallelized loop

  #arranging optimization df
  optimization.df <- dplyr::arrange(
    optimization.df,
    desc(sum)
  )

  #get index of spatial predictor with optimized r-squared and moran.i
  optimized.index <- optimization.df[1, "spatial.predictor.index"]

  #prepare vector with best factor names
  best.spatial.predictors <- spatial.predictors.ranking[1:optimized.index]

  #output list
  out.list <- list()
  out.list$optimization <- optimization.df
  out.list$best.spatial.predictors <- best.spatial.predictors

  #plot
  x11()
  par(mfrow = c(2, 1))
  par(mar = c(1.5,4,2,2))
  plot(
    optimization.df$spatial.predictor.index,
    optimization.df$r.squared,
    xlab = "",
    ylab = "R-squared",
    xaxt = 'n'
  )
  par(mar = c(4,4,0,2))
  plot(
    optimization.df$spatial.predictor.index,
    optimization.df$moran.i,
    xlab = "Spatial predictors added",
    ylab = "Moran's I"
  )

  #return output
  out.list

}


# slope <- function(x, y){
#
#     mx  = mean(x)
#     my  = mean(y)
#     sxx = sum((x - mx)*(x - mx))
#     sxy = sum((x - mx)*(y - my))
#     syy = sum((y - my)*(y - my))
#     slope  = round(sxy/sxx, 3)
#
#     slope
#
# }


rank_spatial_predictors <- function(
  ranking.method = c("moran.i.reduction", "mem"),
  spatial.predictors.df,
  data,
  dependent.variable.name,
  predictor.variable.names,
  reference.moran.i,
  distance.matrix,
  distance.thresholds,
  ranger.arguments,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000,
  multicollinearity.filter = c("vif", "cor", "none")
){

  #testing method argument
  ranking.method <- match.arg(ranking.method)
  multicollinearity.filter <- match.arg(multicollinearity.filter)

  #importing functions from the global environment
  rf <- get("rf", envir = .GlobalEnv)
  root_mean_squared_error <- get("root_mean_squared_error", envir = .GlobalEnv)
  rescale_vector <- get("rescale_vector", envir = .GlobalEnv)
  multiscale_moran <- get("multiscale_moran", envir = .GlobalEnv)
  moran <- get("moran", envir = .GlobalEnv)
  auto_vif <- get("auto_vif", envir = .GlobalEnv)
  .select_by_max_vif <- get(".select_by_max_vif", envir = .GlobalEnv)
  .select_by_preference <- get(".select_by_preference", envir = .GlobalEnv)

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
  doParallel::registerDoParallel(cl = temp.cluster)
  on.exit(parallel::stopCluster(cl = temp.cluster))

  #3.2.3 PREPARING PARALLELIZED LOOP TO ITERATE THROUGH distance.matrix.pca
  spatial.predictors.order <- foreach::foreach(
    spatial.predictors.i = 1:ncol(spatial.predictors.df),
    .combine = "rbind",
    .packages = c(
      "ranger",
      "magrittr"
      ),
    .export = c(
      "root_mean_squared_error",
      "rescale_vector",
      "multiscale_moran",
      "auto_vif",
      ".select_by_max_vif",
      ".select_by_preference",
      "scale_robust"
      )
  ) %dopar% {

    #3.2.3.1 preparing data

    #spatial predictor name
    spatial.predictors.name.i <- colnames(spatial.predictors.df)[spatial.predictors.i]

    #computing reduction in Moran's I
    if(ranking.method == "moran.i.reduction"){

    #training data
    data.i <- data.frame(
      data,
      spatial.predictors.df[, spatial.predictors.i]
    )
    colnames(data.i)[ncol(data.i)] <- spatial.predictors.name.i

    #new predictor.variable.names
    predictor.variable.names.i <- c(predictor.variable.names, spatial.predictors.name.i)

    #fitting model I
    m.i <- rf(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      seed = spatial.predictors.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      scaled.importance = FALSE,
      ranger.arguments = ranger.arguments
    )

    #out.df
    out.i <- data.frame(
      spatial.predictors.name = spatial.predictors.name.i,
      model.r.squared = m.i$r.squared,
      model.moran.i = m.i$spatial.correlation.residuals$max.moran,
      ranking.criteria = reference.moran.i - m.i$spatial.correlation.residuals$max.moran,
      interpretation = m.i$spatial.correlation.residuals$df[which.max(m.i$spatial.correlation.residuals$df$moran.i), "interpretation"]
    )

    }

    #computing Moran's I of the spatial predictors
    if(ranking.method == "mem"){

      #moran's I of spatial predictor
      m.i <- moran(
        x = spatial.predictors.df[, spatial.predictors.i],
        distance.matrix = distance.matrix,
        distance.threshold = distance.thresholds[1]
      )

      #out.df
      out.i <- data.frame(
        spatial.predictors.name = spatial.predictors.name.i,
        ranking.criteria = m.i$moran.i,
        interpretation = m.i$interpretation
      )

    }

    #returning output
    return(out.i)

  } #end of parallelized loop

  #order dataframe
  spatial.predictors.order <- spatial.predictors.order %>%
    dplyr::arrange(dplyr::desc(ranking.criteria))

  #selected spatial.predictorss
  spatial.predictors.order.selected <- dplyr::filter(
      spatial.predictors.order,
      ranking.criteria > 0.01
      )

  #apply vif filtering if requested
  if(multicollinearity.filter == "vif"){

    multicollinearity.df <- auto_vif(
      x = spatial.predictors.df[, spatial.predictors.order.selected$spatial.predictors.name],
      preference.order = spatial.predictors.order.selected$spatial.predictors.name,
      verbose = FALSE
    )

  }

  if(multicollinearity.filter == "cor"){

    multicollinearity.df <- auto_cor(
      x = spatial.predictors.df[, spatial.predictors.order.selected$spatial.predictors.name],
      preference.order = spatial.predictors.order.selected$spatial.predictors.name,
      cor.threshold = 0.75
    )

  }

    #subset spatial.predictors.order
  if(multicollinearity.filter != "none"){
    spatial.predictors.order.selected <- spatial.predictors.order.selected[spatial.predictors.order.selected$spatial.predictors.name %in% multicollinearity.df$selected.variables, ]
  }

  #return output
  out.list <- list()
  out.list$ranking.criteria <- spatial.predictors.order
  out.list$ranking <- spatial.predictors.order.selected$spatial.predictors.name

  #returning output list
  out.list

}



pca_distance_matrix <- function(
  distance.matrix = NULL,
  distance.threshold = NULL
  ){

  #if distance.threshold is null, 0
  if(is.null(distance.threshold)){
    distance.threshold <- 0
  }

  #list to store pca factors
  pca.factors.list <- list()

  #iterating through distance thresholds
  for(distance.threshold.i in distance.threshold){

    #copy distance matrix
    distance.matrix.i <- distance.matrix

    #applying threshold to distance matrix
    distance.matrix.i[distance.matrix.i <= distance.threshold.i] <- distance.threshold.i

    #computing pca factors
    pca.factors.list[[as.character(distance.threshold.i)]] <- pca(
      distance.matrix = distance.matrix.i,
      colnames.prefix = paste0(
        "spatial_predictor_",
        distance.threshold.i
        ),
      plot = FALSE
    )

  }

  #removing names
  names(pca.factors.list) <- NULL

  #to data frame
  pca.factors <- do.call("cbind", pca.factors.list)

  #returning output
  pca.factors

}






pca <- function(
  distance.matrix = NULL,
  colnames.prefix = "pca_factor",
  plot = TRUE
  ){

  #removing columns with zero variance
  distance.matrix <- distance.matrix[ , which(apply(distance.matrix, 2, var) != 0)]

  #computing pca of distance matrix
  distance.matrix.pca <- prcomp(distance.matrix, scale. = TRUE)

  if(plot == TRUE){
    p <- factoextra::fviz_eig(distance.matrix.pca)
    print(p)
  }

  #getting pca factors
  distance.matrix.pca.factors <- as.data.frame(distance.matrix.pca$x)
  colnames(distance.matrix.pca.factors) <- paste(colnames.prefix, 1:ncol(distance.matrix.pca.factors), sep = "_")

  #returning output
  distance.matrix.pca.factors
}

repeat_rf <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  iterations = 10,
  white.noise = FALSE,
  autocorrelated.noise = FALSE,
  trees.per.variable = 100,
  ranger.arguments = list(
    formula = NULL,
    mtry = NULL,
    importance = "permutation",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    max.depth = NULL,
    replace = TRUE,
    case.weights = NULL,
    class.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = TRUE,
    local.importance = FALSE,
    regularization.factor = 1,
    regularization.usedepth = FALSE,
    keep.inbag = FALSE,
    inbag = NULL,
    holdout = FALSE,
    quantreg = FALSE,
    oob.error = TRUE,
    num.threads = parallel::detectCores() - 1,
    save.memory = FALSE,
    verbose = TRUE,
    classification = NULL,
    x = NULL,
    y = NULL,
    sample.fraction = 1
  )
){

  #initializes local.importance
  if(is.null(ranger.arguments$local.importance)){
    local.importance <- FALSE
  } else {
    local.importance <- ranger.arguments$local.importance
  }

  #lists to store results of the iterations
  predictions <- list()
  if(local.importance == TRUE){variable.importance.local <- list()}
  variable.importance <- list()
  prediction.error <- list()
  r.squared <- list()
  pseudo.r.squared <- list()
  rmse <- list()
  nrmse <- list()
  residuals <- list()
  spatial.correlation.residuals <- list()

  #applying robust scaling to the data
  data.scaled <- scale_robust(
    data = data[, c(
      dependent.variable.name,
      predictor.variable.names
    )]
  )

  #if scaling fails, use regular scaling
  if(sum(is.nan(data.scaled[, 1])) > 0 | sum(is.infinite(data.scaled[, 1])) > 0){
    data.scaled <- as.data.frame(scale(data))
  }

  #iterations
  for(i in 1:iterations){

    #setting seed
    set.seed(i)

    #fitting model


    #gathering results
    predictions[[i]] <- m.i$predictions
    if(local.importance == TRUE){
      variable.importance.local[[i]] <- m.i.scaled$variable.importance.local
      }
    variable.importance[[i]] <- m.i.scaled$variable.importance$vector
    prediction.error[[i]] <- m.i$prediction.error
    r.squared[[i]] <- m.i$r.squared
    pseudo.r.squared[[i]] <- m.i$pseudo.r.squared
    rmse[[i]] <- m.i$rmse
    nrmse[[i]] <- m.i$nrmse
    residuals[[i]] <- m.i$residuals
    spatial.correlation.residuals[[i]] <- m.i$spatial.correlation.residuals

  }#end of iterations

  #fitting complete model to allow plotting partial dependence curves
  m.curves <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    seed = i,
    trees.per.variable = trees.per.variable,
    ranger.arguments = ranger.arguments
  )

  #names of iterations columns
  iteration.columns <- paste("iteration", 1:iterations, sep = "_")

  #gathering predictions
  predictions.by.iteration <- as.data.frame(do.call("cbind", predictions))
  colnames(predictions.by.iteration) <- iteration.columns
  predictions.mean <- data.frame(
    prediction_mean = rowMeans(predictions.by.iteration),
    standard_deviation = apply(predictions.by.iteration, 1, sd)
  )
  m.curves$predictions <- NULL #to avoid warning
  m.curves$predictions$df.wide <- predictions.by.iteration
  m.curves$predictions$df <- predictions.mean

  #gathering variable.importance.local
  if(local.importance == TRUE){
    m.curves$variable.importance.local <- apply(simplify2array(variable.importance.local), 1:2, mean)
  }

  #gathering variable.importance
  m.curves$variable.importance <- NULL

  #wide format
  variable.importance.df.wide <- as.data.frame(do.call("cbind", variable.importance))
  colnames(variable.importance.df.wide) <- iteration.columns
  variable.importance.df.wide <- data.frame(
    variable = rownames(variable.importance.df.wide),
    variable.importance.df.wide,
    row.names = NULL
  )

  #mean
  variable.importance.mean <- data.frame(
    variable = variable.importance.df.wide$variable,
    importance = rowMeans(variable.importance.df.wide[, tidyselect::all_of(iteration.columns)]),
    standard_deviation = apply(variable.importance.df.wide[, tidyselect::all_of(iteration.columns)], 1, sd),
    row.names = NULL
  ) %>%
    dplyr::arrange(desc(importance)) %>%
    as.data.frame()

  variable.importance.df.long <- tidyr::pivot_longer(
    data = variable.importance.df.wide,
    cols = tidyselect::all_of(iteration.columns),
    names_to = "iteration",
    values_to = "importance"
  ) %>%
    as.data.frame()

  variable.importance.plot <- ggplot2::ggplot(data = variable.importance.df.long) +
    ggplot2::aes(y = reorder(
      variable,
      importance,
      FUN = max),
      x = importance
      ) +
    ggplot2::geom_boxplot() +
    ggplot2::ylab("") +
    ggplot2::xlab("Importance score") +
    ggplot2::ggtitle(paste("Response variable: ", dependent.variable.name, sep = ""))

  m.curves$variable.importance <- list()
  m.curves$variable.importance$df <- variable.importance.mean
  m.curves$variable.importance$df.wide <- variable.importance.df.wide
  m.curves$variable.importance$df.long <- variable.importance.df.long
  m.curves$variable.importance$plot <- variable.importance.plot


  #gathering prediction.error
  m.curves$prediction.error <- unlist(prediction.error)

  #gathering r.squared
  m.curves$r.squared <- unlist(r.squared)

  #gathering pseudo R squared
  m.curves$pseudo.r.squared <- unlist(pseudo.r.squared)

  #gathering rmse
  m.curves$rmse <- unlist(rmse)

  #gathering nrmse
  m.curves$nrmse <- unlist(nrmse)
  names(m.curves$nrmse) <- NULL

  #gathering spatial.correlation.residuals
  spatial.correlation.residuals.by.iteration <- do.call("rbind", lapply(spatial.correlation.residuals, "[[", 1)) %>%
    dplyr::arrange(distance.threshold)
  spatial.correlation.residuals.by.iteration$iteration <- rep(1:iterations, length(unique(spatial.correlation.residuals.by.iteration$distance.threshold)))

  spatial.correlation.residuals.mean <- spatial.correlation.residuals.by.iteration %>%
    dplyr::group_by(distance.threshold) %>%
    dplyr::summarise(
      moran.i = mean(moran.i),
      p.value = mean(moran.p),
      interpretation = statistical_mode(interpretation)
    ) %>%
    as.data.frame()

  m.curves$spatial.correlation.residuals <- list()
  m.curves$spatial.correlation.residuals$df <- spatial.correlation.residuals.mean
  m.curves$spatial.correlation.residuals$df.long <- spatial.correlation.residuals.by.iteration
  m.curves$spatial.correlation.residuals$plot <- ggplot2::ggplot(data = spatial.correlation.residuals.by.iteration) +
    ggplot2::aes(
      x = distance.threshold,
      y = moran.i,
      group = iteration
    ) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "red4") +
    ggplot2::xlab("Distance threshold") +
    ggplot2::ylab("Moran's I") +
    ggplot2::ggtitle("Multiscale Moran's I")

  m.curves$spatial.correlation.residuals$max.moran <-  mean(unlist(lapply(spatial.correlation.residuals, "[[", 3)))

  m.curves$spatial.correlation.residuals$max.moran.distance.threshold <- statistical_mode(unlist(lapply(spatial.correlation.residuals, "[[", 4)))

  #gathering residuals
  residuals <- as.data.frame(do.call("cbind", residuals))
  colnames(residuals) <- iteration.columns

  residuals.mean <- data.frame(
    residuals_mean = rowMeans(residuals),
    standard_deviation = apply(residuals, 1, sd),
    row.names = NULL
  )

  m.curves$residuals <- NULL
  m.curves$residuals$df <- residuals.mean
  m.curves$residuals$df.long <- residuals
  m.curves$residuals$stats <- summary(residuals.mean$mean)

  #returning results list
  return(m.curves)
}




#function to rescale vectors between given bounds
rescale_vector <- function(x = rnorm(100),
                          new.min = 0,
                          new.max = 100,
                          integer = FALSE){


  #data extremes
  old.min = min(x)
  old.max = max(x)


  #SCALING VECTOR
  #----------------------

  x = ((x - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min


  #FORCES VECTOR INTO INTEGER
  #----------------------

  if(integer == TRUE){
    x = floor(x)
  }

  return(x)

}





#rf model with:
#pseudo R-squared (cor(observations, predictions))
#dataframe with variable importance
#Moran's I of the residuals
rf <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  seed = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  trees.per.variable = NULL,
  scaled.importance = TRUE,
  ranger.arguments = list(
    formula = NULL,
    mtry = NULL,
    importance = "permutation",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    max.depth = NULL,
    replace = TRUE,
    case.weights = NULL,
    class.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = TRUE,
    local.importance = FALSE,
    regularization.factor = 1,
    regularization.usedepth = FALSE,
    keep.inbag = FALSE,
    inbag = NULL,
    holdout = FALSE,
    quantreg = FALSE,
    oob.error = TRUE,
    num.threads = parallel::detectCores() - 1,
    save.memory = FALSE,
    verbose = TRUE,
    seed = NULL,
    classification = NULL,
    x = NULL,
    y = NULL,
    sample.fraction = 1
  )
){

  #subsetting data
  if(!is.null(data)){

    if(!is.null(predictor.variable.names) & !is.null(dependent.variable.name)){

      predictor.variable.names <- predictor.variable.names[predictor.variable.names %in% colnames(data)]
      data <- data[, c(dependent.variable.name, predictor.variable.names)]

    }

    #removing NA
    data <- na.omit(data)

  }

  #default model arguments
  default.ranger.arguments <- list(
    formula = NULL,
    num.trees = 500,
    trees.per.variable = NULL,
    mtry = NULL,
    mtry = NULL,
    importance = "permutation",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    max.depth = NULL,
    replace = TRUE,
    case.weights = NULL,
    class.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = TRUE,
    local.importance = FALSE,
    regularization.factor = 1,
    regularization.usedepth = FALSE,
    keep.inbag = FALSE,
    inbag = NULL,
    holdout = FALSE,
    quantreg = FALSE,
    oob.error = TRUE,
    num.threads = parallel::detectCores() - 1,
    save.memory = FALSE,
    verbose = TRUE,
    seed = NULL,
    classification = NULL,
    x = NULL,
    y = NULL,
    sample.fraction = 1
  )
  list2env(default.ranger.arguments, envir=environment())

  #user arguments
  list2env(ranger.arguments, envir=environment())

  #setting up seed if available
  if(!is.null(seed)){
    set.seed(seed)
  }

  if(!is.null(trees.per.variable)){
    num.trees <- trees.per.variable * (ncol(data) - 1)
  }

  #ranger model for r-squared and predictions
  m <- ranger::ranger(
    data = data,
    dependent.variable.name = dependent.variable.name,
    num.trees = num.trees,
    mtry = mtry,
    importance = importance,
    write.forest = write.forest,
    probability = probability,
    min.node.size = min.node.size,
    max.depth = max.depth,
    replace = replace,
    sample.fraction = sample.fraction,
    case.weights = case.weights,
    class.weights = class.weights,
    splitrule = splitrule,
    num.random.splits = num.random.splits,
    alpha = alpha,
    minprop = minprop,
    split.select.weights = split.select.weights,
    always.split.variables = always.split.variables,
    respect.unordered.factors = respect.unordered.factors,
    scale.permutation.importance = scale.permutation.importance,
    local.importance = local.importance,
    regularization.factor = regularization.factor,
    regularization.usedepth = regularization.usedepth,
    keep.inbag = keep.inbag,
    inbag = inbag,
    holdout = holdout,
    quantreg = quantreg,
    oob.error = oob.error,
    num.threads = num.threads,
    save.memory = save.memory,
    verbose = verbose,
    seed = seed,
    classification = classification,
    x = x,
    y = y
  )

  #if scaled.importance is TRUE
  if(scaled.importance == TRUE){

    #applying robust scaling to the data
    data.scaled <- scale_robust(
      data = data
    )

    #if scaling fails, use regular scaling
    if(sum(is.nan(data.scaled[, 1])) > 0 | sum(is.infinite(data.scaled[, 1])) > 0){
      data.scaled <- as.data.frame(scale(data))
    }

    #ranger model for variable importance
    m.scaled <- ranger::ranger(
      data = data.scaled,
      dependent.variable.name = dependent.variable.name,
      num.trees = num.trees,
      mtry = mtry,
      importance = importance,
      write.forest = write.forest,
      probability = probability,
      min.node.size = min.node.size,
      max.depth = max.depth,
      replace = replace,
      sample.fraction = sample.fraction,
      case.weights = case.weights,
      class.weights = class.weights,
      splitrule = splitrule,
      num.random.splits = num.random.splits,
      alpha = alpha,
      minprop = minprop,
      split.select.weights = split.select.weights,
      always.split.variables = always.split.variables,
      respect.unordered.factors = respect.unordered.factors,
      scale.permutation.importance = FALSE,
      local.importance = local.importance,
      regularization.factor = regularization.factor,
      regularization.usedepth = regularization.usedepth,
      keep.inbag = keep.inbag,
      inbag = inbag,
      holdout = holdout,
      quantreg = quantreg,
      oob.error = oob.error,
      num.threads = num.threads,
      save.memory = save.memory,
      verbose = verbose,
      seed = seed,
      classification = classification,
      x = x,
      y = y
    )

  } else {

    m.scaled <- m

  }

  #adding model arguments
  m$ranger.arguments <- list(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    num.trees = num.trees,
    mtry = mtry,
    importance = importance,
    write.forest = write.forest,
    probability = probability,
    min.node.size = min.node.size,
    max.depth = max.depth,
    replace = replace,
    sample.fraction = sample.fraction,
    case.weights = case.weights,
    class.weights = class.weights,
    splitrule = splitrule,
    num.random.splits = num.random.splits,
    alpha = alpha,
    minprop = minprop,
    split.select.weights = split.select.weights,
    always.split.variables = always.split.variables,
    respect.unordered.factors = respect.unordered.factors,
    scale.permutation.importance = scale.permutation.importance,
    local.importance = local.importance,
    regularization.factor = regularization.factor,
    regularization.usedepth = regularization.usedepth,
    keep.inbag = keep.inbag,
    inbag = inbag,
    holdout = holdout,
    quantreg = quantreg,
    oob.error = oob.error,
    num.threads = num.threads,
    save.memory = save.memory,
    verbose = verbose,
    seed = seed,
    classification = classification
  )

  #importance dataframe
  importance.vector <- m.scaled$variable.importance
  m$variable.importance <- list()
  m$variable.importance$vector <- importance.vector
  m$variable.importance$df <- data.frame(
    variable = names(m.scaled$variable.importance),
    importance = m.scaled$variable.importance
  ) %>%
    tibble::remove_rownames() %>%
    dplyr::arrange(desc(importance))

  m$variable.importance$plot <- ggplot2::ggplot(data = m$variable.importance$df) +
    ggplot2::aes(
      x = importance,
      y = reorder(
        variable,
        importance,
        FUN = max
      )
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::ylab("")

  #getting residuals

  #predicted data
  predicted <- m$predictions

  #getting observed data

  #if data is provided
  if(!is.null(data)){

    #the user used a formula
    if(!is.null(formula)){

      #observed
      observed <- data[, all.vars(formula)[1]]

    }

    #user gave dependent.variable.name
    if(!is.null(dependent.variable.name)){

      #observed
      observed <- data[, dependent.variable.name]

    }

  }

  if(!is.null(y) & !is.null(x)){

    observed = y

  }

  m$pseudo.r.squared <- cor(
    observed,
    predicted
  )

  m$rmse <- root_mean_squared_error(
    o = observed,
    p = predicted,
    type = NULL
  )

  m$nrmse <- root_mean_squared_error(
    o = observed,
    p = predicted,
    type = "iq"
  )

  m$residuals <- observed - predicted


  #compute moran I of residuals if distance.matrix is provided
  if(!is.null(distance.matrix)){

    m$spatial.correlation.residuals <- moran_multithreshold(
      x = m$residuals,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      plot = FALSE
      )

  }

  #replacing variable importance with the scaled one
  m$variable.importance.local <- m.scaled$variable.importance.local

  return(m)

}


#computes vif of a dataframe
vif <- function(x){
  out <- x %>%
    na.omit() %>%
    as.matrix() %>%
    cor() %>%
    solve() %>%
    diag() %>%
    sort(decreasing = TRUE) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variable")
  colnames(out)[2] <- "vif"
  return(out)
}

#' Correlation dendrogram to help reduce multicollinearity in a training dataset.
#'
#' @description Computes the correlation between all pairs of variables in a training dataset and computes a cluster through the expression \code{hclust(as.dist(abs(1 - correlation.matrix)))}. If a \code{\link{s_biserial_cor}} output is provided, the clustering is computed as \code{hclust(as.dist(abs(1 - correlation.matrix)), method = "single")}, and the algorithm selects variables automatically based on the R-squared value obtained by each variable in the biserial correlation analysis.
#'
#' @usage cor_dendrogram(
#'   x,
#'   select.cols = NULL,
#'   omit.cols = c("x", "y", "presence"),
#'   max.cor = 0.75,
#'   biserial.cor = NULL,
#'   plot = TRUE,
#'   text.size = 6
#'   )
#'
#'
#' @param x A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param max.cor Numeric in the interval [0, 1], maximum Pearson correlation of the selected variables. Defaults to 0.75.
#' @param biserial.cor List, output of the function \code{\link{s_biserial_cor}}. Its R-squared scores are used to select variables.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}.
#' @param text.size Numeric, size of the dendrogram labels.
#'
#' @return If \code{biserial.cor} is not NULL, a list with two slots named "dendrogram" (a ggplot2 object) and "selected.variables" with the dendrogram and the character vector with the selected variables. Otherwise it only returns the dendrogram, and the users have to select the variables by themselves.
#'
#' @examples
#' \dontrun{
#'data("virtualSpeciesPB")
#'
#'biserial.cor <- s_biserial_cor(
#'  x = virtualSpeciesPB,
#'  omit.cols = c("x", "y")
#')
#'
#'selected.vars <- cor_dendrogram(
#'  x = virtualSpeciesPB,
#'  select.cols = NULL,
#'  omit.cols = c("x", "y", "presence"),
#'  max.cor = 0.75,
#'  biserial.cor = biserial.cor
#')$selected.variables
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
cor_dendrogram <- function(
  x,
  select.cols = NULL,
  omit.cols = c("x", "y", "presence"),
  max.cor = 0.75,
  biserial.cor = NULL,
  plot = TRUE,
  text.size = 4
){

  #preparing output list
  output.list <- list()

  #dropping omit.cols
  if(sum(omit.cols %in% colnames(x)) == length(omit.cols)){
    x <-
      x %>%
      dplyr::select(-tidyselect::all_of(omit.cols))
  }

  #selecting select.cols
  if(is.null(select.cols) == FALSE){
    if(sum(select.cols %in% colnames(x)) == length(select.cols)){
      x <-
        x %>%
        dplyr::select(tidyselect::all_of(select.cols))
    }
  }

  #getting numeric columns only and removing cases with NA
  x <-
    x[, unlist(lapply(x, is.numeric))] %>%
    na.omit()

  #computes correlation matrix
  cor.matrix <-
    x %>%
    cor() %>%
    as.dist() %>%
    abs()

  #if biserial.cor == NULL
  #-------------------------------------
  if(is.null(biserial.cor) == TRUE | inherits(biserial.cor, "s_biserial_cor") == FALSE){

    #cluster (converts correlation to distance)
    temp.cluster <- hclust(1 - cor.matrix)

    #generates cluster data
    temp.cluster.data <- ggdendro::dendro_data(temp.cluster)

    #plots cluster
    cluster.plot <- ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = ggdendro::segment(temp.cluster.data),
        aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend)
      ) +
      ggplot2::geom_text(
        data = ggdendro::label(temp.cluster.data),
        aes(
          label = label,
          x = x,
          y = 0,
          hjust = 1
        ),
        size = text.size
      ) +
      ggplot2::coord_flip(ylim = c(-0.4, 1)) +
      viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
      ggplot2::theme(
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1,2,1,2), "lines"),
        axis.text.x = element_text(size = text.size * 2),
        legend.position = "bottom",
        legend.key.width = unit(2, "lines")
      ) +
      ggplot2::labs(colour = "R2") +
      ggplot2::geom_hline(
        yintercept = 1 - max.cor,
        col = "red4",
        linetype = "dashed",
        size = 1,
        alpha = 0.5
      ) +
      ggplot2::scale_y_continuous(breaks = c(1 - max.cor, 0, 0.25, 0.5, 0.75, 1)) +
      ggplot2::ylab("1 - correlation")

    if(plot == TRUE){
      ggplot2::theme_set(cowplot::theme_cowplot())
      print(cluster.plot)
    }

    #prepare output
    selected.variables <- colnames(x)

  } else {

    #cluster (converts correlation to distance)
    temp.cluster <- hclust(1 - cor.matrix, method = "single")

    #gets range of heights of the cluster
    height.range <- round(range(temp.cluster$height), 2)

    #gets change step
    height.step <- (max(height.range) - min(height.range))/200

    #initial value for observed.max.cor
    observed.max.cor <- 1

    #iterator counter
    i <- 0

    #iterations to find right height
    while(observed.max.cor > max.cor){

      #plus one iteration
      i <- i + 1

      #computes height cutoff
      height.cutoff <- min(height.range) + (height.step * i)

      #table of groups
      temp.cluster.groups <- data.frame(group = cutree(
        temp.cluster,
        h = height.cutoff
      ))
      temp.cluster.groups$variable <- row.names(temp.cluster.groups)
      temp.cluster.groups <- temp.cluster.groups[
        order(
          temp.cluster.groups$group,
          decreasing = FALSE
        ), ]
      row.names(temp.cluster.groups) <- 1:nrow(temp.cluster.groups)

      #adds biserial correlation to cluster labels
      temp.cluster.groups$R2 <- biserial.cor$df[
        match(
          temp.cluster.groups$variable,     #cluster labels
          biserial.cor$df$variable #variables in biserial correlation output
        ), "R2"
      ]

      #gets the maximum of each group
      selected.variables <-
        temp.cluster.groups %>%
        dplyr::group_by(group) %>%
        dplyr::slice(which.max(R2)) %>%
        .$variable

      #computes observed max cor
      observed.max.cor <-
        x[, selected.variables] %>%
        cor() %>%
        as.dist() %>%
        as.vector() %>%
        abs() %>%
        max()

      observed.max.cor

    }#end of while


    #prepares cluster plotting
    temp.cluster.data <- ggdendro::dendro_data(temp.cluster)

    #gets R2
    temp.cluster.data$labels$R2 <- biserial.cor$df[
      match(
        temp.cluster.data$labels$label, #cluster labels
        biserial.cor$df$variable        #variables biserial.cor
      ), "R2"
    ]

    #gets labels
    labs <- ggdendro::label(temp.cluster.data)

    #adds arrow to label if the variable is selected
    labs$label <- as.character(labs$label)
    for(i in 1:nrow(labs)){
      if(labs[i, "label"] %in% selected.variables){
        labs[i, "label"] <- paste("\u{2192} ", labs[i, "label"], sep = "")
      }
    }
    labs$label <- factor(labs$label)


    #plots dendrogram
    cluster.plot <- ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = ggdendro::segment(temp.cluster.data),
        aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend)
      ) +
      ggplot2::geom_text(
        data = ggdendro::label(temp.cluster.data),
        aes(
          label = labs$label,
          x = x,
          y = 0,
          colour = labs$R2,
          hjust = 1
        ),
        size = text.size
      ) +
      ggplot2::coord_flip(ylim = c(-(max(height.range)/2), max(height.range))) +
      viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
      ggplot2::theme(
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        axis.text.x = element_text(size = text.size * 2),
        legend.position = "bottom",
        legend.key.width = unit(2, "lines")
      ) +
      ggplot2::labs(colour = "Biserial correlation") +
      ggplot2::geom_hline(
        yintercept = height.cutoff,
        col = "red4",
        linetype = "dashed",
        size = 1,
        alpha = 0.5
      ) +
      ggplot2::scale_y_continuous(breaks = c(1 - (max(height.range) / 2), 0, 0.25, 0.5, 0.75, 1)) +
      ggplot2::ylab("Correlation difference")

    if(plot == TRUE){
      ggplot2::theme_set(cowplot::theme_cowplot())
      print(cluster.plot)
    }

  }

  #preparing output
  output.list$plot <- cluster.plot
  output.list$selected.variables <- selected.variables
  return(output.list)

}


#plot map and histogram of a variable in ecoregions
plot_variable_distribution <- function(
  ecoregions_polygons,
  ecoregions,
  variable = bias_records_per_km2,
  title = "Records per km2",
  viridis.direction = 1,
  binwidth = 1){

  pa <- ggplot2::ggplot(data = ecoregions_polygons) +
    ggplot2::geom_sf(aes_string(fill = variable), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = viridis.direction) +
    ggplot2::theme(legend.position = "right", legend.key.height = unit(0.5, "cm")) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(fill = "")

  pb <- ggplot2::ggplot(data = ecoregions) +
    ggplot2::geom_histogram(aes_string(
      x = variable,
      fill = factor(ecoregions[, variable])
    ), binwidth = binwidth
    ) +
    ggplot2::scale_fill_viridis_d(direction = viridis.direction) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab("") +
    ggplot2::xlab("")

  p <- pa | pb

  return(p)

}

nball_volume <- function(n, r)
{
  return(pi^(n/2) * r^n / gamma(n/2+1))
}

kdtree_build_intl <- function(d, nr, nc) {
  .Call('_hypervolume_kdtree_build_intl', PACKAGE = 'hypervolume', d, nr, nc)
}

kdtree_ball_query_multiple <- function(tr, ptlist, nr, nc, r, verb) {
  .Call('_hypervolume_kdtree_ball_query_multiple', PACKAGE = 'hypervolume', tr, ptlist, nr, nc, r, verb)
}

kdtree_ball_query_id_multiple <- function(tr, ptlist, nr, nc, r, verb) {
  .Call('_hypervolume_kdtree_ball_query_id_multiple', PACKAGE = 'hypervolume', tr, ptlist, nr, nc, r, verb)
}

kdtree_range_query_multiple <- function(tr, pminlist, pmaxlist, nr, nc, verb) {
  .Call('_hypervolume_kdtree_range_query_multiple', PACKAGE = 'hypervolume', tr, pminlist, pmaxlist, nr, nc, verb)
}

fastPdist2 <- function(Ar, Br) {
  .Call('_hypervolume_fastPdist2', PACKAGE = 'hypervolume', Ar, Br)
}

kdtree_build <- function(data, verbose=TRUE)
{
  if (any(class(data) == "data.frame"))
  {
    data <- as.matrix(data)
  }
  if (any(class(data) == "matrix"))
  {
    if (verbose==TRUE)
    {
      cat("\nBuilding tree... \n")
    }

    kdt <- kdtree_build_intl(t(data),nrow(data),ncol(data))

    if (verbose==TRUE)
    {
      cat("done.\n")
    }

    return(kdt)
  }
  else
  {
    stop("Input data not a matrix or data frame")
  }
}

sample_ellipsoid = function(center, n, scales) {
  k = length(center)
  points = matrix(rnorm(n * k), nrow = n)   # Start with noise
  points = points / sqrt(rowSums(points^2)) # Project to sphere surface
  radii = runif(n, 0)^(1/k) # Each point has a radius, prior to scaling
  for (i in 1:k) {
    points[,i] = points[,i] * radii * scales[i] + center[i]
  }
  return(points)
}

ellipsoid_volume = function(scales) {
  nball_volume(n = length(scales), r = 1) * prod(scales)
}

ellipsoid_inverse_weight = function(samples, centers, scales, verbose) {
  # Returns the number of center-based ellipsoids that contain each sampled point

  # scale to unit ball
  for (i in 1:ncol(centers)) {
    samples[ , i] = samples[ , i] / scales[i]
    centers[ , i] = centers[ , i] / scales[i]
  }

  # Count the overlap with unit ball
  tree <- kdtree_build(centers,verbose=verbose)
  query <- kdtree_ball_query_multiple(tree, t(samples),
                                      nrow(samples), ncol(samples),
                                      r = 1, verb = verbose)
  rm(tree)

  return(query)
}


sample_model_ellipsoid <- function(predict_function=NULL, data, scales, min.value, samples.per.point, chunk.size=1e3, verbose=TRUE, return.full=FALSE)
{
  # Use only complete cases
  data = na.omit(as.matrix(data))

  # determine dimensionality
  d = ncol(data)

  N.samples <- ceiling(samples.per.point * nrow(data))

  if (is.null(dimnames(data)[[2]]))
  {
    dimnames(data) <- list(NULL,paste("X",1:d,sep=""))
  }

  if (verbose==TRUE)
  {
    pb <- progress_bar$new(total = N.samples)
    pb$tick(0)
  }

  samples = list()
  volume_sampling_extent_all = list()
  total_accepted <- 0
  total_tried <- 0

  while(total_accepted < N.samples)
  {
    if (verbose==TRUE)
    {
      if (!pb$finished==TRUE)
      {
        pb$update(total_accepted/N.samples)
      }
    }

    ## STEP ONE: Collect samples from ellipsoids around each data point
    full_samples = lapply(1:nrow(data),
                          function(i)
                          {
                            se = sample_ellipsoid(data[i, ],
                                                  chunk.size,
                                                  scales = scales)
                            return(data.frame(se))
                          })
    full_samples = as.matrix(rbindlist(full_samples))



    # Discard samples from regions that were over-sampled.
    iweight = ellipsoid_inverse_weight(full_samples, centers = data,
                                       scales = scales, verbose = verbose)
    # scale weights so that the largest value is 1
    weight = 1/(iweight / min(iweight))
    # This is the average weight of the samples
    mean_weight = sum(1/iweight) / nrow(full_samples)
    # Total volume is the volume of one ellipse, times the number of ellipse,
    # times the proportion of samples that can be retained, times the fraction of points included above threshold
    volume_sampling_extent = ellipsoid_volume(scales) * nrow(data) * mean_weight

    # resample sampled points down to uniform density
    included = as.logical(rbinom(length(iweight), size = 1, prob = weight))

    # now we have a uniform grid of points around each data point, samples_retained
    samples_retained = full_samples[included, , drop = FALSE]

    ### STEP TWO: estimate function
    # predict function value at each point
    predicted_values <- predict_function(samples_retained)

    included_thresholded = ( as.numeric(predicted_values) > min.value )

    samples_retained_thresholded = samples_retained[included_thresholded, , drop=FALSE]
    predicted_values_thresholded = predicted_values[included_thresholded]

    samples_final_this = cbind(samples_retained_thresholded, predicted_values_thresholded)
    dimnames(samples_final_this) <- list(NULL, c(dimnames(data)[[2]],"value"))

    # count progress
    total_tried <- total_tried + nrow(samples_retained) # for eventual volume counting
    total_accepted <- total_accepted + nrow(samples_retained_thresholded)

    # store loop output
    samples <- c(samples, list(data.frame(samples_final_this))) # must be df format for rbindlist
    volume_sampling_extent_all <- c(volume_sampling_extent_all, volume_sampling_extent)
  }

  # concatenate results
  samples <- as.matrix(rbindlist(samples))
  samples <- samples[sample(1:nrow(samples),N.samples),,drop=FALSE]
  # calculate volumes
  volume_sampling_extent_all_mean = mean(unlist(volume_sampling_extent_all),na.rm=T)
  volume = volume_sampling_extent_all_mean * total_accepted / total_tried

  if (verbose==TRUE)
  {
    pb$terminate()
  }

  if (return.full==TRUE)
  {
    return(list(samples = samples, full_samples=full_samples, volume=volume))
  }
  else
  {
    return(list(samples = samples, volume=volume))
  }
}

hypervolume_svm_ <- function(data, name=NULL, samples.per.point=ceiling((10^(3+sqrt(ncol(data))))/nrow(data)), svm.nu=0.01, svm.gamma=0.5, scale.factor=1, chunk.size=1e3, verbose=TRUE)
{

  require(hypervolume)

  data <- as.matrix(data)
  d <- ncol(data)
  if (is.null(dimnames(data)[[2]]))
  {
    dimnames(data) <- list(NULL,paste("X",1:d,sep=""))
  }

  if (svm.nu <= 0)
  {
    stop("Parameter svm.nu must be >0.")
  }

  if (svm.gamma <= 0)
  {
    stop("Parameter svm.gamma must be >0.")
  }

  if (verbose == TRUE)
  {
    cat('Building support vector machine model...')
  }
  svm_this <- e1071::svm(data,
                         y=NULL,
                         type='one-classification',
                         nu= svm.nu,
                         gamma= svm.gamma,
                         scale=FALSE,
                         kernel="radial")
  if (verbose == TRUE)
  {
    cat(' done\n')
  }

  # Assuming all of the support vectors were right on top of each other,
  # how far could we move away from that point before we left the interior
  # of the hypervolume?
  # `solve b * exp(-g d_2) == r for d_2` into Wolfram Alpha, where
  #    * "b" is the sum of the SVM coefficients
  #    * "g" is the SVM kernel bandwidth (gamma)
  #    * "r" is the minimum kernel density of the hypervolume (rho)
  #    * "d_2" is squared distance
  squared_scaled_dist = log(sum(svm_this$coefs) / svm_this$rho) / svm.gamma
  scales = scale.factor * apply(data, 2, sd) * sqrt(squared_scaled_dist)

  predict_function_svm <- function(x)
  {
    return(predict(svm_this, x))
  }

  # do elliptical sampling over the hyperbox
  samples_all = sample_model_ellipsoid(
    predict_function = predict_function_svm,
    data = data[svm_this$index,,drop=FALSE],
    scales = scales,
    min.value = 0, # because output is binary
    samples.per.point = samples.per.point,
    chunk.size=chunk.size,
    verbose=verbose)

  random_points = samples_all$samples[,1:d]
  values_accepted <- samples_all$samples[,d+1]
  volume <- samples_all$volume
  point_density = nrow(random_points) / volume

  hv_svm <- new("Hypervolume",
                Data=data,
                Method = 'One-class support vector machine',
                RandomPoints= random_points,
                PointDensity= point_density,
                Volume= volume,
                Dimensionality=d,
                ValueAtRandomPoints=values_accepted,
                Name=ifelse(is.null(name), "untitled", toString(name)),
                Parameters = list(svm.nu=svm.nu, svm.gamma=svm.gamma, samples.per.point=samples.per.point))

  return(hv_svm)
}


#computes betadiversity scores for two ecoregions
betadiversity <- function(
  ecoregion.1,
  ecoregion.2,
  taxa.list,
  taxa.column
){

  #getting species lists
  eco1 <- unique(taxa.list[taxa.list$ecoregion %in% ecoregion.1, taxa.column])
  eco2 <- unique(taxa.list[taxa.list$ecoregion %in% ecoregion.2, taxa.column])

  #extracting betadiversity components
  a <- length(intersect(eco1, eco2))
  b <- length(setdiff(eco1, eco2))
  c <- length(setdiff(eco2, eco1))
  abc <- (a + b + c)

  #betadiversity components as percentage of the species pool
  a. <- (a * 100) / abc
  b. <- (b * 100) / abc
  c. <- (c * 100) / abc

  #richness similarity
  R <- abs((a + b) - (a + c))
  R. <- abs((a. + b.) - (a. + c.))

  #composition similarity
  # C <- (b + c) / a

  #Sorensen similarity index
  #there are different equations out there for this index
  #Koleff: 2 * a / 2 * a + b + c
  #Baselga: b + c / 2 * a + b + c
  #here I substract it to 1 to convert it's values into dissimilarity, as in Benito et al. 2011.
  Bsor <- 1 - (2 * a / (2 * a + b + c))

  #Simpson's similarity index
  Bsim <- min(b, c) / (min(b, c) + a)

  #saving output
  out.list <- list()

  out.list$a <- a
  out.list$b <- b
  out.list$c <- c
  out.list$a_percent <- a.
  out.list$b_percent <- b.
  out.list$c_percent <- c.
  out.list$R <- R
  out.list$R_percent <- R.
  # out.list$C <- C
  out.list$Bsor <- Bsor
  out.list$Bsim <- Bsim

  #as named vector
  out.list <- unlist(
    out.list,
    use.names = TRUE
    )

  return(out.list)

}

#returns ecoregion geom from database connection
pull_ecoregion_geom <- function(ecoregion, plot = TRUE){

  #finding a postgresql connection in the environment
  connection.name <- Filter(
    function(x)
      'PostgreSQLConnection' %in% class(get(x) ),
    ls(envir=.GlobalEnv)
    )[1]

  #if connection not found
  if(is.na(connection.name)){
    stop("PostgreSQL connection not found")
  }

  #pulling geometry
  geom <- rpostgis::pgGetGeom(
    conn = get(connection.name),
    name = c("public", "ecoregions"),
    geom = "geom",
    other.cols = c("ecoregion"),
    clauses = paste(
      "WHERE ecoregion = '",
      ecoregion,
      "';",
      sep = ""
    )
  )

  #plotting geom
  if(plot == TRUE){
  sp::plot(geom, main = ecoregion)
  }

  return(geom)

}

#extracting climate values for a given ecoregion
ecoregion_raster_extract <- function(
  ecoregion,
  raster
  ){

  #pulling ecoregion geom
  ecoregion.geom <- suppressWarnings(
      suppressMessages(
        pull_ecoregion_geom(
        ecoregion,
        plot = FALSE
        )
      )
    )

  #raster extractioin
  out.df <- raster::extract(
    x = raster,
    y = ecoregion.geom,
    df = TRUE,
    cellnumbers = FALSE
  ) %>%
    na.omit() %>%
    dplyr::select(-ID) %>%
    dplyr::mutate(
      ecoregion = ecoregion
    )

  return(out.df)

}

#compute hypervolume of an ecoregion
ecoregion_hypervolume <- function(
  ecoregion,
  raster
){

  #pulling ecoregion geom and extracting raster values
  ecoregion.data <- ecoregion_raster_extract(
    ecoregion,
    raster
  )

  #sampling the data if nrow is too high
  if(nrow(ecoregion.data) > 10000){
    ecoregion.data <- ecoregion.data[sample(
      1:nrow(ecoregion.data),
      size = round(nrow(ecoregion.data)/4, 0)
      ), ]
  }

  #computing hypervolume
  hv <- hypervolume_svm_(
    data = ecoregion.data[, names(raster)],
    name = ecoregion,
    samples.per.point = 10,
    verbose = FALSE,
    chunk.size = 20000
  )

  return(hv)

}

#function to compute ecoregion fragmentation
ecoregion_fragmentation <- function(
  ecoregion
){

  #pulling geometry of ecoregion and reprojecting to LAEA
  ecoregion.geom <- suppressWarnings(
    suppressMessages(
      pull_ecoregion_geom(
        ecoregion = ecoregion,
        plot = FALSE
        )
      )
    ) %>%
    sp::spTransform(CRSobj = "+proj=laea")

  #creating raster template at 5km resolution
  raster.template <- raster::raster(
    x = ecoregion.geom,
    resolution = 2000
  )

  #rasterizing geom
  ecoregion.geom.raster <- raster::rasterize(
    x = ecoregion.geom,
    y = raster.template,
    background = 0,
    silent = TRUE
    )


  #computing fragmentation measures
  ecoregion.fragmentation <- landscapemetrics::calculate_lsm(
    landscape = ecoregion.geom.raster,
    what = c(
      "lsm_c_ca", #total (class) area
      "lsm_c_te", #total edge
      "lsm_c_ed", #edge density
      "lsm_c_ai", #aggregation index
      "lsm_c_cohesion", #patch cohesion index
      "lsm_c_division", #division index
      "lsm_c_clumpy", #clumpiness index
      "lsm_c_lsi", #landscape shape index
      "lsm_c_nlsi", #normalized landscape shape index
      "lsm_c_mesh", #effective mesh size
      "lsm_c_tca", #total core area
      "lsm_c_cpland", #core area percentage of landscape
      "lsm_c_core_mn", #core area mean
      "lsm_c_dcore_mn", #mean disjunct core area
      "lsm_c_ndca", #number of disjunct core areas
      "lsm_c_dcad", #disjunct core area density
      "lsm_c_np", #number of patches
      "lsm_c_pd", #patch density
      "lsm_c_area_mn", #mean patch area
      "lsm_c_contig_mn", #contiguity index mean
      "lsm_c_para_mn", #perimeter area ratio mean
      "lsm_c_shape_mn" #shape index mean
    )
  ) %>%
    dplyr::filter(
      class == 1
    ) %>%
    dplyr::select(
      metric,
      value
    ) %>%
    as.data.frame()

  return(ecoregion.fragmentation)

}


