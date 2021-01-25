rf_spatial <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  method = c(
    "hengl", #all distance matrix columns as predictors
    "hengl.moran.sequential", #distance matrix columns added in the order of their Moran's I
    "hengl.effect.sequential", #distance matrix columns added in order of their effect.
    "hengl.effect.optimized", #distance matrix columns added maximizing their joint effect.
    "pca.moran.sequential", #pca factors added in order of their Moran's I.
    "pca.effect.sequential", #pca factors added in order of effect
    "pca.effect.optimized", #pca factors added maximizing their joint effect.
    "mem.moran.sequential", #mem ordered by their Moran's I.
    "mem.effect.sequential", #mem added in order of effect.
    "mem.effect.optimized" #mem added maximizing their joint effect.
  ),
  max.spatial.predictors = 1000,
  repetitions = 1,
  verbose = TRUE,
  trees.per.variable = NULL,
  n.cores = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
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
  if(repetitions == 1){

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
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      trees.per.variable = trees.per.variable,
      ranger.arguments = ranger.arguments,
      repetitions = repetitions,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
    )

  }

  #extracting autocorrelation of the residuals
  interpretation <- NULL
  m.spatial.correlation.residuals <- m.non.spatial$spatial.correlation.residuals$df %>%
    dplyr::arrange(dplyr::desc(moran.i)) %>%
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
    "hengl.moran.sequential",
    "hengl.effect.sequential",
    "hengl.effect.optimized"
  )
  ){

    #change name of distance matrix
    spatial.predictors.df <- distance.matrix
    if(verbose == TRUE){
      message("Using the distance matrix columns as spatial predictors.")
    }
    colnames(spatial.predictors.df) <- rownames(spatial.predictors.df) <- paste0("spatial_predictor_", 1:ncol(distance.matrix))

  }

  #PCA
  if(method %in% c(
    "pca.moran",
    "pca.sequential",
    "pca.optimized"
  )
  ){

    #computing pca factors for pca methods
    spatial.predictors.df <- pca_multithreshold(
      x = distance.matrix,
      distance.thresholds =  m.spatial.correlation.residuals$distance.threshold,
      max.spatial.predictors = max.spatial.predictors
    )
    if(verbose == TRUE){
      message("Using PCA factors of the distance matrix as spatial predictors.")
    }

  }

  #MEM
  if(method %in% c(
    "mem.moran",
    "mem.sequential",
    "mem.optimized"
    )
  ){


    #computing mem
    #computing pca factors for pca methods
    spatial.predictors.df <- mem_multithreshold(
      x = distance.matrix,
      distance.thresholds =  m.spatial.correlation.residuals$distance.threshold,
      max.spatial.predictors = max.spatial.predictors
    )
    if(verbose == TRUE){
      message("Using Moran's Eigenvector Maps of the double-centered distance weights as spatial predictors.")
    }

  }

  #selecting all column names of the distance matrix by default
  spatial.predictors.selected <- colnames(spatial.predictors.df)


  #RANKING SPATIAL PREDICTORS
  ###########################################################

  #SELECTING RANKING METHOD
  if(method %in% "hengl"){
    ranking.method <- NULL
  }
  if(method %in% c(
    "hengl.moran.sequential",
    "pca.moran.sequential",
    "mem.moran.sequential"
    )
    ){
    ranking.method <- "moran.i"
    if(verbose == TRUE){
      message("Ranking spatial predictors by their Moran's I.")
    }
  }
  if(method %in% c(
    "hengl.effect.sequential",
    "hengl.effect.optimized",
    "pca.effect.sequential",
    "pca.effect.optimized",
    "mem.effect.sequential",
    "mem.effect.optimized"
    )
    ){
    ranking.method <- "moran.i.reduction"
    if(verbose == TRUE){
      message("Ranking spatial predictors by how they reduce the Moran's I of the model residuals.")
    }
  }

  #setting multicollinearity filter
  # if(method %in% c("hengl.sequential", "hengl.optimized")){
    multicollinearity.filter <- "none"
  # } else {
  #   multicollinearity.filter <- "none"
  # }

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
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      spatial.predictors.df = spatial.predictors.df,
      ranking.method = ranking.method,
      reference.moran.i = m.non.spatial$spatial.correlation.residuals$max.moran,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port,
      multicollinearity.filter = multicollinearity.filter
    )

  }

  #SEQUENTIAL SELECTION OF SPATIAL PREDICTORS
  if(method %in% c(
    "hengl.moran.sequential",
    "mem.moran.sequential",
    "pca.moran.sequential",
    "hengl.effect.sequential",
    "mem.effect.sequential",
    "pca.effect.sequential"
    )
    ){

    if(verbose == TRUE){
      message("Selecting spatial predictors in the order of the ranking (sequentially)")
    }

    spatial.predictors.selection <- select_spatial_predictors_sequential(
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
    )

    #broadcast spatial.predictors.selected
    spatial.predictors.selected <- spatial.predictors.selection$best.spatial.predictors

  }


  #OPTIMIZED SELECTION OF SPATIAL PREDICTORS
  if(method %in% c(
    "hengl.effect.optimized",
    "pca.effect.optimized",
    "mem.effect.optimized"
    )
    ){

    if(verbose == TRUE){
      message("Selecting spatial predictors by optimizing their joint effect on the model")
    }

    spatial.predictors.selection <- select_spatial_predictors_optimized(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      spatial.predictors.df = spatial.predictors.df,
      spatial.predictors.ranking = ranking.spatial.predictors,
      n.cores = n.cores,
      cluster.ips = NULL,
      cluster.cores = NULL,
      cluster.user = NULL,
      cluster.port = NULL
    )

    #broadcast spatial.predictors.selected
    spatial.predictors.selected <- spatial.predictors.selection$best.spatial.predictors

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
  if(repetitions == 1){

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
      variable <- NULL
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
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments,
      repetitions = repetitions,
      trees.per.variable = trees.per.variable,
      n.cores = n.cores,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port
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
  model <- NULL
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
    nrmse = round(c(m.non.spatial$nrmse, m.spatial$nrmse), 3),
    moran.i.residuals = round(c(
      m.non.spatial$spatial.correlation.residuals$max.moran,
      m.spatial$spatial.correlation.residuals$max.moran
  ), 3)
  )
  rownames(comparison.df) <- c("Non-spatial model", "Spatial model")

  if(verbose == TRUE){
   message("Model comparison")
   print(comparison.df)
  }

  #adding data to the model
  m.spatial$performance.comparison <- comparison.df

  #adding spatial method and predictors to the model
  m.spatial$spatial.predictors <- list()
  m.spatial$spatial.predictors$method <- method
  m.spatial$spatial.predictors$predictors.names <- spatial.predictors.selected
  if(exists("spatial.predictors.selection")){
    m.spatial$spatial.predictors$selection.criteria <- spatial.predictors.selection$optimization
  }

  #return output
  m.spatial

}
