rf_spatial <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  weight.r.squared = NULL,
  weight.penalization.n.predictors = NULL,
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
  seed = NULL,
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
      ranger.arguments = ranger.arguments,
      seed = seed
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
  m.non.spatial.moran.i <- m.non.spatial$spatial.correlation.residuals$df %>%
    dplyr::arrange(dplyr::desc(moran.i)) %>%
    dplyr::filter(interpretation == "Positive spatial correlation")

  #if residuals are not autocorrelated, return original model
  if(nrow(m.non.spatial.moran.i) == 0){

    if(verbose == TRUE){
      message("Residuals are not spatially correlated, this model is good to go!")
      suppressWarnings(print(m.non.spatial$spatial.correlation.residuals$plot))
    }

    return(m.non.spatial)

  }# END OF FITTING NON-SPATIAL MODEL


  #GENERATING SPATIAL PREDICTORS
  #########################################################

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
    "pca.moran.sequential",
    "pca.effect.sequential",
    "pca.effect.optimized"
  )
  ){

    #computing pca factors for pca methods
    spatial.predictors.df <- pca_multithreshold(
      x = distance.matrix,
      distance.thresholds =  m.non.spatial.moran.i$distance.threshold,
      max.spatial.predictors = max.spatial.predictors
    )
    if(verbose == TRUE){
      message("Using PCA factors of the distance matrix as spatial predictors.")
    }

  }

  #MEM
  if(method %in% c(
    "mem.moran.sequential",
    "mem.effect.sequential",
    "mem.effect.optimized"
    )
  ){


    #computing mem
    #computing pca factors for pca methods
    spatial.predictors.df <- mem_multithreshold(
      x = distance.matrix,
      distance.thresholds =  m.non.spatial.moran.i$distance.threshold,
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
      message("Ranking spatial predictors by how much they reduce the Moran's I of the model residuals.")
    }
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
    spatial.predictors.ranking <- rank_spatial_predictors(
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
      cluster.port = cluster.port
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

    #setting weights
    if(is.null(weight.r.squared)){weight.r.squared <- 0.75}
    if(is.null(weight.penalization.n.predictors)){weight.penalization.n.predictors <- 0.25}
    #setting specific weights
    if(method %in% c("pca.moran.sequential", "pca.effect.sequential")){
      weight.penalization.n.predictors <- 0.1
      weight.r.squared <- 0.5
    }

    spatial.predictors.selection <- select_spatial_predictors_sequential(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      spatial.predictors.df = spatial.predictors.df,
      spatial.predictors.ranking = spatial.predictors.ranking,
      weight.r.squared = weight.r.squared,
      weight.penalization.n.predictors = weight.penalization.n.predictors,
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
      message("Selecting spatial predictors by optimizing their joint effect on the Moran's I of the model's residuals")
    }

    #setting weights
    if(is.null(weight.r.squared)){weight.r.squared <- 0.25}
    if(is.null(weight.penalization.n.predictors)){weight.penalization.n.predictors <- 0}

    spatial.predictors.selection <- select_spatial_predictors_optimized(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      spatial.predictors.df = spatial.predictors.df,
      spatial.predictors.ranking = spatial.predictors.ranking,
      weight.r.squared = weight.r.squared,
      weight.penalization.n.predictors = weight.penalization.n.predictors,
      n.cores = n.cores,
      cluster.ips = NULL,
      cluster.cores = NULL,
      cluster.user = NULL,
      cluster.port = NULL
    )

    #broadcast spatial.predictors.selected
    spatial.predictors.selected <- spatial.predictors.selection$best.spatial.predictors

  }

  #preparing plot of selection criteria
  optimization <- NULL
  r.squared <- NULL
  spatial.predictor.index <- NULL
  plot.df <- spatial.predictors.selection$optimization
  selection.criteria.plot <- ggplot2::ggplot(data = plot.df) +
    ggplot2::aes(
      x = moran.i,
      y = r.squared,
      color = optimization,
      size = spatial.predictor.index
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_color_viridis_c(direction = -1) +
    ggplot2::geom_point(
      data = plot.df[plot.df$selected, ],
      aes(
        x = moran.i,
        y = r.squared),
      colour="black",
      size = 5,
      shape = 1,
      alpha = 0.3
    ) +
    ggplot2::geom_path(data = plot.df[plot.df$selected, ],
                       aes(
                         x = moran.i,
                         y = r.squared
                       ),
                       size = 0.5,
                       color = "black",
                       alpha = 0.3
    ) +
    ggplot2::labs(
      size = "Added spatial predictors",
      color = "Weighted optimization index"
    ) +
    ggplot2::xlab("Moran's I of the model residuals") +
    ggplot2::ylab("Model's R-squared") +
    ggplot2::ggtitle("Selection of spatial predictors (selection path shown in gray)")

  if(verbose == TRUE){
    suppressWarnings(print(selection.criteria.plot))
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
      ranger.arguments = ranger.arguments,
      seed = seed
    )

    #preparing before and after plot.df
    after.df <- m.spatial$spatial.correlation.residuals$df
    after.df$model <- "Spatial"
    before.df <- m.non.spatial$spatial.correlation.residuals$df
    before.df$model <- "Non-spatial"
    plot.df <- rbind(before.df, after.df)
    m.spatial$spatial.correlation.residuals$df <- plot.df
    plot.df$p.value.binary <- "< 0.05"
    plot.df[plot.df$p.value >= 0.05, "p.value.binary"] <- ">= 0.05"

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
    plot.df$p.value.binary <- "< 0.05"
    plot.df[plot.df$p.value >= 0.05, "p.value.binary"] <- ">= 0.05"

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
  p.value.binary <- NULL
  m.spatial$spatial.correlation.residuals$plot <- ggplot2::ggplot(data = plot.df) +
    ggplot2::aes(
      x = distance.threshold,
      y = moran.i,
      color = model,
      size = p.value.binary
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      col = "gray10",
      size = 0.7,
      linetype = "dashed"
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_colour_manual(values = c("#440154FF", "#35B779FF")) +
    ggplot2::xlab("Distance thresholds") +
    ggplot2::ylab("Moran's I of residuals") +
    ggplot2::ggtitle("Residuals' Moran's I of the spatial and non-spatial models") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(color = "Model", size = "Moran's I p-value")

  #printing Moran's I of residuals
  if(verbose == TRUE){
    suppressWarnings(print(m.spatial$spatial.correlation.residuals$plot))
  }

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
  m.spatial$selection.spatial.predictors <- list()
  m.spatial$selection.spatial.predictors$method <- method
  m.spatial$selection.spatial.predictors$names <- spatial.predictors.selected
  if(exists("spatial.predictors.selection")){
    m.spatial$selection.spatial.predictors$df <- spatial.predictors.selection$optimization
    m.spatial$selection.spatial.predictors$plot <- selection.criteria.plot
  }

  #return output
  m.spatial

}
