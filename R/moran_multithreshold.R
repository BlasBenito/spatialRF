#' @title moran_multithreshold
#' @description Applies [moran] to different distance thresholds at the same time, and plots Moran's I value for each distance threshold.
#' @param x numeric vector, generally a model residuals, Default: NULL
#' @param distance.matrix distance matrix among the records represented in the numeric vector. The number of rows of this matrix must be equal to the length of x. Default: NULL
#' @param distance.thresholds numeric vector, distances below each value in the distance matrix are set to 0 for the computation of Moran's I. If NULL, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: NULL
#' @param verbose boolean, plots Moran's I values for each distance threshold if TRUE, Default: TRUE
#' @return a named list with the slots:
#'  \describe{
#'  \item{df}{dataframe with the results of [moran] per distance threshold.}
#'  \item{plot}{a plot of Moran's I across distance thresholds.}
#'  \item{max.moran}{maximum value of Moran's I across thresholds.}
#'  \item{max.moran.distance.threshold}{distance threshold with the maximum Moran's I value.}
#'  }
#' @details The idea behind this function comes from the uncertainty about what "neighborhood" means in ecological systems (1000km in geological time mean little, but 100m might be quite a long distance for a tree to disperse seeds over), and allows to explore spatial autocorrelation of model residuals for several minimum-distance criteria at once.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data(distance_matrix)
#'  data(plant_richness)
#'  moran.out <- moran_multithreshold(
#'    x = plant_richness$richness_species_vascular,
#'    distance.matrix = distance_matrix,
#'    distance.thresholds = c(0, 100, 1000, 10000),
#'    plot = TRUE
#'    )
#'  moran.out
#'  }
#' }
#' @rdname moran_multithreshold
#' @export
moran_multithreshold <- function(
  x = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  verbose = TRUE
){

  #declaring variables to avoid check complaints
  distance.threshold <- NULL
  moran.i <- NULL

  #check x and distance matrix
  if(is.null(x) | !is.vector(x)){
    stop("x must be a numeric vector.")
  }
  if(nrow(distance.matrix) != length(x)){
    stop("length(x) and nrow(distance.matrix) must be equal.")
  }
  if(!is.null(distance.thresholds) & !is.vector(distance.thresholds)){
    stop("distance.thresholds must be a numeric vector.")
  }

  #creating distance thresholds
  if(is.null(distance.thresholds) == TRUE){
    distance.thresholds <- floor(
      seq(
        0,
        max(distance.matrix
        ),
        length.out = 4
      )
    )
  }

  #create output dataframe
  out.df <- data.frame(
    distance.threshold = distance.thresholds,
    moran.i = NA,
    p.value = NA,
    interpretation = NA
  )

  #iterating over out.df
  for(i in 1:nrow(out.df)){

    #compute Moran's I
    moran.out <- moran(
      x = x,
      distance.matrix = distance.matrix,
      distance.threshold = out.df[i, "distance.threshold"]
    )

    out.df[i, "moran.i"] <- moran.out["moran.i"]
    out.df[i, "p.value"] <- moran.out["p.value"]
    out.df[i, "interpretation"] <- moran.out["interpretation"]

  }

  #getting scale of max moran
  distance.threshold.max.moran <- out.df[which.max(out.df$moran.i), "distance.threshold"]


  #preparing output list
  out.list <- list()
  out.list$per.distance <- out.df
  out.list$plot <- plot_moran(x = out.df, verbose = verbose)
  out.list$max.moran <- max(out.df$moran.i)
  out.list$max.moran.distance.threshold <- distance.threshold.max.moran

  out.list

}
