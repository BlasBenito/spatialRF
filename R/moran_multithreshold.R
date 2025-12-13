#' @title Moran's I test on a numeric vector for different neighborhoods
#' @description Applies [moran()] to different distance thresholds at the same time.
#' @param x Numeric vector, generally model residuals, Default: `NULL`
#' @param distance.matrix Distance matrix among cases in `x`. The number of rows of this matrix must be equal to the length of `x`. Default: `NULL`
#' @param distance.thresholds Numeric vector, distances below each value are set to 0 on separated copies of the distance matrix for the computation of Moran's I at different neighborhood distances. If `NULL`, it defaults to `seq(0, max(distance.matrix)/4, length.out = 2)`. Default: `NULL`
#' @param verbose Logical, if `TRUE`, plots Moran's I values for each distance threshold. Default: `TRUE`
#' @return A named list with the slots:
#'  \itemize{
#'  \item `df`: Data frame with the results of [moran] per distance threshold.
#'  \item `plot`: A plot of Moran's I across distance thresholds.
#'  \item `max.moran`: Maximum value of Moran's I across thresholds.
#'  \item `max.moran.distance.threshold`: Distance threshold with the maximum Moran's I value.
#'  }
#' @details Using different distance thresholds helps to take into account the uncertainty about what "neighborhood" means in ecological systems (1000km in geological time means little, but 100m might be quite a long distance for a tree to disperse seeds over), and allows to explore spatial autocorrelation of model residuals for several minimum-distance criteria at once.
#' @seealso [moran()]
#' @examples
#'
#'  #loading example data
#'  data(plants_distance)
#'  data(plant_richness)
#'
#'  #computing Moran's I for the response variable at several reference distances
#'  out <- moran_multithreshold(
#'    x = plant_richness$richness_species_vascular,
#'    distance.matrix = plants_distance,
#'    distance.thresholds = c(0, 100, 1000, 10000),
#'    plot = TRUE
#'    )
#'  out
#'
#' @rdname moran_multithreshold
#' @export
moran_multithreshold <- function(
  x = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  verbose = TRUE
) {
  #check x and distance matrix
  if (is.null(x) | !is.vector(x)) {
    stop("Argument 'x' must be a numeric vector.")
  }
  if (is.null(distance.matrix)) {
    stop("Argument 'distance.matrix' is missing.`")
  }
  #creating distance thresholds
  if (is.null(distance.thresholds)) {
    distance.thresholds <- default_distance_thresholds(
      distance.matrix = distance.matrix
    )
  }

  #create output dataframe
  out.df <- data.frame(
    distance.threshold = distance.thresholds,
    moran.i = NA,
    moran.i.null = NA,
    p.value = NA,
    interpretation = NA
  )

  #iterating over out.df
  for (i in seq(1, nrow(out.df))) {
    #compute Moran's I
    moran.out <- moran(
      x = x,
      distance.matrix = distance.matrix,
      distance.threshold = out.df[i, "distance.threshold"],
      verbose = FALSE
    )

    out.df[i, "moran.i"] <- moran.out$test$moran.i
    out.df[i, "moran.i.null"] <- moran.out$test$moran.i.null
    out.df[i, "p.value"] <- moran.out$test$p.value
    out.df[i, "interpretation"] <- moran.out$test$interpretation
  }

  #getting scale of max moran
  distance.threshold.max.moran <- out.df[
    which.max(out.df$moran.i),
    "distance.threshold"
  ]

  #preparing output list
  out.list <- list()
  out.list$per.distance <- out.df
  out.list$max.moran <- max(out.df$moran.i)
  out.list$max.moran.distance.threshold <- distance.threshold.max.moran
  out.list$plot <- plot_moran(
    model = out.df,
    verbose = verbose
  )

  out.list
}
