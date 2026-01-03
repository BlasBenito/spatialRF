#' @title Finds optimal combinations of spatial predictors
#' @description Selects spatial predictors following these steps:
#' \enumerate{
#'   \item Gets the spatial predictors ranked by [rank_spatial_predictors()] and fits a model of the form `y ~ predictors + best_spatial_predictor_1`. The Moran's I of the residuals of this model is used as reference value for the next step.
#'   \item The remaining spatial predictors are introduced again into [rank_spatial_predictors()], and the spatial predictor with the highest ranking is introduced in a new model of the form `y ~  predictors + best_spatial_predictor_1 + best_spatial_predictor_2`.
#'   \item Steps 1 and 2 are repeated until the Moran's I doesn't improve for a number of repetitions equal to the 20 percent of the total number of spatial predictors introduced in the function.
#' }
#' This method allows to select the smallest set of spatial predictors that have the largest joint effect in reducing the spatial correlation of the model residuals, while maintaining the model's R-squared as high as possible. As a consequence of running [rank_spatial_predictors()] on each iteration, this method includes less spatial predictors in the final model than the sequential method implemented in [select_spatial_predictors_sequential()] would do, while minimizing spatial correlation and maximizing the R squared of the model as much as possible.
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param distance.matrix Squared matrix with the distances among the records in `data`. The number of rows of `distance.matrix` and `data` must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: `NULL`
#' @param distance.thresholds Numeric vector with neighborhood distances. All distances in the distance matrix below each value in `dustance.thresholds` are set to 0 for the computation of Moran's I. If `NULL`, it defaults to seq(0, max(distance.matrix), length.out = 4). Default: `NULL`
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param spatial.predictors.df Data frame of spatial predictors.
#' @param spatial.predictors.ranking Ranking of predictors returned by [rank_spatial_predictors()].
#' @param weight.r.squared Numeric between 0 and 1, weight of R-squared in the optimization index. Default: `0.25`
#' @param weight.penalization.n.predictors Numeric between 0 and 1, weight of the penalization for the number of spatial predictors added in the optimization index. Default: `0`
#' @param n.cores Integer. Number of threads for ranger's internal parallelization. Default: `NULL` (auto-detected: if user has set a parallel plan via `future::plan()`, defaults to 1; otherwise defaults to `future::availableCores(omit = 1)`). Set to 1 for debugging. Note: When using a parallel plan (multiple workers), setting n.cores > 1 may cause oversubscription. The function will warn if this occurs.
#' @return A list with two slots: `optimization`, a data frame with the index of the spatial predictor added on each iteration, the spatial correlation of the model residuals, and the R-squared of the model, and `best.spatial.predictors`, that is a character vector with the names of the spatial predictors that minimize the Moran's I of the residuals and maximize the R-squared of the model.
#' @details The algorithm works as follows. If the function [rank_spatial_predictors()] returns 10 ranked spatial predictors (sp1 to sp10, being sp7 the best one), [select_spatial_predictors_recursive()] is going to first fit the model `y ~ predictors + sp7`. Then, the spatial predictors sp2 to sp9 are again ranked with [rank_spatial_predictors()] using the model `y ~ predictors + sp7` as reference (at this stage, some of the spatial predictors might be dropped due to lack of effect). When the new ranking of spatial predictors is ready (let's say they are sp5, sp3, and sp4), the best one (sp5) is included in the model `y ~ predictors + sp7 + sp5`, and the remaining ones go again to [rank_spatial_predictors()] to repeat the process until spatial predictors are depleted.
#' @examples
#'
#' if (interactive()) {
#'   data(
#'     plants_df,
#'     plants_response,
#'     plants_predictors,
#'     plants_distance,
#'     plants_rf
#'   )
#'
#'   #subset to speed up example
#'   idx <- 1:20
#'   plants_df <- plants_df[idx, ]
#'   plants_distance <- plants_distance[idx, idx]
#'
#'   #generate spatial predictors
#'   mems <- mem_multithreshold(
#'     distance.matrix = plants_distance,
#'     distance.thresholds = 100
#'   )
#'
#'   #rank them from higher to lower moran
#'   mems.rank <- rank_spatial_predictors(
#'     ranking.method = "moran",
#'     spatial.predictors.df = mems,
#'     reference.moran.i = plants_rf$residuals$autocorrelation$max.moran,
#'     distance.matrix = plants_distance,
#'     distance.thresholds = 100,
#'     n.cores = 1
#'   )
#'
#'   #select best subset via sequential addition
#'   selection <- select_spatial_predictors_recursive(
#'     data = plants_df,
#'     dependent.variable.name = plants_response,
#'     predictor.variable.names = plants_predictors,
#'     distance.matrix = plants_distance,
#'     distance.thresholds = 100,
#'     spatial.predictors.df = mems,
#'     spatial.predictors.ranking = mems.rank,
#'     ranger.arguments = list(num.trees = 30),
#'     n.cores = 1
#'   )
#'
#'   #names of selected spatial predictors
#'   selection$best.spatial.predictors
#'
#'   #optimization plot
#'   plot_optimization(selection$optimization)
#' }
#' @rdname select_spatial_predictors_recursive
#' @family spatial_analysis
#' @autoglobal
#' @export
select_spatial_predictors_recursive <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ranger.arguments = NULL,
  spatial.predictors.df = NULL,
  spatial.predictors.ranking = NULL,
  weight.r.squared = 0.25,
  weight.penalization.n.predictors = 0,
  n.cores = NULL
) {
  #preparing fast ranger arguments
  if (is.null(ranger.arguments)) {
    ranger.arguments <- list()
  }
  ranger.arguments$write.forest <- TRUE
  ranger.arguments$importance <- "none"
  ranger.arguments$local.importance <- FALSE
  ranger.arguments$keep.inbag <- FALSE
  ranger.arguments$num.trees <- 500
  ranger.arguments$data <- NULL
  ranger.arguments$formula <- NULL
  ranger.arguments$dependent.variable.name <- NULL
  ranger.arguments$predictor.variable.names <- NULL

  #initializing data for loop
  spatial.predictors.ranking.i <- spatial.predictors.ranking
  spatial.predictors.candidates.i <- spatial.predictors.ranking$ranking

  #weights limits
  if (is.null(weight.r.squared)) {
    weight.r.squared <- 0.25
  }
  if (weight.r.squared > 1) {
    weight.r.squared <- 1
  }
  if (weight.r.squared < 0) {
    weight.r.squared <- 0
  }
  if (is.null(weight.penalization.n.predictors)) {
    weight.penalization.n.predictors <- 0
  }
  if (weight.penalization.n.predictors > 1) {
    weight.penalization.n.predictors <- 1
  }
  if (weight.penalization.n.predictors < 0) {
    weight.penalization.n.predictors <- 0
  }

  #copy of data
  data.i <- data
  predictor.variable.names.i <- predictor.variable.names

  #putting together the optimization data frame
  optimization.df <- data.frame(
    spatial.predictor.name = rep(
      NA,
      length(spatial.predictors.ranking$ranking)
    ),
    spatial.predictor.index = rep(
      NA,
      length(spatial.predictors.ranking$ranking)
    ),
    moran.i = rep(NA, length(spatial.predictors.ranking$ranking)),
    p.value = rep(NA, length(spatial.predictors.ranking$ranking)),
    p.value.binary = rep(NA, length(spatial.predictors.ranking$ranking)),
    r.squared = rep(NA, length(spatial.predictors.ranking$ranking)),
    penalization.per.variable = rep(
      NA,
      length(spatial.predictors.ranking$ranking)
    ),
    optimization = rep(NA, length(spatial.predictors.ranking$ranking))
  )

  #starting row counter
  i <- 0

  #vector to store the index of max(optimization.df$optimization)
  recursive.index.tracking <- vector()

  #iterating
  while (length(spatial.predictors.candidates.i) > 0) {
    i <- i + 1

    #add the first factor to data
    data.i <- data.frame(
      data.i,
      spatial.predictors.df[, spatial.predictors.candidates.i[1]]
    )
    colnames(data.i)[ncol(data.i)] <- spatial.predictors.candidates.i[1]

    #new predictor.variable.names
    predictor.variable.names.i <- c(
      predictor.variable.names.i,
      spatial.predictors.candidates.i[1]
    )

    #reference moran I
    reference.moran.i <- spatial.predictors.ranking.i$criteria[
      spatial.predictors.ranking.i$criteria$spatial.predictors.name ==
        spatial.predictors.candidates.i[1],
      "moran.i"
    ]

    #subset and order spatial.predictors
    #only proceed to ranking if there are remaining candidates (length > 1)
    remaining.candidates <- length(spatial.predictors.candidates.i) > 1

    if (remaining.candidates) {
      spatial.predictors.df.i <- spatial.predictors.df[,
        spatial.predictors.candidates.i[
          2:length(spatial.predictors.candidates.i)
        ],
        drop = FALSE
      ]

      #rank pca factors
      spatial.predictors.ranking.i <- rank_spatial_predictors(
        data = data.i,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names.i,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        ranger.arguments = ranger.arguments,
        spatial.predictors.df = spatial.predictors.df.i,
        ranking.method = "effect",
        reference.moran.i = reference.moran.i,
        verbose = FALSE,
        n.cores = n.cores
      )

      #redo spatial.predictors.candidates.i
      spatial.predictors.candidates.i <- spatial.predictors.ranking.i$ranking
    }

    #gathering data for optimization df.
    # always record the iteration, even if no remaining candidates to rank
    optimization.df[i, "spatial.predictor.index"] <- i

    if (remaining.candidates) {
      optimization.df[
        i,
        "spatial.predictor.name"
      ] <- spatial.predictors.ranking.i$ranking[1]
      optimization.df[i, "moran.i"] <- spatial.predictors.ranking.i$criteria[
        1,
        "moran.i"
      ]
      optimization.df[i, "p.value"] <- spatial.predictors.ranking.i$criteria[
        1,
        "p.value"
      ]
      optimization.df[i, "r.squared"] <- spatial.predictors.ranking.i$criteria[
        1,
        "model.r.squared"
      ]
    } else {
      # no remaining candidates - record the last predictor that was added
      optimization.df[i, "spatial.predictor.name"] <- predictor.variable.names.i[length(predictor.variable.names.i)]
      # moran.i, p.value, r.squared left as NA for the final iteration
    }

    # after recording, clear candidates if none remaining so loop exits
    if (!remaining.candidates) {
      spatial.predictors.candidates.i <- vector()
    }

    optimization.df[i, "p.value.binary"] <- ifelse(
      optimization.df[i, "p.value"] >= 0.05,
      1,
      0
    )
    optimization.df[i, "penalization.per.variable"] <- (1 /
      nrow(optimization.df)) *
      i
    optimization.df[i, "optimization"] <- (1 -
      optimization.df[i, "moran.i"]) +
      (weight.r.squared * optimization.df[i, "r.squared"]) -
      (weight.penalization.n.predictors *
        optimization.df[i, "penalization.per.variable"])

    #getting the index with the maximum optimization
    recursive.index.tracking[i] <- optimization.df[
      which.max(optimization.df$optimization),
      "spatial.predictor.index"
    ]

    #finding repetitions in the maximum value of recursive index
    #only check after at least 3 iterations to avoid premature exit
    #the threshold is 20% of iterations so far (not total rows)
    if (
      i >= 3 &&
      sum(recursive.index.tracking == max(recursive.index.tracking)) >
        max(1, floor(i / 5))
    ) {
      break
    }
  } #end of while loop

  #remove unfilled rows (those without spatial.predictor.name)
  optimization.df <- optimization.df[!is.na(optimization.df$spatial.predictor.name), ]

  #get index of spatial predictor with recursive r-squared and moran.i
  recursive.index <- which.max(optimization.df$optimization)

  #prepare vector with best factor names
  best.spatial.predictors <- optimization.df$spatial.predictor.name[
    1:recursive.index
  ]

  #add column selected to optimization.df
  optimization.df$selected <- FALSE
  optimization.df[
    optimization.df$spatial.predictor.name %in% best.spatial.predictors,
    "selected"
  ] <- TRUE

  #output list
  out.list <- list()
  out.list$optimization <- optimization.df
  out.list$best.spatial.predictors <- best.spatial.predictors

  #return output
  out.list
}
