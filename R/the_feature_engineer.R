#' @title Suggest variable interactions and composite features for random forest models
#' @description Suggests candidate variable interactions and composite features able to improve predictive accuracy over data not used to train the model via spatial cross-validation with [rf_evaluate()]. For a pair of predictors `a` and `b`, interactions are build via multiplication (`a * b`), while composite features are built by extracting the first factor of a principal component analysis performed with [pca()], after rescaling `a` and `b` between 1 and 100. Interactions and composite features are named `a..x..b` and `a..pca..b` respectively.
#'
#'Candidate variables `a` and `b` are selected from those predictors in `predictor.variable.names` with a variable importance above `importance.threshold` (set by default to the median of the importance scores).
#'
#' For each interaction and composite feature, a model including all the predictors plus the interaction or composite feature is fitted, and it's R squared (or AUC if the response is binary) computed via spatial cross-validation (see [rf_evaluate()]) is compared with the R squared of the model without interactions or composite features.
#'
#'From all the potential interactions screened, only those with a positive increase in R squared (or AUC when the response is binomial) of the model, a variable importance above the median, and a maximum correlation among themselves and with the predictors in `predictor.variable.names` not higher than `cor.threshold` (set to 0.5 by default) are selected. Such a restrictive set of rules ensures that the selected interactions can be used right away for modeling purposes without increasing model complexity unnecessarily. However, the suggested variable interactions might not make sense from a domain expertise standpoint, so please, examine them with care.
#'
#'The function returns the criteria used to select the interactions, and the data required to use these interactions a model.
#'
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables, or object of class `"variable_selection"` produced by [auto_vif()] and/or [auto_cor()]. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". If not provided, the comparison between models with and without variable interactions is not done.
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param training.fraction Proportion between 0.5 and 0.9 indicating the proportion of records to be used as training set during spatial cross-validation. Default: `0.75`
#' @param importance.threshold Numeric between 0 and 1, quantile of variable importance scores over which to select individual predictors to explore interactions among them. Larger values reduce the number of potential interactions explored. Default: `0.75`
#' @param cor.threshold Numeric, maximum Pearson correlation between any pair of the selected interactions, and between any interaction and the predictors in `predictor.variable.names`. Default: `0.75`
#' @param point.color Colors of the plotted points. Can be a single color name (e.g. "red4"), a character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(100)`). Default: `viridis::viridis(100, option = "F", alpha = 0.8)`
#' @param seed Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: `NULL`
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#' @param n.cores Integer, number of cores to use for parallel execution. Creates a socket cluster with `parallel::makeCluster()`, runs operations in parallel with `foreach` and `%dopar%`, and stops the cluster with `parallel::clusterStop()` when the job is done. Default: `parallel::detectCores() - 1`
#' @param cluster A cluster definition generated with `parallel::makeCluster()`. If provided, overrides `n.cores`. When `cluster = NULL` (default value), and `model` is provided, the cluster in `model`, if any, is used instead. If this cluster is `NULL`, then the function uses `n.cores` instead. The function does not stop a provided cluster, so it should be stopped with `parallel::stopCluster()` afterwards. The cluster definition is stored in the output list under the name "cluster" so it can be passed to other functions via the `model` argument, or using the `|>` pipe. Default: `NULL`
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
#'
#' if (interactive()) {
#'   data(
#'     plants_df,
#'     plants_response,
#'     plants_predictors,
#'     plants_xy,
#'     plants_rf
#'   )
#'
#'   #get five most important predictors from plants_rf to speed-up example
#'   predictors <- get_importance(plants_rf)[1:5, "variable"]
#'
#'   #subset to speed-up example
#'   idx <- 1:30
#'   plants_df <- plants_df[idx, ]
#'   plants_xy <- plants_xy[idx, ]
#'
#'   #data subsetted to speed-up example runtime
#'   y <- the_feature_engineer(
#'     data = plants_df,
#'     dependent.variable.name = plants_response,
#'     predictor.variable.names = predictors,
#'     xy = plants_xy,
#'     repetitions = 5,
#'     n.cores = 1,
#'     ranger.arguments = list(
#'       num.trees = 30
#'     ),
#'     verbose = TRUE
#'   )
#'
#'   #all tested interactions
#'   y$screening
#'
#'   #selected interaction (same as above in this case)
#'   y$selected
#'
#'   #new column added to data
#'   head(y$data[, y$selected$interaction.name])
#' }
#'
#' @importFrom foreach %do%
#' @rdname the_feature_engineer
#' @family preprocessing
#' @autoglobal
#' @export
the_feature_engineer <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  xy = NULL,
  ranger.arguments = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  importance.threshold = 0.75,
  cor.threshold = 0.75,
  point.color = viridis::viridis(
    100,
    option = "F",
    alpha = 0.8
  ),
  seed = NULL,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
) {
  #coerce to data frame if tibble
  if (is.null(data)) {
    stop("Argument 'data' is missing.")
  } else {
    if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
      data <- as.data.frame(data)
    }
  }

  if (is.null(xy)) {
    stop("Argument 'xy' is missing")
  } else {
    if (inherits(xy, "tbl_df") || inherits(xy, "tbl")) {
      xy <- as.data.frame(xy)
    }
  }

  #CLUSTER SETUP
  parallel_config <- setup_parallel_execution(cluster, n.cores)
  on.exit(parallel_config$cleanup(), add = TRUE)

  #finding out if the response is binary
  is.binary <- is_binary(
    data = data,
    dependent.variable.name = dependent.variable.name
  )
  if (is.binary) {
    metric <- "auc"
  } else {
    metric <- "r.squared"
  }

  #predictor.variable.names comes from auto_vif or auto_cor
  if (!is.null(predictor.variable.names)) {
    if (inherits(predictor.variable.names, "variable_selection")) {
      predictor.variable.names <- predictor.variable.names$selected.variables
    }
  }

  if (importance.threshold > 1) {
    importance.threshold <- 0.99
  }
  if (importance.threshold < 0) {
    importance.threshold <- 0.01
  }

  if (verbose) {
    message("Training and evaluating a model without interactions.")
  }

  #without
  model.without.interactions <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    ranger.arguments = ranger.arguments,
    seed = seed,
    n.cores = n.cores,
    verbose = FALSE
  )

  #evaluation
  model.without.interactions <- rf_evaluate(
    model = model.without.interactions,
    repetitions = repetitions,
    training.fraction = training.fraction,
    xy = xy,
    metrics = metric,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores,
    cluster = parallel_config$cluster
  )

  #metric
  model.without.interactions.evaluation <- model.without.interactions$evaluation$aggregated
  model.without.interactions.metric <- model.without.interactions.evaluation[
    model.without.interactions.evaluation$model == "Testing",
    "median"
  ]

  #ranger.arguments.i
  ranger.arguments.i <- ranger.arguments
  ranger.arguments.i$data <- NULL
  ranger.arguments.i$dependent.variable.name <- NULL
  ranger.arguments.i$predictor.variable.names <- NULL

  #setting quantile of the importance threshold
  importance.threshold <- stats::quantile(
    x = model.without.interactions$importance$per.variable$importance,
    probs = importance.threshold
  )

  if (importance.threshold <= 0) {
    warning(
      "Quantile ",
      importance.threshold,
      " of predictor importances is negative, the argument 'predictor.variable.names' might contain meaningless predictors."
    )
  }

  #selected variables
  variables.to.test <- model.without.interactions$importance$per.variable[
    model.without.interactions$importance$per.variable$importance >=
      importance.threshold,
    "variable"
  ]

  if (length(variables.to.test) < 2) {
    warning(
      "There are not enough predictors above 'importance.threshold = ",
      importance.threshold,
      "' for this analysis, returning NULL."
    )

    return(NULL)
  }

  #pairs of variables
  variables.pairs <- as.data.frame(
    t(
      utils::combn(
        x = variables.to.test,
        m = 2
      )
    ),
    stringsAsFactors = FALSE
  )

  if (verbose) {
    message(paste0(
      "Testing ",
      nrow(variables.pairs),
      " candidate interactions."
    ))
  }

  #testing interactions
  interaction.screening.1 <- foreach::foreach(
    i = seq(1, nrow(variables.pairs)),
    .combine = "rbind",
    .verbose = FALSE
  ) %dopar%
    {
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

      #computing max correlation with predictors
      cor.out <- stats::cor(data.i[, predictor.variable.names.i])
      diag(cor.out) <- NA
      max.cor <- max(cor.out[pair.i.name, ], na.rm = TRUE)

      #if the maximum correlation is lower than the threshold, fit model
      if (max.cor <= cor.threshold) {
        #without
        model.i <- spatialRF::rf(
          data = data.i,
          dependent.variable.name = dependent.variable.name,
          predictor.variable.names = predictor.variable.names.i,
          ranger.arguments = ranger.arguments.i,
          seed = seed,
          n.cores = 1,
          verbose = FALSE
        )

        #evaluation
        model.i <- spatialRF::rf_evaluate(
          model = model.i,
          repetitions = repetitions,
          training.fraction = training.fraction,
          xy = xy,
          metrics = metric,
          seed = seed,
          verbose = FALSE,
          n.cores = 1
        )

        #importance data frame
        model.i.importance <- model.i$importance$per.variable

        #metric
        model.i.evaluation <- model.i$evaluation$aggregated
        model.i.metric <- model.i.evaluation[
          model.i.evaluation$model == "Testing",
          "median"
        ]

        #gathering results
        out.df <- data.frame(
          interaction.name = pair.i.name,
          interaction.importance = round(
            (model.i.importance[
              model.i.importance$variable == pair.i.name,
              "importance"
            ] *
              100) /
              max(model.i.importance$importance),
            3
          ),
          interaction.metric.gain = model.i.metric -
            model.without.interactions.metric,
          max.cor.with.predictors = max.cor,
          variable.a.name = pair.i[1],
          variable.b.name = pair.i[2]
        )
      } else {
        #gathering results
        out.df <- data.frame(
          interaction.name = NA,
          interaction.importance = NA,
          interaction.metric.gain = NA,
          max.cor.with.predictors = NA,
          variable.a.name = NA,
          variable.b.name = NA
        )
      }

      return(out.df)
    } #end of parallelized loop

  interaction.screening.2 <- foreach::foreach(
    i = seq(1, nrow(variables.pairs)),
    .combine = "rbind",
    .verbose = FALSE
  ) %dopar%
    {
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

      #computing max correlation with predictors
      cor.out <- stats::cor(data.i[, predictor.variable.names.i])
      diag(cor.out) <- NA
      max.cor <- max(cor.out[pair.i.name, ], na.rm = TRUE)

      #if the maximum correlation is lower than the threshold, fit model
      if (max.cor <= cor.threshold) {
        #without
        model.i <- spatialRF::rf(
          data = data.i,
          dependent.variable.name = dependent.variable.name,
          predictor.variable.names = predictor.variable.names.i,
          ranger.arguments = ranger.arguments.i,
          seed = seed,
          n.cores = 1,
          verbose = FALSE
        )

        #evaluation
        model.i <- spatialRF::rf_evaluate(
          model = model.i,
          repetitions = repetitions,
          training.fraction = training.fraction,
          xy = xy,
          metrics = metric,
          seed = seed,
          verbose = FALSE,
          n.cores = 1
        )

        #importance data frame
        model.i.importance <- model.i$importance$per.variable

        #metric
        model.i.evaluation <- model.i$evaluation$aggregated
        model.i.metric <- model.i.evaluation[
          model.i.evaluation$model == "Testing",
          "median"
        ]

        #gathering results
        out.df <- data.frame(
          interaction.name = pair.i.name,
          interaction.importance = round(
            (model.i.importance[
              model.i.importance$variable == pair.i.name,
              "importance"
            ] *
              100) /
              max(model.i.importance$importance),
            2
          ),
          interaction.metric.gain = round(
            model.i.metric -
              model.without.interactions.metric,
            4
          ),
          max.cor.with.predictors = round(max.cor, 2),
          variable.a.name = pair.i[1],
          variable.b.name = pair.i[2]
        )
      } else {
        #gathering results
        out.df <- data.frame(
          interaction.name = NA,
          interaction.importance = NA,
          interaction.metric.gain = NA,
          max.cor.with.predictors = NA,
          variable.a.name = NA,
          variable.b.name = NA
        )
      }

      return(out.df)
    } #end of parallelized loop

  interaction.screening <- rbind(
    interaction.screening.1,
    interaction.screening.2
  ) |>
    na.omit()

  if (nrow(interaction.screening) == 0) {
    warning("No promising interactions found, returned NULL.")
    return(NULL)
  }

  #adding column of selected interactions
  interaction.screening$selected <- ifelse(
    interaction.screening$interaction.metric.gain > 0.01 &
      interaction.screening$interaction.metric.gain > 0.01,
    TRUE,
    FALSE
  )

  if (sum(interaction.screening$selected) == 0) {
    warning("No promising interactions found, returned NULL.")
    return(NULL)
  }

  #compute order
  interaction.screening$order <-
    spatialRF::rescale_vector(interaction.screening$interaction.importance) +
    spatialRF::rescale_vector(interaction.screening$interaction.metric.gain) -
    spatialRF::rescale_vector(1 - interaction.screening$max.cor.with.predictors)

  #arrange by gain
  interaction.screening <- interaction.screening |>
    dplyr::arrange(
      dplyr::desc(order)
    ) |>
    dplyr::select(
      -order
    )

  #selected only
  interaction.screening.selected <- interaction.screening[
    interaction.screening$selected,
  ]
  interaction.screening.selected <- interaction.screening.selected[, c(
    "interaction.name",
    "interaction.importance",
    "interaction.metric.gain",
    "max.cor.with.predictors",
    "variable.a.name",
    "variable.b.name"
  )]

  #preparing data frame of interactions
  interaction.df <- data.frame(
    dummy.column = rep(NA, nrow(data))
  )

  for (i in seq(1, nrow(interaction.screening.selected))) {
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
    if (grepl("..pca..", pair.i.name, fixed = TRUE)) {
      interaction.df[, interaction.screening.selected[
        i,
        "interaction.name"
      ]] <- spatialRF::pca(
        x = data.frame(
          pair.i.1,
          pair.i.2
        )
      )$pca_factor_1
    } else {
      interaction.df[, interaction.screening.selected[
        i,
        "interaction.name"
      ]] <- pair.i.1 * pair.i.2
    }
  }
  interaction.df$dummy.column <- NULL

  #removing variable names
  interaction.screening.selected$variable.a.name <- NULL
  interaction.screening.selected$variable.b.name <- NULL

  #filtering out interactions by correlation among themselves
  if (nrow(interaction.screening.selected) > 1) {
    interaction.selection <- auto_cor(
      x = interaction.df,
      preference.order = interaction.screening.selected$interaction.name,
      cor.threshold = cor.threshold,
      verbose = FALSE
    )

    interaction.screening.selected <- interaction.screening.selected[
      interaction.screening.selected$interaction.name %in%
        interaction.selection$selected.variables,
    ]

    interaction.df <- interaction.df[,
      interaction.selection$selected.variables,
      drop = FALSE
    ]
  }

  if (verbose) {
    message(
      paste0(
        "Interactions identified: ",
        nrow(interaction.screening.selected)
      )
    )
  }

  #printing suggested interactions
  if (verbose) {
    x <- interaction.screening.selected
    if (metric == "r.squared") {
      colnames(x) <- c(
        "Interaction",
        "Importance (% of max)",
        "R-squared improvement",
        "Max cor with predictors"
      )
    }
    if (metric == "auc") {
      colnames(x) <- c(
        "Interaction",
        "Importance (% of max)",
        "AUC improvement",
        "Max cor with predictors"
      )
    }

    #format numeric columns
    x[, 2] <- sprintf("%.1f", x[, 2])
    x[, 3] <- sprintf("%.3f", x[, 3])
    x[, 4] <- sprintf("%.2f", x[, 4])

    #print
    print(x, row.names = FALSE)
  }

  #plot interactions
  plot.list <- list()
  for (variable in names(interaction.df)) {
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
          "Gain (",
          metric,
          "): ",
          round(
            interaction.screening.selected[
              interaction.screening.selected$interaction.name == variable,
              "interaction.metric.gain"
            ],
            3
          ),
          "\n Importance (%): ",
          round(
            interaction.screening.selected[
              interaction.screening.selected$interaction.name == variable,
              "interaction.importance"
            ],
            0
          ),
          "\n Max cor: ",
          round(
            interaction.screening.selected[
              interaction.screening.selected$interaction.name == variable,
              "max.cor.with.predictors"
            ],
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

  #generating training df
  training.df <- cbind(
    data,
    interaction.df
  )

  #preparing out list
  out.list <- list()
  out.list$screening <- interaction.screening
  out.list$selected <- interaction.screening.selected
  out.list$plot <- plot.list
  out.list$data <- training.df
  out.list$dependent.variable.name <- dependent.variable.name
  out.list$predictor.variable.names <- c(
    predictor.variable.names,
    colnames(interaction.df)
  )

  if (verbose) {
    message(
      "Comparing models with and without interactions via spatial cross-validation."
    )
  }

  #fitting models with and without the selected interactions

  #with
  model.with.interactions <- spatialRF::rf(
    data = training.df,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = c(
      predictor.variable.names,
      colnames(interaction.df)
    ),
    ranger.arguments = ranger.arguments,
    verbose = FALSE
  )

  #pick colors for comparison from the palette
  n.colors <- length(point.color)
  line.color <- point.color[1]
  color.a <- point.color[floor(n.colors / 4)]
  color.b <- point.color[floor((n.colors / 4) * 3)]

  #comparison
  comparison <- rf_compare(
    models = list(
      with.interactions = model.with.interactions,
      without.interactions = model.without.interactions
    ),
    repetitions = repetitions,
    xy = xy,
    metrics = metric,
    fill.color = c(color.a, color.b),
    line.color = line.color,
    seed = seed,
    verbose = FALSE,
    n.cores = n.cores,
    cluster = parallel_config$cluster
  )

  #adding it to the plot list
  plot.list[["comparison"]] <- comparison$plot

  #saving new elements into the output list
  out.list$comparison.df <- comparison$comparison.df
  out.list$plot <- plot.list

  if (verbose) {
    print(patchwork::wrap_plots(out.list$plot))
  }

  out.list
}
