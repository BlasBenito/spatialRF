#' @title Fit random forest models with variable interactions
#' @description Identifies and incorporates variable interactions into random forest models. Tests multiplicative (`a * b`) and PCA-based composite features, selects promising interactions via spatial cross-validation, and returns a fitted model with selected interactions included as predictors.
#'
#' Candidate variables `a` and `b` are selected from predictors with variable importance above `importance.threshold` (default: 75th percentile). For each interaction, a model including all predictors plus the interaction is fitted and evaluated via spatial cross-validation. Only interactions with positive metric gain (R² or AUC), importance above the median, and correlation below `cor.threshold` are selected.
#'
#' @param model A model fitted with [rf()]. If provided, arguments are extracted from the model. Default: `NULL`
#' @param data Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name Character string with the name of the response variable. Must be in the column names of `data`. If the dependent variable is binary with values 1 and 0, the argument `case.weights` of `ranger` is populated by the function [collinear::case_weights()]. Default: `NULL`
#' @param predictor.variable.names Character vector with the names of the predictive variables. Default: `NULL`
#' @param xy Data frame or matrix with two columns containing coordinates and named "x" and "y". Required for spatial cross-validation.
#' @param ranger.arguments Named list with \link[ranger]{ranger} arguments (other arguments of this function can also go here). All \link[ranger]{ranger} arguments are set to their default values except for 'importance', that is set to 'permutation' rather than 'none'. Please, consult the help file of \link[ranger]{ranger} if you are not familiar with the arguments of this function.
#' @param repetitions Integer, number of spatial folds to use during cross-validation. Must be lower than the total number of rows available in the model's data. Default: `30`
#' @param ... Advanced arguments:
#'   - `training.fraction`: Proportion between 0.5 and 0.9 for training set during cross-validation (default: 0.75)
#'   - `importance.threshold`: Quantile (0-1) of variable importance for selecting predictors to test interactions (default: 0.75)
#'   - `cor.threshold`: Maximum Pearson correlation between interactions and predictors (default: 0.75)
#'   - `point.color`: Colors for plotted points (default: `grDevices::hcl.colors(100, palette = "Zissou 1", alpha = 0.8)`)
#' @param seed Integer, random seed to facilitate reproducibility. If set to a given number, the results of the function are always the same. Default: `NULL`
#' @param verbose Logical. If `TRUE`, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#' @param n.cores Integer. Number of threads for ranger's internal parallelization. Default: `NULL` (auto-detected: when a parallel plan is active via `future::plan()`, n.cores is set to 1; otherwise defaults to `future::availableCores(omit = 1)`). When a parallel plan is active, n.cores is always set to 1 to prevent oversubscription, regardless of user input.
#' @return A ranger model (class `c("rf", "rf_interactions", "ranger")`) with all standard slots from [rf()] plus an `interactions` slot containing:
#' \itemize{
#'   \item `method`: Character vector of interaction types (c("multiplicative", "pca"))
#'   \item `screening`: Data frame with all tested interactions and scores
#'   \item `selected`: Data frame with selected interactions only
#'   \item `values`: Data frame with computed interaction values
#'   \item `names`: Character vector of selected interaction names
#'   \item `plots`: Named list of ggplot objects (interaction vs response)
#'   \item `comparison`: List comparing models with/without interactions
#'   \item `parameters`: List of selection parameters used
#' }
#'
#' @details
#' For a pair of predictors `a` and `b`, multiplicative interactions are built via `a * b`, while PCA-based composite features are built by extracting the first factor from a PCA performed with [pca()] after rescaling `a` and `b` between 1 and 100. Interactions are named `a..x..b` and `a..pca..b` respectively.
#'
#' @examples
#'
#' if (interactive()) {
#'   data(plants_df, plants_xy, plants_rf)
#'
#'   # Approach 1: From existing model
#'   m_int <- rf_interactions(
#'     model = plants_rf,
#'     xy = plants_xy,
#'     repetitions = 5,
#'     n.cores = 1
#'   )
#'
#'   # Approach 2: From scratch with advanced args
#'   predictors <- c("bio1", "bio12", "pH", "aridityIndexThornthwaite")
#'   m_int <- rf_interactions(
#'     data = plants_df[1:30, ],
#'     dependent.variable.name = "richness_species_vascular",
#'     predictor.variable.names = predictors,
#'     xy = plants_xy[1:30, ],
#'     repetitions = 5,
#'     training.fraction = 0.75,
#'     importance.threshold = 0.75,
#'     n.cores = 1
#'   )
#'
#'   # Access results
#'   m_int$interactions$selected
#'   m_int$interactions$plots
#'   m_int$interactions$comparison$comparison.plot
#'
#'   # Use like any rf model
#'   plot_importance(m_int)
#'   get_performance(m_int)
#' }
#'
#' @rdname rf_interactions
#' @family main_models
#' @autoglobal
#' @export
rf_interactions <- function(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  xy = NULL,
  ranger.arguments = NULL,
  repetitions = 30,
  ...,
  seed = NULL,
  verbose = TRUE,
  n.cores = NULL
) {
  # Extract advanced arguments from ellipsis
  ellipsis_args <- list(...)

  training.fraction <- if ("training.fraction" %in% names(ellipsis_args)) {
    ellipsis_args$training.fraction
  } else {
    0.75
  }

  importance.threshold <- if ("importance.threshold" %in% names(ellipsis_args)) {
    ellipsis_args$importance.threshold
  } else {
    0.75
  }

  cor.threshold <- if ("cor.threshold" %in% names(ellipsis_args)) {
    ellipsis_args$cor.threshold
  } else {
    0.75
  }

  point.color <- if ("point.color" %in% names(ellipsis_args)) {
    ellipsis_args$point.color
  } else {
    grDevices::hcl.colors(100, palette = "Zissou 1", alpha = 0.8)
  }

  # If model is provided, extract arguments
  if (!is.null(model)) {
    ranger.arguments <- model$ranger.arguments
    data <- ranger.arguments$data
    dependent.variable.name <- ranger.arguments$dependent.variable.name
    predictor.variable.names <- ranger.arguments$predictor.variable.names
    seed <- ranger.arguments$seed

    # Get xy from model if not provided
    if (is.null(xy)) {
      if (!is.null(model$xy)) {
        xy <- model$xy
      } else {
        stop("Argument 'xy' is required when model does not have coordinates.")
      }
    }
  } else {
    # Validate required arguments
    if (is.null(data)) {
      stop("The argument 'data' is missing.")
    }
    if (is.null(xy)) {
      stop("The argument 'xy' is missing.")
    }
    if (is.null(dependent.variable.name)) {
      stop("The argument 'dependent.variable.name' is missing.")
    }
    if (is.null(predictor.variable.names)) {
      stop("The argument 'predictor.variable.names' is missing.")
    }
  }

  # Coerce to data frame if tibble
  if (inherits(data, "tbl_df") || inherits(data, "tbl")) {
    data <- as.data.frame(data)
  }

  if (inherits(xy, "tbl_df") || inherits(xy, "tbl")) {
    xy <- as.data.frame(xy)
  }

  # Auto-detect: if parallel plan is active, use single-threaded ranger
  if (!inherits(x = future::plan(), what = "sequential")) {
    n.cores <- 1
  }

  # Set default for sequential plans
  if (is.null(n.cores)) {
    n.cores <- future::availableCores(omit = 1)
  }

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
    verbose = FALSE
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

  # Create progressor for first loop
  p1 <- progressr::progressor(along = seq_len(nrow(variables.pairs)))

  # Testing multiplicative interactions (a * b)
  interaction.screening.1_list <- future.apply::future_lapply(
    X = seq_len(nrow(variables.pairs)),
    FUN = function(i) {
      out.df <- .test_single_interaction(
        i = i,
        variables.pairs = variables.pairs,
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        ranger.arguments.i = ranger.arguments.i,
        seed = seed,
        repetitions = repetitions,
        training.fraction = training.fraction,
        xy = xy,
        metric = metric,
        cor.threshold = cor.threshold,
        model.without.interactions.metric = model.without.interactions.metric,
        interaction_namer = function(pair) paste0(pair, collapse = "..x.."),
        interaction_builder = function(a, b) a * b
      )

      # Signal progress
      p1()

      return(out.df)
    },
    future.seed = TRUE
  )

  # Combine results from loop 1
  interaction.screening.1 <- do.call(rbind, interaction.screening.1_list)

  # Create progressor for second loop
  p2 <- progressr::progressor(along = seq_len(nrow(variables.pairs)))

  # Testing PCA composite features
  interaction.screening.2_list <- future.apply::future_lapply(
    X = seq_len(nrow(variables.pairs)),
    FUN = function(i) {
      out.df <- .test_single_interaction(
        i = i,
        variables.pairs = variables.pairs,
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        ranger.arguments.i = ranger.arguments.i,
        seed = seed,
        repetitions = repetitions,
        training.fraction = training.fraction,
        xy = xy,
        metric = metric,
        cor.threshold = cor.threshold,
        model.without.interactions.metric = model.without.interactions.metric,
        interaction_namer = function(pair) paste0(pair[1], "..pca..", pair[2]),
        interaction_builder = function(a, b) {
          pca(x = data.frame(a, b))$pca_factor_1
        }
      )

      # Signal progress
      p2()

      return(out.df)
    },
    future.seed = TRUE
  )

  # Combine results from loop 2
  interaction.screening.2 <- do.call(rbind, interaction.screening.2_list)

  interaction.screening <- rbind(
    interaction.screening.1,
    interaction.screening.2
  ) |>
    stats::na.omit()

  if (nrow(interaction.screening) == 0) {
    warning("No promising interactions found, returned NULL.")
    return(NULL)
  }

  #adding column of selected interactions
  interaction.screening$selected <- ifelse(
    interaction.screening$interaction.metric.gain > 0.01 &
      interaction.screening$interaction.importance >=
        stats::median(
          interaction.screening$interaction.importance,
          na.rm = TRUE
        ),
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
  interaction.screening <- interaction.screening[
    order(interaction.screening$order, decreasing = TRUE),
  ]
  interaction.screening <- interaction.screening[,
    colnames(interaction.screening) != "order",
    drop = FALSE
  ]

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
    interaction.selection <- collinear::cor_select(
      df = interaction.df,
      predictors = interaction.screening.selected$interaction.name,
      preference_order = interaction.screening.selected$interaction.name,
      max_cor = cor.threshold,
      quiet = TRUE
    )

    interaction.screening.selected <- interaction.screening.selected[
      interaction.screening.selected$interaction.name %in%
        interaction.selection,
    ]

    interaction.df <- interaction.df[,
      interaction.selection,
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

  # Generating training df
  training.df <- cbind(
    data,
    interaction.df
  )

  # Prepare full predictor list
  predictor.variable.names.full <- c(
    predictor.variable.names,
    colnames(interaction.df)
  )

  if (verbose) {
    message(
      "Comparing models with and without interactions via spatial cross-validation."
    )
  }

  # Fitting models with and without the selected interactions

  # Model with interactions (final model to be returned)
  if (verbose) {
    message("Fitting final random forest model with selected interactions.")
  }

  model.final <- rf(
    data = training.df,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names.full,
    distance.matrix = if (!is.null(model)) model$distance.matrix else NULL,
    distance.thresholds = if (!is.null(model)) model$distance.thresholds else NULL,
    xy = xy,
    ranger.arguments = ranger.arguments,
    seed = seed,
    n.cores = n.cores,
    verbose = FALSE
  )

  # Pick colors for comparison from the palette
  n.colors <- length(point.color)
  line.color <- point.color[1]
  color.a <- point.color[floor(n.colors / 4)]
  color.b <- point.color[floor((n.colors / 4) * 3)]

  # Comparison between models with and without interactions
  comparison <- rf_compare(
    models = list(
      with.interactions = model.final,
      without.interactions = model.without.interactions
    ),
    repetitions = repetitions,
    xy = xy,
    metrics = metric,
    fill.color = c(color.a, color.b),
    line.color = line.color,
    seed = seed,
    verbose = FALSE
  )

  # Adding it to the plot list
  plot.list[["comparison"]] <- comparison$plot

  # SET CLASS
  class(model.final) <- c("rf", "rf_interactions", "ranger")

  # BUILD $interactions SLOT
  model.final$interactions <- list()
  model.final$interactions$method <- c("multiplicative", "pca")
  model.final$interactions$screening <- interaction.screening
  model.final$interactions$selected <- interaction.screening.selected
  model.final$interactions$values <- interaction.df
  model.final$interactions$names <- colnames(interaction.df)
  model.final$interactions$plots <- plot.list
  model.final$interactions$comparison <- list(
    baseline.model = model.without.interactions,
    comparison.df = comparison$comparison.df,
    comparison.plot = comparison$plot
  )
  model.final$interactions$parameters <- list(
    importance.threshold = importance.threshold,
    cor.threshold = cor.threshold,
    repetitions = repetitions,
    training.fraction = training.fraction
  )

  # PRINT IF VERBOSE
  if (verbose) {
    print(model.final)
    message("\nModel comparison:")
    print(comparison$plot)
    if (requireNamespace("patchwork", quietly = TRUE)) {
      message("\nInteraction plots:")
      print(patchwork::wrap_plots(plot.list))
    }
  }

  return(model.final)
}

# Helper function to test a single interaction
# Reduces code duplication between multiplicative and PCA-based interactions
.test_single_interaction <- function(
  i,
  variables.pairs,
  data,
  dependent.variable.name,
  predictor.variable.names,
  ranger.arguments.i,
  seed,
  repetitions,
  training.fraction,
  xy,
  metric,
  cor.threshold,
  model.without.interactions.metric,
  interaction_namer,
  interaction_builder
) {
  # Get pair
  pair.i <- c(variables.pairs[i, 1], variables.pairs[i, 2])
  pair.i.name <- interaction_namer(pair.i)

  # Rescale values
  pair.i.1 <- rescale_vector(
    x = data[, pair.i[1]],
    new.min = 1,
    new.max = 100
  )
  pair.i.2 <- rescale_vector(
    x = data[, pair.i[2]],
    new.min = 1,
    new.max = 100
  )

  # Create interaction using builder function
  interaction_value <- interaction_builder(pair.i.1, pair.i.2)

  # Prepare data.i
  data.i <- data.frame(
    data,
    interaction = interaction_value
  )
  colnames(data.i)[ncol(data.i)] <- pair.i.name

  # Prepare predictor.variable.names.i
  predictor.variable.names.i <- c(
    predictor.variable.names,
    pair.i.name
  )

  # Computing max correlation with predictors
  cor.out <- stats::cor(data.i[, predictor.variable.names.i])
  diag(cor.out) <- NA
  max.cor <- max(cor.out[pair.i.name, ], na.rm = TRUE)

  # If the maximum correlation is lower than the threshold, fit model
  if (max.cor <= cor.threshold) {
    # Without
    model.i <- rf(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      ranger.arguments = ranger.arguments.i,
      seed = seed,
      n.cores = 1,
      verbose = FALSE
    )

    # Evaluation
    model.i <- rf_evaluate(
      model = model.i,
      repetitions = repetitions,
      training.fraction = training.fraction,
      xy = xy,
      metrics = metric,
      seed = seed,
      verbose = FALSE
    )

    # Importance data frame
    model.i.importance <- model.i$importance$per.variable

    # Metric
    model.i.evaluation <- model.i$evaluation$aggregated
    model.i.metric <- model.i.evaluation[
      model.i.evaluation$model == "Testing",
      "median"
    ]

    # Gathering results
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
      interaction.metric.gain = round(
        model.i.metric -
          model.without.interactions.metric,
        3
      ),
      max.cor.with.predictors = round(max.cor, 2),
      variable.a.name = pair.i[1],
      variable.b.name = pair.i[2]
    )
  } else {
    # Gathering results
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
}
