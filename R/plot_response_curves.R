#' @title Plots the response curves of a model.
#' @description Plots the response curves of models fitted with [rf()], [rf_repeat()], or  [rf_spatial()].
#' @param model A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param variables Character vector, names of predictors to plot. If `NULL`, the most important variables (importance higher than the median) in `x` are selected. Default: `NULL`.
#' @param quantiles Numeric vector with values between 0 and 1, argument `probs` of \link[stats]{quantile}. Quantiles to set the other variables to. Default: `c(0.1, 0.5, 0.9)`
#' @param grid.resolution Integer between 20 and 500. Resolution of the plotted curve Default: `100`
#' @param line.color Character vector with colors, or function to generate colors for the lines representing `quantiles`. Must have the same number of colors as `quantiles` are defined. Default: `viridis::viridis(length(quantiles), option = "F", end = 0.9)`
#' @param ncol Integer, argument of \link[patchwork]{wrap_plots}. Defaults to the rounded squared root of the number of plots. Default: `2`
#' @param show.data Logical, if `TRUE`, the observed data is plotted along with the response curves. Default: `FALSE`
#' @param verbose Logical, if TRUE the plot is printed. Default: `TRUE`
#' @return A list with slots named after the selected `variables`, with one ggplot each.
#' @details All variables that are not plotted in a particular response curve are set to the values of their respective quantiles, and the response curve for each one of these quantiles is shown in the plot. When the input model was fitted with [rf_repeat()] with `keep.models = TRUE`, then the plot shows the median of all model runs, and each model run separately as a thinner line. The output list can be plotted all at once with `patchwork::wrap_plots(p)` or `cowplot::plot_grid(plotlist = p)`, or one by one by extracting each plot from the list.
#' @seealso [plot_response_surface()]
#' @examples
#' if(interactive()){
#'
#'#loading example data
#'data(plant_richness_df)
#'
#'#fitting a random forest model
#'m <- rf(
#'  data = plant_richness_df,
#'  dependent.variable.name = "richness_species_vascular",
#'  predictor.variable.names = colnames(plant_richness_df)[5:21],
#'  n.cores = 1,
#'  verbose = FALSE
#')
#'
#'#response curves of most important predictors
#'plot_response_curves(model = m)
#'
#' }
#' @rdname plot_response_curves
#' @export
plot_response_curves <- function(
  model = NULL,
  variables = NULL,
  quantiles = c(0.1, 0.5, 0.9),
  grid.resolution = 200,
  line.color = viridis::viridis(
    length(quantiles),
    option = "F",
    end = 0.9
  ),
  ncol = 2,
  show.data = FALSE,
  verbose = TRUE
) {
  if (is.null(model)) {
    stop("Argument 'model' must not be empty.")
  }

  if (length(quantiles) < length(line.color)) {
    line.color <- line.color[1:length(quantiles)]
  }

  if (length(line.color) < length(quantiles)) {
    if (verbose == TRUE) {
      message(
        "Insufficient colors provided in 'line.color', used the default palette insted."
      )
    }
    line.color = viridis::viridis(
      length(quantiles),
      option = "F",
      end = 0.9
    )
  }

  #preparing grid resolution
  grid.resolution <- floor(grid.resolution)
  if (grid.resolution > 500) {
    grid.resolution <- 500
  }
  if (grid.resolution < 20) {
    grid.resolution <- 20
  }

  #quantile limits
  quantiles <- quantiles[quantiles >= 0]
  quantiles <- quantiles[quantiles <= 1]

  #getting the training data
  data <- model$ranger.arguments$data

  #getting the response variable
  response.variable <- model$ranger.arguments$dependent.variable.name

  #getting the predictors
  predictors <- model$ranger.arguments$predictor.variable.names

  #removing spatial predictors
  if (inherits(model, "rf_spatial")) {
    predictors <- predictors[!(predictors %in% model$spatial$names)]
  }

  #default values for variables
  if (is.null(variables)) {
    variables <- model$importance$per.variable[
      model$importance$per.variable$variable %in% predictors,
      "variable"
    ][1:floor(length(predictors) / 2)]
  }

  if (sum(variables %in% colnames(data)) != length(variables)) {
    stop(
      "Variable names in 'variables' must be column names of x$ranger.arguments$data."
    )
  }

  #PREPARING NEWDATA
  #list to store plots
  variables.list <- list()

  #iterating through variables
  for (variable.i in variables) {
    #names of the other variables
    other.variables <- setdiff(
      model$ranger.arguments$predictor.variable.names,
      variable.i
    )

    #generating grid
    variable.i.grid <- data.frame(
      variable = seq(
        min(data[, variable.i]),
        max(data[, variable.i]),
        length.out = grid.resolution
      )
    )
    colnames(variable.i.grid) <- variable.i
    variable.i.grid$id <- seq(1, nrow(variable.i.grid))

    #list to store quantile results
    variable.i.quantiles <- list()

    #iterating through quantiles
    for (quantile.i in quantiles) {
      #grid copy
      variable.i.grid.copy <- variable.i.grid

      #iterating through variables
      for (variable.j in other.variables) {
        variable.i.grid.copy[, variable.j] <- quantile(
          data[, variable.j],
          quantile.i
        )
      }

      #adding quantile column
      variable.i.grid.copy$quantile <- quantile.i

      #save to list
      variable.i.quantiles[[as.character(quantile.i)]] <- variable.i.grid.copy
    }

    #plot data frame
    quantiles.temp <- do.call("rbind", variable.i.quantiles)
    quantiles.temp$quantile <- factor(quantiles.temp$quantile)

    #saving into variables.list
    variables.list[[variable.i]] <- quantiles.temp
  }

  #ITERATING THROUGH VARIABLES AND MODELS TO PREDICT AND PLOT
  #list to store plots
  variables.plots <- list()

  #models
  if ("models" %in% names(model)) {
    models.list <- model$models
  } else {
    models.list <- list()
    models.list[[1]] <- model
  }

  #iterating through variables to predict
  for (variable.i in variables) {
    # Force local evaluation for ggplot aes()
    local_variable <- variable.i
    local_response <- response.variable

    #list to store predictions by different models
    variable.i.list <- list()

    #predictions by models
    for (i in seq(1, length(models.list))) {
      #add new data to variable.i.list
      variable.i.list[[i]] <- variables.list[[variable.i]]

      #get model.i
      model.i <- models.list[[i]]
      class(model.i) <- "ranger"

      #predict
      variable.i.list[[i]][, response.variable] <- stats::predict(
        object = model.i,
        data = variable.i.list[[i]]
      )$predictions

      #add model index
      variable.i.list[[i]][, "model"] <- i
    }

    #putting together data frame for plotting
    variable.i.df <- do.call("rbind", variable.i.list)

    #are there several models?
    if (max(variable.i.df$model) > 1) {
      #variable to trigger change in line size
      several.models <- TRUE

      #computing the median of each curve
      id <- NULL
      variable.i.df.median <- variable.i.df %>%
        dplyr::group_by(quantile, id) %>%
        dplyr::summarise_at(
          .vars = response.variable,
          .funs = median
        )

      #remove response variable from variable.i.df
      variable.i.df.copy <- variable.i.df
      variable.i.df.copy[, response.variable] <- NULL

      #join median with distinct by id
      variable.i.df.median <- dplyr::left_join(
        variable.i.df.median,
        variable.i.df.copy,
        by = c("id", "quantile")
      )
    } else {
      several.models <- FALSE
      variable.i.df.median <- variable.i.df
    }

    #plotting with data
    if (show.data == TRUE) {
      p.i <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(
            x = .data[[local_variable]],
            y = .data[[local_response]]
          ),
          shape = 16,
          alpha = 0.15
        ) +
        ggplot2::geom_path(
          data = variable.i.df,
          ggplot2::aes(
            x = .data[[local_variable]],
            y = .data[[local_response]],
            group = paste(
              .data[["model"]],
              .data[["quantile"]]
            ),
            color = .data[["quantile"]]
          ),
          linewidth = ifelse(several.models, 0.2, 1),
          alpha = ifelse(several.models, 0.4, 1)
        ) +
        ggplot2::geom_path(
          data = variable.i.df.median,
          ggplot2::aes(
            x = .data[[local_variable]],
            y = .data[[local_response]],
            group = .data[["quantile"]],
            color = .data[["quantile"]]
          ),
          linewidth = ifelse(several.models, 0.8, 0),
          alpha = ifelse(several.models, 1, 0)
        ) +
        ggplot2::scale_color_manual(values = line.color) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          color = "   Quantiles of \n other predictors"
        )

      #plotting without data
    } else {
      p.i <- ggplot2::ggplot() +
        ggplot2::geom_path(
          data = variable.i.df,
          ggplot2::aes(
            x = .data[[local_variable]],
            y = .data[[local_response]],
            group = paste(
              .data[["model"]],
              .data[["quantile"]]
            ),
            color = .data[["quantile"]]
          ),
          linewidth = ifelse(several.models, 0.2, 1),
          alpha = ifelse(several.models, 0.4, 1)
        ) +
        ggplot2::geom_path(
          data = variable.i.df.median,
          ggplot2::aes(
            x = .data[[local_variable]],
            y = .data[[local_response]],
            group = .data[["quantile"]],
            color = .data[["quantile"]]
          ),
          linewidth = ifelse(several.models, 0.8, 0),
          alpha = ifelse(several.models, 1, 0)
        ) +
        ggplot2::scale_color_manual(values = line.color) +
        ggplot2::theme_bw() +
        ggplot2::labs(
          color = "   Quantiles of \n other predictors"
        )
    }

    variables.plots[[variable.i]] <- p.i
  } #end of plotting

  #computing ncol if NULL
  if (is.null(ncol)) {
    ncol <- floor(sqrt(length(variables.plots)))
  }

  if (length(variables) == 1) {
    variables.plots.out <- variables.plots[[1]]
  } else {
    variables.plots.out <- patchwork::wrap_plots(
      variables.plots,
      ncol = ncol,
      guides = "collect"
    )
  }

  if (verbose == TRUE) {
    variables.plots.out
  }
}
