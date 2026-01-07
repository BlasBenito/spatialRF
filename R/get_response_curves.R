#' @title Extract response curve data for plotting
#' @description Extracts data for plotting partial dependence (response) curves showing how predictions vary with each predictor from models fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param model Model object from [rf()], [rf_repeat()], or [rf_spatial()].
#' @param variables Character vector of predictor names to plot. If `NULL`, automatically selects the top 50% most important variables. Default: `NULL`.
#' @param quantiles Numeric vector of quantiles (0 to 1) at which to fix non-plotted predictors. Multiple quantiles show response variation under different scenarios. Default: `c(0.1, 0.5, 0.9)`.
#' @param grid.resolution Integer (20 to 500) specifying the number of points along the predictor axis. Higher values produce smoother curves. Default: `200`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default: `TRUE`.
#' @return Data frame with the following columns:
#' \itemize{
#'   \item `response`: Predicted response values.
#'   \item `predictor`: Predictor values along the gradient.
#'   \item `quantile`: Factor indicating which quantile was used to fix other predictors.
#'   \item `model`: Model index (only for [rf_repeat()] models with multiple repetitions).
#'   \item `predictor.name`: Character name of the focal predictor.
#'   \item `response.name`: Character name of the response variable.
#' }
#' @details
#' Response curves (also called partial dependence plots) show how predicted values change as a focal predictor varies while holding other predictors constant at specified quantile values. This reveals the marginal effect of each predictor.
#'
#' The function generates curves by:
#' \enumerate{
#'   \item Creating a grid of values for the focal predictor
#'   \item Fixing non-plotted predictors at each quantile (e.g., 0.1, 0.5, 0.9)
#'   \item Predicting responses across the grid
#'   \item Repeating for each selected predictor and quantile combination
#' }
#'
#' Multiple quantiles reveal whether the effect of a predictor is consistent across different environmental contexts (parallel curves) or varies depending on other conditions (non-parallel curves).
#' @seealso [rf()], [rf_repeat()], [rf_spatial()], [plot_response_curves()], [get_importance()]
#' @examples
#' data(plants_rf)
#'
#' # Extract response curve data for plotting
#' curves <- get_response_curves(
#'   model = plants_rf,
#'   variables = NULL,  # auto-select important variables
#'   quantiles = c(0.1, 0.5, 0.9)
#' )
#'
#' # View structure
#' head(curves)
#' str(curves)
#'
#' # Check unique predictors included
#' unique(curves$predictor.name)
#'
#' @rdname get_response_curves
#' @family model_info
#' @export
#' @autoglobal
get_response_curves <- function(
  model = NULL,
  variables = NULL,
  quantiles = c(0.1, 0.5, 0.9),
  grid.resolution = 200,
  verbose = TRUE
) {
  if (is.null(model)) {
    stop("Argument 'model' must not be empty.")
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

  # Drop geometry before processing
  data <- drop_geometry_if_sf(data)

  #getting the response variable
  response.variable <- model$ranger.arguments$dependent.variable.name
  predictors <- model$ranger.arguments$predictor.variable.names
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
      "Variable names in 'variables' must be column names of model$ranger.arguments$data."
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
        variable.i.grid.copy[, variable.j] <- stats::quantile(
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
  variables.df.list <- list()

  #models
  if ("models" %in% names(model)) {
    models.list <- model$models
  } else {
    models.list <- list()
    models.list[[1]] <- model
  }

  #iterating through variables to predict
  for (variable.i in variables) {
    #list to store predictions by different models
    variable.i.list <- list()

    #predictions by models
    for (i in seq(1, length(models.list))) {
      #add new data to variable.i.list
      variable.i.list[[i]] <- variables.list[[variable.i]]

      #get model.i
      model.i <- models.list[[i]]

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

    #subsetting
    variable.i.df <- variable.i.df[, c(
      response.variable,
      variable.i,
      "quantile",
      "model"
    )]

    #adding predictor name
    variable.i.df$predictor.name <- variable.i
    variable.i.df$response.name <- response.variable

    #replacing colnames
    colnames(variable.i.df)[c(1, 2)] <- c("response", "predictor")

    #adding to variables.df list
    variables.df.list[[variable.i]] <- variable.i.df
  } #end of plotting

  #turning list into df
  variables.df <- do.call("rbind", variables.df.list)

  #reseting rownames
  rownames(variables.df) <- NULL

  #returning variables.df
  variables.df
}
