#' Extract Numeric Columns from a Data Frame
#'
#' This function takes a data frame and a set of columns and returns the names of the numeric columns in the selected data frame.
#'
#' @param data A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @return A character vector with the names of the numeric columns in the selected data frame.
#' @export
#' @rdname internal
#' @keywords internal
non_numeric_columns <- function(
    data,
    columns = NULL
){

  if(is.null(columns)){
    columns <- colnames(data)
  }

  data <- data[, columns]

  out <- colnames(data)[!sapply(data, is.numeric)]

  out

}


#' Extract Non-numeric Columns from a Data Frame
#'
#' This function takes a data frame and a set of columns and returns the names of the non-numeric columns in the selected data frame.
#'
#' @param data A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @return A character vector with the names of the non-numeric columns in the selected data frame.
#' @export
#' @rdname internal
#' @keywords internal
numeric_columns <- function(
    data = NULL,
    columns = NULL
){

  if(is.null(columns)){
    columns <- colnames(data)
  }

  data <- data[, columns]

  out <- colnames(data)[sapply(data, is.numeric)]

  out

}



#' Checks 'data' argument
#'
#' @param data data argument.
#' @param na.allowed logical, changes the check depending on whether NAs are allowed in data or not.
#'
#' @return data
#' @export
#' @rdname internal
#' @keywords internal
check_data <- function(
    data = NULL,
    na.allowed = TRUE,
    verbose = TRUE
){

  #check if it's NULL
  if(is.null(data)){
    stop("Argument 'data' is missing.")
  }

  if(!("data.frame" %in% class(data))){
    stop("Argument 'data' must be a data frame (tibbles and sf data frames are supported as well).")
  }

  #check if it has NA
  sum.na <- sum(is.na(data))

  if(sum.na > 0){
    if(na.allowed == TRUE){
      if(verbose == TRUE){
        message("Argument 'data' has ", sum.na, " NA values.")
      }
    } else {

      #removes NA
      original.nrow <- nrow(data)
      data <- na.omit(data)

      warning(
        original.nrow - nrow(data),
        " with NA values where removed from argument 'data'."
      )

    }

  }

  #check number of rows
  if(nrow(data) < 30){
    if(verbose == TRUE){
      message("Argument 'data' has too few rows .")
    }
  }

  data

}


#' Checks 'predictors.names' argument
#'
#' @param data data argument.
#' @param predictors.names predictors.names.argument
#'
#' @return predictor.names
#' @export
#' @rdname internal
#' @keywords internal
check_predictors_names <- function(
    predictors.names = NULL,
    data = NULL,
    numeric.only = TRUE,
    is.required = TRUE,
    verbose = TRUE
){

  if(is.null(predictors.names) == TRUE){
    if(is.required == TRUE){
      stop("Argument 'predictors.names' is required.")
    }
  }

  if(is.character(predictors.names) == FALSE){
    stop("Argument 'predictors.names' must be a character vector.")
  }

  if(length(predictors.names) == 0){
    if(is.required == TRUE){
      stop("Argument 'predictors.names' is empty.")
    }
  }

  #check that all predictors are in data
  if(sum(predictors.names %in% colnames(data)) < length(predictors.names)){

    if(verbose == TRUE){
      message(
        paste0(
          "The predictors.names ",
          paste0(
            predictors.names[!(predictors.names %in% colnames(data))],
            collapse = ", "
          ),
          " are missing from 'data'."
        )
      )
    }

    predictors.names <- predictors.names[predictors.names %in% colnames(data)]

  }

  #check that all predictors are numeric
  if(numeric.only == TRUE){

    numeric.predictors.names <- lapply(
      X = data[, predictors.names],
      FUN = is.numeric
    ) %>%
      unlist()

    if(sum(numeric.predictors.names) < length(predictors.names)){

      if(verbose == TRUE){
        message(
          "These non-numeric predictors will be removed:\n",
          paste0(
            predictors.names[!numeric.predictors.names],
          collapse = "\n"
          )
        )
      }

      predictors.names <- predictors.names[numeric.predictors.names]

    }

  }

  predictors.names

}

#' Checks 'response.name' argument
#'
#' @param data data argument.
#' @param response.name response.name
#'
#' @return response.name
#' @export
#' @rdname internal
#' @keywords internal
check_response_name <- function(
    response.name = NULL,
    data = NULL,
    is.required = TRUE,
    verbose = TRUE
){

  if(is.null(response.name) == TRUE){
    if(is.required == TRUE){
      stop("Argument 'response.name' is required.")
    }
  }

  if(is.character(response.name) == FALSE){
    stop("Argument 'response.name' must be a character vector.")
  }

  if(length(response.name) != 1){
    if(is.required == TRUE){
      stop("Argument 'response.name' must be of length 1 but it is empty.")
    }
  }

  #check that all predictors are in data
  if(!(response.name %in% colnames(data))){
    if(is.required == TRUE){
      stop("Argument 'response.name' must be a column name of 'data'.")
    } else {
      if(verbose == TRUE){
        message("Argument 'response.name' must be a column name of 'data'.")
      }
    }
  }

  if(is.numeric(data[[response.name]]) == FALSE){
    if(is.required == TRUE){
      stop("Argument 'response.name' must be the name of a numeric column of 'data'.")
    } else {
      if(verbose == TRUE){
        message("Argument 'response.name' is not the name of a numeric column of 'data' and will be ignored.")
      }
      return(NULL)
    }

  }

  response.name

}



#' @title Optimization equation to select spatial predictors
#' @description Optimizes the selection of spatial predictors using two different methods: "moran.i", and "p.value".
#' @param x Optimization data frame generated internally by [select_spatial_predictors_sequential()] or [select_spatial_predictors_recursive()]. Default: `NULL`
#' @param weight.performance Numeric between 0 and 1, weight of R-squared in the optimization process. Default: `NULL`
#' @param weight.penalization.n.predictors Numeric between 0 and 1, weight of the penalization on the number of added spatial predictors. Default: `NULL`
#' @param optimization.method Character, one of "moran.i", and "p.value". Default: `"moran.i"`
#' @return A numeric vector with the optimization criteria.
#' @details The method "moran.i" tries to maximize `1 - Moran's` I while taking into account the R-squared of the model and a penalization on the number of introduced spatial predictors through the expression
#'
#'
#' (1 - Moran's I) + w1 * rsquared - w2 * penalization
#'
#' The method "p.value" uses a binary version of the p-values of Moran's I (1 if >= 0.05, 0 otherwise), and uses the expression
#'
#'
#' max(1 - Moran's I, binary p-value) + w1 * rsquared - w2 * penalization
#'
#' The "moran.i" method generally selects more spatial predictors than the "p.value" method.
#' @seealso [select_spatial_predictors_recursive()], [select_spatial_predictors_sequential()]
#' @rdname internal
#' @keywords internal
#' @export
optimization_function <- function(
    x = NULL,
    weight.performance = NULL,
    weight.penalization.n.predictors = NULL,
    optimization.method = "moran.i"
){

  #using Moran's I and its p-value
  if(optimization.method == "p.value"){

    optimization <- rescale_vector(
      pmax(
        rescale_vector(1 - x$moran.i),
        x$p_value_binary
      ) +
        (weight.performance * rescale_vector(x$performance)) -
        (weight.penalization.n.predictors * rescale_vector(x$penalization.per.variable))
    )

  }

  #Using only Moran's I
  if(optimization.method == "moran.i"){

    optimization <- rescale_vector(rescale_vector(1 - x$moran.i) +
                                     (weight.performance * rescale_vector(x$performance)) -
                                     (weight.penalization.n.predictors * rescale_vector(x$penalization.per.variable)))

  }

  optimization

}


#' @title Removes redundant spatial predictors
#' @description Removes spatial predictors that are pair-wise correlated with other spatial predictors (which happens when there are several close distance thresholds), and spatial predictors correlated with non-spatial predictors.
#' @param data (required, data frame) Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param predictors.names (required, character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param response.name (optional; character string) Name of the dependent variable. Only required when there are categorical variables within `predictors.names`. Default: `NULL`
#' @param spatial.predictors.df (required, data frame) Data frame of spatial predictors.
#' @param max.cor (optional, numeric) Numeric between 0 and 1, maximum Pearson correlation between any pair of the selected variables. Default: `0.50`
#' @return A data frame with non-redundant spatial predictors.
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_numeric_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#' #computing Moran's Eigenvector Maps
#' spatial.predictors.df <- mem_multithreshold(
#'   distance.matrix = ecoregions_distance_matrix,
#'   distance.thresholds = c(0, 1000)
#'   )
#'
#' #filtering spatial predictors
#' spatial.predictors.df <- filter_spatial_predictors(
#'   data = ecoregions_df,
#'   predictors.names = ecoregions_numeric_predictors,
#'   spatial.predictors.df = spatial.predictors.df,
#'   max.cor = 0.50
#'  )
#'
#'  spatial.predictors.df
#'
#'
#' }
#' @rdname internal
#' @keywords internal
#' @export
filter_spatial_predictors <- function(
    data = NULL,
    predictors.names = NULL,
    response.name = NULL,
    spatial.predictors.df = NULL,
    max.cor = 0.50
){

  #predictors.names comes from mc_auto_vif or mc_auto_cor
  if(!is.null(predictors.names)){
    if(inherits(predictors.names, "variable_selection")){
      predictors.names <- predictors.names$selected.variables
    }
  }

  #filtering spatial predictors by pair-wise correlation
  spatial.predictors.selection <- mc_auto_cor(
    data = spatial.predictors.df,
    max.cor = max.cor,
    verbose = FALSE
  )

  spatial.predictors.df <- spatial.predictors.df[, spatial.predictors.selection]

  #converting factor predictors to numeric
  data <- fe_target_encoding(
    data = data,
    response.name = response.name,
    predictors.names = predictors.names,
    methods = "mean",
    replace = TRUE,
    verbose = FALSE
  )

  #filtering spatial predictors by correlation with non-spatial ones

  #generating df of non-spatial predictors
  non.spatial.predictors.df <- data[, predictors.names, drop = FALSE]

  #correlation between spatial and non-spatial predictors
  cor.predictors <- abs(cor(
    non.spatial.predictors.df,
    spatial.predictors.df
  ))

  #max correlation of the spatial predictors
  max.cor.spatial.predictors <- apply(cor.predictors, 2, FUN = max)

  #selected spatial predictors
  selected.spatial.predictors <- names(max.cor.spatial.predictors[max.cor.spatial.predictors < max.cor])

  #subsetting spatial.predictors.df
  spatial.predictors.df <- spatial.predictors.df[, selected.spatial.predictors, drop = FALSE]

  #returning result
  spatial.predictors.df

}


#' @title Default distance thresholds to generate spatial predictors
#' @description Generates four distance thresholds, from 0 to max(distance.matrix)/2.
#' @param distance.matrix Distance matrix. Default: `NULL`.
#' @return A numeric vector with distance thresholds.
#' @examples
#' if(interactive()){
#'
#'  #loading example distance matrix
#'  data(ecoregions_distance_matrix)
#'
#'  #computing set of default distance thresholds
#'  default_distance_thresholds(ecoregions_distance_matrix)
#'
#'  }
#' @rdname internal
#' @keywords internal
#' @export
default_distance_thresholds <- function(
    distance.matrix = NULL
){

  #stopping if no distance matrix
  if(is.null(distance.matrix)){
    stop("The argument 'distance.matrix' is missing.")
  }

  distance.thresholds <- floor(
    seq(
      0,
      max(distance.matrix, na.rm = TRUE)/2,
      length.out = 4
    )
  )

  distance.thresholds

}





#' @rdname internal
#' @keywords internal
#' @export
is_binary_response <- function(
    x
){

  #unique values
  unique.values <- sort(unique(x))

  #checking equality
  if(
    length(unique.values) == 2 &&
    all(unique.values == c(0, 1)) == TRUE
  ){
    return(TRUE)
  } else {
    return(FALSE)
  }


}


#' @title Prepares variable importance objects for spatial models
#' @description Prepares variable importance data frames and plots for models fitted with [rf_spatial()].
#' @param model An importance data frame with spatial predictors, or a model fitted with [rf_spatial()].
#' @return A list with importance data frames in different formats depending on whether the model was fitted with [rf()] or [rf_repeat()].
#' @examples
#' if(interactive()){
#'
#'#loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_numeric_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#'  #fittind spatial model
#'  model <- rf_spatial(
#'    data = ecoregions_df,
#'    response.name = ecoregions_continuous_response,
#'    predictors.names = ecoregions_numeric_predictors,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds =  0,
#'    n.cores = 1
#'  )
#'
#'  #preparing the importance data frame
#'  importance <- prepare_importance_spatial(model)
#'  names(importance)
#'
#' }
#' @rdname internal
#' @keywords internal
#' @export
prepare_importance_spatial <- function(model){

  importance <- NULL
  variable <- NULL

  #getting importance df
  if(inherits(model, "rf_spatial") == FALSE){
    stop("This function requires a model fitted with rf_spatial()")
  }
  importance.df <- model$importance$global

  #spatial predictors only
  spatial.predictors <- importance.df[grepl(
    "spatial_predictor",
    importance.df$variable
  ),]
  spatial.predictors$variable <- "spatial_predictors"

  #non-spatial predictors
  non.spatial.predictors <- importance.df[!grepl(
    "spatial_predictor",
    importance.df$variable
  ),]

  #joining for plot
  importance.plot.df <- rbind(
    spatial.predictors,
    non.spatial.predictors
  )

  #aggregating spatial predictors
  #min, max, median and mean of the spatial predictors
  spatial.predictors.stats <- data.frame(
    variable = c(
      "spatial_predictors (max)",
      "spatial_predictors (min)",
      "spatial_predictors (median)",
      "spatial_predictors (quantile 0.25)",
      "spatial_predictors (quantile 0.75)"
    ),
    importance = c(
      max(spatial.predictors$importance),
      min(spatial.predictors$importance),
      median(spatial.predictors$importance),
      quantile(spatial.predictors$importanc, probs = 0.25),
      quantile(spatial.predictors$importanc, probs = 0.75)
    )
  )

  #formatting importance.df
  importance.df <- rbind(
    non.spatial.predictors,
    spatial.predictors.stats
  ) %>%
    dplyr::arrange(dplyr::desc(importance))

  #preparing out list
  out.list <- list()

  #common slots
  out.list$per.variable <- model$importance$global
  out.list$per.variable.plot <- plot_importance(
    model$importance$global,
    verbose = FALSE
  )
  out.list$spatial_predictors <- importance.plot.df
  out.list$spatial_predictors.plot <- plot_importance(
    importance.plot.df,
    verbose = FALSE
  )
  out.list$spatial_predictors.stats <- importance.df
  out.list$spatial_predictors_stats_plot <- plot_importance(
    importance.df,
    verbose = FALSE
  )

  #returning the list
  out.list

}
