#' @title Generates case weights for binary responses
#' @description When the data is binary, setting the `ranager` argument `case.weights` helps to minimize the issues produced by class imbalance. This function takes a binary response variable and returns a vector of weights populated with the values `1/#zeros` and `1/#ones`. It is used internally by the function [rf()].
#' @param x (required, numeric vector) Numeric vector with values 1 and 0 representing the response column to train a binary-response model. Default: `NULL`
#' @param case.weights  (optional, numeric vector) Numeric vector with case weights. Only for internal use within the package. Default: `NULL`
#' @return A vector with a length equal to `x` with case weights.
#' @examples
#' if(interactive()){
#'
#'  case_weights(
#'    x = c(0, 0, 0, 1, 1)
#'  )
#'
#'  }
#' @rdname internal
#' @keywords internal
#' @export
case_weights <- function(
    x = NULL,
    case.weights = NULL
){

  binary <- is_binary_response(x)

  if(is.null(x) | !binary){
    return(case.weights)
  }

  #if the user did not provide case weights
  if(is.null(case.weights)){

    #computing case weights
    #counting number of ones and zeros
    n <- length(x)
    n.1 <- sum(x)
    n.0 <- n - n.1

    #computing weights
    weight.1 <- 1/n.1
    weight.0 <- 1/n.0

    #vector of weights
    case.weights <- rep(NA, n)
    case.weights[x == 1] <- weight.1
    case.weights[x == 0] <- weight.0

    return(case.weights)

  } else {

    if(length(case.weights) == length(x)){

      return(case.weights)

    } else {
      stop("The length of the argument 'case.weights' must match the number of rows of 'data'.")
    }

  }

}

#' Extract zero-variance Columns from a Data Frame
#'
#' This function takes a data frame and a set of columns and returns the names of the columns with zero variance.
#'
#' @param data A data frame to extract numeric columns from.
#' @param columns A character vector specifying the columns of the data frame to extract.
#' @param decimal.places Integer, number of decimal places to round `columns` to. Defines the tolerance of the test. Default: 4
#' @return A character vector with the names of the numeric columns in the selected data frame.
#' @export
#' @rdname internal
#' @keywords internal
zero_variance_columns <- function(
    data,
    columns = NULL,
    decimal.places = 4
    ){

  if(is.null(columns)){
    columns <- colnames(data)
  }

  numeric.columns <- numeric_columns(
    data = data,
    columns = columns
    )

  data <- data[, numeric.columns]

  zero.variance.columns <- colnames(data)[
    round(
      apply(
        X = data,
        MARGIN = 2,
        FUN = var
      ),
      4
    ) == 0
  ]

  zero.variance.columns

}

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


#' Checks 'distance.thresholds' argument
#'
#' @param distance.thresholds main argument.
#' @param distance.matrix distance.matrix argument.
#' @param is.required logical
#'
#' @return distance thresholds
#' @export
#' @rdname internal
#' @keywords internal
check_distance_thresholds <- function(
    distance.thresholds = NULL,
    distance.matrix = NULL,
    is.required = TRUE,
    verbose = TRUE
){

  #if required
  if(is.required == TRUE){

       if(is.null(distance.matrix)){

         stop("Argument 'distance.matrix' is required.")

       } else {

         if(is.null(distance.thresholds)){

           distance.thresholds <- default_distance_thresholds(
             distance.matrix = distance.matrix
           )

           if(verbose == TRUE){
             message(
               "Generating default values for the argument 'distance.thresholds' from the distance matrix: ",
               paste0(
                 distance.thresholds,
                 collapse = ", "
                 )
               )

           }

         }

       }
  }

  #not required
  if(!is.null(distance.thresholds)){

    if(is.numeric(distance.thresholds) == FALSE){
      as.numeric(distance.thresholds)
    }


    if(max(distance.thresholds) > max(distance.matrix)){
      distance.thresholds[distance.thresholds > distance.matrix] <- max(distance.matrix)

      if(verbose == TRUE){
        "Setting maximum value of argument 'distance.thresholds' to the maximum value of the argument 'distance.matrix'."
      }

    }

    distance.thresholds <- sort(unique(distance.thresholds))

  }


}

#' Checks 'distance.matrix' argument
#'
#' @param data data argument.
#' @param distance.matrix distance.matrix argument.
#' @param is.required logical
#'
#' @return distance matrix
#' @export
#' @rdname internal
#' @keywords internal
check_distance_matrix <- function(
    data = NULL,
    distance.matrix = NULL,
    is.required = TRUE,
    verbose = TRUE
){


  if(is.null(distance.matrix) & is.required == TRUE){
      stop("The argument 'distance.matrix' must be provided.")
  }


  if(!is.null(data)){
    if(nrow(data) != nrow(distance.matrix)){
      stop("The arguments 'data' and 'distance.matrix' must have the same number of rows.")
    }
  }


  if(nrow(distance.matrix) != ncol(distance.matrix)){
    stop("The argument 'distance.matrix' must have the same number of rows and columns.")
  }


  if(sum(diag(distance.matrix)) != 0){
    diag(distance.matrix) <- 0
    if(verbose == TRUE){
      message("Diagonal of argument 'distance.matrix' was set to 0.")
    }
  }


  if(is.matrix(distance.matrix) == FALSE){
    distance.matrix <- as.matrix(distance.matrix)

    if(verbose == TRUE){
      message("Argument 'distance.matrix' was coerced to the class 'matrix'.")
    }

  }

  #fill lower triangle if empty
  if(all(is.na(distance.matrix[lower.tri(distance.matrix)]))){

    distance.matrix[lower.tri(distance.matrix)] <- t(distance.matrix)[upper.tri(t(distance.matrix))]

    if(verbose == TRUE){
      message("Lower triangle of the argument 'distance.matrix' was filled with the data from the upper triangle.")
    }

  }

  #fill upper triangle if empty
  if(all(is.na(distance.matrix[upper.tri(distance.matrix)]))){

    distance.matrix[upper.tri(distance.matrix)] <- t(distance.matrix)[lower.tri(t(distance.matrix))]

    if(verbose == TRUE){
      message("Upper triangle of the argument 'distance.matrix' was filled with the data from the lower triangle.")
    }

  }

  if(sum(is.na(distance.matrix)) != 0){
    stop("The argument 'distance.matrix' must not have NA values.")
  }

  distance.matrix


}

#' Checks 'data' argument
#'
#' @param data data argument.
#' @param drop.gemetry drops geometry column if data is an sf data frame
#'
#' @return data
#' @export
#' @rdname internal
#' @keywords internal
check_data <- function(
    data = NULL,
    drop.geometry = FALSE,
    verbose = TRUE
){

  #check if it's NULL
  if(is.null(data)){
    stop("Argument 'data' is missing.")
  }

  if(!("data.frame" %in% class(data))){
    stop("Argument 'data' must be a data frame (tibbles and sf data frames are supported as well).")
  }

  #check number of rows
  if(nrow(data) < 30){
    if(verbose == TRUE){
      message("Argument 'data' has too few rows to fit a model.")
    }
  }

  if(drop.geometry == TRUE){
    if("sf" %in% class(data)){
      if(verbose == TRUE){
        message("Dropping geometry column from the 'data' data frame.")
      }
      data <- sf::st_drop_geometry(data)
    }
  }

  data

}


#' Checks 'predictors.names' argument
#'
#' @param data data argument.
#' @param predictors.names predictors.names.argument
#' @param numeric.only logical
#' @param is.required logical
#' @param na.allowed logical, changes the check depending on whether NAs are allowed in data or not.
#' @param zero.variance.allowed logical
#' @param decimal.places integer, number of decimals for the zero variance test
#' @param verbose logical
#'
#' @return predictor.names
#' @export
#' @rdname internal
#' @keywords internal
check_predictors_names <- function(
    predictors.names = NULL,
    data = NULL,
    is.required = TRUE,
    numeric.only = TRUE,
    na.allowed = FALSE,
    zero.variance.allowed = FALSE,
    decimal.places = 4,
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
  if(all(predictors.names %in% colnames(data)) == FALSE){

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

    non.numeric.predictors <- non_numeric_columns(
      data = data,
      columns = predictors.names
    )

    if(length(non.numeric.predictors) > 0){

      if(verbose == TRUE){
        message(
          "These non-numeric predictors will be ignored:\n",
          paste0(
            non.numeric.predictors,
          collapse = "\n"
          )
        )
      }

      predictors.names <- setdiff(
        predictors.names,
        non.numeric.predictors
      )

    }

  }

  #remove predictors with NA
  if(na.allowed == FALSE){

    na.columns <- apply(
      X = data[, predictors.names],
      MARGIN = 2,
      FUN = function(x){sum(is.na(x)) > 0}
    )

    if(all(na.columns) == FALSE){

      na.columns <- names(na.columns[na.columns])

      if(verbose == TRUE){
        message(
          "These predictors have NA and will be dropped:\n",
          paste0(
            na.columns,
            collapse = "\n"
            )
          )
      }

      predictors.names <- setdiff(
        predictors.names,
        na.columns
      )

    }

  }


  #removing columns with zero variance
  if(zero.variance.allowed == FALSE){

    zero.variance.columns <- zero_variance_columns(
      data = data,
      columns = predictors.names,
      decimal.places = decimal.places
    )

    if(length(zero.variance.columns) > 0){

      if(verbose == TRUE){
        message(
          "These predictors have near zero variance and will be dropped:\n",
          paste0(
            zero.variance.columns,
            collapse = "\n"
          )
        )
      }

      predictors.names <- setdiff(
        predictors.names,
        zero.variance.columns
      )

    }

  }


  predictors.names

}

#' Checks 'response.name' argument
#'
#' @param data data argument.
#' @param response.name response.name
#' @param na.allowed logical, changes the check depending on whether NAs are allowed in data or not.
#' @param zero.variance.allowed logical
#' @param decimal.places integer, number of decimals for the zero variance test
#'
#' @return response.name
#' @export
#' @rdname internal
#' @keywords internal
check_response_name <- function(
    response.name = NULL,
    data = NULL,
    is.required = TRUE,
    na.allowed = FALSE,
    zero.variance.allowed = FALSE,
    decimal.places = 4,
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
      stop("Argument 'response.name' must be a numeric column of 'data'.")
    } else {
      if(verbose == TRUE){
        message("Argument 'response.name' is not a numeric column of 'data' and will be ignored.")
      }
      return(NULL)
    }

  } else {

    if(zero.variance.allowed == FALSE){
      if(var(round(data[[response.name]], decimal.places)) == 0){
        if(is.required == TRUE){
          stop("Argument 'response.name' is the name of a column with near zero variance.")
        } else {
          if(verbose == TRUE){
            message("Argument 'response.name' is the name of a column with near zero variance. This might cause numerical issues.")
          }
        }
      }
    }

  }

  if(na.allowed == FALSE){

    if(sum(is.na(data[[response.name]])) > 0){
      if(is.required == TRUE){
        stop("Argument 'response.name' is the name of a column with NA values.")
      } else {
        if(verbose == TRUE){
          message("Argument 'response.name' is the name of a column with NA values. This might cause unintended issues.")
        }
      }
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
  cor.predictors <- abs(
    cor(
      non.spatial.predictors.df,
      spatial.predictors.df
    )
  )

  #max correlation of the spatial predictors
  max.cor.spatial.predictors <- apply(
    X = cor.predictors,
    MARGIN = 2,
    FUN = max,
    na.rm = TRUE
    )

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

  distance.thresholds <- pretty(distance.thresholds)

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
