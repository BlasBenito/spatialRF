#' @title Automated multicollinearity reduction via Variance Inflation Factor
#'
#' @description
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all the remaining variables of the input data set as predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' The function `auto_vif()` applies a recursive algorithm to remove variables with a VIF higher than a given threshold (defined by the argument `max.vif`).
#'
#' The function allows the user to define a preference selection order via the argument `preference.order`. This argument helps "protect" variables that might be interesting or even required for the given analysis. Please, see the examples to understand better how this feature works.
#'
#'
#' If there are categorical variables named in `predictor.variable.names` and `dependent.variable.name` is provided, then the function applies [fe_target_encoding()] with the method "mean" to transform the categorical variables into numeric before the VIF analysis
#'
#' @param data (required; data.frame or tibble) A data frame, tibble, or sf. Default: `NULL`.
#' @param dependent.variable.name (optional; character string) Name of the dependent variable. Required when there are categorical variables within `predictor.variable.names`. Default: `NULL`
#' @param predictor.variable.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. If `NULL`, all the columns in data except `dependent.variable.name` are used. Default: `NULL`
#' @param preference.order (optional, character vector) Character vector indicating the preference order to protect variables from elimination.  Predictors not included in this argument are ranked by their VIFs. Default: `NULL`.
#' @param max.vif (optional, numeric) Numeric between 2.5 and 10 defining the maximum VIF allowed in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: 5.
#' @param verbose (optional, logical) Logical. if `TRUE`, `auto_vif()` prints messages describing its operations on the input data. Default:: `TRUE`
#' @return Character vector with selected predictor variable names.
#' @seealso [auto_cor()]
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_continuous_response,
#'   ecoregions_numeric_predictors,
#'   ecoregions_all_predictors
#' )
#'
#' #NUMERIC PREDICTORS ONLY
#' ################################
#'
#'
#' #automatic VIF reduction with low max.vif
#' #-------------------------------
#' selected.predictors <- auto_vif(
#'   data = ecoregions_df,
#'   predictor.variable.names = ecoregions_numeric_predictors,
#'   max.vif = 2
#' )
#'
#' #check vif of the resulting dataset
#' #variance inflation factor
#' vif(data = ecoregions_df[, selected.predictors])
#'
#' #a small number of predictors is selected
#' length(selected.predictors)
#'
#'
#' #automatic VIF reduction with high max.vif
#' #-------------------------------
#' selected.predictors <- auto_vif(
#'   data = ecoregions_df,
#'   predictor.variable.names = ecoregions_numeric_predictors,
#'   max.vif = 10
#' )
#'
#' #a larger number of predictors is selected
#' length(selected.predictors)
#'
#'
#' #supervised VIF reduction
#' #------------------------------------------------
#' #1. Notice that "fragmentation_ca" is selected
#' #even though it has the maximum VIF in the dataset
#' #2. "climate_bio12_average" is excluded from the result
#' #because it's highly correlated to "climate_bio1_average"
#' selected.predictors <- auto_vif(
#'   data = ecoregions_df,
#'   predictor.variable.names = ecoregions_numeric_predictors,
#'   preference.order = c(
#'     "fragmentation_ca",
#'     "climate_bio1_average",
#'     "climate_bio12_average"
#'   )
#' )
#'
#'
#' #using quantitative criteria as preference.order
#' #-----------------------------------------------
#' #computing univariate correlation between each predictor and the response
#' preference.order <- apply(
#'   X = ecoregions_df[, ecoregions_numeric_predictors],
#'   MARGIN = 2,
#'   FUN = function(x){
#'     cor(
#'       x = x,
#'       y = ecoregions_df[, ecoregions_continuous_response]
#'       )
#'     }
#'   )
#'
#' #sorting from maximum to minimum and getting names only
#' preference.order <- names(
#'   sort(
#'     preference.order,
#'     decreasing = TRUE
#'     )
#'   )
#'
#' #using it in auto_vif as preference.order
#' selected.predictors <- auto_vif(
#'   data = ecoregions_df,
#'   predictor.variable.names = ecoregions_numeric_predictors,
#'   preference.order = preference.order
#' )
#'
#'
#' #using auto_vif and auto_cor together
#' #-----------------------------------------------
#' auto_cor.result <- auto_cor(
#'   data = ecoregions_df,
#'   predictor.variable.names = ecoregions_numeric_predictors,
#'   preference.order = preference.order,
#'   max.cor = 0.75
#' )
#'
#' selected.predictors <- auto_vif(
#'   data = ecoregions_df,
#'   predictor.variable.names = auto_cor.result,
#'   preference.order = preference.order
#' )
#'
#' #NUMERIC AND CATEGORICAL PREDICTORS
#' #-----------------------------------
#'
#' #there are two categorial predictors in the dataset below
#' class(ecoregions_df$dominant_landcover)
#' class(ecoregions_df$primary_productivity)
#'
#' #these variables are included in the vector ecoregions_all_predictors
#' ecoregions_all_predictors[45:46]
#'
#' #automatic VIF reduction using target-encoding for categorical predictors
#' #---------------------------------------------------
#' #requires the name of the response to compute the target-encoding
#' selected.predictors <- auto_vif(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_continuous_response,
#'   predictor.variable.names = ecoregions_all_predictors
#' )
#'
#' #both categorical predictors have been selected
#' ecoregions_all_predictors[45:46] %in% selected.predictors
#'
#'
#' #applying target-encoding before vif
#' #---------------------------------------------------
#' #if you wish more control over the target-encoding
#' #you can apply fe_target_encoding() to your categoricals
#' #with the desired method before the vif
#' data.encoded <- fe_target_encoding(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_continuous_response,
#'   predictor.variable.names = ecoregions_all_predictors,
#'   methods = "mean",
#'   replace = TRUE
#' )
#'
#' #now the function does not require the response's name
#' selected.predictors <- auto_vif(
#'   data = data.encoded,
#'   predictor.variable.names = ecoregions_all_predictors
#' )
#'
#' #both categorical predictors have been selected again
#' ecoregions_all_predictors[45:46] %in% selected.predictors
#'
#'
#' }
#' @rdname auto_vif
#' @importFrom magrittr `%>%`
#' @importFrom stats cor
#' @export
auto_vif <- function(
    predictor.variable.names = NULL,
    data = NULL,
    dependent.variable.name = NULL,
    preference.order = NULL,
    max.vif = 5,
    verbose = TRUE
){

  variable <- NULL

  if(is.null(data)){
    stop("Argument 'data' is required.")
  }

  if(max.vif > 10){
    if(verbose == TRUE){
      warning("Argument max.vif is higher than 10. Recommended values for this argument are in the range [2.5, 10]")
    }
  }

  if(max.vif < 0){
    max.vif <- 0
    if(verbose == TRUE){
      message("max.vif is negative, setting it to 0.")
    }
  }

  if(is.null(max.vif)){
    if(verbose == TRUE){
      message("max.vif is NULL, returning all predictors.")
    }
    return(predictor.variable.names)
  }

  #dropping geometry if sf
  if("sf" %in% class(data)){
    data <- sf::st_drop_geometry(data)
  }

  #setting predictor.variable.names
  if(!is.null(predictor.variable.names)){
    predictor.variable.names <- intersect(
      x = predictor.variable.names,
      y = colnames(data)
    )
  } else {
    predictor.variable.names <- setdiff(
      x = colnames(data),
      y = dependent.variable.name
    )
  }

  #coerce categorical to numeric with target encoding
  if(!is.null(dependent.variable.name) && dependent.variable.name %in% colnames(data)){

    data <- fe_target_encoding(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      methods = "mean",
      replace = TRUE,
      verbose = verbose
    )

  }

  data <- data[, predictor.variable.names]

  #finding zero variance columns
  zero.variance.columns <- colnames(data)[round(apply(data, 2, var), 6) == 0]
  if(length(zero.variance.columns) > 0){
    if(verbose == TRUE){
      message(
        "These columns have almost zero variance and will be ignored: ",
        paste(
          zero.variance.columns,
          collapse = ", "
        )
      )
    }
  }

  #remove zero variance columns
  if(length(zero.variance.columns) > 0){

    predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero.variance.columns)]

    #subset for correlation analysis
    data <- data[, predictor.variable.names]

  }

  #auto preference order by vif
  preference.order.auto <- vif(data)[["variable"]]

  #if preference.order is not provided, use auto
  if(is.null(preference.order)){

    preference.order <- preference.order.auto

  } else {

    #subset preference.order to colnames(x)
    preference.order <- preference.order[preference.order %in% colnames(data)]

    #if there are variables not in preference.order, add them in the order of preference.order.auto
    if(length(preference.order) < ncol(data)){

      not.in.preference.order <- colnames(data)[!(colnames(data) %in% preference.order)]
      preference.order <- c(preference.order, preference.order.auto[preference.order.auto %in% not.in.preference.order])

    }

  }

  #order x according to preference order
  data <- data[, preference.order]

  #rank of interest
  data.rank <- data.frame(
    variable = colnames(data),
    rank = 1:ncol(data)
  )

  #iterating through reversed preference order
  for(i in seq(from = nrow(data.rank), to = 2)){

    vif.i <- vif(data = data[, data.rank$variable]) %>%
      dplyr::filter(
        variable == data.rank[i, "variable"]
      ) %>%
      dplyr::pull(vif)

    #removing var if vif is above threshold
    if(vif.i > max.vif){

      #removing it from x.rank
      data.rank <- dplyr::filter(
        data.rank,
        variable != data.rank[i, "variable"]
      )

    }

  }

  removed.vars <- setdiff(
    x = colnames(data),
    y = data.rank$variable
  )

  #message
  if(verbose == TRUE){
    if(length(removed.vars) == 0){
      message("Variables are not collinear, nothing to do here.")
    }
  }

  #selected variables
  selected.variables <- preference.order[preference.order %in% setdiff(colnames(data), removed.vars)]

  if(verbose == TRUE){
    message(
      paste0(
        "\nThe selected variables are:\n",
        paste(selected.variables, collapse = "\n")
      )
    )
  }


  selected.variables

}


#' @rdname auto_vif
#' @export
vif <- function(data){

  out <- data.frame(
    diag(
      solve(
        cor(
          x = data,
          use = "complete.obs"
        ),
        tol = 0
      )
    ),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::rename(vif = 1) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::mutate(vif = round(vif, 3)) %>%
    dplyr::arrange(vif)

  out

}





