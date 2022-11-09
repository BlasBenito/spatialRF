#' @title Multicollinearity reduction via Variance Inflation Factor
#'
#' @description
#'
#' NOTE: It is highly recommended to run [auto_cor()] before [auto_vif()].
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all the remaining variables of the input data set as predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' The function `auto_vif()` applies a recursive algorithm to remove variables with a VIF higher than a given threshold (defined by the argument `max.vif`). However, `auto_vif()` allows the user to define preference selection order via the argument `preference.order`.
#'
#' If `preference.order` is, for example, `c("a", "b", "c")`, `max.vif` equals 5, and the VIFs of these variables are 15, 10, and 5, then variable `"b"` is removed instead of `"a"`.
#'
#' The argument `preference.order` allows the user to "protect" variables that might be interesting or even required for the given analysis.
#'
#' If `preference.order` is not provided, then the predictors are ranked from lower to higher VIF, and removed one by one until their VIF is lower than `max.vif`.
#'
#' @param data (required; data.frame or tibble) A data frame or tibble, or the result of [auto_cor()]. Default: `NULL`.
#' @param predictor.variable.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. If `NULL`, all the columns in data are used. Default: `NULL`
#' @param preference.order (optional, character vector) Character vector indicating the preference order to protect variables from elimination.  Predictors not included in this argument are ranked by their VIFs. Default: `NULL`.
#' @param max.vif (optional, numeric) Numeric between 2.5 and 10 defining the maximum VIF allowed in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: 5.
#' @param verbose (optional, logical) Logical. if `TRUE`, `auto_vif()` prints messages describing its operations on the input data. Default:: `TRUE`
#' @return List with three slots:
#' \itemize{
#'   \item `vif`: data frame with the names of the selected variables and their respective VIF scores.
#'   \item `selected.variables`: character vector with the names of the selected variables.
#'   \item `selected.variables.df`: data frame with the selected variables.
#'  }
#' @seealso [auto_cor()]
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_predictor_variable_names
#'   )
#'
#'#on a data frame
#'out <- auto_vif(
#'  data = ecoregions_df,
#'  predictor.variable.names = ecoregions_predictor_variable_names
#'  )
#'
#'#getting out the vif data frame
#'out$vif
#'
#'#getting the names of the selected variables
#'out$selected.variables
#'
#'#getting the data frame of selected variables
#'out$selected.variables.df
#'
#'#with preference order
#'  out <- auto_vif(
#'    data = ecoregions_df,
#'    predictor.variable.names = ecoregions_predictor_variable_names,
#'    preference.order = ecoregions_predictor_variable_names[1:5],
#'  )
#'
#'#with pipes
#'  out <- auto_cor(
#'    data = ecoregions_df,
#'    predictor.variable.names = ecoregions_predictor_variable_names
#'  ) %>%
#'  auto_vif()
#'
#'  #vif function
#'  vif(data = ecoregions_df[, ecoregions_predictor_variable_names])
#'
#'
#' }
#' @rdname auto_vif
#' @importFrom magrittr `%>%`
#' @importFrom stats cor
#' @export
auto_vif <- function(
    data = NULL,
    predictor.variable.names = NULL,
    preference.order = NULL,
    max.vif = 5,
    verbose = TRUE
){

  variable <- NULL

  if(is.null(data)){
    stop("Argument 'data' is required.")
  }

  #subsetting data
  if(!is.null(predictor.variable.names)){
    predictor.variable.names <- intersect(
      x = predictor.variable.names,
      y = colnames(data)
    )
    data <- data[, predictor.variable.names]
  }

  if(max.vif > 10){
    if(verbose == TRUE){
      warning("Argument max.vif is higher than 10.. Recommended values for this argument are in the range [2.5, 10]")
    }
  }

  if(max.vif < 0){
    max.vif <- 2.5
    if(verbose == TRUE){
      message("max.vif is negative, setting it to 0.")
    }
  }

  if(is.null(max.vif)){
    max.vif <- Inf
    if(verbose == TRUE){
      message("max.vif is NULL, setting it to Inf. All variables will be selected.")
    }
  }

  if(inherits(data, "variable_selection") == TRUE){
    data <- data$selected.variables.df
    preference.order <- colnames(data)
  }

  #removing non-numeric and zero variance columns

  #finding and removing non-numeric columns
  non.numeric.columns <- colnames(data)[!sapply(data, is.numeric)]
  if(length(non.numeric.columns) > 0){
    if(verbose == TRUE){
      message(
        "These columns are non-numeric and will be removed: ",
        paste(
          non.numeric.columns,
          collapse = ", "
        )
      )
    }
    data <- data[, !(colnames(data) %in% non.numeric.columns), drop = FALSE]
  }

  #finding zero variance columns
  zero.variance.columns <- colnames(data)[round(apply(data, 2, var), 6) == 0]
  if(length(zero.variance.columns) > 0){
    if(verbose == TRUE){
      message(
        "These columns have zero variance and might cause issues: ",
        paste(
          zero.variance.columns,
          collapse = ", "
        )
      )
    }
  }

  #auto preference order by vif
  preference.order.auto <- vif(data) %>%
    dplyr::pull(variable)

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

  #vector to store variables to remove
  removed.vars <- vector()

  #iterating through reversed preference order
  for(i in seq(from = nrow(data.rank), to = 2)){

    vif.i <- vif(data = data[, data.rank$variable]) %>%
      dplyr::filter(
        variable == data.rank[i, "variable"]
      ) %>%
      dplyr::pull(vif)

    #removing var if vif is above threshold
    if(vif.i > max.vif){

      #adding it to removed.vars
      removed.vars <- c(removed.vars, data.rank[i, "variable"])

      #removing it from x.rank
      data.rank <- dplyr::filter(
        data.rank,
        variable != data.rank[i, "variable"]
      )

    }

  }

  #message
  if(verbose == TRUE){
    if(length(removed.vars) != 0){
      message(
        paste0(
          "[auto_vif()]: Removed variables: \n",
          paste0(
            removed.vars,
            collapse = ", \n"
          )
        )
      )
    } else {
      message("[auto_vif()]: Variables are not collinear, nothing to do here.")
    }
  }

  #selected variables
  selected.variables <- preference.order[preference.order %in% setdiff(colnames(data), removed.vars)]
  selected.variables.df <- data[, selected.variables, drop = FALSE]

  if(verbose == TRUE){
    message(
      paste0(
        "The selected variables are:\n",
        paste(selected.variables, collapse = "\n")
      )
    )
  }

  #final vif.df
  vif.df <- vif(data = selected.variables.df)

  #output list
  output.list <- list()
  output.list$vif <- vif.df[, c("variable", "vif")]
  output.list$selected.variables <- selected.variables
  output.list$selected.variables.df <- data[, selected.variables, drop = FALSE]

  class(output.list) <- "variable_selection"

  output.list

}


#' @rdname auto_vif
#' @export
vif <- function(data){

  #finding and removing non-numeric columns
  non.numeric.columns <- colnames(data)[!sapply(data, is.numeric)]
  if(length(non.numeric.columns) > 0){
    warning(
      "These columns are non-numeric and will be removed: ",
      paste(
        non.numeric.columns,
        collapse = ", "
      )
    )
    data <- data[, !(colnames(data) %in% non.numeric.columns)]
  }

  #finding zero variance columns
  zero.variance.columns <- colnames(data)[round(apply(data, 2, var), 4) == 0]
  if(length(zero.variance.columns) > 0){
    warning(
      "These columns have zero variance and might cause issues: ",
      paste(
        zero.variance.columns,
        collapse = ", "
      )
    )
  }

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





