#' @title Multicollinearity reduction via Variance Inflation Factor
#'
#' @description Selects predictors that are not linear combinations of other predictors by using computing their variance inflation factors (VIF). Allows the user to define an order of preference for the selection of predictors. \strong{Warning}: variables in `preference.order` not in `colnames(x)`, and non-numeric columns are removed from `x` and `preference.order`. The same happens with rows having NA values ([na.omit()] is applied). The function issues a message if zero-variance columns are found.
#' @usage
#' auto_vif(
#'   x = NULL,
#'   preference.order = NULL,
#'   vif.threshold = 5,
#'   verbose = TRUE
#' )
#' @param x A data frame with predictors or the result of [auto_cor()]. Default: `NULL`.
#' @param preference.order Character vector indicating the user's order of preference to keep variables. Predictors not included in this argument are ranked by their VIF. Default: `NULL`.
#' @param vif.threshold Numeric between 0 and 10 defining the selection threshold for the VIF analysis. Higher numbers result in a more relaxed variable selection. Default: 5.
#' @param verbose Logical. if `TRUE`, describes the function operations to the user. Default:: `TRUE`
#' @return List with three slots:
#' \itemize{
#'   \item `vif`: data frame with the names of the selected variables and their respective VIF scores.
#'   \item `selected.variables`: character vector with the names of the selected variables.
#'   \item `selected.variables.df`: data frame with the selected variables.
#'  }
#' @details
#'This function has two modes of operation:
#' \itemize{
#' \item 1. When the argument `preference.order` is `NULL`, the function removes on each iteration the variable with the highest VIF until all VIF values are lower than `vif.threshold`.
#' \item 2. When `preference.order` is provided, the variables are selected by giving them priority according to their order in `preference.order`. If there are variables not in `preference.order`, these are selected as in option 1. Once both groups of variables have been processed, all variables are put together and selected by giving priority to the ones in `preference.order`. This method preserves the variables desired by the user as much as possible.
#' }
#'  Can be chained together with [auto_cor()] through pipes, see the examples below.
#' @seealso [auto_cor()]
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_predvar_names
#'   )
#'
#'#on a data frame
#'out <- auto_vif(x = ecoregions_df[, ecoregions_predvar_names])
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
#'#on the result of auto_cor
#'out <- auto_cor(x = ecoregions_df[, ecoregions_predvar_names])
#'out <- auto_vif(x = out)
#'
#'#with pipes
#'out <- ecoregions_df[, ecoregions_predvar_names] %>%
#'  auto_cor() %>%
#'  auto_vif()
#'
#' }
#' @rdname auto_vif
#' @importFrom magrittr `%>%`
#' @importFrom stats cor
#' @export
auto_vif <- function(
    x = NULL,
    preference.order = NULL,
    vif.threshold = 5,
    verbose = TRUE
){

  variable <- NULL

  if(vif.threshold < 0){
    vif.threshold <- 0
    if(verbose == TRUE){
      message("vif.threshold is negative, setting it to 0.")
    }
  }

  if(vif.threshold > 10){
    vif.threshold <- 10
    if(verbose == TRUE){
      message("vif.threshold is larger than 10, setting it to 10.")
    }
  }

  if(inherits(x, "variable_selection") == TRUE){
    x <- x$selected.variables.df
    preference.order <- colnames(x)
  }

  #coercing to data frame
  #coerce to data frame if tibble
  if(inherits(x, "tbl_df") | inherits(x, "tbl")){
    x <- as.data.frame(x)
  }

  #removing non-numeric and zero variance columns
  #removing NA
  x <- na.omit(x)

  #finding and removing non-numeric columns
  non.numeric.columns <- colnames(x)[!sapply(x, is.numeric)]
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
    x <- x[, !(colnames(x) %in% non.numeric.columns), drop = FALSE]
  }

  #finding zero variance columns
  zero.variance.columns <- colnames(x)[round(apply(x, 2, var), 6) == 0]
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
  preference.order.auto <- .vif_to_df(x) %>%
    dplyr::pull(variable)

  #AND preference.order IS NOT PROVIDED
  if(is.null(preference.order)){

    preference.order <- preference.order.auto

  } else {

    #subset preference.order to colnames(x)
    preference.order <- preference.order[preference.order %in% colnames(x)]

    #if there are variables not in preference.order, add them in the order of preference.order.auto
    if(length(preference.order) < ncol(x)){

      not.in.preference.order <- colnames(x)[!(colnames(x) %in% preference.order)]
      preference.order <- c(preference.order, preference.order.auto[preference.order.auto %in% not.in.preference.order])

    }

  }

  #order x according to preference order
  x <- x[, preference.order]

  #rank of interest
  x.rank <- data.frame(
    variable = colnames(x),
    rank = 1:ncol(x)
  )

  #vector to store variables to remove
  removed.vars <- vector()

  #iterating through reversed preference order
  for(i in seq(from = nrow(x.rank), to = 2)){

    vif.i <- .vif_to_df(x = x[, x.rank$variable]) %>%
      dplyr::filter(
        variable == x.rank[i, "variable"]
      ) %>%
      dplyr::pull(vif)

    #removing var if vif is above threshold
    if(vif.i > vif.threshold){

      #adding it to removed.vars
      removed.vars <- c(removed.vars, x.rank[i, "variable"])

      #removing it from x.rank
      x.rank <- dplyr::filter(
        x.rank,
        variable != x.rank[i, "variable"]
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
      message("[auto_vif()]: Variables are not collinear.")
    }
  }

  #selected variables
  selected.variables <- setdiff(colnames(x), removed.vars)
  selected.variables.df <- x[, selected.variables, drop = FALSE]

  if(verbose == TRUE){
    message(
      paste0(
        "The selected variables are:\n",
        paste(selected.variables, collapse = "\n")
      )
    )
  }

  #final vif.df
  vif.df <- .vif_to_df(x = selected.variables.df)

  #output list
  output.list <- list()
  output.list$vif <- vif.df[, c("variable", "vif")]
  output.list$selected.variables <- selected.variables
  output.list$selected.variables.df <- x[, selected.variables, drop = FALSE]

  class(output.list) <- "variable_selection"

  output.list

}


#' @export
.vif_to_df <- function(x){

  #defining global variable
  vif <- NULL

  #turns vif output into tidy df
  df <- data.frame(
      diag(solve(cor(x))),
      stringsAsFactors = FALSE
    ) %>%
    dplyr::rename(vif = 1) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::mutate(vif = round(vif, 3)) %>%
    dplyr::arrange(vif) %>%
    as.data.frame()

  df
}




