#' @title auto_vif
#' @description Selects variables within a training dataframe that are not linear combinations of other variables by using the variance inflation factor (VIF). This function has two modes of operation:
#' \itemize{
#' \item 1. When the argument \code{preference.order} is \code{NULL}, the function removes on on each iteration the variable with the highest VIF until all VIF values are lower than 10. This operation is performed by the hidden function \code{.select_by_max_vif}.
#' \item 2. When \code{preference.order} is provided, the variables are selected by giving them priority according to their order in \code{preference.order}. If there are variables not in \code{preference.order}, these are selected as in option 1, and finally, all variables are put together and selected by giving priority to the ones in \code{preference.order}. This method preserves the variables desired by the user as much as possible.
#' }
#' @param x a data frame
#' @param preference.order a character vector with columns names of x ordered by the user preference, Default: NULL
#' @param vif.threshold numeric between 5 and 10 defining the selection threshold for the VIF analysis. Default: 5.
#' @param verbose when TRUE, describes the function operations to the user.
#' @return list with two slots:
#' \itemize{
#'   \item{vif}{data frame with the names of the selected variables and their respective VIF scores}
#'   \item{selected.variables}{character vector with the names of the selected variables}
#'  }
#' @examples
#' \dontrun{
#' if(interactive()){
#'  out <- auto_vif(x = plant_richness_df[, 5:20])
#'  }
#' }
#' @rdname auto_vif
#' @importFrom magrittr `%>%`
#' @importFrom stats cor
#' @export
auto_vif <- function(
  x,
  preference.order = NULL,
  vif.threshold = 5,
  verbose = FALSE
){

  #message
  if(verbose == TRUE){cat("Removed variables: \n")}

  #AND preference.order IS NOT PROVIDED
  if(is.null(preference.order)){

    #OPTION 3: SELECT BY MAX VIF
    output.list <- .select_by_max_vif(
      x = x,
      vif.threshold = vif.threshold,
      verbose = verbose
    )

  } else {

    #OPTION 2: preference.order IS PROVIDED

    #getting only preference.order in colnames(x)
    preference.order <- preference.order[preference.order %in% colnames(x)]

    #selecting by preference
    output.list <- .select_by_preference(
      x = x,
      preference.order = preference.order,
      vif.threshold = vif.threshold,
      verbose = verbose
    )

    #if there are variables outside of preference.order
    if(sum(preference.order %in% colnames(x)) != ncol(x)){

      #selecting by max vif (variables not in preference.order)
      output.list.by.max.vif <- .select_by_max_vif(
        x = x[, !(colnames(x) %in% preference.order)],
        vif.threshold = vif.threshold,
        verbose = verbose
      )

      #merging selected.vars
      selected.vars <- c(
        output.list$selected.variables,
        output.list.by.max.vif$selected.variables
      )

      #vif by preference again
      output.list <- .select_by_preference(
        x = x,
        preference.order = selected.vars,
        vif.threshold = vif.threshold,
        verbose = verbose
      )

    }

  }

  #message
  if(verbose == TRUE){cat("Done! \n")}

  #returning output
  output.list

}



#' @export
.vif_to_df <- function(x){

  #defining global variable
  vif <- NULL

  #turns vif output into tidy df
  df <-
    data.frame(
      diag(solve(cor(x))),
      stringsAsFactors = FALSE
    ) %>%
    dplyr::rename(vif = 1) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::arrange(dplyr::desc(vif))

  df
}


#' @export
.select_by_max_vif <- function(x, vif.threshold, verbose){

  #global variables
  vif <- variable <- NULL

  #initializing selected vars
  selected.vars <- colnames(x)

  #computes vif
  repeat {

    #computes vif
    vif.df <- .vif_to_df(x = x[, selected.vars])

    #selects variables with vif lower than 5
    var.to.remove <-
      vif.df %>%
      dplyr::filter(vif > vif.threshold) %>%
      dplyr::filter(vif == max(vif)) %>%
      dplyr::slice(1) %>%
      dplyr::select(variable) %>%
      as.character()

    #if the first row contains a vif higher than 5
    if(var.to.remove != "character(0)"){

      #updates select.cols
      if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
      selected.vars <- selected.vars[selected.vars != var.to.remove]

      #stops if there are less than 3 vars left
      if(length(selected.vars) == 1){
        break
      }

    } else {
      break
    } #end of "if(var.to.remove != "character(0)")"

  } #end of repeat

  #final vif.df
  vif.df <- .vif_to_df(x = x[, selected.vars])

  #output list
  output.list <- list()
  output.list$vif <- vif.df
  output.list$selected.variables <- selected.vars

  output.list

}


#' @export
.select_by_preference <- function(
  x,
  preference.order,
  vif.threshold,
  verbose
){

  #subsets to the variables already available in x
  preference.order <- preference.order[preference.order %in% colnames(x)]

  #initiating selected vars
  selected.vars <- preference.order[1]

  #iterates through preference order
  for(i in 2:length(preference.order)){

    #new.var
    new.var <- preference.order[i]

    #computes vif
    vif.df <- .vif_to_df(x = x[, c(selected.vars, new.var)])

    #if vif of new.var lower than vif.threshold, keep it
    if(max(vif.df$vif) <= vif.threshold){

      selected.vars <- c(selected.vars, new.var)

    } else {

      #message
      if(verbose == TRUE){cat(paste(new.var, ", ", sep = ""))}
      next

    }

  }

  #final vif.df
  vif.df <- .vif_to_df(x = x[, selected.vars])

  #output list
  output.list <- list()
  output.list$vif <- vif.df[, c("variable", "vif")]
  output.list$selected.variables <- selected.vars

  output.list

}

