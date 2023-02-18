#' Generates PCA factors to summarize groups of variables
#'
#' @description
#'
#' The function selects what predictors to use as input for the PCA as follows:
#' If `predictor.groups` is provided, then the predictors listed each slot of the list are used to generate separate sets of PCA factors, and the respective names of this list are used to identify the output PCA factors. If `predictor.groups = NULL`, then all predictors in `predictors.names` are used as input for the PCA analysis.
#'
#' @param data (required, data frame, tibble, or sf data frame) Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param predictors.names (required; character vector with column names of `data`) Character vector with the names of numeric predictors. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param predictor.groups (required, named list of character strings or vectors) List indicating the predictor groups for which PCA factors should be generated. The predictors to use on each group are either indicated by their actual names, or by a pattern. For example, `predictor.groups = list(a = c("x", "y", "z"))` generates the PCA factors `a_PC1`, `a_PC2`, and `a_PC3` From the variables `"x"`, `"y"`, and `"z"`. Also, a pattern can be used. For example, `predictor.groups = list(climate = "climate")` will generate PCA factors named with the prefix `"climate_"` for all the predictors containing the word "climate" in `predictors.names`, or the column names of `data` if `predictors.names = NULL`. If `NULL`, then all predictors in `predictors.names` are used as group, all columns in `data`. Default: `NULL`
#' @param replace (optional, logical) If `TRUE`, the original groups of predictors are replaced in the output data frame with the PCA factors. Default: `FALSE`
#' @param max.explained.variance (optional, numeric) Maximum cumulative sum of the explained variance of the selected PCA factors. Default: `0.80`
#' @param verbose (optional, logical) If `TRUE`, the function prints message indicating what columns have been scaled. Default: `TRUE`
#'
#' @return The input data frame with PCA factors.
#'
#' @examples
#'
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_all_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#' df <- fe_pca_factors(
#'   data = ecoregions_df,
#'   predictors.names = ecoregions_all_predictors,
#'   predictor.groups = list(
#'     climate = "climate",
#'     landcover = "landcover",
#'     fragmentation = "fragmentation"
#'   ),
#'   replace = TRUE,
#'   max.explained.variance = 0.80,
#'   verbose = TRUE
#' )
#'
#' colnames(df)
#'
#' }
#' @export
#' @rdname fe_pca
#' @importFrom utils capture.output
fe_pca_factors <- function(
    data = NULL,
    predictors.names = NULL,
    predictor.groups = NULL,
    replace = FALSE,
    max.explained.variance = 0.80,
    verbose = TRUE
){

  `Explained variance` <- NULL

  if(max.explained.variance > 1){
    max.explained.variance <- 1
  }
  if(max.explained.variance < 0){
    max.explained.variance <- 0.25
  }

  #checking data
  ##############
  data <- check_data(
    data = data,
    drop.geometry = TRUE,
    verbose = verbose
  )

  predictors.names <- check_predictors_names(
    predictors.names = predictors.names,
    data = data,
    is.required = TRUE,
    numeric.only = TRUE,
    na.allowed = FALSE,
    zero.variance.allowed = FALSE,
    verbose = verbose
  )

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #CHECK PREDICTORS GROUPS
  if(is.null(predictor.groups)){

      if(verbose == TRUE){
        message("Argument 'predictor.groups' was not provided. PCA factors from all columns in 'predictors.names' will be generated.")
      }

      predictor.groups <- list(
        predictors = predictors.names
      )

  }

  #storing predictors to remove if replace == TRUE
  predictors.to.remove <- vector()

  #iterating over groups of variables
  for(predictor.group.i in names(predictor.groups)){

    variables.i <- predictor.groups[[predictor.group.i]]

    #variables.i might be column names
    if(length(variables.i) > 1){

      #variables.i are column names
      if(sum(variables.i %in% predictors.names) == length(variables.i)){

        pca.columns <- variables.i

      } else {

        pca.columns <- variables.i[variables.i %in% predictors.names]

      }

    }

    #variables.i is a pattern
    if(length(variables.i) == 1){

      pca.columns <- grep(
        pattern = variables.i,
        x = predictors.names,
        value = TRUE
      )

      if(length(pca.columns) == 0){
        warning(
          paste0(
            "Variables with the word '",
            variables.i,
            "' were not found in 'predictors.names', ignoring this pattern."
          )
        )
      }

    }

    #store predictors to remove
    predictors.to.remove <- c(
      predictors.to.remove,
      pca.columns
    )

    #df for pca
    pca.df.i <- data %>%
      dplyr::select(
        dplyr::all_of(pca.columns)
      )

    #pca result
    pca.i <- stats::prcomp(
      x = pca.df.i,
      scale = TRUE
    )

    #pca summary
    pca.i.summary <- summary(pca.i)$importance %>%
      as.data.frame() %>%
      round(2)

    #subset rows
    pca.i.summary <- pca.i.summary[2:3, ]

    #changing column names
    colnames(pca.i.summary) <- paste0(
      predictor.group.i,
      "_",
      colnames(pca.i.summary)
    )

    rownames(pca.i.summary) <- c(
      "Explained variance",
      "Sum"
    )

    #pca factors
    pca.i.factors <- pca.i$x %>%
      as.data.frame()

    colnames(pca.i.factors) <- paste0(
      predictor.group.i,
      "_",
      colnames(pca.i.factors)
    )

    #selected pca factors
    pca.i.factors.selected <- colnames(pca.i.summary)[which(pca.i.summary[2, ] <= max.explained.variance)]

    #subset pca.i.factors
    pca.i.factors <- pca.i.factors[, pca.i.factors.selected, drop = FALSE]

    #merge with data
    data <- cbind(
      data,
      pca.i.factors
    )

    #showing message
    if(verbose == TRUE){

      message(
        "\n### group: ",
        predictor.group.i,
        " ###\n"
      )

      message(
        "Selected predictors: \n",
        paste0(
          pca.columns,
          collapse = "\n"
        )
      )

      message(
        "\nPCA factors: \n"
      )

      message(
        paste0(
          utils::capture.output(
            t(pca.i.summary) %>%
              as.data.frame() %>%
              dplyr::filter(
                `Explained variance` > 0
              )
          ),
          collapse = "\n"
        )
      )

      message(
        "\nSelected PCA factors: \n",
        paste0(
          colnames(pca.i.factors),
          collapse = "\n"
        ),
        "\n"
      )

    }

  }

  #removing predictors
  if(replace == TRUE){

    data <- data %>%
      dplyr::select(
        -dplyr::all_of(predictors.to.remove)
      )

  }

  #return tibble
  if(return.tibble == FALSE){
    data <- as.data.frame(data)
  } else {
    data <- tibble::as_tibble(data)
  }

  data

}
