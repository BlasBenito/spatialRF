#' Generates PCA factors to summarize groups of variables
#'
#' @description
#'
#' The function selects what predictors to use as input for the PCA as follows:
#'
#' \itemize{
#'   \item If `predictor.groups` is provided, then the predictors listed each slot of the list are used to generate separate sets of PCA factors, and the respective names of this list are used to identify the output PCA factors. If `NULL`, then it goes to the next step.
#'   \item If `predictor.variable.names` is provided, then all predictors listed here are used as input for the PCA and the resulting PCA factors have the prefix "predictors_". If `NUL`, then it goes to the next step.
#'   \item All the columns in `data` are used as input for the PCA analysis and the resulting PCA factors have the prefix "predictors_".
#' }
#'
#' The function uses [fe_target_encoding()] to transform non-numeric columns into numeric. If there are non-numeric columns you wish to use as input for PCA factor computation, then `dependent.variable.name` must be provided.
#'
#' @param data (required, data frame, tibble, or sf data frame) Data frame with a response variable and a set of predictors. Default: `NULL`
#' @param dependent.variable.name (optional; character string) Character string with the name of the response variable. Must be in the column names of `data`. Only required when there are non-numeric variables in `predictor.groups`. Default: `NULL`
#' @param predictor.variable.names (optional; character vector with column names of `data`) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param predictor.groups (required, named list of character strings or vectors) List indicating the predictor groups for which PCA factors should be generated. The predictors to use on each group are either indicated by their actual names, or by a pattern. For example, `predictor.groups = list(a = c("x", "y", "z"))` generates the PCA factors `a_PC1`, `a_PC2`, and `a_PC3` From the variables `"x"`, `"y"`, and `"z"`. Also, a pattern can be used. For example, `predictor.groups = list(climate = "climate")` will generate PCA factors named with the prefix `"climate_"` for all the predictors containing the word "climate" in `predictor.variable.names`, or the column names of `data` if `predictor.variable.names = NULL`. If `NULL`, then all predictors in `predictor.variable.names` are used as group, all columns in `data`. Default: `NULL`
#' @param replace (optional, logical) If `TRUE`, the original groups of predictors are replaced in the output data frame with the PCA factors. Default: `FALSE`
#' @param max.explained.variance (optional, numeric) Maximum cumulative sum of the explained variance of the PCA factors selected. Default: `0.80`
#' @param verbose (optional, logical) If `TRUE`, the function prints message indicating what columns have been scaled. Default: `TRUE`
#'
#' @return The input data frame with PCA factors.
#' @export
#'
#' @examples
#' if(interactive()){
#' data(
#'   ecoregions_df,
#'   ecoregions_all_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#' df <- fe_pca_factors(
#'   data = ecoregions_df,
#'   dependent.variable.name = ecoregions_continuous_response,
#'   predictor.variable.names = ecoregions_all_predictors,
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
fe_pca_factors <- function(
    data = NULL,
    dependent.variable.name = NULL,
    predictor.variable.names = NULL,
    predictor.groups = NULL,
    replace = FALSE,
    max.explained.variance = 0.80,
    verbose = TRUE
){

  #check list pca factors, either character strings or column names that show up in predictor.variable.names

  if(is.null(data)){
    stop("Argument 'data' must be a data frame or tibble.")
  }

  #check if input is tibble
  if(tibble::is_tibble(data) == TRUE){
    return.tibble <- TRUE
  } else {
    return.tibble <- FALSE
  }

  #storing predictors to remove if replace == TRUE
  predictors.to.remove <- vector()

  if(is.null(predictor.groups)){

    if(!is.null(predictor.variable.names)){

      predictor.groups <- list(
        predictors = predictor.variable.names
      )

    } else {

      predictor.groups <- list(
        predictors = colnames(data)
      )

    }

  }

  #iterating over groups of variables
  for(predictor.group.i in names(predictor.groups)){

    variables.i <- predictor.groups[[predictor.group.i]]

    #variables.i might be column names
    if(length(variables.i) > 1){

      #variables.i are column names
      if(sum(variables.i %in% predictor.variable.names) == length(variables.i)){

        pca.columns <- variables.i

      } else {

        pca.columns <- variables.i[variables.i %in% predictor.variable.names]

      }

    }

    #variables.i is a pattern
    if(length(variables.i) == 1){

      pca.columns <- grep(
        pattern = variables.i,
        x = predictor.variable.names,
        value = TRUE
      )

      if(length(pca.columns) == 0){
        warning(
          paste0(
            "Variables with the word '",
            variables.i,
            "' were not found in 'predictor.variable.names', ignoring this pattern."
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

    #check if there are non-numeric predictors
    numeric.predictors <- colnames(pca.df.i)[sapply(pca.df.i, is.numeric)]

    non.numeric.predictors <- setdiff(
      x = pca.columns,
      y = numeric.predictors
    )

    #coerce categorical to numeric
    if(
      length(non.numeric.predictors) > 0 &
      !is.null(dependent.variable.name)
    ){

      pca.df.i[[dependent.variable.name]] <- data[[dependent.variable.name]]

      pca.df.i <- fe_target_encoding(
        data = pca.df.i,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = pca.columns,
        methods = "mean",
        replace = TRUE,
        verbose = verbose
      )

      pca.df.i[[dependent.variable.name]] <- NULL

    }

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
        "Predictors selected for group '",
        predictor.group.i,
        "': \n",
        paste0(
          pca.columns,
          collapse = "\n"
        )
      )

      message(
        "\nPCA factors of group '",
        predictor.group.i,
        "':"
      )

      message(
        paste0(
          capture.output(
            t(pca.i.summary)
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
