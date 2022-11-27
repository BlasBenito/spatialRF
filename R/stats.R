#' @title Standard error of the mean of a numeric vector
#' @description Computes the standard error of the mean of a numeric vector as `round(sqrt(var(x)/length(x)), 3)`
#' @param x A numeric vector.
#' @return A numeric value.
#' @details The function removes `NA` values before computing the standard error, and rounds the result to 3 decimal places.
#' @examples
#' if(interactive()){
#'
#'  standard_error(runif(10))
#'
#' }
#' @rdname standard_error
#' @export
standard_error <- function(x){
  x <- na.omit(x)
  x <- round(sqrt(var(x)/length(x)), 3)
  x
}

#' @title RMSE and normalized RMSE
#' @description Computes the rmse or normalized rmse (nrmse) between two numeric vectors of the same length representing observations and model predictions.
#' @param o Numeric vector with observations, must have the same length as `p`.
#' @param p Numeric vector with predictions, must have the same length as `o`.
#' @param normalization character, normalization method, Default: "rmse" (see Details).
#' @return Named numeric vector with either one or 5 values, as selected by the user.
#' @details The normalization methods go as follows:
#' \itemize{
#'   \item `"rmse"`: RMSE with no normalization.
#'   \item `"mean"`: RMSE dividied by the mean of the observations (rmse/mean(o)).
#'   \item `"sd"`: RMSE dividied by the standard deviation of the observations (rmse/sd(o)).
#'   \item `"maxmin"`: RMSE divided by the range of the observations (rmse/(max(o) - min(o))).
#'   \item "`iq"`: RMSE divided by the interquartile range of the observations (rmse/(quantile(o, 0.75) - quantile(o, 0.25)))
#' }
#' @examples
#' if(interactive()){
#'
#'  root_mean_squared_error(
#'    o = runif(10),
#'    p = runif(10)
#'    )
#'
#' }
#' @rdname root_mean_squared_error
#' @importFrom stats na.omit quantile
#' @export
root_mean_squared_error <- function(
    o,
    p,
    normalization = c(
      "rmse",
      "all",
      "mean",
      "sd",
      "maxmin",
      "iq"
      )
    ) {

  normalization <- match.arg(normalization)

  #computes rmse
  squared_sums <- sum((o - p)^2)
  mse <- squared_sums/length(o)
  rmse <- round(sqrt(mse), 4)
  names(rmse) <- "rmse"

  #computes nrmse
  if(normalization != "rmse"){

    #computing nrmse
    nrmse.sd <- rmse/sd(o)
    nrmse.mean <- rmse/mean(o)
    nrmse.maxmin <- rmse/ (max(o) - min(o))
    nrmse.iq <- rmse/ (quantile(o, 0.75) - quantile(o, 0.25))

    #building vector with nrmse values
    rmse <- c(rmse, nrmse.iq, nrmse.maxmin, nrmse.mean, nrmse.sd)
    names(rmse) <- c("rmse", "iq", "maxmin", "mean", "sd")

    #removing infinites
    rmse[!is.finite(rmse)] <- NA
    rmse <- na.omit(rmse)

    #if the user selects one particular option
    if(normalization != "all"){
      rmse <- rmse[names(rmse) == normalization]
    }
  }

  #return NA if no number is returned
  if(length(rmse) == 0){
    rmse <- NA
  }

  rmse
}




#' @title Statistical mode of a vector
#' @description Computes the mode of a numeric or character vector
#' @param x Numeric or character vector.
#' @return Statistical mode of `x`.
#' @examples
#' if(interactive()){
#'
#'  statistical_mode(c(10, 9, 10, 8))
#'
#' }
#' @rdname statistical_mode
#' @export
statistical_mode <- function(x){

  x.unique <- unique(x)

  x.unique[which.max(tabulate(match(x, x.unique)))]

}


#' @title Principal Components Analysis
#' @description Extracts all factors of a principal component analysis of a matrix or data frame. Just a convenient wrapper for [prcomp].
#' @usage
#' pca(
#'   x = NULL,
#'   colnames.prefix = "pca_factor"
#' )
#' @param x numeric matrix or data frame, Default: NULL
#' @param colnames.prefix character, name prefix for the output columns, Default: 'pca_factor'
#' @return A data frame with the PCA factors of `x`.
#' @details Columns in `x` with zero variance are removed before computing the PCA.
#' @examples
#' if(interactive()){
#'
#'  #load example distance matrix
#'  data(ecoregions_distance_matrix)
#'
#'  #PCA of the distance matrix
#'  out <- pca(x = ecoregions_distance_matrix)
#'  out
#'
#' }
#' @rdname pca
#' @importFrom stats prcomp var
#' @export
pca <- function(
    x = NULL,
    colnames.prefix = "pca_factor"
){

  #removing columns with zero variance
  x <- x[ , which(apply(x, 2, var) != 0)]

  #computing pca of distance matrix
  x.pca <- prcomp(x, scale. = TRUE)

  #getting pca factors
  x.pca.factors <- as.data.frame(x.pca$x)
  colnames(x.pca.factors) <- paste(
    colnames.prefix,
    seq(1, ncol(x.pca.factors)
    ),
    sep = "_"
  )

  #returning output
  x.pca.factors

}


#' @title Area under the ROC curve
#' @description Computes the area under the ROC curve in models with binary responses.
#' @param o Numeric vector with observations, must have the same length as `p`.
#' @param p Numeric vector with predictions, must have the same length as `o`.
#' @return Numeric, AUC value.
#' @examples
#' if(interactive()){
#'
#'  out <- auc(
#'    o = c(0, 0, 1, 1),
#'    p = c(0.1, 0.6, 0.4, 0.8)
#'    )
#'
#' }
#' @rdname auc
#' @export
auc <- function(o, p){

  #predicted values of the ones and the zeroes
  ones <- stats::na.omit(p[o == 1])
  zeros <- stats::na.omit(p[o == 0])

  #lengths of each vector
  n.ones <- length(ones)
  n.zeros <- length(zeros)

  #curve computation
  curve <- sum(rank(c(ones, zeros))[1:n.ones]) - (n.ones*(n.ones+1)/2)

  #area under the curve
  auc <- curve / (n.zeros * n.ones)

  auc

}

#' @title ROC curve
#' @description Computes tROC curve in models with binary responses.
#' @param o Numeric vector with observations, must have the same length as `p`.
#' @param p Numeric vector with predictions, must have the same length as `o`.
#' @return A data frame with the following columns:
#' \itemize{
#'   \item prediction: prediction thresholds from 0 to 1.
#'   \item true_positives: proportion of correctly predicted ones per threshold.
#'   \item false_positives: proportion of incorrectly predicted ones per threshold.
#'   \item false_negatives: proportion of correctly predicted zeroes per threshold.
#'   \item true_negatives: proportion of incorrectly predicted zeroes per threshold.
#'   \item sensitivity: probability of predicting a one per threshold.
#'   \item specificity: probability of predicting a zero per threshold.
#' }
#' @examples
#' if(interactive()){
#'
#'  out <- roc_curve(
#'    o = c(0, 0, 1, 1),
#'    p = c(0.1, 0.6, 0.4, 0.8)
#'    )
#'
#' }
#' @rdname roc_curve
#' @export
roc_curve <- function(o, p){

  #predicted values of the ones and the zeroes
  ones <- stats::na.omit(p[o == 1])
  zeros <- stats::na.omit(p[o == 0])

  #ROC data frame
  roc_df <- data.frame(
    prediction = seq(
      from = 0,
      to = 1,
      by = 0.1
    ),
    true_positives = rep(NA, 11),
    false_positives = rep(NA, 11),
    false_negatives = rep(NA, 11),
    true_negatives = rep(NA, 11)
  )

  #iterating to fill data frame
  for(i in 1:nrow(roc_df)){

    #component a of confusion matrix (true positives)
    roc_df[i, "true_positives"] <- round(length(
      ones[ones >= roc_df[i, "prediction"]]
      ) / length(p), 3)

    #component b of confusion matrix (false positives)
    roc_df[i, "false_positives"] <- round(length(
      zeros[zeros >= roc_df[i, "prediction"]]
    ) / length(p), 3)

    #component c of confusion matrix (false negatives)
    roc_df[i, "false_negatives"] <- round(length(
      ones[ones < roc_df[i, "prediction"]]
    ) / length(p), 3)

    #component d of confusion matrix (true negatives)
    roc_df[i, "true_negatives"] <- round(length(
      zeros[zeros < roc_df[i, "prediction"]]
    ) / length(p), 3)

  }

  #sensitivity (a/(a+c))
  roc_df$sensitivity <- round(roc_df$true_positives /
    (roc_df$true_positives + roc_df$false_negatives), 3)

  #especificity
  roc_df$specificity <- round(roc_df$true_negatives /
    (roc_df$true_negatives + roc_df$false_positives), 3)


  roc_df
}
