#' @title Autommated multicollinearity reduction
#'
#' @description Reduces multicollinearity in a set of predictors by sequentially applying [mc_auto_cor()] and [mc_auto_vif()].
#'
#' The function [mc_auto_cor()] applies a recursive algorithm to remove variables with a Pearson correlation with another variable higher than a given threshold (defined by the argument `max.cor`).  When two variables are correlated above this threshold, the one with the highest sum of R-squared with all the other variables is removed.
#'
#' The function [mc_auto_vif()] applyies a Variance Inflation Factor (VIF) analysis to reduce multicollinearity. The VIF for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all the remaining variables of the input data set as predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model". The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' The argument `preference.order` allows the user to "protect" variables that might be interesting or even required for the given analysis.
#'
#' If `preference.order` is not provided, then the predictors are ranked from lower to higher sum of R-squared with the other preodictors, and removed one by one until the maximum R-squared of the correlation matrix is lower than `max.cor` and the maximum VIF is below `max.vif`.
#'
#' If there are categorical variables named in `predictor.variable.names` and `dependent.variable.name` is provided, then the function applies [fe_target_encoding()] with the method "mean" to transform the categorical variables into numeric. If a categorical variable is selected, then its original categorical values are returned.
#'
#'
#' @param data (required; data.frame or tibble) A data frame, tibble, or sf. Default: `NULL`.
#' @param dependent.variable.name (optional; character string) Name of the dependent variable. Required when there are categorical variables within `predictor.variable.names`. Default: `NULL`
#' @param predictor.variable.names (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. If `NULL`, all the columns in data except `dependent.variable.name` are used. Default: `NULL`
#' @param preference.order (optional, character vector) Character vector indicating the preference order to protect variables from elimination.  Predictors not included in this argument are ranked by their VIFs. Default: `NULL`.
#' @param max.cor (optional; numeric) Numeric between 0 and 1, with recommended values between 0.5 and 0.9. Maximum Pearson correlation between any pair of the selected variables. Default: `0.75`
#' @param max.vif (optional, numeric) Numeric between 2.5 and 10 defining the maximum VIF allowed in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: 5.
#' @param verbose (optional, logical) Logical. if `TRUE`, describes the function operations to the user. Default: `TRUE`
#'
#' @return Character vector with the names of uncorrelated predictors.
#'
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions_df,
#'   ecoregions_continuous_response,
#'   ecoregions_all_predictors
#' )
#'
#'
#'
#' }
mc_auto <- function(
    data = NULL,
    predictor.variable.names = NULL,
    dependent.variable.name = NULL,
    preference.order = NULL,
    max.cor = 0.75,
    max.vif = 5,
    verbose = TRUE
){

  #applying auto_cor
  selected.predictors <- mc_auto_cor(
    data = data,
    predictor.variable.names = predictor.variable.names,
    dependent.variable.name = dependent.variable.name,
    max.cor = max.cor,
    verbose = FALSE
  )

  #applying auto_vif
  selected.predictors <- mc_auto_vif(
    data = data,
    predictor.variable.names = selected.predictors,
    dependent.variable.name = dependent.variable.name,
    max.vif = max.vif,
    verbose = FALSE
  )

  selected.predictors


}
