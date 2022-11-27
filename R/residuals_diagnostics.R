#' @title Normality test of a numeric vector
#' @description Applies a Shapiro-Wilks test to a numeric vector, and plots the qq plot and the histogram.
#' @param residuals Numeric vector, model residuals.
#' @param predictions Numeric vector, model predictions.
#' @return A list with four slots:
#' \itemize{
#'  /item `w` W statistic returned by [shapiro.test()].
#'  /item `p.value` p-value of the Shapiro test.
#'  /item `interpretation` Character vector, one of "x is normal", "x is not normal".
#'  /item `plot` A patchwork plot with the qq plot and the histogram of x.
#' }
#' @details The function `shapiro.test()` has a hard limit of 5000 cases. If the model residuals have more than 5000 cases, then `sample(x = residuals, size = 5000)` is applied to the model residuals before the test.
#' @examples
#' if(interactive()){
#'
#'  residuals_diagnostics(
#'    residuals = runif(100),
#'    predictions = runif(100)
#'  )
#'
#'  }
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_qq_line}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_freqpoly}},\code{\link[ggplot2]{geom_abline}}
#'  \code{\link[patchwork]{plot_annotation}}
#' @rdname residuals_diagnostics
#' @export
#' @importFrom ggplot2 stat_qq stat_qq_line geom_histogram element_text
#' @importFrom patchwork plot_annotation
#' @importFrom stats shapiro.test IQR
residuals_diagnostics <- function(
  residuals,
  predictions
  ){

  #declaring variables
  Predicted <- NULL
  Residuals <- NULL

  #list to store results
  y <- list()

  #normality of x
  if(length(residuals) > 5000){
    shapiro.out <- shapiro.test(
      sample(
        x = residuals,
        size = 5000
        )
      )
  } else {
    shapiro.out <- shapiro.test(residuals)
  }


  #writing results to list
  names(shapiro.out$statistic) <- NULL
  y$shapiro.w <- shapiro.out$statistic
  y$p.value <- shapiro.out$p.value
  y$interpretation <- ifelse(
    shapiro.out$p.value > 0.05,
    "Residuals are normal",
    "Residuals are not normal"
  )

  #returning output
  y

}
