#' @title Normality test of a numeric vector
#' @description Applies a Shapiro-Wilks test to a numeric vector, and returns a list with the statistic W, its p-value, and a character string with the interpretation.
#' @param residuals Numeric vector, model residuals.
#' @return A list with four slots:
#' \itemize{
#'  /item `w` W statistic returned by [shapiro.test()].
#'  /item `p.value` p-value of the Shapiro test.
#'  /item `interpretation` Character vector, one of "x is normal", "x is not normal".
#'  /item `plot` A patchwork plot with the qq plot and the histogram of x.
#' }
#' @examples
#' if(interactive()){
#'
#'  residuals_test(residuals = runif(100))
#'
#' }
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_qq_line}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_freqpoly}},\code{\link[ggplot2]{geom_abline}}
#'  \code{\link[patchwork]{plot_annotation}}
#' @rdname normality
#' @export
#' @importFrom stats shapiro.test IQR
residuals_test <- function(
  residuals
  ){

  #declaring varialbes
  Predicted <- NULL
  Residuals <- NULL

  #list to store results
  y <- list()

  #normality of x
  shapiro.out <- shapiro.test(residuals)

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
