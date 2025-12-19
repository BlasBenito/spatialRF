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
#'
#' data(plants_rf)
#'
#' y <- residuals_diagnostics(
#'   residuals = get_residuals(plants_rf),
#'   predictions = get_predictions(plants_rf)
#' )
#' y
#'
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_qq_line}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_freqpoly}},\code{\link[ggplot2]{geom_abline}}
#'  \code{\link[patchwork]{plot_annotation}}
#' @rdname residuals_diagnostics
#' @family spatial_analysis
#' @export
#' @importFrom ggplot2 stat_qq stat_qq_line geom_histogram element_text
#' @importFrom patchwork plot_annotation
#' @importFrom stats shapiro.test IQR
residuals_diagnostics <- function(
  residuals,
  predictions
) {
  #declaring variables
  Predicted <- NULL
  Residuals <- NULL

  #list to store results
  y <- list()

  #normality of x
  if (length(residuals) > 5000) {
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

  #plot title
  plot.title <- paste0(
    "Shapiro W = ",
    round(shapiro.out$statistic, 3),
    "; p-value = ",
    round(shapiro.out$p.value, 4),
    "; ",
    y$interpretation
  )

  #qqplot
  p1 <- ggplot2::ggplot(data = as.data.frame(residuals)) +
    ggplot2::aes(sample = residuals) +
    ggplot2::stat_qq(alpha = 0.7) +
    ggplot2::stat_qq_line(
      col = "red4",
      linewidth = 0.7,
      linetype = "dashed"
    ) +
    ggplot2::theme_bw() +
    ggplot2::ylab("Residuals") +
    ggplot2::xlab("Theoretical")

  #computing optimal binwidth for histogram
  #using the max of the Freedman-Diaconist rule
  #or 1/100th of the data range
  bw <- max(
    2 * stats::IQR(residuals) / length(residuals)^(1 / 3),
    (range(residuals)[2] - range(residuals)[1]) / 100
  )

  #histogram
  p2 <- ggplot2::ggplot(data = as.data.frame(residuals)) +
    ggplot2::aes(x = residuals) +
    ggplot2::geom_histogram(
      binwidth = bw,
      fill = "gray95",
      color = "gray20"
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(
      xintercept = median(residuals),
      col = "red4",
      linewidth = 0.7,
      linetype = "dashed"
    ) +
    ggplot2::ylab("Count") +
    ggplot2::xlab("Residuals")

  #residuals vs predictions
  p3 <- ggplot2::ggplot(
    data = data.frame(
      Residuals = residuals,
      Predicted = predictions
    )
  ) +
    ggplot2::aes(
      x = Predicted,
      y = Residuals
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "red4",
      linewidth = 0.7
    ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Residuals vs. predictions") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  #final plot
  y$plot <- (p1 + p2) /
    p3 +
    patchwork::plot_annotation(
      title = plot.title,
      theme = ggplot2::theme(plot.title = element_text(hjust = 0.5))
    )

  #returning output
  y
}
