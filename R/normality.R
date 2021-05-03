#' @title Normality test of a numeric vector
#' @description Applies a Shapiro-Wilks test to a numeric vector, and plots the qq plot and the histogram.
#' @param x Numeric vector.
#' @return A list with four slots:
#' \itemize{
#'  /item `w` W statistic returned by [shapiro.test()].
#'  /item `p.value` p-value of the Shapiro test.
#'  /item `interpretation` Character vector, one of "x is normal", "x is not normal".
#'  /item `plot` A patchwork plot with the qq plot and the histogram of x.
#' }
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  normality(x = runif(100))
#'
#'  }
#' }
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_qq_line}},\code{\link[ggplot2]{ggtheme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_freqpoly}},\code{\link[ggplot2]{geom_abline}}
#'  \code{\link[patchwork]{plot_annotation}}
#' @rdname normality
#' @export
#' @importFrom ggplot2 stat_qq stat_qq_line geom_histogram
#' @importFrom patchwork plot_annotation
normality <- function(x){

  #list to store results
  y <- list()

  #normality of x
  shapiro.out <- shapiro.test(x)

  #writing results to list
  names(shapiro.out$statistic) <- NULL
  y$shapiro.w <- shapiro.out$statistic
  y$p.value <- shapiro.out$p.value
  y$interpretation <- ifelse(
    shapiro.out$p.value > 0.05,
    "x is normal",
    "x is not normal"
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
  p1 <- ggplot2::ggplot(data = as.data.frame(x)) +
    ggplot2::aes(sample = x) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(col = "red4") +
    ggplot2::theme_bw() +
    ggplot2::ylab("x")

  #computing optimal binwidth for histogram
  #using the max of the Freedman-Diaconist rule
  #or 1/100th of the data range
  bw <- max(
    2 * IQR(x) / length(x)^(1/3),
    (range(x)[2] - range(x)[1]) / 100
    )

  #histogram
  p2 <- ggplot2::ggplot(data = as.data.frame(x)) +
    ggplot2::aes(x = x) +
    ggplot2::geom_histogram(
      binwidth = bw,
      fill = "gray95",
      color = "gray20"
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(
      xintercept = mean(x),
      col = "red4",
      size = 0.7,
      linetype = "dashed"
    )

  #final plot
  y$plot <- p1 + p2 + patchwork::plot_annotation(
    title = plot.title
  )

  #returning output
  y

}
