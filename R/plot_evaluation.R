#' @title plor_evaluation
#' @description Plots the results of an evaluation performed with [rf_evaluate()].
#' @param x A model resulting from [rf_evaluate()]
#' @param verbose logical, if TRUE the plot is printed, Default: TRUE
#' @return A ggplot.
#' @rdname plot_evaluation
#' @export
#' @importFrom ggplot2 ggplot facet_wrap geom_pointrange scale_color_viridis_d theme xlab ylab labs
plot_evaluation <- function(x, verbose = TRUE){


  if(!("evaluation" %in% names(x))){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  evaluation.df <- x$evaluation$aggregated

  evaluation.df$performance.measure <- factor(
    evaluation.df$performance.measure,
    levels = c("r.squared", "rmse","pseudo.r.squared","nrmse"),
    labels=c("R squared", "RMSE", "pseudo R squared","NRMSE")
  )

  evaluation.df$model <- factor(
    evaluation.df$model,
    levels = c("Full", "Training","Testing"),
    labels=c("Full", "Training","Testing")
  )

  p <- ggplot2::ggplot(data = evaluation.df) +
    ggplot2::facet_wrap(
      "performance.measure",
      scales = "free",
      drop = TRUE
    ) +
    ggplot2::geom_pointrange(
      aes(
        y = performance.measure,
        x = performance.mean,
        xmin = performance.mean + performance.se,
        xmax = performance.mean - performance.se,
        color = model
      ),
      position=position_dodge(width=0.3)
    ) +
    ggplot2::scale_color_viridis_d(end = 0.8) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y = element_blank()
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(color = "Model")

  if(verbose == TRUE){
    print(p)
  }

  return(p)

}
