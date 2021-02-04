#' @title plot_evaluation
#' @description Plots the results of an evaluation performed with [rf_evaluate()].
#' @param x A model resulting from [rf_evaluate()]
#' @param verbose logical, if TRUE the plot is printed, Default: TRUE
#' @return A ggplot.
#' @rdname plot_evaluation
#' @export
#' @importFrom ggplot2 ggplot facet_wrap theme xlab ylab labs
plot_evaluation <- function(x, verbose = TRUE){

  #stop if no evaluation slot
  if(!inherits(x, "rf_evaluate")){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }


  #function to fix labels
  .pretty_labels <- function(x){

    x$performance.measure <- factor(
      x$performance.measure,
      levels = c("r.squared", "pseudo.r.squared", "rmse", "nrmse"),
      labels = c("R squared", "pseudo R squared" , "RMSE","NRMSE")
    )

    x$model <- factor(
      x$model,
      levels = rev(c("Full", "Training","Testing")),
      labels = rev(c("Full", "Training","Testing"))
    )

    x

  }

  #evaluation df in long format
  evaluation.df <- x$evaluation$per.model %>%
    tidyr::pivot_longer(
      cols = 1:4,
      names_to = "performance.measure",
      values_to = "performance.value"
    ) %>%
    as.data.frame() %>%
    .pretty_labels()

  #the plot
  p <- suppressMessages(ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = evaluation.df,
      ggplot2::aes(
        group = model,
        y = model,
        x = performance.value
      ),
      notch = TRUE,
    ) +
    ggplot2::facet_wrap(
      "performance.measure",
      scales = "free",
      drop = TRUE,
      ncol = 1
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(color = "Model") +
    ggplot2::ggtitle(
      paste0(
        "Evaluation results on ",
        length(x$evaluation$spatial.folds),
        " spatial folds."
      )
    ))

  if(verbose == TRUE){
    suppressMessages(print(p))
  }

}
