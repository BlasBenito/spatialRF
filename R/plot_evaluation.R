#' @title Plots the results of a spatial cross-validation
#' @description Plots the results of an spatial cross-validation performed with [rf_evaluate()].
#' @usage
#' plot_evaluation(
#'   x,
#'   verbose = TRUE,
#'   notch = TRUE
#' )
#' @param x A model resulting from [rf_evaluate()].
#' @param verbose Logical, if `TRUE` the plot is printed. Default: `TRUE`
#' @param notch Logical, if `TRUE`, boxplot notches are plotted. Default: `TRUE`
#' @return A ggplot.
#' @seealso [rf_evaluate()], [get_evaluation()], [print_evaluation()].
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = c(0, 1000, 2000),
#'   verbose = FALSE
#' )
#'
#' rf.model <- rf_evaluate(
#'   model = rf.model,
#'   xy = plant_richness_df[, c("x", "y")],
#'   n.cores = 1
#' )
#'
#' plot_evaluation(rf.model)
#'
#' }
#' }
#'
#' @rdname plot_evaluation
#' @export
#' @importFrom ggplot2 ggplot facet_wrap theme xlab ylab labs
plot_evaluation <- function(x, verbose = TRUE, notch = TRUE){

  #declaring variable because of check BS
  performance.value <- NULL
  model <- NULL

  #stop if no evaluation slot
  if(!inherits(x, "rf_evaluate")){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  #getting plotting df
  n.spatial.folds <- length(x$evaluation$spatial.folds)
  x <- x$evaluation$per.fold.long

  #prettier labels
  x[x$metric == "r.squared", "metric"] <- "R squared"
  x[x$metric == "pseudo.r.squared", "metric"] <- "pseudo R squared"
  x[x$metric == "rmse", "metric"] <- "RMSE"
  x[x$metric == "nrmse", "metric"] <- "NRMSE"

  #ordering models
  x$model <- factor(x$model,levels = c("Testing", "Training", "Full"))

  #the plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = x,
      ggplot2::aes(
        group = model,
        y = model,
        x = value,
        fill = model
      ),
      notch = notch,
    ) +
    ggplot2::facet_wrap(
      "metric",
      scales = "free",
      drop = TRUE,
      ncol = 1
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::scale_fill_viridis_d(end = 0.8, alpha = 0.75) +
    ggplot2::labs(fill = "Model") +
    ggplot2::ggtitle(
      paste0(
        "Evaluation results on ",
        n.spatial.folds,
        " spatial folds."
      )
    )

  if(verbose == TRUE){
    suppressMessages(print(p))
  }

}
