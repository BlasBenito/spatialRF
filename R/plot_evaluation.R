#' @title Plots the results of a spatial cross-validation
#' @description Plots the results of an spatial cross-validation performed with [rf_evaluate()].
#' @usage
#' plot_evaluation(
#'   model,
#'   fill.color = viridis::viridis(
#'     3,
#'     option = "F",
#'     alpha = 0.8,
#'     direction = -1
#'     ),
#'   line.color = "gray30",
#'   verbose = TRUE,
#'   notch = TRUE
#' )
#' @param model A model resulting from [rf_evaluate()].
#' @param fill.color Character vector with three hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(3)`). Default: `viridis::viridis(3, option = "F", alpha = 0.8, direction = -1)`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"gray30"`
#' @param verbose Logical, if `TRUE` the plot is printed. Default: `TRUE`
#' @param notch Logical, if `TRUE`, boxplot notches are plotted. Default: `TRUE`
#' @return A ggplot.
#' @seealso [rf_evaluate()], [get_evaluation()], [print_evaluation()].
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(plant_richness_df)
#' data(distance_matrix)
#'
#' #fitting a random forest model
#' rf.model <- rf(
#'   data = plant_richness_df,
#'   dependent.variable.name = "richness_species_vascular",
#'   predictor.variable.names = colnames(plant_richness_df)[5:21],
#'   distance.matrix = distance_matrix,
#'   distance.thresholds = 0,
#'   n.cores = 1,
#'   verbose = FALSE
#' )
#'
#' #evaluating the model with spatial cross-validation
#' rf.model <- rf_evaluate(
#'   model = rf.model,
#'   xy = plant_richness_df[, c("x", "y")],
#'   n.cores = 1
#' )
#'
#' #plotting the evaluation results
#' plot_evaluation(rf.model)
#'
#' }
#'
#' @rdname plot_evaluation
#' @export
#' @importFrom ggplot2 ggplot facet_wrap theme xlab ylab labs
plot_evaluation <- function(
  model,
  fill.color = viridis::viridis(
    3,
    option = "F",
    alpha = 0.8,
    direction = -1
  ),
  line.color = "gray30",
  verbose = TRUE,
  notch = TRUE
) {
  #declaring variable because of check BS
  value <- NULL

  #stop if no evaluation slot
  if (!inherits(model, "rf_evaluate")) {
    stop("Object 'model' does not have an 'evaluation' slot.")
  }

  #getting plotting df
  n.spatial.folds <- length(model$evaluation$spatial.folds)
  x <- model$evaluation$per.fold.long

  #removing NA
  x <- na.omit(x)

  #prettier labels
  x[x$metric == "r.squared", "metric"] <- "R squared"
  x[x$metric == "pseudo.r.squared", "metric"] <- "pseudo R squared"
  x[x$metric == "rmse", "metric"] <- "RMSE"
  x[x$metric == "nrmse", "metric"] <- "NRMSE"
  x[x$metric == "auc", "metric"] <- "AUC"

  #ordering models
  x$model <- factor(x$model, levels = c("Testing", "Training", "Full"))

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
      color = line.color
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
    ggplot2::scale_fill_manual(values = fill.color) +
    ggplot2::labs(fill = "Model") +
    ggplot2::ggtitle(
      paste0(
        "Evaluation results on ",
        n.spatial.folds,
        " spatial folds."
      )
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if (verbose == TRUE) {
    suppressMessages(print(p))
  }
}
