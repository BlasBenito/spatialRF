#' @title Visualize spatial cross-validation results
#' @description Creates boxplots comparing model performance metrics across training, testing, and full datasets from spatial cross-validation performed by [rf_evaluate()]. Displays distributions of R-squared, RMSE, and other metrics across all spatial folds.
#' @param model Model fitted with [rf_evaluate()]. Must be of class "rf_evaluate".
#' @param fill.color Character vector with three colors (one for each model type: Testing, Training, Full) or a function that generates a color palette. Accepts hexadecimal codes (e.g., `c("#440154FF", "#21908CFF", "#FDE725FF")`) or palette functions (e.g., `grDevices::hcl.colors(3)`). Default: `grDevices::hcl.colors(3, palette = "Zissou 1", alpha = 0.8, rev = FALSE)`.
#' @param line.color Character string specifying the color of boxplot borders. Default: `"gray30"`.
#' @param verbose Logical. If `TRUE`, prints the plot to the graphics device. Default: `TRUE`.
#' @param notch Logical. If `TRUE`, displays notched boxplots where notches represent approximate 95% confidence intervals around the median. Non-overlapping notches suggest significant differences between medians. Default: `TRUE`.
#' @return ggplot object that can be further customized or saved. The plot displays boxplots of performance metrics (R-squared, RMSE, NRMSE, pseudo R-squared, or AUC depending on model type) across spatial folds, faceted by metric.
#' @details
#' This function visualizes the distribution of performance metrics across spatial folds, with separate boxplots for three model variants:
#' \itemize{
#'   \item **Testing**: Performance on spatially independent testing folds (most reliable estimate of generalization)
#'   \item **Training**: Performance on training folds (typically optimistic)
#'   \item **Full**: Performance on the complete dataset (reference baseline)
#' }
#'
#' **Interpreting the plot:**
#'
#' The boxplots show the distribution of each metric across all spatial folds. Ideally:
#' \itemize{
#'   \item Testing performance should be reasonably close to training performance (indicates good generalization)
#'   \item Large gaps between training and testing suggest overfitting
#'   \item Low variance across folds indicates stable, consistent model performance
#'   \item High variance suggests performance depends strongly on spatial location
#' }
#'
#' The plot includes a title showing the number of spatial folds used in the evaluation.
#'
#' **Available metrics:**
#'
#' Displayed metrics depend on the response variable type:
#' \itemize{
#'   \item **Continuous response**: R-squared, RMSE (Root Mean Squared Error), NRMSE (Normalized RMSE)
#'   \item **Binary response**: AUC (Area Under ROC Curve), pseudo R-squared
#' }
#' @seealso [rf_evaluate()], [get_evaluation()], [print_evaluation()]
#' @examples
#' if(interactive()){
#'
#' data(plants_rf, plants_xy)
#'
#' # Perform spatial cross-validation
#' plants_rf <- rf_evaluate(
#'   model = plants_rf,
#'   xy = plants_xy,
#'   repetitions = 5,
#'   n.cores = 1
#' )
#'
#' # Visualize evaluation results
#' plot_evaluation(plants_rf)
#'
#' # Without notches for simpler boxplots
#' plot_evaluation(plants_rf, notch = FALSE)
#'
#' # Custom colors
#' plot_evaluation(
#'   plants_rf,
#'   fill.color = c("#E64B35FF", "#4DBBD5FF", "#00A087FF")
#' )
#'
#' # Print summary statistics
#' print_evaluation(plants_rf)
#'
#' # Extract evaluation data for custom analysis
#' evaluation_data <- get_evaluation(plants_rf)
#' head(evaluation_data)
#'
#' }
#' @rdname plot_evaluation
#' @family visualization
#' @export
#' @autoglobal
plot_evaluation <- function(
  model,
  fill.color = grDevices::hcl.colors(
    n = 3,
    palette = "Zissou 1",
    alpha = 0.8,
    rev = FALSE
  ),
  line.color = "gray30",
  verbose = TRUE,
  notch = TRUE
) {
  #stop if no evaluation slot
  if (!inherits(model, "rf_evaluate")) {
    stop("Object 'model' does not have an 'evaluation' slot.")
  }

  #getting plotting df
  n.spatial.folds <- length(model$evaluation$spatial.folds)
  x <- model$evaluation$per.fold.long

  #removing NA
  x <- stats::na.omit(x)

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
    ggplot2::labs(fill = "Model", x = unique(x$metric)) +
    ggplot2::ggtitle(
      paste0(
        "Evaluation results on ",
        n.spatial.folds,
        " spatial folds."
      )
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p
}
