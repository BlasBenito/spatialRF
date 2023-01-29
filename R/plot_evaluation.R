#' @title Plots the results of a spatial cross-validation
#' @description Plots the results of an spatial cross-validation performed with [rf_evaluate()].
#' @param model A model resulting from [rf_evaluate()].
#' @param fill.color Character vector with three hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. `viridis::viridis(3)`). Default: `viridis::viridis(3, option = "F", alpha = 0.8, direction = -1)`
#' @param line.color Character string, color of the line produced by `ggplot2::geom_smooth()`. Default: `"gray30"`
#' @param verbose Logical, if `TRUE` the plot is printed. Default: `TRUE`
#' @param notch Logical, if `TRUE`, boxplot notches are plotted. Default: `TRUE`
#' @return A ggplot.
#' @seealso [rf_evaluate()], [print_evaluation()].
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_numeric_predictors,
#'   ecoregions_continuous_response
#'   )
#'
#'  #fitting random forest model
#'  rf.model <- rf(
#'    data = ecoregions_df,
#'    response.name = ecoregions_continuous_response,
#'    predictors.names = ecoregions_numeric_predictors,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
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
    2,
    option = "F",
    alpha = 0.8,
    direction = -1
  ),
  line.color = "gray30",
  verbose = TRUE,
  notch = TRUE
  ){

  #declaring variable because of check BS
  value <- NULL
  evaluation.set <- NULL

  #stop if no evaluation slot
  if(!inherits(model, "rf_evaluate") | !("evaluation" %in% names(model))){
    stop("Object 'model' does not have an 'evaluation' slot.")
  }

  #getting plotting df
  n.spatial.folds <- nrow(model$evaluation$per_fold)/2
  x <- model$evaluation$per_fold

  #removing NA
  x <- na.omit(x)

  #ordering models
  x$evaluation.set <- factor(
    x$evaluation.set,
    levels = c("testing", "training")
    )

  #the plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = x,
      ggplot2::aes(
        group = evaluation.set,
        y = evaluation.set,
        x = value,
        fill = evaluation.set
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

  if(verbose == TRUE){
    suppressMessages(print(p))
  }

}
