#' @title Plots a tuning object produced by [rf_tuning()]
#' @description Plots the tuning of the hyperparameters `num.trees`, `mtry`, and `min.node.size` performed by [rf_tuning()].
#' @param x A model fitted with [rf_tuning()]. Default: `NULL`
#' @param verbose Logical, if `TRUE`, the plot is printed. Default: `TRUE`
#' @return A ggplot.
#' @seealso [rf_tuning()]
#' @examples
#' \donttest{
#' if(interactive()){
#'
#' data(plant_richness_df)
#'
#' rf.model <- rf_tuning(
#'  data = plant_richness_df,
#'  dependent.variable.name = "richness_species_vascular",
#'  predictor.variable.names = colnames(plant_richness_df)[5:21],
#'  verbose = FALSE
#' )
#'
#' plot_tuning(x = rf.model)
#'
#' }
#' }
#' @rdname plot_tuning
#' @export
plot_tuning <- function(x, verbose = TRUE){

  #declaring variables
  r.squared <- NULL
  value <- NULL

  if(!("tuning" %in% names(x))){
    stop("Object 'x' does not have a 'tuning' slot.")
  }

  #extracting tuning data frame
  tuning <- x$tuning$tuning.df

  #to long format
  tuning.long <- tidyr::pivot_longer(
    tuning,
    cols = 1:3,
    names_to = "parameter",
    values_to = "value"
  ) %>%
    as.data.frame()

  #tuning plot
  p <- ggplot2::ggplot(
    data = tuning.long,
    ggplot2::aes(
      y = r.squared,
      x = value,
      fill = r.squared
    )) +
    ggplot2::geom_smooth(
      method = "lm",
      se = TRUE,
      color = "gray20",
      alpha = 0.5,
      formula = y ~ x) +
    ggplot2::geom_point(
      shape = 21,
      alpha = 0.5,
      size = 3
    ) +
    ggplot2::facet_wrap(
      "parameter",
      ncol = 1,
      scales = "free"
    ) +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab(
      paste0(
        "R squared (method: ",
        x$tuning$method,
        ")"
        )
      )

  if(verbose == TRUE){
    print(p)
  }

}
