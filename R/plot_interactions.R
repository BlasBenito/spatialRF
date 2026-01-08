#' @title Plot variable interactions
#' @description Plot selected interactions from rf_interactions model
#' @param model Model from [rf_interactions()]
#' @param which Character vector of interaction names, or "all" (default)
#' @param verbose Logical. If `TRUE`, the plot is printed. Default: `TRUE`
#' @return ggplot or patchwork object
#' @examples
#' if (interactive()) {
#'   data(plants_df, plants_xy, plants_rf)
#'
#'   # Fit model with interactions
#'   m_int <- rf_interactions(
#'     model = plants_rf,
#'     xy = plants_xy,
#'     repetitions = 5,
#'     n.cores = 1
#'   )
#'
#'   # Plot all interactions
#'   plot_interactions(m_int)
#'
#'   # Plot specific interactions
#'   int_names <- m_int$interactions$selected$interaction.name[1:2]
#'   plot_interactions(m_int, which = int_names)
#' }
#' @family plot_functions
#' @autoglobal
#' @export
plot_interactions <- function(model, which = "all", verbose = TRUE) {
  if (!inherits(model, "rf_interactions")) {
    stop("Model must be from rf_interactions()")
  }

  plots <- model$interactions$plots

  if (which[1] != "all") {
    plots <- plots[which]
  }

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p <- patchwork::wrap_plots(plots)
  } else {
    p <- plots
  }

  if (verbose) {
    print(p)
  }

  return(invisible(p))
}
