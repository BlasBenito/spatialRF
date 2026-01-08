#' @title Extract interaction details
#' @description Extract interaction selection details from rf_interactions model
#' @param model Model from [rf_interactions()]
#' @param type Character: "selected", "screening", or "all". Default: "selected"
#' @return Data frame or list with interaction details
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
#'   # Get selected interactions
#'   get_interactions(m_int, type = "selected")
#'
#'   # Get all tested interactions
#'   get_interactions(m_int, type = "screening")
#'
#'   # Get both
#'   get_interactions(m_int, type = "all")
#' }
#' @family get_functions
#' @autoglobal
#' @export
get_interactions <- function(model, type = c("selected", "screening", "all")) {
  type <- match.arg(type)

  if (!inherits(model, "rf_interactions")) {
    stop("Model must be from rf_interactions()")
  }

  switch(type,
    selected = model$interactions$selected,
    screening = model$interactions$screening,
    all = list(
      selected = model$interactions$selected,
      screening = model$interactions$screening
    )
  )
}
