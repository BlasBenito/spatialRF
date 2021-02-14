#' @title Variance Inflation Factor of a data frame
#' @description Computes the variance inflation factor (VIF) of the colums in a data frame.
#' @param x Data frame with numeric columns, typically containing a set of model predictors.
#' @return A data frame with two columns having the name of the variables in 'x' and their respective VIF values.
#' @seealso [auto_vif()], [auto_cor()]
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  data(plant_richness_df)
#'
#'  vif(plant_richness_df[, 5:21])
#'
#' }
#' }
#' @rdname vif
#' @importFrom tibble rownames_to_column
#' @export
vif <- function(x){
  if(!is.data.frame(x)){
    stop("x must be a data frame with numeric columns")
  }
  out <- x %>%
    na.omit() %>%
    as.matrix() %>%
    cor() %>%
    solve() %>%
    diag() %>%
    sort(decreasing = TRUE) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "variable")
  colnames(out)[2] <- "vif"
  out$vif <- round(out$vif, 3)
  return(out)
}
