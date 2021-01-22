#' @title vif
#' @description computes the variance inflation factor (VIF) of the colums in a data frame.
#' @param x data frame with numeric columns, typically with a set of predictors.
#' @return a data frame with two columns having the name of the variables in 'x' and their respective VIF values.
#' @examples
#' \dontrun{
#' if(interactive()){
#' data(plant_richness_df)
#'  out <- vif(plant_richness_df[, 5:21])
#'  out
#'  }
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
  return(out)
}
