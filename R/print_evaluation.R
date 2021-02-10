#' @title Prints cross-validation results
#' @description Prints the results of an spatial cross-validation performed with [rf_evaluate()].
#' @param x A model resulting from [rf_evaluate()].
#' @return A table printed to the standard output.
#' @seealso [plot_evaluation()], [get_evaluation()]
#' @rdname print_evaluation
#' @export
print_evaluation <- function(x){

  #stop if no evaluation slot
  if(!inherits(x, "rf_evaluate")){
    stop("Object 'x' does not have an 'evaluation' slot.")
  }

  .to_huxtable <- function(
    x,
    measure = "r.squared"
    ){

    performance.measure <- NULL

    x <- x %>%
      dplyr::filter(
        performance.measure == measure
      ) %>%
      dplyr::select(
        model,
        mean,
        sd,
        min,
        max
      ) %>%
      dplyr::mutate(
        mean = round(mean, 3),
        sd = round(sd, 3),
        min = round(min, 3),
        max = round(max, 3)
      ) %>%
      dplyr::rename(
        Model = model,
        Mean = mean,
        `Standard deviation` = sd,
        minimum = min,
        maximum = max
      )

      x <- x[c(1, 3, 2), ] %>%
      huxtable::hux() %>%
      huxtable::set_bold(
        row = 1,
        col = huxtable::everywhere,
        value = TRUE
      ) %>%
      huxtable::set_all_borders(TRUE)
      huxtable::number_format(x)[2:4, 2:5] <- 3

    x

  }

  #evaluation data frames
  model <- x
  x <- x$evaluation$aggregated
  x.rmse <- .to_huxtable(x, measure = "rmse")
  x.nrmse <- .to_huxtable(x, measure = "nrmse")
  x.r.squared <- .to_huxtable(x, measure = "r.squared")
  x.pseudo.r.squared <- .to_huxtable(x, measure = "pseudo.r.squared")


  #printing output
  cat("\n")
  cat("Spatial evaluation \n")
  cat("  - Training fraction:             ", model$evaluation$training.fraction, "\n", sep="")
  cat("  - Spatial folds:                 ", length(model$evaluation$spatial.folds), "\n", sep="")
  cat("  - R squared: \n")
  huxtable::print_screen(x.r.squared, colnames = FALSE)
  cat("\n")
  cat("  - Pseudo R squared: \n")
  huxtable::print_screen(x.pseudo.r.squared, colnames = FALSE)
  cat("\n")
  cat("  - RMSE: \n")
  huxtable::print_screen(x.rmse, colnames = FALSE)
  cat("\n")
  cat("  - NRMSE: \n")
  huxtable::print_screen(x.nrmse, colnames = FALSE)

}
