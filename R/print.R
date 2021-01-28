#' @importFrom huxtable hux add_colnames set_bold set_all_borders number_format print_screen set_width number_format
#' @export
print.ranger <- function(x, ...) {

    cat("Random forest parameters\n")
    cat("  - Type:                            ", x$treetype, "\n")
    cat("  - Number of trees:                 ", x$num.trees, "\n")
    cat("  - Sample size:                     ", x$num.samples, "\n")
    cat("  - Number of independent variables: ", x$num.independent.variables, "\n")
    cat("  - Mtry:                            ", x$mtry, "\n")
    cat("  - Minimum node size:               ", x$min.node.size, "\n")

    cat("\n")
    cat("Model performance \n")
    cat("  - R squared (OOB):                 ", x$r.squared, "\n")
    cat("  - Pseudo R squared:                ", x$performance$pseudo.r.squared, "\n")
    cat("  - RMSE:                            ", x$performance$rmse, "\n")
    cat("  - Normalized RMSE:                 ", x$performance$nrmse, "\n")

    cat("\n")
    cat("Model residuals \n")
    cat("  - Stats: \n")
    residuals.stats <- as.data.frame(t(summary(x$residuals)))[, 2:3]
    rownames(residuals.stats) <- residuals.stats$Var2
    residuals.stats$Var2 <- NULL
    residuals.stats <- t(residuals.stats)
    rownames(residuals.stats) <- NULL
    colnames(residuals.stats) <- c("Min.", "1st Q.", "Median", "Mean", "3rd Q.", "Max.")

    residuals.stats <-
      huxtable::hux(residuals.stats) %>%
      huxtable::add_colnames() %>%
      huxtable::set_bold(row = 1, col = huxtable::everywhere, value = TRUE) %>%
      huxtable::set_all_borders(TRUE)
    huxtable::number_format(residuals.stats)[2, ] <- 2
    huxtable::print_screen(residuals.stats, colnames = FALSE)

    # print(knitr::kable(residuals.stats))
    cat("\n")
    cat("  - Spatial autocorrelation: \n")
    # print(knitr::kable(x$spatial.correlation.residuals$df))
    spatial.correlation <- x$spatial.correlation.residuals$df
    colnames(spatial.correlation) <- c("Distance", "Moran's I", "p-value", "Interpretation")
    if(inherits(x, "rf_spatial")){
      colnames(spatial.correlation) <- c("Distance", "Moran's I", "p-value", "Interpretation", "Model")
    }
    spatial.correlation <-
      huxtable::hux(spatial.correlation) %>%
      huxtable::set_bold(row = 1, col = huxtable::everywhere, value = TRUE) %>%
      huxtable::set_all_borders(TRUE) %>%
      huxtable::set_width(1)
    huxtable::number_format(spatial.correlation)[, 1] <- NA
    huxtable::number_format(spatial.correlation)[, 2] <- 2
    huxtable::number_format(spatial.correlation)[, 3] <- 4
      huxtable::print_screen(spatial.correlation, colnames = FALSE)

      if("variable.importance" %in% names(x)){
        cat("\n")
        cat("Variable importance \n")
        if(class(x$variable.importance) == "numeric"){
          Importance <- NULL
          variable.importance <- as.data.frame(x$variable.importance)
          variable.importance <- data.frame(
            Variable = rownames(variable.importance),
            Importance = variable.importance[, 1]
          ) %>%
            dplyr::arrange(dplyr::desc(`Importance`))

        }
        if(class(x$variable.importance) == "list"){
          variable.importance <- x$variable.importance$df
          colnames(variable.importance) <- c("Variable", "Importance")
        }
        variable.importance <- huxtable::hux(variable.importance) %>%
          huxtable::set_bold(row = 1, col = huxtable::everywhere, value = TRUE) %>%
          huxtable::set_all_borders(TRUE) %>%
          huxtable::set_width(1)
        huxtable::number_format(variable.importance)[, 2] <- 3
        huxtable::print_screen(variable.importance, colnames = FALSE)
      }

}

