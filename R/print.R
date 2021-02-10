#' @title Custom print method for random forest models
#' @description Custom print method for models fitted with [rf()], [rf_repeat()], and [rf_spatial()].
#' @param x A model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @param ... Additional arguments for print methods.
#' @return Prints model details to the console.
#' @seealso [print_evaluation()], [print_importance()], [print_moran()], [print_performance()]
#' @rdname print
#' @export
#' @importFrom huxtable hux add_colnames set_bold set_all_borders number_format print_screen set_width number_format
print.rf <- function(x, ...) {

  #getting model features

    cat("Model type\n")
    cat("  - Fitted with:                     ", class(x)[2], "()\n", sep="")
    if(inherits(x, "rf_repeat")){
    cat("  - Repetitions:                     ", x$ranger.arguments$repetitions, "\n", sep="")
    }
    if(inherits(x, "rf_spatial")){
    cat("  - rf_spatial() method:             ", x$selection.spatial.predictors$method, "\n", sep="")
    }
    cat("  - Response variable:               ", x$ranger.arguments$dependent.variable.name, "\n", sep="")

    cat("\n")
    cat("Random forest parameters\n")
    cat("  - Type:                            ", x$treetype, "\n", sep="")
    cat("  - Number of trees:                 ", x$num.trees, "\n", sep="")
    cat("  - Sample size:                     ", x$num.samples, "\n", sep="")
    cat("  - Number of predictors:            ", x$num.independent.variables, "\n", sep="")
    cat("  - Mtry:                            ", x$mtry, "\n", sep="")
    cat("  - Minimum node size:               ", x$min.node.size, "\n", sep="")
    cat("\n")

    print_performance(x)

    cat("\n")
    cat("Model residuals \n")
    cat("  - Stats: \n")
    if(length(x$performance$r.squared) == 1){
      residuals.stats <- as.data.frame(t(summary(x$residuals)))[, 2:3]
    } else {
      residuals.stats <- as.data.frame(t(summary(x$residuals$mean$mean)))[, 2:3]
    }
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

    if("spatial.correlation.residuals" %in% names(x)){
      cat("\n")
      cat("  - Spatial autocorrelation: \n")
      print_moran(x)
    }

    if("performance.comparison" %in% names(x)){
      cat("\n")
      cat("Comparing performance (spatial vs. non-spatial model): \n")
      comparison.df <- x$performance.comparison
      colnames(comparison.df) <- c("Model", "R squared", "RMSE", "NRMSE", "Max. Moran's I")
      comparison.hux <-
        huxtable::hux(comparison.df) %>%
        huxtable::set_bold(row = 1, col = huxtable::everywhere, value = TRUE) %>%
        huxtable::set_bold(col = 1, row = huxtable::everywhere, value = TRUE) %>%
        huxtable::set_all_borders(TRUE)
      huxtable::number_format(comparison.hux)[2:3, 2:5] <- 3
      huxtable::print_screen(comparison.hux, colnames = FALSE)
    }

    if("variable.importance" %in% names(x)){
      cat("\n")
      cat("Variable importance: \n")
      print_importance(x)
    }

    if(inherits(x, "rf_evaluate")){
      print_evaluation(x)
    }

}

