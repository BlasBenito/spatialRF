#' @title print
#' @description Custom print method for models fitted with [rf()], [rf_repeat()], and [rf_spatial())].
#' @param x A model fitted with [rf()], [rf_repeat()], and [rf_spatial())].
#' @param ... Additional arguments for print methods.
#' @return Prints model details to the standard output.
#' @rdname print
#' @export
#' @importFrom huxtable hux add_colnames set_bold set_all_borders number_format print_screen set_width number_format
print.ranger <- function(x, ...) {

  #standard error function
  .se <- function(x){
    x <- na.omit(x)
    x<- round(sqrt(var(x)/length(x)), 3)
    x
    }

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

    if(length(x$performance$r.squared) == 1){
    cat("\n")
    cat("Model performance \n")
    cat("  - R squared (OOB):                 ", x$performance$r.squared, "\n", sep="")
    cat("  - Pseudo R squared:                ", x$performance$pseudo.r.squared, "\n", sep="")
    cat("  - RMSE:                            ", x$performance$rmse, "\n", sep="")
    cat("  - Normalized RMSE:                 ", x$performance$nrmse, "\n", sep="")
    } else {
      cat("\n")
      cat("Model performance (mean +/- standard error) \n")
      cat("  - R squared (OOB):          ", mean(x$performance$r.squared), " +/- ", .se(x$performance$r.squared), "\n", sep="")
      cat("  - Pseudo R squared:         ", mean(x$performance$pseudo.r.squared), " +/- ", .se(x$performance$pseudo.r.squared), "\n", sep="")
      cat("  - RMSE:                     ", mean(x$performance$rmse), " +/- ", .se(x$performance$rmse), "\n", sep="")
      cat("  - Normalized RMSE:          ", mean(x$performance$nrmse), " +/- ", .se(x$performance$nrmse), "\n", sep="")
    }

    cat("\n")
    cat("Model residuals \n")
    cat("  - Stats: \n")
    if(length(x$performance$r.squared) == 1){
      residuals.stats <- as.data.frame(t(summary(x$residuals)))[, 2:3]
    } else {
      residuals.stats <- as.data.frame(t(summary(x$residuals$mean$residuals_mean)))[, 2:3]
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

    cat("\n")
    cat("  - Spatial autocorrelation: \n")
    print_moran(x)

    cat("\n")
    cat("Variable importance: \n")
    print_importance(x)


}

