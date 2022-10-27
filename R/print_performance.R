#' @title print_performance
#' @description Prints the performance slot of a model fitted with [rf()], [rf_repeat()], or [rf_spatial()]. For models fitted with [rf_repeat()] it shows the median and the median absolute deviation of each performance measure.
#' @param model Model fitted with [rf()], [rf_repeat()], or [rf_spatial()].
#' @return Prints model performance scores to the console.
#' @seealso [print_performance()], [get_performance()]
#' @examples
#' if(interactive()){
#'
#'  #loading example data
#' data(
#'   ecoregions_df,
#'   ecoregions_distance_matrix,
#'   ecoregions_predictor_variable_names,
#'   ecoregions_dependent_variable_name
#'   )
#'
#'  #fitting random forest model
#'  rf.model <- rf(
#'    data = ecoregions_df,
#'    dependent.variable.name = ecoregions_dependent_variable_name,
#'    predictor.variable.names = ecoregions_predictor_variable_names,
#'    distance.matrix = ecoregions_distance_matrix,
#'    distance.thresholds = 0,
#'    n.cores = 1,
#'    verbose = FALSE
#'  )
#'
#'  #printing performance scores
#'  print_performance(rf.model)
#'
#' }
#' @rdname print_performance
#' @export
print_performance <- function(model){

  #set decimal options
  user.options <- options()
  options(digits = 4)
  on.exit(options(user.options))

  #getting intrinsic performance
  x <- model$performance

  if("rf_repeat" %in% class(model)){

    cat("\n")

    cat(" Out-of-bag (median +/- median absolute deviation) \n")

    cat(
      "  - R-squared:             ",
      median(x$r.squared.oob),
      " +/- ",
      mad(x$r.squared.oob),
      "\n",
      sep = ""
    )

    cat(
      "  - RMSE:                  ",
      median(x$rmse.oob),
      " +/- ",
      mad(x$rmse.oob),
      "\n",
      sep = ""
    )

    cat(
      "  - Normalized RMSE:       ",
      median(x$nrmse.oob),
      " +/- ",
      mad(x$nrmse.oob),
      "\n",
      sep = ""
    )

    if(!is.na(x$auc.oob[1])){

      cat(
        "  - AUC:                   ",
        median(x$auc.oob),
        " +/- ",
        mad(x$auc.oob),
        "\n",
        sep = ""
      )

    }

    cat("\n")

    cat("In-bag (median +/- median absolute deviation)  \n")

    cat(
      "  - R-squared:             ",
      median(x$r.squared.ib),
      " +/- ",
      mad(x$r.squared.ib),
      "\n",
      sep = ""
    )

    cat(
      "  - RMSE:                  ",
      median(x$rmse.ib),
      " +/- ",
      mad(x$rmse.ib),
      "\n",
      sep = ""
    )

    cat(
      "  - Normalized RMSE:       ",
      median(x$nrmse.ib),
      " +/- ",
      mad(x$nrmse.ib),
      "\n",
      sep = ""
    )

    if(!is.na(x$auc.oob[1])){

      cat(
        "  - AUC:                 ",
        median(x$auc.ib),
        " +/- ",
        mad(x$auc.ib),
        "\n",
        sep = ""
      )
    }

  } else {

    cat("\n")

    cat("Out-of-bag \n")

    cat(
      "  - R-squared:             ",
      x$r.squared.oob,
      "\n",
      sep = ""
    )

    cat(
      "  - RMSE:                  ",
      x$rmse.oob,
      "\n",
      sep = ""
    )

    cat(
      "  - Normalized RMSE:       ",
      x$nrmse.oob,
      "\n",
      sep = ""
    )

    if(!is.na(x$auc.oob)){

      cat(
        "  - AUC:                 ",
        x$auc.oob,
        "\n",
        sep = ""
      )

    }

    cat("\n")

    cat("In-bag \n")

    cat(
      "  - R-squared:             ",
      x$r.squared.ib,
      "\n",
      sep = ""
    )

    cat(
      "  - RMSE:                  ",
      x$rmse.ib,
      "\n",
      sep = ""
    )

    cat(
      "  - Normalized RMSE:       ",
      x$nrmse.ib,
      "\n",
      sep = ""
    )

    if(!is.na(x$auc.oob)){

      cat(
        "  - AUC:                 ",
        x$auc.ib,
        "\n",
        sep = ""
      )

    }

  }


}
