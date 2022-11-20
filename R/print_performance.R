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

  #performance methods and their pretty names
  methods_dictionary <- data.frame(
    method = c(
      "ib",
      "oob",
      "scv"
    ),
    name = c(
      "In_bag",
      "Out_of_bag",
      "Spatial_CV"
    )
  )

  #performance metrics and their pretty names
  metrics_dictionary <- data.frame(
    metric = c(
      "rsquared",
      "rmse",
      "nrmse",
      "auc"
    ),
    name = c(
      "R-squared",
      "RMSE",
      "nRMSE",
      "AUC"
    )
  )

  #getting performance
  x <- model$performance

  #remove slots with NA
  # x <- x[!is.na(x)]

  x <- Filter(Negate(anyNA), x)

  #compute median and mad
  x.median <- lapply(X = x, FUN = median)

  #checking what metrics are available
  metrics <- unique(
    gsub(
      pattern = ".ib|.oob|.scv",
      replacement = "",
      x = names(x.median)
    )
  )

  #checking what methods are available
  methods <- unique(
    gsub(
      pattern = "rsquared.|rmse.|auc.|nrmse.",
      replacement = "",
      x = names(x.median)
    )
  )


  #empty performance data frame
  performance_df <- matrix(
    data = NA,
    nrow = length(metrics),
    ncol = length(methods),
    dimnames = list(
      metrics,
      methods
    )
  ) %>%
    as.data.frame()

  #filling performance data frame with medians
  for(metric.i in metrics){
    for(method.i in methods){
      performance_df[metric.i, method.i] <-
        x.median[[paste(
          metric.i,
          method.i,
          sep = "."
          )]]
    }
  }

  #ordering columns as in methods_dictionary
  performance_df <- performance_df[
    metrics_dictionary$metric[metrics_dictionary$metric %in% metrics],
    methods_dictionary$method[methods_dictionary$method %in% methods]
    ]

  #renaming with pretty names
  colnames(performance_df) <- methods_dictionary$name[methods_dictionary$method %in% methods]

  #renaming rows
  rownames(performance_df) <- metrics_dictionary$name[metrics_dictionary$metric %in% metrics]

  #rownames as Metric
  performance_df <- data.frame(
    Metric = rownames(performance_df),
    performance_df
  )

  #remove rownames
  rownames(performance_df) <- NULL

  performance_df_hux <-
    huxtable::hux(performance_df) %>%
    huxtable::set_bold(
      row = 1,
      col = huxtable::everywhere,
      value = TRUE
      ) %>%
    huxtable::set_all_borders(TRUE)

  huxtable::number_format(performance_df_hux) <- 3

  huxtable::print_screen(
    ht = performance_df_hux,
    colnames = FALSE
    )

  cat("\nInterpretation:\n")
  cat(" - In_bag: from the prediction of the entire training set. Please, never report this number. It is highly inflated.\n")
  cat("- Out_of_bag: from the prediction of data left out during the training of each regression tree. It might be inflated when the training data shows a strong spatial structure.\n")
  cat("- Spatial_CV: median performance from predictions over spatially independent data. Trustworthy, but negatively affected when the importance of spatial predictors is high.\n")


}
