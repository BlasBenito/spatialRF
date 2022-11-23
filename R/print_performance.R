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
print_performance <- function(
    model,
    centrality.fun = stats::median,
    dispersion.fun = stats::mad
    ){

  #getting performance
  x <- model$performance

  #remove slots with NA
  # x <- x[!is.na(x)]
  x <- Filter(Negate(anyNA), x)

  #get roc curves in another list if any
  x.roc <- x[grep(
    pattern = "roc",
    x = names(x)
  )]

  x <- x[!(names(x) %in% names(x.roc))]

  #checking what metrics are available
  metrics <- unique(
    gsub(
      pattern = ".ib|.oob|.scv",
      replacement = "",
      x = names(x)
    )
  )

  #adding sensitivity and specificty
  if("roc.ib" %in% names(x.roc)){
    metrics <- c(
      metrics,
      "sensitivity",
      "specificity"
    )
  }

  #checking what methods are available
  methods <- unique(
    gsub(
      pattern = "rsquared.|rmse.|auc.|nrmse.|rbiserial.",
      replacement = "",
      x = names(x)
    )
  )

  #data frame
  performance_df <- expand.grid(
    metrics,
    methods
  ) %>%
    dplyr::rename(
      metric = Var1,
      method = Var2
    ) %>%
    dplyr::mutate(
      value = NA,
      median = NA,
      mad = NA
    ) %>%
    dplyr::arrange(
      metric,
      method
    )


  #filling performance data frame with medians
  for(metric.i in metrics){
    for(method.i in methods){

      #capture values
      values <- x[[paste(
        metric.i,
        method.i,
        sep = "."
      )]]

      if(length(values) == 1){

        performance_df[performance_df$metric == metric.i & performance_df$method == method.i, "value"] <- values

      } else {

        performance_df[performance_df$metric == metric.i & performance_df$method == method.i, "median"] <- stats::median(values)

        performance_df[performance_df$metric == metric.i & performance_df$method == method.i, "mad"] <- stats::mad(values)

      }
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

  cat("\nModel performance:\n")

  huxtable::print_screen(
    ht = performance_df_hux,
    colnames = FALSE
    )

  cat("\nInterpretation:\n")
  cat(" - In_bag: from predictions of the entire training set. Highly inflated and overoptimistic.\n")
  cat("- Out_of_bag: from predictions on data left out during model training. Inflated when spatial autocorrelation is strong.\n")
  cat("- Spatial_CV: from the prediction over spatially-independent data. Deflated when spatial autocorrelation is strong.\n")


}
