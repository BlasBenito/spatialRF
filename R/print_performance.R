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

  titlecase <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

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

  #keep non roc data
  x <- x[!(names(x) %in% names(x.roc))]

  #available metrics
  metrics <- unique(
    gsub(
      pattern = "_full|_oob|_scv",
      replacement = "",
      x = names(x)
    )
  )

  #adding sensitivity and specificty
  if("roc_full" %in% names(x.roc)){

    metrics <- c(
      metrics,
      "sensitivity_0.5",
      "specificity_0.5"
    )

    x$sensitivity_0.5_full <- median(x.roc$roc_full$sensitivity_0.5)

    x$sensitivity_0.5_oob <- median(x.roc$roc_oob$sensitivity_0.5)

    x$specificity_0.5_full <- median(x.roc$roc_full$specificity_0.5)

    x$specificity_0.5_oob <- median(x.roc$roc_oob$specificity_0.5)

  }

  #available methods
  methods <- unique(
    gsub(
      pattern = "rsquared.|rmse.|auc.|nrmse.|rbiserial.|sensitivity_0.5.|specificity_0.5.",
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

      performance_df[performance_df$metric == metric.i & performance_df$method == method.i, "value"] <- values

    }
  }

  #remove columns with all NA
  performance_df <- Filter(function(x)!all(is.na(x)), performance_df)

  #pretty names
  performance_df <- performance_df %>%
    dplyr::mutate(
      method = dplyr::case_when(
        method == "ib" ~ "In-bag",
        method == "oob" ~ "Out-of-bag",
        method == "scv" ~ "Spatial CV"
      )
    ) %>%
    dplyr::mutate(
      metric = dplyr::case_when(
        metric == "rsquared" ~ "R-squared",
        metric == "rmse" ~ "RMSE",
        metric == "nrmse" ~ "nRMSE",
        metric == "auc" ~ "AUC",
        metric == "rbiserial" ~ "Biserial R-squared",
        metric == "sensitivity_0.5" ~ "Sensitivity (0.5)",
        metric == "specificity_0.5" ~ "Specificity (0.5)"
      )
    )

  #pretty titles
  names(performance_df) <- titlecase(x = names(performance_df))


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
  cat(" - In-bag: from predictions of the entire training set. Highly inflated and overoptimistic.\n")
  cat("- Out-of-bag: from predictions on data left out during model training. Inflated when spatial autocorrelation is strong.\n")
  if("scv" %in% methods){
    cat("- Spatial CV: from the prediction over spatially-independent data. Deflated when spatial autocorrelation is strong.\n")
  }



}
