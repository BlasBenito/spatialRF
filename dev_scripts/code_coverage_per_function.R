library(covr)
library(devtools)
library(dplyr)

devtools::load_all()

# run coverage once, using all tests
x <- covr::package_coverage(
  type = "tests",
  quiet = TRUE
)


x

# spatialRF Coverage: 81.64%
# R/get_importance_local.R: 0.00%
# R/get_response_curves.R: 0.00%
# R/plot_evaluation.R: 0.00%
# R/plot_training_df_moran.R: 0.00%
# R/plot_training_df.R: 0.00%
# R/print_evaluation.R: 0.00%
# R/print_importance.R: 0.00%
# R/print_moran.R: 0.00%
# R/print_performance.R: 0.00%
# R/print.R: 0.00%
# R/rf_tuning.R: 77.64%
# R/mem_multithreshold.R: 77.78%
# R/rf_spatial.R: 78.70%
# R/pca_multithreshold.R: 82.86%
# R/thinning.R: 82.86%
# R/plot_response_curves.R: 84.92%
# R/plot_optimization.R: 85.48%
# R/the_feature_engineer.R: 86.91%
# R/get_performance.R: 87.50%
# R/rescale_vector.R: 87.50%
# R/rf_evaluate.R: 88.31%
# R/default_distance_thresholds.R: 90.00%
# R/double_center_distance_matrix.R: 90.00%
# R/get_importance.R: 90.91%
# R/rf.R: 91.03%
# R/pca.R: 91.67%
# R/weights_from_distance_matrix.R: 92.31%
# R/plot_response_surface.R: 92.39%
# R/thinning_til_n.R: 93.10%
# R/rf_importance.R: 94.30%
# R/rf_repeat.R: 94.43%
# R/mem.R: 94.44%
# R/root_mean_squared_error.R: 94.74%
# R/moran_multithreshold.R: 94.87%
# R/select_spatial_predictors_sequential.R: 95.00%
# R/select_spatial_predictors_recursive.R: 96.00%
# R/moran.R: 97.50%
# R/plot_residuals_diagnostics.R: 98.04%
# R/plot_tuning.R: 98.21%
# R/plot_moran.R: 98.31%
# R/rank_spatial_predictors.R: 98.89%
# R/rf_compare.R: 99.07%
# R/filter_spatial_predictors.R: 100.00%
# R/get_evaluation.R: 100.00%
# R/get_moran.R: 100.00%
# R/get_predictions.R: 100.00%
# R/get_residuals.R: 100.00%
# R/get_spatial_predictors.R: 100.00%
# R/is_binary.R: 100.00%
# R/make_spatial_fold.R: 100.00%
# R/make_spatial_folds.R: 100.00%
# R/objects_size.R: 100.00%
# R/optimization_function.R: 100.00%
# R/plot_importance.R: 100.00%
# R/prepare_importance_spatial.R: 100.00%
# R/residuals_diagnostics.R: 100.00%
# R/residuals_test.R: 100.00%
# R/standard_error.R: 100.00%
# R/statistical_mode.R: 100.00%
