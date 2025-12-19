# Package index

## Main modeling functions

Primary entry points for fitting random forest and spatial random forest
models.

- [`rf()`](https://blasbenito.github.io/spatialRF/reference/rf.md) :
  Random forest models with Moran's I test of the residuals
- [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
  : Fits spatial random forest models

## Model workflow and evaluation

Functions for model comparison, evaluation, tuning, and advanced
modeling operations.

- [`rf_compare()`](https://blasbenito.github.io/spatialRF/reference/rf_compare.md)
  : Compares models via spatial cross-validation
- [`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)
  : Evaluates random forest models with spatial cross-validation
- [`rf_importance()`](https://blasbenito.github.io/spatialRF/reference/rf_importance.md)
  : Contribution of each predictor to model transferability
- [`rf_repeat()`](https://blasbenito.github.io/spatialRF/reference/rf_repeat.md)
  : Fits several random forest models on the same data
- [`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)
  : Tuning of random forest hyperparameters via spatial cross-validation

## Data preprocessing

Functions for variable selection, multicollinearity reduction, distance
matrix manipulation, and spatial fold creation.

- [`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md)
  : Multicollinearity reduction via Pearson correlation
- [`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md)
  : Multicollinearity reduction via Variance Inflation Factor
- [`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md)
  : Generate case weights for imbalanced binary data
- [`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md)
  : Default distance thresholds for spatial predictors
- [`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md)
  : Double-center a distance matrix
- [`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md)
  : Check if variable is binary with values 0 and 1
- [`make_spatial_fold()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_fold.md)
  : Create spatially independent training and testing folds
- [`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md)
  : Create multiple spatially independent training and testing folds
- [`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md)
  : Suggest variable interactions and composite features for random
  forest models
- [`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)
  : Transforms a distance matrix into a matrix of weights

## Spatial analysis methods

Functions for generating spatial predictors (MEMs, PCA), testing spatial
autocorrelation (Moran’s I), and selecting/filtering spatial predictors.

- [`filter_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/filter_spatial_predictors.md)
  : Remove redundant spatial predictors
- [`mem()`](https://blasbenito.github.io/spatialRF/reference/mem.md) :
  Compute Moran's Eigenvector Maps from distance matrix
- [`mem_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/mem_multithreshold.md)
  : Compute Moran's Eigenvector Maps across multiple distance thresholds
- [`moran()`](https://blasbenito.github.io/spatialRF/reference/moran.md)
  : Moran's I test for spatial autocorrelation
- [`moran_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/moran_multithreshold.md)
  : Moran's I test across multiple distance thresholds
- [`residuals_test()`](https://blasbenito.github.io/spatialRF/reference/normality.md)
  : Normality test of a numeric vector
- [`pca()`](https://blasbenito.github.io/spatialRF/reference/pca.md) :
  Compute Principal Component Analysis
- [`pca_multithreshold()`](https://blasbenito.github.io/spatialRF/reference/pca_multithreshold.md)
  : Compute Principal Component Analysis at multiple distance thresholds
- [`rank_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/rank_spatial_predictors.md)
  : Ranks spatial predictors
- [`residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/residuals_diagnostics.md)
  : Normality test of a numeric vector
- [`select_spatial_predictors_recursive()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_recursive.md)
  : Finds optimal combinations of spatial predictors
- [`select_spatial_predictors_sequential()`](https://blasbenito.github.io/spatialRF/reference/select_spatial_predictors_sequential.md)
  : Sequential introduction of spatial predictors into a model

## Model information and output

Functions to extract model components and print results.

- [`get_evaluation()`](https://blasbenito.github.io/spatialRF/reference/get_evaluation.md)
  : Extract evaluation metrics from cross-validated model
- [`get_importance()`](https://blasbenito.github.io/spatialRF/reference/get_importance.md)
  : Extract variable importance from model
- [`get_importance_local()`](https://blasbenito.github.io/spatialRF/reference/get_importance_local.md)
  : Extract local variable importance from model
- [`get_moran()`](https://blasbenito.github.io/spatialRF/reference/get_moran.md)
  : Extract Moran's I test results for model residuals
- [`get_performance()`](https://blasbenito.github.io/spatialRF/reference/get_performance.md)
  : Extract out-of-bag performance metrics from model
- [`get_predictions()`](https://blasbenito.github.io/spatialRF/reference/get_predictions.md)
  : Extract fitted predictions from model
- [`get_residuals()`](https://blasbenito.github.io/spatialRF/reference/get_residuals.md)
  : Extract model residuals
- [`get_response_curves()`](https://blasbenito.github.io/spatialRF/reference/get_response_curves.md)
  : Extract response curve data for plotting
- [`get_spatial_predictors()`](https://blasbenito.github.io/spatialRF/reference/get_spatial_predictors.md)
  : Extract spatial predictors from spatial model
- [`print(`*`<rf>`*`)`](https://blasbenito.github.io/spatialRF/reference/print.md)
  : Custom print method for random forest models
- [`print_evaluation()`](https://blasbenito.github.io/spatialRF/reference/print_evaluation.md)
  : Prints cross-validation results
- [`print_importance()`](https://blasbenito.github.io/spatialRF/reference/print_importance.md)
  : Prints variable importance
- [`print_moran()`](https://blasbenito.github.io/spatialRF/reference/print_moran.md)
  : Prints results of a Moran's I test
- [`print_performance()`](https://blasbenito.github.io/spatialRF/reference/print_performance.md)
  : print_performance

## Visualization functions

Functions for creating diagnostic, exploratory, and results plots.

- [`plot_evaluation()`](https://blasbenito.github.io/spatialRF/reference/plot_evaluation.md)
  : Visualize spatial cross-validation results

- [`plot_importance()`](https://blasbenito.github.io/spatialRF/reference/plot_importance.md)
  : Visualize variable importance scores

- [`plot_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_moran.md)
  : Plots a Moran's I test of model residuals

- [`plot_optimization()`](https://blasbenito.github.io/spatialRF/reference/plot_optimization.md)
  : Optimization plot of a selection of spatial predictors

- [`plot_residuals_diagnostics()`](https://blasbenito.github.io/spatialRF/reference/plot_residuals_diagnostics.md)
  : Plot residuals diagnostics

- [`plot_response_curves()`](https://blasbenito.github.io/spatialRF/reference/plot_response_curves.md)
  : Plots the response curves of a model.

- [`plot_response_surface()`](https://blasbenito.github.io/spatialRF/reference/plot_response_surface.md)
  : Plots the response surfaces of a random forest model

- [`plot_training_df()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df.md)
  : Scatterplots of a training data frame

- [`plot_training_df_moran()`](https://blasbenito.github.io/spatialRF/reference/plot_training_df_moran.md)
  : Moran's I plots of a training data frame

- [`plot_tuning()`](https://blasbenito.github.io/spatialRF/reference/plot_tuning.md)
  :

  Plots a tuning object produced by
  [`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)

## Utility functions

Low-level helper functions for statistical computations and parallel
execution.

- [`auc()`](https://blasbenito.github.io/spatialRF/reference/auc.md) :
  Area under the ROC curve
- [`beowulf_cluster()`](https://blasbenito.github.io/spatialRF/reference/beowulf_cluster.md)
  : Create a Beowulf cluster for parallel computing
- [`.vif_to_df()`](https://blasbenito.github.io/spatialRF/reference/dot-vif_to_df.md)
  : Convert VIF values to data frame
- [`objects_size()`](https://blasbenito.github.io/spatialRF/reference/objects_size.md)
  : Display sizes of objects in current R environment
- [`optimization_function()`](https://blasbenito.github.io/spatialRF/reference/optimization_function.md)
  : Compute optimization scores for spatial predictor selection
- [`prepare_importance_spatial()`](https://blasbenito.github.io/spatialRF/reference/prepare_importance_spatial.md)
  : Prepares variable importance objects for spatial models
- [`rescale_vector()`](https://blasbenito.github.io/spatialRF/reference/rescale_vector.md)
  : Rescales a numeric vector into a new range
- [`root_mean_squared_error()`](https://blasbenito.github.io/spatialRF/reference/root_mean_squared_error.md)
  : RMSE and normalized RMSE
- [`setup_parallel_execution()`](https://blasbenito.github.io/spatialRF/reference/setup_parallel_execution.md)
  : Setup parallel execution with automatic backend detection
- [`standard_error()`](https://blasbenito.github.io/spatialRF/reference/standard_error.md)
  : Standard error of the mean of a numeric vector
- [`statistical_mode()`](https://blasbenito.github.io/spatialRF/reference/statistical_mode.md)
  : Statistical mode of a vector
- [`thinning()`](https://blasbenito.github.io/spatialRF/reference/thinning.md)
  : Applies thinning to pairs of coordinates
- [`thinning_til_n()`](https://blasbenito.github.io/spatialRF/reference/thinning_til_n.md)
  : Applies thinning to pairs of coordinates until reaching a given n

## Example datasets

Datasets for testing and learning spatialRF functionality.

- [`plants_df`](https://blasbenito.github.io/spatialRF/reference/plants_df.md)
  : Plant richness and predictors for American ecoregions
- [`plants_distance`](https://blasbenito.github.io/spatialRF/reference/plants_distance.md)
  : Distance matrix between ecoregion edges
- [`plants_predictors`](https://blasbenito.github.io/spatialRF/reference/plants_predictors.md)
  : Predictor variable names for plant richness examples
- [`plants_response`](https://blasbenito.github.io/spatialRF/reference/plants_response.md)
  : Response variable name for plant richness examples
- [`plants_rf`](https://blasbenito.github.io/spatialRF/reference/plants_rf.md)
  : Example fitted random forest model
- [`plants_rf_spatial`](https://blasbenito.github.io/spatialRF/reference/plants_rf_spatial.md)
  : Example fitted spatial random forest model
- [`plants_xy`](https://blasbenito.github.io/spatialRF/reference/plants_xy.md)
  : Coordinates for plant richness data
