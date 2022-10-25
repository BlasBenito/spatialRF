## Version 2.0.0 (work in progress)

Expanded the performance metrics provided by rf() and company. They now report: r.squared.oob, rmse.oob, nrmse.oob, and if the response is binary, auc.oob. The in-bag metrics are: r.squared.ib, rmse.ib, nrmse.ib, and auc.ib. These changes have been implemented in rf_repeat() as well. Both types of predictions are now provided as well in `model$predictions$ib` and `model$predictions$oob`. Task to propagate these changes to other functions is still pending.

All functions support tibbles as input.

The functions `mem()` and `mem_multithreshold()` now share the same documentation.

The functions `moran()` and `moran_multithreshold()` now share the same documentation.

The performance metric `pseudo.r.squared` has been removed from all functions.

Simplified the methods in `rf_spatial()`, and now all experimental methods have been removed.

`auto_cor()` and `auto_vif()` now have explicit `data` and `predictor.variable.names` arguments.

`rf_importance()` is now `rf_jackknife()`, and works in a slightly different way (please, check the help file).

Refactored auto_vif and auto_cor to improve how they generate preference.order automatically when the user does not provide it. Now both functions can accept a preference.order of length 1.

Added the function `rf_select()` to select variables using the performance of univariate models as criteria.

The function `rf_repeat()` now can be run with `repetitions = 1`.

Changed the default value of the argument cor.threshold in `auto_cor()` from 0.5 to 0.75.

Added the argument `top.n` to the function `plot_importance()` to allow the user to plot the most `n` important predictors.

The function `rf_evaluate()` now filters out the results of redundant spatial folds.

Fixed a bug in rf_evaluate where metrics could be mixed due to a badly rearranged column names inside of the function. My apologies for any inconveniences this bug might have caused.

Added the functions `make_cluster()` and `stop_cluster()`.

Made the function `rf_evaluate()` more efficient, but now it cannot return the training and testing folds it used during evaluation. In return, it can handle large data now when reducing `n.cores`.

Changed the names of the example data, added new predictors, and a couple of new objects. All these objects are documented in the file `ecoregions_df.R`.

  - `plant_richness_df` is now `ecoregions_df`. I added new predictors to this data frame, and removed two ecoregions.
  - `distance_matrix` is now `ecoregions_distance_matrix`.
  - `ecoregion_polygons` is an "sf" data frame with the simplified ecoregion polygons.
  - `ecoregions_predictor_variable_names` is a character vector with the names of the predictors in `ecoregions_df`. 
  - `ecoregions_dependent_variable_name` is a character string with the name of the dependent variable in `ecoregions_df`.
  
Changed the names of these objects in the package documentation, the README, and the tests folder.

Fixed an issue with `mem_multithreshold()`, `pca_multithreshold()`, and `moran_multithreshold()` where giving values of `distance.thresholds` larger than the maximum of the distance matrix would yield an error.

## Version 1.1.3 (22/9/2021)

Added the function `rf_importance()`. It fits models with and without each predictor, compares them via spatial cross validation with `rf_evaluate()`, and returns the increase/decrease in performance when a given variable is included in the model.

The default random seed for all functions have changed from `NULL` to `1` to facilitate reproducibility.

The function `rf_evaluate()` has a new argument named `grow.testing.folds`. When set to `TRUE`, it uses `1 - training.fraction` instead of `training.fraction` to grow the spatial folds, and then flips the names of the training and testing folds. As a result, the testing folds are generally surrounded by the training folds (just the opposite of the default behavior of the function), which might be beneficial for particular spatial structures of the training data. Thanks to **Aleksandra Kulawska** for the suggestion!

## Version 1.1.2 (1/7/2021)

Overhaul of the methods used for parallelization. The functions `rf_spatial()`, `rf_repeat()`, `rf_evaluate()`, `rf_tuning()`, `rf_compare()`, and `rf_interactions()` can now accept a cluster definition generated with `parallel::makeCluster()` via the `cluster` argument. Also, models resulting from these functions and `rf()` carry the cluster definition with themselves in the slot `model$cluster`, so the cluster definition can be passed from function to function using a pipe, as shown below:

```
library(spatialRF)
library(magrittr)

#loading the example data
data(plant_richness_df)
data("distance_matrix")
xy <- plant_richness_df[, c("x", "y")]
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]


#creating cluster
my.cluster <- parallel::makeCluster(
  4,
  type = "PSOCK"
)

#registering cluster (rf functions register it anyway)
doParallel::registerDoParallel(cl = cluster)

  #fitting model
  m <- rf(
    data = plant_richness_df,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    distance.matrix = distance_matrix,
    xy = xy,
    cluster = my.cluster
  ) %>%
  rf_spatial() %>%
  rf_tuning() %>%
  rf_evaluate() %>%
  rf_repeat()

#stopping cluster
parallel::stopCluster(cl = my.cluster)
```

The system works as follows: If `cluster` is not `NULL` and `model` is provided, the function looks into the model. If there is a cluster definition there, it is used to parallelize computations, but the cluster is not stopped within the function. If there is not a cluster in `model`, then the function falls back to the argument `n.cores` to generate a cluster that is stopped when the function ends its operations.

These changes should improve performance when working with several functions in the same script, becuase these functions do not have to waste time in generating their own clusters.

The function `rf_interactions()` is now named `the_feature_engineer()`.

The function `cluster_definition()` is now named `beowulf_cluster()`, and returns a cluster instead of a cluster definition to be used as input for `parallel::makeCluster()`.

## Version 1.1.1 (11/5/2021)

rf_repeat() now generates a proper "importance" slot for models fitted with rf_spatial(), and preserves the "evaluation" and "tuning" slots if they exist.

Simplified rf_spatial() by removing options to generate an rf_repeat() model on the fly. rf_repeat() should only be used now at the end of a workflow, as described in the documentation.

Fixed issue with the area of the violin plots generated by plot_importance().

Improved the function rf_interactions() with a new type of interaction (first factor of a PCA between two predictors), added criteria to reduce multicollinearity among interactions, and between interactions and predictors, and now the function returns data helpful to fit models right away.

## Version 1.1.0 (6/5/2021)

Added new residuals diagnostics with the functions residuals_diagnostics() and plot_residuals_diagnostics(). This changed the name of the slot "spatial.autocorrelation.residuals" to "residuals", that now stores all the information relative to the residuals. 

All plotting functions now allow to change the color of their key components.

## Version 1.0.9 (20/4/2021)

Changed the names of function arguments from 'x' to 'model' or 'distance.matrix' for consistency. This might break code written previously, but I hope argument names are more self-explanatory now.

The function rf_spatial() now fits a non-spatial model first, and only generates spatial predictors for these distance.thresholds that show positive spatial autocorrelation.

## Version 1.0.8 (16/4/2021)

Added a new function named filter_spatial_predictors(), that removes redundant spatial predictors within rf_spatial(). It shouldn't lead to changes in the spatial models fitted with previous versions, but it will make them more parsimonious.

Changed the style of the package's boxplots.

When using rf_repeat(), the median of the variable importance scores, performance scores, and Moran's I is reported, instead of the mean.

## Version 1.0.7 (9/3/2021)

Added the functions plot_training_data() and plot_moran_training_data() to help explore the training data prior to modeling.

Also fixed an issue where response variables could be identified as binary by mistake.

## Version 1.0.6 (9/3/2021)

A bug regarding the predictions generated by `rf()` that affected every other function fitting models has been fixed. Previously, the model predictions came from the "predictions" slot produced by `ranger()`. Such predictions are produced from the out-of-bag data during model training, and are different and lead to lower R squared values than those produced with predict(). Now the predictions yielded by rf() are generated with predict(), and therefore you might notice that now models fitted with spatialRF functions perform better than before, because they do.

The function `print_evaluation()` does not use huxtable any longer to print the evaluation results, and only shows the results of the testing model.

## Version 1.0.5 (6/3/2021)

Added support for binary data (0 and 1). The function `rf()` now tests if the data is binary, and if so, it populates the `case.weights` argument of `ranger` with the new function `case_weights()` to minimize the side effects of unbalanced data.

## Version 1.0.4 (2/3/2021)

Fixed an issue where rf() applied the wrong is.numeric check to the response variable and the predictors that caused issues with tibbles.

## Version 1.0.3 (25/2/2021)

Removed the function scale_robust() from rf(), and replaced it with scale(). It was giving more troubles than benefits.

## Version 1.0.2 (24/2/2021)

Simplified rf_spatial().

Modified rf_tuning() to better tune models fitted with rf_spatial().

Minor fixes in several other functions.

## Version 1.0.1 (22/2/2021)

All 'sf' dependencies removed from the package.

## Version 1.0.0 is ready!
