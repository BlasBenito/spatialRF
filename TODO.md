## Doesn't run

select_spatial_predictors_recursive-examples

## Slow tests

get_spatial_predictors-examples
rf_spatial-examples
rf_tuning-examples
select_spatial_predictors_recursive-examples


## spatialRF::plot_training_df()

Warning :`aes_string()` was deprecated in ggplot2 3.0.0.
ℹ Please use tidy evaluation idioms with `aes()`.
ℹ See also `vignette("ggplot2-in-packages")` for more
  information.
ℹ The deprecated feature was likely used in the spatialRF
  package.
  Please report the issue at
  <https://github.com/BlasBenito/spatialRF/issues/>.
  
## spatialRF::plot_training_df_moran()

Warning :Using `size` aesthetic for lines was deprecated in ggplot2
3.4.0.
ℹ Please use `linewidth` instead.
ℹ The deprecated feature was likely used in the spatialRF
  package.
  Please report the issue at
  <https://github.com/BlasBenito/spatialRF/issues/>.

## remove spatialRF::auto_cor() and spatialRF::auto_vif() with call to collinear?

## spatialRF::the_feature_engineer()

interactions <- spatialRF::the_feature_engineer(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  xy = xy,
  importance.threshold = 0.50, #uses 50% best predictors
  cor.threshold = 0.60, #max corr between interactions and predictors
  seed = random.seed,
  repetitions = 100,
  verbose = TRUE
  )
  
Error in validate_prop(value, prop, check_fun, check_values, reset_na) : 
  check_fun(value) is not TRUE
