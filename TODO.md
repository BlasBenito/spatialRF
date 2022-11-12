#TODO

## add function to plot roc curve

## Remove r.squared, rmse, and nrmse for binary responses

## Add sensitivity and specificty for binary models

## Add point biserial correlation for binary outcomes: https://www.r-bloggers.com/2021/07/point-biserial-correlation-in-r-quick-guide/

## simplify distance.step.x and distance.step.y to distance.step with a vector with two values as input.

## support sf as input

## Make clear what arguments are optional or required

auto_vif()
auto_cor()
rf()
rf_repeat()

## Sparse distance matrix

## Predict to raster

## Accept sf

## Return tibble if input is tibble

auto_vif()
auto_cor()
rf()
rf_repeat()
rf_evaluate()

## Accept factor predictors

## document get_xxx() together.

## document plot_xx() together.

## document print_xx() together

## Implement robust argument handling rules in all functions

  + rf() DONE
  + rf_select() DONE
  + rf_repeat() DONE
  + rf_spatial() DONE
  + rf_tuning() DONE
  
## Update tests for al modified functions

  + rf() DONE
  + rf_select() DONE
  + rf_repeat() DONE
  + auto_cor() DONE
  + auto_vif() DONE
  + rf_spatial() DONE
  + rf_tuning() DONE
  
## Add cluster handling rules and add cluster to model output when required

  + rf() DONE
  + rf_select() DONE
  + rf_repeat() DONE
  + rf_spatial() DONE
  + rf_tuning() DONE

## the_feature_engineer()

  + Change name to rf_engineer().
  + Accept model as input.

## auto_vif() and auto_cor(): accept preference order of length 1.

## rf_tuning() should be able to use any metric and max.depth
