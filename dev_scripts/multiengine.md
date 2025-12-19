# Multi-Engine Architecture for spatialML

**Status:** Planning / Design Document
**Created:** 2025-12-14
**Purpose:** Architectural planning for extending spatialRF to support multiple modeling engines

---

## Executive Summary

This document outlines a strategy for evolving spatialRF into a multi-engine spatial modeling framework (potentially spatialML), supporting Random Forest, linear models, XGBoost, GAMs, and other algorithms while maintaining the core spatial predictor functionality.

---

## Motivation

### Current State
- spatialRF is tightly coupled to the ranger Random Forest implementation
- Spatial predictor generation and modeling are intertwined
- Users cannot easily compare spatial models across different algorithms
- Some domains prefer interpretable linear models or GAMs over RF

### Desired State
- Users can fit spatial models with multiple algorithms
- Spatial predictor generation is algorithm-agnostic
- Consistent interface across all engines
- Easy model comparison across algorithms
- Backward compatibility with current spatialRF API

### Use Cases

**Ecology/Biogeography:**
- Compare RF vs. GAM for species distribution
- Use linear models for coefficient interpretation
- XGBoost for prediction competitions

**Environmental Science:**
- GAMs for interpretable environmental relationships
- RF for complex nonlinear patterns
- Linear models for regulatory reporting

**Social Science:**
- Linear models for hypothesis testing
- RF for prediction and variable selection
- Ensemble across engines

---

## Architecture Options

### Option 1: Engine Abstraction Layer (Custom)

Create an internal engine system where spatial predictor generation is decoupled from the modeling backend.

#### User-Facing API

```r
# Unified interface
model <- spatial_fit(
  data = df,
  response = "y",
  predictors = c("x1", "x2"),
  engine = "rf",  # or "lm", "xgboost", "gam"
  spatial_method = "mem",
  distance_matrix = dm
)

# Or with engine objects for more control
model <- spatial_fit(
  data = df,
  response = "y",
  predictors = c("x1", "x2"),
  engine = engine_rf(num_trees = 1000, mtry = 3),
  spatial = spatial_config(
    method = "mem",
    distance_matrix = dm,
    distance_thresholds = c(0, 1000, 2000)
  )
)
```

#### Implementation Structure

```r
# R/engines/engine_base.R
# Abstract interface all engines must implement

engine_base <- function() {
  structure(
    list(
      name = character(),
      supports_importance = logical(),
      supports_oob = logical(),
      supports_probabilities = logical()
    ),
    class = "spatial_engine"
  )
}

# Required methods for all engines:
engine_fit <- function(engine, ...) UseMethod("engine_fit")
engine_predict <- function(engine, model, newdata, ...) UseMethod("engine_predict")
engine_importance <- function(engine, model, ...) UseMethod("engine_importance")
engine_tune_grid <- function(engine) UseMethod("engine_tune_grid")
```

#### Pros
- Full control over implementation
- Can optimize for spatial-specific needs
- No external framework dependencies
- Clean separation of concerns

#### Cons
- Reinventing the wheel
- Higher maintenance burden
- Need to implement each engine from scratch
- May miss ecosystem benefits

---

### Option 2: Build on tidymodels/parsnip

Integrate with the parsnip framework, which already provides unified interfaces for 100+ model types.

#### User-Facing API

```r
library(tidymodels)
library(spatialML)

# Define spatial preprocessing with recipes
spatial_recipe <- recipe(y ~ ., data = df) %>%
  step_spatial_predictors(
    distance_matrix = dm,
    method = "mem",
    distance_thresholds = c(0, 1000, 2000)
  ) %>%
  step_spatial_filter(moran_threshold = 0.05)

# Use any parsnip engine
rf_spec <- rand_forest(trees = 1000, mtry = 3) %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_spec <- boost_tree(trees = 1000) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

lm_spec <- linear_reg() %>%
  set_engine("lm")

# Fit with spatial workflow
spatial_wf <- workflow() %>%
  add_recipe(spatial_recipe) %>%
  add_model(rf_spec)

model <- fit(spatial_wf, data = df)

# Spatial cross-validation with rsample
spatial_folds <- spatial_cv(df, coords = c("x", "y"), v = 10)
cv_results <- fit_resamples(spatial_wf, spatial_folds)
```

#### Implementation Structure

```r
# R/recipes/step_spatial_predictors.R
# Custom recipe step for spatial predictor generation

step_spatial_predictors <- function(
  recipe,
  ...,
  distance_matrix = NULL,
  method = "mem",
  distance_thresholds = NULL,
  role = "predictor",
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = rand_id("spatial_predictors")
) {
  # Implementation
}

# R/rsample/spatial_cv.R
# Custom rsample function for spatial cross-validation

spatial_cv <- function(data, coords, v = 10, ...) {
  # Use make_spatial_folds() internally
}
```

#### Pros
- Leverages mature, well-tested ecosystem
- Automatic support for 100+ model types
- Integration with tune, workflows, workflowsets
- Familiar API for tidymodels users
- Active development and community
- Built-in parallel processing with furrr

#### Cons
- Learning curve for non-tidymodels users
- Less control over internals
- May need custom recipe steps
- Current users need to learn new API
- Heavier dependency footprint

---

### Option 3: Hybrid Approach (Recommended)

Keep current API for backward compatibility but abstract the backend, with optional tidymodels integration for advanced users.

#### User-Facing API

```r
# LEGACY: Current API (fully backward compatible)
model <- rf_spatial(
  data = df,
  dependent.variable.name = "y",
  predictor.variable.names = c("x1", "x2"),
  distance.matrix = dm
)

# NEW: Unified API with engine support
model <- spatial_fit(
  data = df,
  response = "y",
  predictors = c("x1", "x2"),
  engine = engine_rf(num_trees = 1000),
  spatial = spatial_config(
    method = "mem",
    distance_matrix = dm
  )
)

# ADVANCED: tidymodels integration (opt-in)
library(tidymodels)
spatial_recipe <- recipe(y ~ ., data = df) %>%
  add_spatial_predictors(distance_matrix = dm)

workflow() %>%
  add_recipe(spatial_recipe) %>%
  add_model(rand_forest() %>% set_engine("ranger")) %>%
  fit(data = df)
```

#### Implementation Structure

```
R/
  # Legacy functions (wrappers around new system)
  rf.R                    # Calls spatial_fit() with engine_rf()
  rf_spatial.R            # Calls spatial_fit() + spatial optimization

  # New unified system
  spatial_fit.R           # Main fitting function
  spatial_config.R        # Spatial configuration

  # Engine system
  engines/
    engine_base.R         # Abstract interface
    engine_rf.R           # Random Forest (ranger)
    engine_lm.R           # Linear models
    engine_xgb.R          # XGBoost
    engine_gam.R          # GAMs (mgcv)

  # Spatial logic (engine-agnostic)
  spatial/
    mem.R                 # Moran's Eigenvector Maps
    distance_predictors.R # Distance-based predictors
    spatial_cv.R          # Spatial cross-validation
    moran.R               # Moran's I testing
    optimize.R            # Spatial predictor selection

  # Evaluation (mostly engine-agnostic)
  evaluate.R              # Spatial cross-validation
  compare.R               # Multi-model comparison
  importance.R            # Variable importance (S3 methods)

  # Tidymodels integration (optional)
  recipes/
    step_spatial_predictors.R
    step_spatial_filter.R
  rsample/
    spatial_cv.R
```

#### Pros
- Maintains backward compatibility
- Gradual migration path for users
- Can support both simple and advanced workflows
- Optional tidymodels integration
- Full control over core functionality

#### Cons
- More complex codebase (two APIs to maintain)
- Need clear documentation on when to use each API
- May confuse new users

---

## Implementation Strategy

### Phase 0: Refactoring Preparation

**Goal:** Separate spatial logic from modeling without breaking changes.

#### Tasks

1. **Audit current dependencies**
   - Identify all ranger-specific code
   - Document assumptions about Random Forest
   - List features that require RF (OOB error, etc.)

2. **Create abstraction layer**
   - Define engine interface
   - Implement base engine class
   - Create engine registry system

3. **Refactor spatial functions**
   - Make `mem()` completely independent
   - Make `mem_multithreshold()` independent
   - Make `moran()` independent
   - Make `make_spatial_folds()` independent

4. **Refactor evaluation functions**
   - Separate CV logic from RF assumptions
   - Make performance metrics engine-agnostic
   - Create S3 dispatch for engine-specific operations

#### File Reorganization

```r
# Before
R/
  rf.R                    # Mix of RF and spatial logic
  rf_spatial.R            # Mix of RF and spatial logic
  mem.R                   # Already independent

# After
R/
  spatial/               # Pure spatial logic
    mem.R
    distance_predictors.R
    spatial_cv.R
    moran.R
    optimize.R

  engines/              # Engine implementations
    engine_base.R
    engine_rf.R         # Current code moved here

  modeling/             # Engine-agnostic modeling
    fit.R
    predict.R
    evaluate.R
    tune.R

  legacy/               # Backward compatible wrappers
    rf.R                # Calls modeling/fit.R
    rf_spatial.R        # Calls modeling/fit.R + spatial/optimize.R
```

---

### Phase 1: Add Linear Model Engine

**Goal:** Prove the engine concept with the simplest case.

#### Implementation

```r
# R/engines/engine_lm.R

engine_lm <- function(family = gaussian(), ...) {
  structure(
    list(
      name = "lm",
      family = family,
      extra_args = list(...),
      supports_importance = TRUE,   # Via permutation
      supports_oob = FALSE,          # No OOB for LM
      supports_probabilities = TRUE  # For GLM
    ),
    class = c("engine_lm", "spatial_engine")
  )
}

engine_fit.engine_lm <- function(engine, data, response, predictors, ...) {
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))

  if (inherits(engine$family, "family")) {
    # GLM
    model <- glm(formula, data = data, family = engine$family, ...)
  } else {
    # LM
    model <- lm(formula, data = data, ...)
  }

  structure(
    list(
      model = model,
      engine = engine,
      response = response,
      predictors = predictors
    ),
    class = c("spatial_lm", "spatial_model")
  )
}

engine_predict.engine_lm <- function(engine, model, newdata, type = "response", ...) {
  predict(model$model, newdata = newdata, type = type, ...)
}

engine_importance.engine_lm <- function(engine, model, method = "permutation", ...) {
  if (method == "coefficient") {
    # Standardized coefficients
    coef_vals <- abs(coef(model$model)[-1])  # Remove intercept
    importance <- data.frame(
      variable = names(coef_vals),
      importance = coef_vals,
      method = "coefficient"
    )
  } else if (method == "permutation") {
    # Permutation importance (similar to RF)
    importance <- permutation_importance(model, ...)
  }

  importance
}
```

#### User API

```r
# Fit linear model with spatial predictors
model.lm <- spatial_fit(
  data = plant_richness_df,
  response = "richness_species_vascular",
  predictors = predictor.variable.names,
  engine = engine_lm(),
  spatial = spatial_config(
    method = "mem",
    distance_matrix = distance_matrix,
    distance_thresholds = distance_thresholds
  )
)

# Evaluate with spatial CV (same as RF)
model.lm <- spatial_evaluate(
  model.lm,
  coords = xy,
  repetitions = 30
)

# Compare with RF
comparison <- spatial_compare(
  models = list(
    "Random Forest" = model.rf,
    "Linear Model" = model.lm
  )
)
```

#### Challenges

1. **No OOB error:** Use CV-based metrics instead
2. **Variable importance:** Implement permutation or use coefficients
3. **Different diagnostics:** LM has residual plots, normality tests
4. **Assumptions:** LM assumes linearity, RF doesn't

---

### Phase 2: Add XGBoost Engine

**Goal:** Add high-performance tree-based alternative to Random Forest.

#### Implementation

```r
# R/engines/engine_xgb.R

engine_xgboost <- function(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  ...
) {
  structure(
    list(
      name = "xgboost",
      params = list(
        nrounds = nrounds,
        max_depth = max_depth,
        eta = eta,
        gamma = gamma,
        subsample = subsample,
        colsample_bytree = colsample_bytree,
        ...
      ),
      supports_importance = TRUE,
      supports_oob = FALSE,
      supports_probabilities = TRUE
    ),
    class = c("engine_xgboost", "spatial_engine")
  )
}

engine_fit.engine_xgboost <- function(engine, data, response, predictors, ...) {
  # Prepare data
  X <- as.matrix(data[, predictors])
  y <- data[[response]]

  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

  # Fit model
  model <- xgboost::xgb.train(
    params = engine$params,
    data = dtrain,
    nrounds = engine$params$nrounds,
    ...
  )

  structure(
    list(
      model = model,
      engine = engine,
      response = response,
      predictors = predictors
    ),
    class = c("spatial_xgb", "spatial_model")
  )
}

engine_importance.engine_xgboost <- function(engine, model, ...) {
  imp <- xgboost::xgb.importance(
    model = model$model,
    feature_names = model$predictors
  )

  data.frame(
    variable = imp$Feature,
    importance = imp$Gain,
    method = "gain"
  )
}
```

#### Benefits of XGBoost

- Often outperforms Random Forest
- Handles missing data natively
- Faster training on large datasets
- Built-in regularization
- Can output feature interactions

#### User API

```r
model.xgb <- spatial_fit(
  data = plant_richness_df,
  response = "richness_species_vascular",
  predictors = predictor.variable.names,
  engine = engine_xgboost(
    nrounds = 1000,
    max_depth = 6,
    eta = 0.01,
    subsample = 0.8
  ),
  spatial = spatial_config(
    method = "mem",
    distance_matrix = distance_matrix
  )
)
```

---

### Phase 3: Add GAM Engine

**Goal:** Add interpretable smooth modeling for spatial data.

#### Implementation

```r
# R/engines/engine_gam.R

engine_gam <- function(
  family = gaussian(),
  method = "REML",
  select = FALSE,
  ...
) {
  structure(
    list(
      name = "gam",
      family = family,
      method = method,
      select = select,
      extra_args = list(...),
      supports_importance = TRUE,   # Via smooth significance
      supports_oob = FALSE,
      supports_probabilities = TRUE
    ),
    class = c("engine_gam", "spatial_engine")
  )
}

engine_fit.engine_gam <- function(engine, data, response, predictors, ...) {
  # Build formula with smooths
  smooth_terms <- paste0("s(", predictors, ")")
  formula_str <- paste(response, "~", paste(smooth_terms, collapse = "+"))
  formula <- as.formula(formula_str)

  model <- mgcv::gam(
    formula,
    data = data,
    family = engine$family,
    method = engine$method,
    select = engine$select,
    ...
  )

  structure(
    list(
      model = model,
      engine = engine,
      response = response,
      predictors = predictors
    ),
    class = c("spatial_gam", "spatial_model")
  )
}

engine_importance.engine_gam <- function(engine, model, ...) {
  # Use p-values or explained deviance of smooth terms
  summary_obj <- summary(model$model)

  importance <- data.frame(
    variable = gsub("s\\((.+)\\)", "\\1", rownames(summary_obj$s.table)),
    importance = summary_obj$s.table[, "edf"],  # Effective degrees of freedom
    p_value = summary_obj$s.table[, "p-value"],
    method = "smooth_significance"
  )

  importance
}
```

#### Benefits of GAM

- Interpretable smooth functions
- Can include spatial smooth: `s(x, y)`
- Natural for environmental gradients
- Good for checking RF results
- Statistical inference available

#### Advanced: Direct Spatial Smooth

```r
# GAM with direct spatial smooth instead of MEMs
model.gam <- spatial_fit(
  data = plant_richness_df,
  response = "richness_species_vascular",
  predictors = predictor.variable.names,
  engine = engine_gam(
    spatial_smooth = "s(x, y, bs = 'gp')",  # Gaussian process smooth
    family = gaussian()
  ),
  spatial = spatial_config(
    method = "none"  # Don't add MEMs, using direct smooth
  )
)
```

---

## Key Design Decisions

### 1. What Stays Engine-Agnostic?

#### ✅ Engine-Agnostic (Core Spatial Functionality)

- **Spatial predictor generation**
  - `mem()` - Moran's Eigenvector Maps
  - `mem_multithreshold()` - Multi-scale MEMs
  - Distance-based predictors
  - All work independently of modeling engine

- **Spatial cross-validation**
  - `make_spatial_folds()` - Fold generation
  - `spatial_evaluate()` - CV execution
  - Performance metric calculation
  - Works with any engine

- **Moran's I testing**
  - `moran()` - Autocorrelation testing
  - `plot_moran()` - Visualization
  - Works on any model residuals

- **Model comparison**
  - `spatial_compare()` - Compare multiple models
  - Cross-engine comparisons
  - Consistent metrics

- **Response curve framework**
  - Partial dependence concept
  - Grid generation
  - Plotting infrastructure

#### ❌ Engine-Specific

- **Variable importance**
  - RF: Permutation importance (OOB)
  - LM: Coefficient magnitude or permutation
  - XGBoost: Gain, cover, frequency
  - GAM: Smooth term significance
  - *Solution:* S3 methods + common interface

- **Hyperparameter tuning**
  - RF: num.trees, mtry, min.node.size
  - XGBoost: nrounds, max_depth, eta, etc.
  - LM: None or regularization params
  - GAM: Smoothing parameter selection
  - *Solution:* Engine-specific tuning grids

- **Model diagnostics**
  - RF: OOB error, tree depth distributions
  - LM: Residual plots, normality tests, multicollinearity
  - XGBoost: Training/validation curves
  - GAM: Smooth term plots, concurvity
  - *Solution:* Engine-specific diagnostic functions

- **Response curves**
  - Implementation differs between engines
  - Linear: Analytical
  - Tree-based: Prediction averaging
  - GAM: Smooth term evaluation
  - *Solution:* Engine-specific implementations with common output format

---

### 2. How to Handle Engine-Specific Features?

Use **S3 method dispatch** with a common interface:

```r
# Generic functions
importance(model)
tune(model, ...)
diagnostics(model)
response_curves(model, ...)

# S3 Methods for Random Forest
importance.spatial_rf <- function(model, type = "permutation", ...) {
  # Use ranger's built-in importance
  model$model$variable.importance
}

diagnostics.spatial_rf <- function(model, ...) {
  # RF-specific: OOB error, tree depth
  list(
    oob_error = model$model$prediction.error,
    num_trees = model$model$num.trees,
    tree_depth = mean(treeInfo(model$model)$depth)
  )
}

# S3 Methods for Linear Models
importance.spatial_lm <- function(model, type = "coefficient", ...) {
  if (type == "coefficient") {
    # Standardized coefficients
    coefs <- abs(coef(model$model)[-1])
    data.frame(variable = names(coefs), importance = coefs)
  } else {
    # Permutation importance
    permutation_importance(model)
  }
}

diagnostics.spatial_lm <- function(model, ...) {
  # LM-specific: R-squared, residual diagnostics
  list(
    r_squared = summary(model$model)$r.squared,
    adj_r_squared = summary(model$model)$adj.r.squared,
    sigma = summary(model$model)$sigma,
    normality_test = shapiro.test(residuals(model$model))
  )
}

# S3 Methods for XGBoost
importance.spatial_xgb <- function(model, type = "gain", ...) {
  imp <- xgboost::xgb.importance(
    model = model$model,
    feature_names = model$predictors
  )

  # Return requested importance type
  importance_col <- switch(type,
    "gain" = imp$Gain,
    "cover" = imp$Cover,
    "frequency" = imp$Frequency,
    imp$Gain
  )

  data.frame(
    variable = imp$Feature,
    importance = importance_col,
    type = type
  )
}

# S3 Methods for GAM
importance.spatial_gam <- function(model, ...) {
  summ <- summary(model$model)

  data.frame(
    variable = gsub("s\\((.+)\\)", "\\1", rownames(summ$s.table)),
    importance = summ$s.table[, "edf"],
    p_value = summ$s.table[, "p-value"]
  )
}
```

#### Common Output Format

All importance methods should return consistent structure:

```r
# Standard importance data frame
data.frame(
  variable = character(),      # Variable name
  importance = numeric(),      # Importance score
  importance_type = character(), # "permutation", "gain", "coefficient", etc.
  rank = integer()            # Rank order
)
```

---

### 3. Unified vs. Engine-Specific Arguments

#### Approach: Hybrid System

```r
spatial_fit(
  # Common arguments (all engines)
  data = df,
  response = "y",
  predictors = c("x1", "x2"),

  # Spatial configuration (all engines)
  spatial = spatial_config(
    method = "mem",
    distance_matrix = dm,
    distance_thresholds = c(0, 1000, 2000),
    optimize = TRUE
  ),

  # Engine specification (engine-specific)
  engine = engine_rf(
    num_trees = 1000,
    mtry = 3,
    min_node_size = 5
  ),

  # Or
  engine = engine_xgboost(
    nrounds = 1000,
    max_depth = 6,
    eta = 0.01
  ),

  # Cross-validation (all engines)
  cv = cv_config(
    repetitions = 30,
    training_fraction = 0.75
  ),

  # Other common arguments
  seed = 123,
  verbose = TRUE
)
```

---

### 4. Package Naming Strategy

Three options for package evolution:

#### Option A: Evolution within spatialRF (Recommended)

- **v1.x:** Current spatialRF (RF only)
- **v2.0:** Modernized spatialRF (RF only, improved backend)
- **v2.5:** Multi-engine support added to spatialRF
- **v3.0:** Full multi-engine, RF remains default

**Pros:**
- Maintains existing user base
- No confusion about which package to use
- Continuous evolution
- Single codebase to maintain

**Cons:**
- Name doesn't reflect multi-engine capability
- May confuse users expecting RF only

#### Option B: New Package (spatialML)

- **spatialRF:** Maintenance mode, RF only
- **spatialML:** New package, multi-engine from start
- spatialML imports spatialRF for RF engine

**Pros:**
- Clear separation of concerns
- Name reflects multi-engine capability
- Can start fresh with clean API
- No backward compatibility constraints

**Cons:**
- Splits user base
- Duplicate maintenance burden
- Users must choose between packages
- May reduce overall adoption

#### Option C: Rename on Major Version

- **v1.x:** spatialRF
- **v2.0:** Rename to spatialML, add deprecation notice
- Package provides both names for transition period

**Pros:**
- Clear evolution path
- Name reflects new capability
- Single package to maintain

**Cons:**
- May break existing code
- CRAN submission complexity
- User confusion during transition

---

## Backward Compatibility Strategy

### Maintain Legacy API

All current functions remain unchanged:

```r
# These continue to work exactly as before
model <- rf(data = df, ...)
model <- rf_spatial(model = model, ...)
model <- rf_evaluate(model = model, ...)
model <- rf_compare(models = list(...))
```

### Implementation: Wrapper Functions

```r
# R/legacy/rf.R
#' @export
rf <- function(
  data,
  dependent.variable.name,
  predictor.variable.names,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  ...
) {
  # Translate to new API
  model <- spatial_fit(
    data = data,
    response = dependent.variable.name,
    predictors = predictor.variable.names,
    engine = engine_rf(...),  # ... contains ranger args
    spatial = if (!is.null(distance.matrix)) {
      spatial_config(
        method = "none",  # Don't auto-add spatial predictors
        distance_matrix = distance.matrix,
        distance_thresholds = distance.thresholds
      )
    } else {
      NULL
    }
  )

  # Ensure output structure matches legacy format
  structure_as_legacy(model)
}

# R/legacy/rf_spatial.R
#' @export
rf_spatial <- function(
  model = NULL,
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  method = "mem.moran.sequential",
  ...
) {
  # Handle both model input and data input
  if (!is.null(model)) {
    # Update existing model
    model <- spatial_optimize(model, method = method, ...)
  } else {
    # Fit new model with spatial predictors
    model <- spatial_fit(
      data = data,
      response = dependent.variable.name,
      predictors = predictor.variable.names,
      engine = engine_rf(...),
      spatial = spatial_config(
        method = method,
        distance_matrix = distance.matrix,
        distance_thresholds = distance.thresholds,
        optimize = TRUE
      )
    )
  }

  structure_as_legacy(model)
}
```

---

## Model Comparison Framework

### Cross-Engine Comparison

Users should be able to easily compare models across engines:

```r
# Fit multiple engines
model.rf <- spatial_fit(
  data = df,
  response = "y",
  predictors = predictors,
  engine = engine_rf(),
  spatial = spatial_config(...)
)

model.xgb <- spatial_fit(
  data = df,
  response = "y",
  predictors = predictors,
  engine = engine_xgboost(),
  spatial = spatial_config(...)
)

model.lm <- spatial_fit(
  data = df,
  response = "y",
  predictors = predictors,
  engine = engine_lm(),
  spatial = spatial_config(...)
)

model.gam <- spatial_fit(
  data = df,
  response = "y",
  predictors = predictors,
  engine = engine_gam(),
  spatial = spatial_config(...)
)

# Compare all models
comparison <- spatial_compare(
  models = list(
    "Random Forest" = model.rf,
    "XGBoost" = model.xgb,
    "Linear Model" = model.lm,
    "GAM" = model.gam
  ),
  coords = xy,
  repetitions = 30,
  metrics = c("rmse", "r_squared", "mae")
)

# Visualization
plot(comparison)  # Box plots of CV performance
summary(comparison)  # Statistical comparison
```

### Output Format

```r
# comparison object structure
list(
  models = list(...),           # Original models
  cv_results = data.frame(      # Cross-validation results
    model = character(),
    fold = integer(),
    metric = character(),
    value = numeric()
  ),
  summary = data.frame(         # Summary statistics
    model = character(),
    metric = character(),
    median = numeric(),
    mad = numeric(),
    min = numeric(),
    max = numeric()
  ),
  best_model = character(),     # Name of best performing model
  spatial_autocorrelation = data.frame(  # Moran's I for each model
    model = character(),
    distance_threshold = numeric(),
    moran_i = numeric(),
    p_value = numeric()
  )
)
```

---

## tidymodels Integration (Optional Advanced Track)

For users who want full tidymodels ecosystem integration:

### Custom Recipe Steps

```r
# R/recipes/step_spatial_predictors.R

#' Add Spatial Predictors to Recipe
#'
#' Creates spatial predictors using Moran's Eigenvector Maps
#'
#' @export
step_spatial_predictors <- function(
  recipe,
  ...,
  distance_matrix = NULL,
  method = "mem",
  distance_thresholds = NULL,
  max_predictors = NULL,
  role = "predictor",
  trained = FALSE,
  spatial_predictors = NULL,
  skip = FALSE,
  id = recipes::rand_id("spatial_predictors")
) {

  recipes::add_step(
    recipe,
    step_spatial_predictors_new(
      terms = recipes::ellipse_check(...),
      distance_matrix = distance_matrix,
      method = method,
      distance_thresholds = distance_thresholds,
      max_predictors = max_predictors,
      role = role,
      trained = trained,
      spatial_predictors = spatial_predictors,
      skip = skip,
      id = id
    )
  )
}

step_spatial_predictors_new <- function(...) {
  recipes::step(
    subclass = "spatial_predictors",
    ...
  )
}

#' @export
prep.step_spatial_predictors <- function(x, training, info = NULL, ...) {
  # Generate spatial predictors from training data
  if (x$method == "mem") {
    spatial_preds <- mem_multithreshold(
      distance_matrix = x$distance_matrix,
      distance_thresholds = x$distance_thresholds
    )
  }

  # Rank and select
  ranked <- rank_spatial_predictors(
    distance_matrix = x$distance_matrix,
    spatial_predictors_df = spatial_preds
  )

  if (!is.null(x$max_predictors)) {
    spatial_preds <- spatial_preds[, ranked$ranking[1:x$max_predictors]]
  }

  step_spatial_predictors_new(
    terms = x$terms,
    distance_matrix = x$distance_matrix,
    method = x$method,
    distance_thresholds = x$distance_thresholds,
    max_predictors = x$max_predictors,
    role = x$role,
    trained = TRUE,
    spatial_predictors = spatial_preds,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_spatial_predictors <- function(object, new_data, ...) {
  # Add spatial predictors to new data
  cbind(new_data, object$spatial_predictors)
}
```

### Spatial Resampling for rsample

```r
# R/rsample/spatial_cv.R

#' Spatial Cross-Validation
#'
#' Creates spatially separated folds for cross-validation
#'
#' @export
spatial_clustering_cv <- function(
  data,
  coords = c("x", "y"),
  v = 10,
  ...
) {
  # Extract coordinates
  xy <- data[, coords]

  # Use existing make_spatial_folds()
  folds <- make_spatial_folds(
    xy = xy,
    repetitions = v,
    training_fraction = 0.75
  )

  # Convert to rsample format
  splits <- purrr::map(folds, function(fold) {
    rsample::make_splits(
      ind = list(
        analysis = fold$training,
        assessment = fold$testing
      ),
      data = data
    )
  })

  rsample::new_rset(
    splits = splits,
    ids = paste0("Fold", seq_along(splits)),
    attrib = list(v = v, coords = coords),
    subclass = c("spatial_clustering_cv", "rset")
  )
}
```

### Complete tidymodels Workflow Example

```r
library(tidymodels)
library(spatialML)

# Recipe with spatial predictors
spatial_rec <- recipe(richness ~ ., data = plant_richness_df) %>%
  step_spatial_predictors(
    distance_matrix = distance_matrix,
    method = "mem",
    max_predictors = 20
  ) %>%
  step_normalize(all_numeric_predictors())

# Model specifications
rf_spec <- rand_forest(trees = 1000, mtry = tune()) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

xgb_spec <- boost_tree(trees = 1000, tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Workflows
rf_wf <- workflow() %>%
  add_recipe(spatial_rec) %>%
  add_model(rf_spec)

xgb_wf <- workflow() %>%
  add_recipe(spatial_rec) %>%
  add_model(xgb_spec)

# Spatial CV
spatial_folds <- spatial_clustering_cv(
  plant_richness_df,
  coords = c("x", "y"),
  v = 10
)

# Tune
rf_tune <- tune_grid(
  rf_wf,
  resamples = spatial_folds,
  grid = 10
)

xgb_tune <- tune_grid(
  xgb_wf,
  resamples = spatial_folds,
  grid = 10
)

# Compare
autoplot(rf_tune)
autoplot(xgb_tune)

# Select best and finalize
best_rf <- select_best(rf_tune, metric = "rmse")
final_rf_wf <- finalize_workflow(rf_wf, best_rf)
final_rf_fit <- fit(final_rf_wf, plant_richness_df)
```

---

## Performance Considerations

### Memory Efficiency

Different engines have different memory footprints:

- **Random Forest:** High (stores all trees)
- **XGBoost:** Medium (compressed tree structures)
- **Linear Models:** Low (just coefficients)
- **GAM:** Medium (smooth basis functions)

Spatial predictors add to memory:
- MEMs: One vector per spatial predictor
- For 1000 samples, 50 spatial predictors ≈ 400KB
- Distance matrix: For 1000 samples ≈ 8MB

### Computational Speed

Relative training speed (rough estimates):

1. **Linear Models:** Fastest (analytical solution)
2. **XGBoost:** Fast (efficient gradient boosting)
3. **Random Forest:** Medium (parallelizable)
4. **GAM:** Slow (iterative smoothing)

With spatial optimization:
- Sequential selection adds overhead
- Each candidate spatial predictor requires model fitting
- Can be parallelized across predictors

### Recommendations

```r
# For large datasets (n > 5000):
# - Use sampling for spatial predictor selection
# - Prefer XGBoost over RF
# - Use sparse distance matrices
# - Limit number of spatial predictors

# For small datasets (n < 500):
# - Full spatial optimization is feasible
# - Can try multiple engines easily
# - GAM is computationally reasonable

# For wide datasets (many predictors):
# - Multicollinearity filtering essential
# - RF/XGBoost handle better than LM
# - Consider regularized LM (glmnet)
```

---

## Testing Strategy

### Unit Tests by Component

```r
# tests/testthat/test-engines.R
test_that("engine_rf works", {
  model <- spatial_fit(
    data = test_data,
    response = "y",
    predictors = c("x1", "x2"),
    engine = engine_rf(num_trees = 100)
  )

  expect_s3_class(model, "spatial_rf")
  expect_true(!is.null(model$model))
  expect_equal(model$response, "y")
})

test_that("engine_lm works", {
  model <- spatial_fit(
    data = test_data,
    response = "y",
    predictors = c("x1", "x2"),
    engine = engine_lm()
  )

  expect_s3_class(model, "spatial_lm")
  expect_s3_class(model$model, "lm")
})

# tests/testthat/test-importance.R
test_that("importance works for all engines", {
  model.rf <- spatial_fit(test_data, "y", c("x1", "x2"), engine_rf())
  model.lm <- spatial_fit(test_data, "y", c("x1", "x2"), engine_lm())

  imp.rf <- importance(model.rf)
  imp.lm <- importance(model.lm)

  expect_s3_class(imp.rf, "data.frame")
  expect_s3_class(imp.lm, "data.frame")
  expect_equal(nrow(imp.rf), 2)
  expect_equal(nrow(imp.lm), 2)
})

# tests/testthat/test-spatial-agnostic.R
test_that("spatial predictors work with all engines", {
  engines <- list(
    engine_rf(),
    engine_lm(),
    engine_xgboost(),
    engine_gam()
  )

  for (eng in engines) {
    model <- spatial_fit(
      data = test_data,
      response = "y",
      predictors = c("x1", "x2"),
      engine = eng,
      spatial = spatial_config(
        method = "mem",
        distance_matrix = test_dist_matrix
      )
    )

    expect_true(any(grepl("spatial_predictor", colnames(model$data))))
  }
})
```

### Integration Tests

```r
# tests/testthat/test-workflows.R
test_that("complete workflow works for all engines", {
  models <- list(
    rf = spatial_fit(test_data, "y", preds, engine_rf()),
    lm = spatial_fit(test_data, "y", preds, engine_lm()),
    xgb = spatial_fit(test_data, "y", preds, engine_xgboost())
  )

  # Evaluate all
  models_eval <- purrr::map(models, ~{
    spatial_evaluate(.x, coords = xy, repetitions = 10)
  })

  # Compare
  comp <- spatial_compare(models_eval)

  expect_s3_class(comp, "spatial_comparison")
  expect_equal(length(unique(comp$cv_results$model)), 3)
})
```

---

## Documentation Strategy

### Vignettes Needed

1. **"Introduction to Multi-Engine Spatial Modeling"**
   - When to use each engine
   - Strengths and limitations
   - Quick comparison examples

2. **"Linear Models for Spatial Data"**
   - When LM is appropriate
   - Interpretation of coefficients
   - Spatial predictors in LM context

3. **"XGBoost for Spatial Modeling"**
   - Hyperparameter tuning for XGBoost
   - Comparison with Random Forest
   - Best practices

4. **"GAMs for Spatial Data"**
   - Understanding smooth terms
   - Direct spatial smooths vs. MEMs
   - Interpretation and visualization

5. **"Comparing Models Across Engines"**
   - Fair comparison practices
   - Interpreting cross-engine differences
   - Ensemble approaches

6. **"tidymodels Integration"**
   - Using spatial recipes
   - Spatial cross-validation with rsample
   - Full tidymodels workflows

### Function Documentation

Each engine should have clear documentation:

```r
#' Random Forest Engine
#'
#' Specify Random Forest as the modeling engine using ranger
#'
#' @param num_trees Number of trees (default: 500)
#' @param mtry Number of variables to try at each split
#' @param min_node_size Minimum size of terminal nodes
#' @param ... Additional arguments passed to ranger
#'
#' @return An engine specification object
#'
#' @examples
#' # Basic usage
#' model <- spatial_fit(
#'   data = df,
#'   response = "y",
#'   predictors = c("x1", "x2"),
#'   engine = engine_rf(num_trees = 1000, mtry = 2)
#' )
#'
#' @family engines
#' @export
engine_rf <- function(num_trees = 500, mtry = NULL, min_node_size = 5, ...) {
  # Implementation
}
```

---

## Migration Path for Users

### Phase 1: Current Users (No Change Required)

Existing code continues to work:

```r
# This still works exactly as before
model <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix
)
```

### Phase 2: Try New API (Optional)

Users can experiment with new API alongside old:

```r
# New API for RF (equivalent to above)
model <- spatial_fit(
  data = plant_richness_df,
  response = "richness_species_vascular",
  predictors = predictor.variable.names,
  engine = engine_rf(),
  spatial = spatial_config(
    method = "mem",
    distance_matrix = distance_matrix
  )
)
```

### Phase 3: Try Other Engines

Users discover multi-engine capability:

```r
# Try linear model
model.lm <- spatial_fit(
  data = plant_richness_df,
  response = "richness_species_vascular",
  predictors = predictor.variable.names,
  engine = engine_lm(),
  spatial = spatial_config(
    method = "mem",
    distance_matrix = distance_matrix
  )
)

# Compare with RF
comparison <- spatial_compare(
  list(RF = model, LM = model.lm)
)
```

### Phase 4: Full Adoption

Users embrace multi-engine workflows:

```r
# Systematic comparison across engines
engines <- list(
  "RF" = engine_rf(),
  "XGBoost" = engine_xgboost(),
  "LM" = engine_lm(),
  "GAM" = engine_gam()
)

models <- purrr::map(engines, ~{
  spatial_fit(
    data = plant_richness_df,
    response = "richness_species_vascular",
    predictors = predictor.variable.names,
    engine = .x,
    spatial = spatial_config(
      method = "mem",
      distance_matrix = distance_matrix
    )
  )
})

comparison <- spatial_compare(models)
```

---

## Roadmap Integration

Add to `vignettes/articles/development_roadmap.Rmd`:

### Phase 8: Multi-Engine Support (Long-term)

**Rationale:** Extend spatial modeling capabilities beyond Random Forest while maintaining current functionality and providing users flexibility in algorithm choice.

#### 8.1 Architecture Refactoring

- Decouple spatial predictor generation from modeling engine
- Create abstract engine interface with consistent S3 methods
- Move ranger-specific code to dedicated engine module
- Ensure all core spatial functions are engine-agnostic

#### 8.2 Engine Implementations

**Linear Models (lm/glm)**
- Coefficient-based and permutation importance
- Standard regression diagnostics
- Fast baseline for comparisons
- Statistical inference available

**XGBoost**
- Tree-based importance (gain, cover, frequency)
- Often superior predictive performance
- Better handling of missing data
- Faster training on large datasets

**GAMs (mgcv)**
- Interpretable smooth terms
- Direct spatial smooth: `s(x, y)`
- Natural for environmental gradients
- Complementary to tree methods

#### 8.3 Unified Interface

- Consistent `spatial_fit()` function across engines
- Engine-agnostic evaluation and comparison
- S3 methods for engine-specific operations
- Backward-compatible legacy API maintained

#### 8.4 Optional tidymodels Integration

- Custom recipe steps for spatial predictors
- Spatial cross-validation for rsample
- Integration with tune and workflows
- Advanced users leverage full tidymodels ecosystem

**Timeline:** 6-12 months after Phase 7
**Breaking Changes:** None (maintains full backward compatibility)
**Dependencies:** Adds optional dependencies on xgboost, mgcv, tidymodels

---

## Open Questions

### 1. Should we support model ensembles?

```r
# Ensemble across engines
model.ensemble <- spatial_ensemble(
  models = list(
    engine_rf(weight = 0.4),
    engine_xgboost(weight = 0.4),
    engine_gam(weight = 0.2)
  ),
  data = df,
  response = "y",
  predictors = predictors,
  spatial = spatial_config(...)
)
```

**Pros:** Often improves predictions
**Cons:** Adds complexity, harder to interpret

### 2. Should spatial predictor selection be engine-specific?

Currently: Select spatial predictors based on RF performance

Alternative: Select different spatial predictors for each engine

**Pros:** Optimize for each engine
**Cons:** More computation, harder to compare

### 3. How to handle categorical predictors?

- RF handles natively
- LM needs dummy encoding
- XGBoost handles natively
- GAM can use `s(x, bs = "re")` for random effects

**Solution:** Engine-specific handling with user warning?

### 4. Should we support automated engine selection?

```r
model <- spatial_fit_auto(
  data = df,
  response = "y",
  predictors = predictors,
  try_engines = c("rf", "xgb", "lm", "gam"),
  selection_metric = "rmse",
  spatial = spatial_config(...)
)
# Returns best engine automatically
```

**Pros:** Easier for beginners
**Cons:** Computationally expensive, black box

---

## Success Metrics

### Technical Metrics

- [ ] All current tests pass with refactored architecture
- [ ] At least 3 additional engines implemented
- [ ] 80%+ code coverage including new engines
- [ ] No performance regression in RF engine
- [ ] Documentation coverage for all engines

### User Metrics

- [ ] Positive feedback on multi-engine capability
- [ ] Examples of cross-engine comparisons in publications
- [ ] Integration with tidymodels demonstrated
- [ ] Reduction in feature requests for other engines

### Adoption Metrics

- [ ] 30%+ of users try non-RF engines
- [ ] Package downloads increase after multi-engine release
- [ ] Citations expand beyond ecology
- [ ] Community contributions of new engines

---

## Next Steps

1. **Gather Community Feedback**
   - Post architecture proposal to GitHub Discussions
   - Survey users on engine preferences
   - Identify key use cases

2. **Prototype Engine Interface**
   - Implement minimal engine abstraction
   - Prove concept with LM engine
   - Test backward compatibility

3. **Refactor Core Functions**
   - Separate spatial logic from RF code
   - Create engine-agnostic evaluation
   - Maintain full test coverage

4. **Document Architecture**
   - Internal developer documentation
   - Contribution guidelines for new engines
   - API stability guarantees

---

*This is a living document. Last updated: 2025-12-14*
