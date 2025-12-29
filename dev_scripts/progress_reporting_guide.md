# Progress Reporting in spatialRF

## Overview

All parallelized functions in spatialRF now support progress reporting via the `progressr` package. Progress bars are **not enabled by default** - users must explicitly enable them.

## How to Enable Progress Bars

```r
library(spatialRF)
library(progressr)

# Enable progress reporting globally
progressr::handlers(global = TRUE)

# Now all functions will show progress bars
```

## Functions with Progress Support

The following functions track progress during execution:

1. **`make_spatial_folds()`** - Tracks fold creation across spatial points
2. **`rf_evaluate()`** - Tracks both fold creation and model evaluation
3. **`rf_repeat()`** - Tracks model repetitions
4. **`rf_tuning()`** - Tracks hyperparameter combination testing
5. **`rf_importance()`** - Tracks variable importance calculation across predictors
6. **`rf_compare()`** - Tracks model comparison across folds
7. **`rf_spatial()`** - Tracks spatial predictor ranking and selection
8. **`rank_spatial_predictors()`** - Tracks predictor ranking
9. **`select_spatial_predictors_sequential()`** - Tracks sequential selection
10. **`the_feature_engineer()`** - Tracks interaction screening

## Complete Example

```r
library(spatialRF)
library(future)
library(progressr)

# Load data
data(plants_df, plants_xy)

# Step 1: Enable progress reporting
progressr::handlers(global = TRUE)

# Step 2: Set parallel strategy (optional)
future::plan(future::multisession, workers = 4)

# Step 3: Run analysis with progress tracking
m <- rf(
  data = plants_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plants_df)[5:21]
)

# Evaluate with progress bars
m_eval <- rf_evaluate(
  model = m,
  xy = plants_xy,
  repetitions = 30
)

# Step 4: Clean up
future::plan(future::sequential)
```

## Different Progress Handler Types

You can customize the progress bar appearance:

```r
# Default handler (text-based progress bar)
progressr::handlers(global = TRUE)

# Specific handler types
progressr::handlers("progress")  # Simple progress bar
progressr::handlers("txtprogressbar")  # Text progress bar
progressr::handlers("cli")  # Modern CLI progress (if installed)

# Multiple handlers
progressr::handlers(list(
  progressr::handler_progress(),
  progressr::handler_txtprogressbar()
))
```

## Progress with Parallel Execution

Progress reporting works seamlessly with all parallel backends:

```r
library(future)
library(progressr)

# Enable progress
progressr::handlers(global = TRUE)

# Sequential execution (single process)
future::plan(future::sequential)
m_seq <- rf_evaluate(model = m, xy = xy, repetitions = 10)

# Parallel execution (multiple processes)
future::plan(future::multisession, workers = 4)
m_par <- rf_evaluate(model = m, xy = xy, repetitions = 30)

# Fork-based (Unix/Mac only, faster)
future::plan(future::multicore, workers = 4)
m_fork <- rf_evaluate(model = m, xy = xy, repetitions = 30)

# Reset
future::plan(future::sequential)
```

## Technical Details

### Implementation

- All parallelized functions use `progressr::progressor()` to create progress trackers
- Progress is signaled via `p()` calls within parallel loops
- No `with_progress()` wrappers are used (per progressr best practices)
- Progress reporting has minimal performance overhead

### Why Progress Isn't Enabled by Default

Following progressr's design philosophy:
- Functions don't assume users want progress bars
- Users have full control over progress appearance and behavior
- Enables/disables globally via handlers
- Reduces package dependencies and complexity

### Nested Progress

Some functions have nested progress tracking:
- `rf_evaluate()`: Outer progress for fold creation, inner progress for model fitting
- `the_feature_engineer()`: Two separate progress bars for different screening methods

## Troubleshooting

### "No progress bars appear"

```r
# Solution: Enable handlers
progressr::handlers(global = TRUE)
```

### "Progress bars are too verbose"

```r
# Solution: Disable progress
progressr::handlers(NULL)
# or use a quieter handler
progressr::handlers("void")  # Completely silent
```

### "Progress interferes with logging"

```r
# Solution: Use conditional handlers
if (interactive()) {
  progressr::handlers(global = TRUE)
} else {
  progressr::handlers(NULL)  # Disable in non-interactive sessions
}
```

## Performance Impact

Progress reporting adds minimal overhead:
- Signaling progress: ~0.1-1% performance cost
- Handler rendering: Depends on handler type
- Recommended: Enable for long-running tasks (>30 seconds)
- Optional: Disable for benchmarking or production runs

## See Also

- `?progressr::handlers` - Handler configuration
- `?future::plan` - Parallel execution strategies
- `dev_scripts/future_workflow.R` - Complete workflow examples
- `FUTURE_MIGRATION_GUIDE.md` - Migration details
