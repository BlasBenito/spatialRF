# Development Roadmap

## spatialRF Modernization Roadmap

This document outlines the planned modernization of `spatialRF`,
organized into logical phases to minimize disruption and maximize
coherence.

------------------------------------------------------------------------

### Phase 1: Backend Infrastructure (Non-Breaking)

**Rationale:** Modernize core dependencies before API changes.

#### 1.1 Dependency Modernization

- Replace `foreach` + `doParallel` with `future` + `future.apply`
  - More flexible execution strategies
  - Better control over parallel backends
- Add `progressr` for progress reporting
  - Works seamlessly with `future`
  - User-configurable progress handlers
- Remove `dplyr` and `tidyr` dependencies
  - Use base R equivalents to reduce dependency footprint
  - Improve installation reliability
  - Faster package load times
- Replace `magrittr` pipe (`%>%`) with base pipe (`|>`)
  - Native R syntax
- Remove `huxtable` from print functions to reduce dependency footprint.

#### 1.2 Function Delegation

- Replace
  [`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md)
  and
  [`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md)
  with `collinear` package calls
  - Leverage actively maintained specialized package

#### 1.3 Namespace Improvements

- Replace all `@importFrom` with explicit `package::function()` calls
  - Clearer code dependencies
  - Easier debugging
  - Better IDE support
  - Reduced namespace conflicts

#### 1.4 Code Quality

- Add parameter validation with \`custom validators
  - Clear error messages
  - Consistent validation across functions
  - Reduce runtime errors
- Standardize internal helper functions
  - Consistent naming conventions
  - Proper documentation
  - Reduce code duplication

------------------------------------------------------------------------

### Phase 2: Testing & Performance (Non-Breaking)

**Rationale:** Establish robust testing before API changes, and optimize
performance with current architecture.

#### 2.1 Test Suite Enhancement

- Expand unit test coverage to \>80%
  - Test edge cases and error conditions
  - Add tests for all exported functions
  - Test parallel execution paths
- Add integration tests
  - Test complete workflows
  - Test piped operations
  - Test model comparison scenarios
- Add benchmarking suite
  - Track performance across versions
  - Identify regression
  - Document expected execution times

#### 2.2 Performance Optimization

- Profile and optimize bottleneck functions
  - [`rf_spatial()`](https://blasbenito.github.io/spatialRF/reference/rf_spatial.md)
    spatial predictor generation
  - [`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)
    cross-validation loops
  - [`rf_tuning()`](https://blasbenito.github.io/spatialRF/reference/rf_tuning.md)
    grid search
  - Distance matrix operations
- Add intelligent caching where appropriate
  - Cache expensive computations
  - Clear cache invalidation rules
- Optimize memory usage
  - Reduce object copying
  - Better memory management in parallel operations
  - Streaming where possible for large operations

------------------------------------------------------------------------

### Phase 3: Documentation Overhaul (Non-Breaking)

**Rationale:** Improve documentation while maintaining current API,
preparing for future changes.

#### 3.1 Roxygen Improvements

- Improve explanations in roxygen docs.

- Define clear input types for args, and output types for returns

- Use `@inheritParams` to reduce redundancy

  - Document common parameters once
  - Easier maintenance
  - Consistent parameter descriptions

#### 3.2 Vignettes & Articles

- Add technical vignette: “Spatial Predictors Theory”
  - Conceptual background on MEMs
  - When to use which method
  - Interpretation guidelines
- Add technical vignette: “Model Evaluation & Cross-Validation”
  - Spatial cross-validation theory
  - Interpretation of metrics
  - Comparison with standard CV
- Add practical vignette: “Working with Large Datasets”
  - Memory management strategies
  - Parallel processing best practices
  - Sampling strategies
- Add practical vignette: “Species Distribution Modeling”
  - Binary response workflows
  - Class imbalance handling
  - Spatial projection considerations
- Add practical vignette: “Hyperparameter Tuning Guide”
  - When tuning is necessary
  - Computational considerations
  - Interpretation of results

#### 3.3 Package Website

- Enhance pkgdown site
  - Better organization of function reference
  - Add search functionality
  - Include changelog
  - Add gallery of use cases

------------------------------------------------------------------------

### Phase 4: User Experience Improvements (Non-Breaking)

**Rationale:** Improve UX without breaking changes.

#### 4.1 Messaging & Feedback

- Replace message/warning/stop with `cli` package
  - Prettier, more informative messages
  - Better formatting
  - Consistent style
- Improve error messages
  - Suggest fixes for common errors
  - Include relevant context
  - Point to documentation

#### 4.2 Object Consistency

- Formalize S3 classes for model objects
  - Consistent structure across model types
  - Better print methods
  - Better summary methods
- Improve print methods
  - Show most relevant information first
  - Cleaner formatting
- Improve plot methods
  - Consistent theming across all plots
  - Better defaults
  - Return ggplot objects for user modification

------------------------------------------------------------------------

### Phase 5: API Modernization (BREAKING CHANGES - Major Version)

**Rationale:** Breaking changes grouped together in a major version
release.

#### 5.1 Argument Name Standardization

**Old → New:**

- `data` → `df`
- `dependent.variable.name` → `response`
- `predictor.variable.names` → `predictors`
- `distance.matrix` → `distance_matrix`
- `distance.thresholds` → `distance_thresholds`
- Use ellipsis for `ranger` arguments and deprecate argument
  \`ranger.arguments\`\`
- `num.trees` → `n_trees`
- `min.node.size` → `min_node_size` …

**Implementation:**

- Create wrapper functions maintaining old API with
  [`.Deprecated()`](https://rdrr.io/r/base/Deprecated.html)
- Provide clear migration guide
- Add lifecycle badges to all functions

#### 5.3 Simplified Function Interfaces

- Reduce number of required arguments where possible
  - Better defaults based on data
  - Auto-detection of data characteristics
  - Smart parameter inference
- Group related parameters into list arguments
  - `spatial_config` for spatial-related parameters
  - `cv_config` for cross-validation parameters
  - `plot_config` for plotting parameters

#### 5.4 Full support for spatial types

- Support for `terra` and `sf` objects across functions.

#### 5.5 Ingetration with `tidymodels` and `mlr3`

- Integration with `tidymodels`
  - parsnip engine for spatialRF
  - recipes integration
  - tune integration
- Integration with `mlr3`
  - Custom learner implementation
  - Spatial resampling strategies

------------------------------------------------------------------------

### Phase 6: New Features & Extensions

**Rationale:** Add new functionality after core modernization is
complete.

#### 6.1 Core Features

- Temporal autocorrelation support

  - Extend spatial methods to space-time
  - Temporal cross-validation
  - Spatiotemporal predictors

- Additional modelling engines: lm, gam, xgboost

- Additional spatial predictor methods

  - Spatial wavelets
  - Additional kernel methods
  - User-defined spatial basis functions

#### 6.2 Model Interpretation

- SHAP values integration
  - Better feature importance interpretation
  - Local explanations
  - Interaction detection
- Partial dependence improvements
  - ICE (Individual Conditional Expectation) plots
  - Accumulated local effects (ALE)
  - Faster computation

------------------------------------------------------------------------

### Phase 7: Advanced Optimizations

**Rationale:** Long-term performance improvements for specialized use
cases.

#### 7.1 Computational Efficiency

- Rcpp integration for bottlenecks
  - Distance matrix operations
  - Spatial predictor generation
  - Moran’s I calculations
- Sparse matrix support
  - Memory efficiency for large distance matrices
  - Faster operations on sparse structures
- Approximate methods for large datasets
  - Sampling-based spatial predictors
  - Approximate cross-validation
  - Scalability beyond current limits

#### 7.2 Specialized Algorithms

- GPU acceleration (optional)
  - CUDA/OpenCL support for ranger
  - GPU-accelerated distance calculations
  - Optional dependency

------------------------------------------------------------------------

### Implementation Principles

#### Versioning Strategy

- **Minor versions (1.x.y)**: Phases 1-4 (non-breaking)
- **Major version (2.0.0)**: Phase 5 (breaking changes)
- **Subsequent versions**: Phases 6-7 (new features)

#### Backward Compatibility

- Maintain deprecated functions for at least one major version
- Clear deprecation warnings with migration paths
- Comprehensive changelog
- Migration vignette for major version transitions

### Notes

This is a living document and will be updated based on community
feedback and emerging best practices. Some phases can overlap and be
developed in parallel. Priority may shift based on user needs and bug
reports. Emergency releases for critical bugs take precedence over
roadmap.

------------------------------------------------------------------------

*Last updated: 2025-12-20*
