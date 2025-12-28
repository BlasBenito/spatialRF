# Package Decomposition Analysis: spatialRF Split Recommendations

## Executive Summary

After analyzing the spatialRF package structure, I recommend extracting **two standalone packages**:

1. **`moranR`** - Spatial autocorrelation analysis (Moran's I computation)
2. **`spatialPredictors`** - Spatial predictor generation (MEMs, PCA-based methods)

Both packages contain general-purpose spatial statistics functionality currently embedded in spatialRF but useful to a much wider audience.

---

## Package 1: `moranR` - Spatial Autocorrelation Analysis

### Purpose
Provide Moran's I spatial autocorrelation testing with multi-scale analysis capabilities.

### Target Audience
- Ecologists testing spatial patterns in species distributions
- Epidemiologists analyzing disease clustering
- Geographers studying spatial phenomena
- Anyone working with spatially-structured data needing autocorrelation tests
- Researchers checking residuals from ANY spatial model (not just RF)

### Core Functionality (7 functions)
1. `moran()` - Single-threshold Moran's I test
2. `moran_multithreshold()` - Multi-scale Moran's I analysis
3. `weights_from_distance_matrix()` - Distance to spatial weights conversion
4. `default_distance_thresholds()` - Intelligent threshold generation
5. `plot_moran()` - Visualization of Moran's I results
6. `print_moran()` - Formatted output
7. `get_moran()` - Extraction from model objects (would become more generic)

### Dependencies
- **Minimal**: `stats` (pnorm, sd), `ggplot2` (optional, plotting only)
- **No ML dependencies**
- **No ranger or random forest code**
- **Completely framework-agnostic**

### Why This Works
- Moran's I is a **general spatial statistics tool** used far beyond random forests
- Current implementation has **zero RF-specific code**
- Clean pipeline: distance matrix → weights → Moran's I statistic
- Multi-threshold analysis valuable for exploring spatial scale
- Already well-tested within spatialRF

### Current Usage in spatialRF
- `rf()` uses it to diagnose residual autocorrelation
- `rank_spatial_predictors()` uses it for ranking spatial predictors
- Selection functions use it during optimization
- **Coupling is loose** - spatialRF just consumes the output

### Value Proposition
- **Lighter than spdep**: Focused on Moran's I only
- **Multi-threshold built-in**: Explore spatial scale systematically
- **Modern R**: Clean API, ggplot2 integration, tidy output
- **Fast**: Vectorized calculations, parallel-ready

---

## Package 2: `spatialPredictors` - Spatial Predictor Generation

### Purpose
Generate spatial eigenvector predictors (MEMs) and PCA-based spatial predictors from distance matrices for any spatial modeling framework.

### Target Audience
- Ecologists fitting spatial GLMs, GAMs, mixed models
- Data scientists using spatial features in ML models (GBM, neural nets, XGBoost)
- Researchers implementing custom spatial modeling pipelines
- Anyone needing to account for spatial structure without GIS dependencies

### Core Functionality (8-10 functions)
1. `mem()` - Generate Moran's Eigenvector Maps (single threshold)
2. `mem_multithreshold()` - Multi-scale MEM generation
3. `pca()` - PCA-based spatial predictors (wrapper with scaling)
4. `pca_multithreshold()` - Multi-scale PCA approach
5. `double_center_distance_matrix()` - Matrix transformation for MEM
6. `filter_spatial_predictors()` - Remove redundant spatial predictors
7. `weights_from_distance_matrix()` - Shared utility
8. `default_distance_thresholds()` - Shared utility
9. **Optional**: `thinning()`, `thinning_til_n()` - Spatial point thinning

### Dependencies
- **Minimal**: `stats` (prcomp, eigen, var), base R
- **Optional import**: `moranR` (for filtering by spatial autocorrelation)
- **No ranger or RF code**

### Why This Works
- **MEMs are methodology-agnostic** (Dray et al. 2006, Ecological Modelling)
- Used successfully in GLMs, GAMs, ordination, regression frameworks
- PCA-based spatial predictors offer alternative to MEMs
- Clean input/output: distance matrix in → spatial predictor data frame out
- Multi-threshold approach explores spatial scales systematically
- Generation is **completely decoupled** from RF modeling

### Current Usage in spatialRF
- `rf_spatial()` generates candidate spatial predictors
- Output is ranked/selected by `rank_spatial_predictors()` and `select_spatial_predictors_*()`
- **Generation is fully independent** of selection/fitting

### Value Proposition
- **Framework-agnostic**: Works with ANY regression/ML method
- **Fills a gap**: Few R packages offer easy MEM generation
- **Multi-scale built-in**: Systematic exploration of spatial structure
- **Lighter than vegan**: Focused on spatial predictor generation only
- **Research-grade**: Based on published spatial ecology methods

---

## Package Relationships & Dependencies

```
┌─────────────────────┐
│  spatialPredictors  │ (standalone, optionally depends on moranR)
└─────────────────────┘
          │
          │ (optional import for filtering)
          ▼
┌─────────────────────┐
│      moranR         │ (standalone, no dependencies beyond stats/ggplot2)
└─────────────────────┘
          │
          │ (both imported by)
          ▼
┌─────────────────────┐
│     spatialRF       │ (RF modeling, imports both packages)
└─────────────────────┘
```

### Shared Functions
Both packages need:
- `weights_from_distance_matrix()`
- `default_distance_thresholds()`

**Solution**: Include in `moranR`, `spatialPredictors` imports from `moranR`. They're small (~20-30 lines each) so duplication is also acceptable.

---

## Functions Remaining in spatialRF

### Core RF Modeling
- `rf()`, `rf_spatial()` - Main model fitting
- `rf_evaluate()`, `rf_tuning()`, `rf_repeat()`, `rf_compare()`, `rf_importance()`

### RF-Dependent Selection
- `rank_spatial_predictors()` - Uses ranger to fit models
- `select_spatial_predictors_sequential()` - Uses ranger
- `select_spatial_predictors_recursive()` - Uses ranger
- `optimization_function()` - Selection-specific

### RF Model Utilities
- All `get_*` functions (10 functions) - Extract from RF model objects
- All `print_*` functions (5 functions) - Format RF output
- All `plot_*` functions (13 functions) - Visualize RF results
- `make_spatial_fold()`, `make_spatial_folds()` - Spatial CV for RF
- `prepare_importance_spatial()` - RF importance preparation
- `the_feature_engineer()` - RF-specific feature suggestions

### Delegated to Other Packages
- `auto_cor()`, `auto_vif()` → Already moved to `collinear` package

---

## Implementation Strategy

### Phase 1: Extract `moranR` (Priority 1)
**Why first**: Fewer dependencies, cleaner separation, broader appeal

**Steps**:
1. Create new package repository
2. Move 7 functions + documentation + tests
3. Publish to CRAN
4. Update spatialRF to import `moranR`
5. Re-export key functions for backward compatibility

**Estimated complexity**: Low (functions are self-contained)

### Phase 2: Extract `spatialPredictors` (Priority 2)
**Why second**: Can optionally depend on moranR, more specialized

**Steps**:
1. Create new package repository
2. Move 8-10 functions + documentation + tests
3. Decide on moranR dependency (optional vs required)
4. Publish to CRAN
5. Update spatialRF to import `spatialPredictors`
6. Re-export key functions for backward compatibility

**Estimated complexity**: Low-Medium (need to resolve shared utilities)

### Phase 3: Update spatialRF
**Steps**:
1. Add `moranR` and `spatialPredictors` to Imports
2. Update function calls to use `moranR::moran()`, etc.
3. Re-export key functions via `@importFrom` + `@export`
4. Update documentation to reference new packages
5. Add deprecation warnings for next major version
6. Update vignettes to show integration
7. Add new packages to "Suggests" citations

**Backward compatibility**: Full for 1-2 versions via re-exports

---

## Advantages of Extraction

### For Broader R Community
1. **moranR**: Lightweight spatial autocorrelation testing without spdep/sp dependencies
2. **spatialPredictors**: Easy MEM generation for non-RF spatial models
3. **Citation clarity**: Each methodology can be cited independently
4. **Discoverability**: Easier to find via package search

### For spatialRF
1. **Clearer focus**: Pure RF modeling with spatial awareness
2. **Smaller codebase**: ~15-17 fewer exported functions
3. **Easier maintenance**: Spatial statistics delegated to specialists
4. **Follows ecosystem pattern**: Like `collinear` delegation
5. **Modular architecture**: Easier to swap/extend methods

### For Package Ecosystem
1. **Consistency**: Aligns with `collinear` / `distantia` / `spatialRF` philosophy
2. **Reusability**: Other developers can build on spatial predictor methods
3. **Testing**: Isolated testing of spatial statistics improves reliability
4. **Specialization**: Each package does one thing well

---

## Potential Challenges & Solutions

### Challenge 1: Cross-Validation Functions
**Issue**: `make_spatial_fold()` and `make_spatial_folds()` are general but currently in spatialRF

**Options**:
- Keep in spatialRF (most conservative)
- Move to `spatialPredictors` (logical for spatial utilities)
- Create separate `spatialCV` package (future consideration)

**Recommendation**: Keep in spatialRF for now (RF-focused usage)

### Challenge 2: Thinning Functions
**Issue**: `thinning()` and `thinning_til_n()` useful for both sampling and CV

**Options**:
- Keep in spatialRF
- Move to `spatialPredictors` (spatial utilities)
- Duplicate in both packages (they're small)

**Recommendation**: Move to `spatialPredictors` (general spatial utility)

### Challenge 3: Plotting Dependencies
**Issue**: ggplot2 adds package weight but provides value

**Options**:
- Keep ggplot2 in both packages (user-friendly)
- Make plotting optional via suggests (lighter)
- Provide base R plotting alternatives

**Recommendation**: Keep ggplot2 in Imports (modern R standard, worth the weight)

### Challenge 4: User Disruption
**Issue**: Existing spatialRF users need to install additional packages

**Solutions**:
1. Add `moranR` and `spatialPredictors` to spatialRF Imports (automatic installation)
2. Re-export all functions for 1-2 versions (full backward compatibility)
3. Update documentation with migration guide
4. Add helpful messages on first use

### Challenge 5: Testing Migration
**Issue**: spatialRF tests currently cover Moran's I and MEM functions

**Solutions**:
1. Copy relevant tests to new packages
2. Extend test coverage in new packages (better isolated testing)
3. Keep integration tests in spatialRF
4. Use new packages in spatialRF tests (validates integration)

---

## Alternative Consideration: Single Package

### Could combine into one `spatialStats` package?

**Arguments FOR**:
- Single dependency for spatialRF users
- Shared utilities (weights, thresholds) in one place
- Easier to maintain two packages (spatialRF + spatialStats) than three

**Arguments AGAINST**:
- Moran's I users may not need MEM generation (and vice versa)
- Different target audiences (diagnostics vs modeling)
- Less focused packages (does two things instead of one thing well)
- Harder to discover ("I need Moran's I" → find `moranR` is clearer)

**Recommendation**: Keep separate - focused packages are better for discovery and adoption

---

## Recommendation

**Extract both packages**, prioritizing `moranR` first:

### Priority 1: `moranR`
- **Broader immediate impact**: Spatial autocorrelation testing is universally needed
- **Easier extraction**: Self-contained, minimal dependencies
- **Quick wins**: Fast to CRAN, immediate utility

### Priority 2: `spatialPredictors`
- **Fills methodological gap**: Easy MEM generation currently missing in R ecosystem
- **Enables framework-agnostic spatial modeling**: Value beyond RF
- **Natural follow-up**: Can depend on moranR for filtering

### Result
spatialRF becomes a **focused RF modeling package** that delegates:
- Multicollinearity → `collinear` (already done)
- Spatial autocorrelation testing → `moranR` (new)
- Spatial predictor generation → `spatialPredictors` (new)
- RF modeling with spatial awareness → `spatialRF` (core focus)

This follows the Unix philosophy: **each package does one thing well**.

---

## Decisions Made

Based on your feedback:

1. ✅ **Extract both packages**: moranR (priority 1) + spatialPredictors (priority 2)
2. ✅ **Primary goals**: Maintenance simplification + ecosystem consistency
3. ✅ **Thinning functions**: Will remain in spatialRF (you plan separate Rcpp thinning library)
4. ✅ **Approach**: Follow the pattern established with collinear package delegation

## Refined Function Allocation

### moranR (7 functions)
- `moran()`
- `moran_multithreshold()`
- `weights_from_distance_matrix()`
- `default_distance_thresholds()`
- `plot_moran()`
- `print_moran()`
- `get_moran()` (becomes model-agnostic)

### spatialPredictors (6 functions - thinning excluded)
- `mem()`
- `mem_multithreshold()`
- `pca()`
- `pca_multithreshold()`
- `double_center_distance_matrix()`
- `filter_spatial_predictors()`

**Note**: `weights_from_distance_matrix()` and `default_distance_thresholds()` will be in `moranR`, imported by `spatialPredictors`.

### Remaining in spatialRF
- All RF modeling functions (`rf*()`)
- All selection functions (`rank_*`, `select_*`, `optimization_function()`)
- All accessors/printing/plotting for RF models (`get_*`, `print_*`, `plot_*`)
- Cross-validation functions (`make_spatial_fold*()`)
- **Thinning functions** (`thinning()`, `thinning_til_n()`) - will eventually be superseded by your Rcpp package
- Various RF-specific utilities

## Implementation Roadmap

### Phase 1: Create moranR Package
1. Set up new package structure
2. Move 7 Moran's I functions + tests
3. Update documentation (remove RF-specific references, make generic)
4. Add vignette showing Moran's I usage across different model types (lm, glm, gam examples)
5. Submit to CRAN

### Phase 2: Create spatialPredictors Package
1. Set up new package structure
2. Move 6 spatial predictor functions + tests
3. Add `moranR` to Imports
4. Update documentation (emphasize framework-agnostic usage)
5. Add vignette showing MEM usage with GLM, GAM, and ML frameworks
6. Submit to CRAN

### Phase 3: Refactor spatialRF
1. Add `moranR` and `spatialPredictors` to Imports in DESCRIPTION
2. Update function calls to use `moranR::moran()`, `spatialPredictors::mem()`, etc.
3. Re-export key functions for backward compatibility:
   ```r
   #' @importFrom moranR moran moran_multithreshold
   #' @export moranR::moran
   #' @export moranR::moran_multithreshold
   ```
4. Update all documentation to reference new packages
5. Add "Note: This function is re-exported from {package}" to help pages
6. Update README to show new package ecosystem
7. Update vignettes to explain integration
8. Run `devtools::check()` - ensure all tests pass
9. Consider: Add deprecation notices for direct usage (optional, for future v2.0.0)

### Phase 4: Documentation & Communication
1. Write blog post / announcement about package ecosystem
2. Update spatialRF citation to mention both packages
3. Create migration guide (though backward compatibility is maintained)
4. Update GitHub README badges to show all three packages

## Benefits for Your Goals

### Maintenance Simplification
- **Reduced spatialRF complexity**: 13 fewer exported functions (~19% reduction from 68 to 55)
- **Isolated testing**: Spatial statistics bugs don't affect RF code and vice versa
- **Clearer responsibilities**: RF modeling vs spatial statistics vs multicollinearity
- **Easier onboarding**: Contributors can focus on specific packages
- **Faster CI/CD**: Smaller test suites run faster

### Ecosystem Consistency
- **Matches collinear pattern**: Specialized packages with clear purposes
- **Coherent trio**: `spatialRF` (modeling) + `collinear` (preprocessing) + `moranR` (diagnostics) + `spatialPredictors` (spatial features)
- **Cross-package integration**: Packages work together but standalone
- **Shared design**: Snake_case, base R preference, `future` parallelization
- **Citation clarity**: Each package cites its own methodology

## Technical Considerations

### Shared Dependencies Resolution
**Problem**: Both moranR and spatialPredictors need `weights_from_distance_matrix()` and `default_distance_thresholds()`

**Solution**:
1. Include both functions in `moranR` (makes sense - they're spatial statistics utilities)
2. `spatialPredictors` imports from `moranR`
3. Benefit: Users of spatialPredictors get Moran's I testing capability automatically

### Backward Compatibility Strategy
**Approach**: Full backward compatibility via re-exports for at least 2 minor versions

**Implementation**:
```r
# In spatialRF NAMESPACE (generated by roxygen2)
#' @importFrom moranR moran moran_multithreshold weights_from_distance_matrix
#' @importFrom spatialPredictors mem mem_multithreshold pca pca_multithreshold
#' @export
```

**Result**: Existing spatialRF code continues to work without changes. Functions available via `spatialRF::moran()` and `moranR::moran()`.

### Testing Strategy
1. **Copy tests** from spatialRF to new packages
2. **Expand tests** in new packages (better isolated testing)
3. **Integration tests** remain in spatialRF (test package interoperability)
4. **Remove** duplicated tests from spatialRF once extraction complete

## Next Steps

Ready to proceed when you are:

1. **Package names**: Confirm `moranR` and `spatialPredictors` (or suggest alternatives)
2. **Timing**: Extract now, or after current spatialRF maintenance phase?
3. **Scope**: Want detailed implementation plans for each phase?
4. **Authorship**: Author list for new packages?

This extraction will significantly simplify spatialRF maintenance while making powerful spatial statistics methods accessible to the broader R community.
