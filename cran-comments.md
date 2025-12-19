# CRAN Submission Comments - spatialRF 1.1.5

## Test environments

* Local Ubuntu 22.04 LTS, R 4.4.2
* GitHub Actions:
  * ubuntu-latest: R-release
  * windows-latest: R-release
* win-builder: R-devel (via devtools::check_win_devel())
* R-hub v2 (via rhub::rhub_check()):
  * linux (R-release)
  * macos-arm64 (R-release)
  * windows (R-devel)
  * atlas (ATLAS/BLAS)
  * c23 (C23 standard)
  * clang-asan (Address Sanitizer)
  * clang16, clang17, clang18, clang19 (multiple Clang versions)
  * gcc13, gcc14 (GCC compilers)
  * intel (Intel compiler)
  * mkl (Intel MKL)
  * nold (no long double)
  * nosuggests (no suggested packages)
  * ubuntu-clang, ubuntu-gcc12 (Ubuntu variants)
  * ubuntu-next, ubuntu-release (Ubuntu R versions)

## R CMD check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes in this version

This is a maintenance release that fixes a Windows-specific bug and improves code quality:

### Bug Fixes

* **Critical Windows fix**: Fixed error where `predict()` method dispatch failed to find `predict.ranger()` on Windows in `get_response_curves()`, `plot_response_curves()`, `rf_evaluate()`, and `rf()`. The issue was resolved by adding `@importFrom ranger ranger` to ensure ranger's namespace is properly loaded and S3 methods are registered during R CMD check.

* **Option restoration bug**: Fixed two instances where `on.exit(options <- user.options)` wasn't actually restoring user options. Changed to correct syntax `on.exit(options(user.options))` in `rf_compare()` and `rf_evaluate()`.

### Code Quality Improvements

* Fixed 129 linting issues identified by the jarl linter:
  - Replaced inefficient vector logic operators with short-circuit operators in 33 if statements (`|` → `||`, `&` → `&&`)
  - Removed 69 redundant equality comparisons (`== TRUE` → direct boolean evaluation)
  - Standardized 16 assignments to use `<-` instead of `=`
  - Additional minor improvements (regex optimization, seq usage, etc.)

### Dependency Updates

* Fixed compatibility with updated ggplot2 (deprecated `aes_string()`, `size` aesthetic, `draw_quantiles`)
* Fixed compatibility with updated huxtable (`set_all_borders()`)

### Other Improvements

* Improved parallel backend management with new `setup_parallel_execution()` helper
* Fixed parallelization issues with zombie processes
* Removed `if(interactive()){}` wrappers from examples to ensure they run during R CMD check

## Notes for CRAN maintainers

* The Windows-specific bug was discovered during CRAN checks and has been verified as fixed on Windows (GitHub Actions) and via rhub.
* All changes are backward compatible.
* Package size (8.2 MB installed) is due to example datasets required for spatial modeling demonstrations.

Thank you for maintaining CRAN!
