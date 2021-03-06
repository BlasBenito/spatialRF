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
