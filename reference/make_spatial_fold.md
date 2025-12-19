# Create spatially independent training and testing folds

Generates two spatially independent data folds by growing a rectangular
buffer from a focal point until a specified fraction of records falls
inside. Used internally by
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md)
and
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md)
for spatial cross-validation.

## Usage

``` r
make_spatial_fold(
  data = NULL,
  dependent.variable.name = NULL,
  xy.i = NULL,
  xy = NULL,
  distance.step.x = NULL,
  distance.step.y = NULL,
  training.fraction = 0.8
)
```

## Arguments

- data:

  Data frame containing response variable and predictors. Required only
  for binary response variables.

- dependent.variable.name:

  Character string with the name of the response variable. Must be a
  column name in `data`. Required only for binary response variables.

- xy.i:

  Single-row data frame with columns "x" (longitude), "y" (latitude),
  and "id" (record identifier). Defines the focal point from which the
  buffer grows.

- xy:

  Data frame with columns "x" (longitude), "y" (latitude), and "id"
  (record identifier). Contains all spatial coordinates for the dataset.

- distance.step.x:

  Numeric value specifying the buffer growth increment along the x-axis.
  Default: `NULL` (automatically set to 1/1000th of the x-coordinate
  range).

- distance.step.y:

  Numeric value specifying the buffer growth increment along the y-axis.
  Default: `NULL` (automatically set to 1/1000th of the y-coordinate
  range).

- training.fraction:

  Numeric value between 0.1 and 0.9 specifying the fraction of records
  to include in the training fold. Default: `0.8`.

## Value

List with two elements:

- `training`: Integer vector of record IDs (from `xy$id`) in the
  training fold.

- `testing`: Integer vector of record IDs (from `xy$id`) in the testing
  fold.

## Details

This function creates spatially independent training and testing folds
for spatial cross-validation. The algorithm works as follows:

1.  Starts with a small rectangular buffer centered on the focal point
    (`xy.i`)

2.  Grows the buffer incrementally by `distance.step.x` and
    `distance.step.y`

3.  Continues growing until the buffer contains the desired number of
    records (`training.fraction * total records`)

4.  Assigns records inside the buffer to training and records outside to
    testing

**Special handling for binary response variables:**

When `data` and `dependent.variable.name` are provided and the response
is binary (0/1), the function ensures that `training.fraction` applies
to the number of presences (1s), not total records. This prevents
imbalanced sampling in presence-absence models.

## See also

[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md),
[`rf_evaluate()`](https://blasbenito.github.io/spatialRF/reference/rf_evaluate.md),
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md)

Other preprocessing:
[`auto_cor()`](https://blasbenito.github.io/spatialRF/reference/auto_cor.md),
[`auto_vif()`](https://blasbenito.github.io/spatialRF/reference/auto_vif.md),
[`case_weights()`](https://blasbenito.github.io/spatialRF/reference/case_weights.md),
[`default_distance_thresholds()`](https://blasbenito.github.io/spatialRF/reference/default_distance_thresholds.md),
[`double_center_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/double_center_distance_matrix.md),
[`is_binary()`](https://blasbenito.github.io/spatialRF/reference/is_binary.md),
[`make_spatial_folds()`](https://blasbenito.github.io/spatialRF/reference/make_spatial_folds.md),
[`the_feature_engineer()`](https://blasbenito.github.io/spatialRF/reference/the_feature_engineer.md),
[`weights_from_distance_matrix()`](https://blasbenito.github.io/spatialRF/reference/weights_from_distance_matrix.md)

## Examples

``` r
data(plants_df, plants_xy)

# Create spatial fold centered on first coordinate
fold <- make_spatial_fold(
  xy.i = plants_xy[1, ],
  xy = plants_xy,
  training.fraction = 0.6
)

# View training and testing record IDs
fold$training
#>   [1]   1   2   4   5   6   7  10  12  14  15  19  20  21  22  23  26  28  31
#>  [19]  32  33  34  35  36  37  45  47  48  50  51  54  56  58  59  62  63  64
#>  [37]  65  66  67  71  73  74  77  78  79  80  81  83  87  94  95  96  97  98
#>  [55]  99 100 102 104 106 107 108 109 110 111 112 113 114 115 118 119 122 125
#>  [73] 126 127 129 131 135 136 138 139 141 145 151 153 154 155 156 157 159 160
#>  [91] 161 162 163 165 166 168 171 174 177 178 179 181 182 185 186 187 188 189
#> [109] 191 192 193 194 195 198 201 204 205 206 207 208 209 210 212 213 214 215
#> [127] 216 218 219 220 221 222 223 224 226 227
fold$testing
#>  [1]   3   8   9  11  13  16  17  18  24  25  27  29  30  38  39  40  41  42  43
#> [20]  44  46  49  52  53  55  57  60  61  68  69  70  72  75  76  82  84  85  86
#> [39]  88  89  90  91  92  93 101 103 105 116 117 120 121 123 124 128 130 132 133
#> [58] 134 137 140 142 143 144 146 147 148 149 150 152 158 164 167 169 170 172 173
#> [77] 175 176 180 183 184 190 196 197 199 200 202 203 211 217 225

# Visualize the spatial split (training = red, testing = blue, center = black)
if (interactive()) {
  plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
  points(plants_xy[fold$training, c("x", "y")], col = "red4", pch = 15)
  points(plants_xy[fold$testing, c("x", "y")], col = "blue4", pch = 15)
  points(plants_xy[1, c("x", "y")], col = "black", pch = 15, cex = 2)
}
```
