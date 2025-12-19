pkgname <- "spatialRF"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "spatialRF-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('spatialRF')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("auc")
### * auc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: auc
### Title: Area under the ROC curve
### Aliases: auc

### ** Examples


auc(
  o = c(0, 0, 1, 1),
  p = c(0.1, 0.6, 0.4, 0.8)
  )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("auc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("auto_cor")
### * auto_cor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: auto_cor
### Title: Multicollinearity reduction via Pearson correlation
### Aliases: auto_cor

### ** Examples

data(
  plants_df,
  plants_predictors
)

y <- auto_cor(
  x = plants_df[, plants_predictors]
)

y$selected.variables
y$cor
head(y$selected.variables.df)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("auto_cor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("auto_vif")
### * auto_vif

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: auto_vif
### Title: Multicollinearity reduction via Variance Inflation Factor
### Aliases: auto_vif

### ** Examples

data(
  plants_df,
  plants_predictors
)

y <- auto_vif(
  x = plants_df[, plants_predictors]
)

y$selected.variables
y$vif
head(y$selected.variables.df)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("auto_vif", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("beowulf_cluster")
### * beowulf_cluster

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: beowulf_cluster
### Title: Create a Beowulf cluster for parallel computing
### Aliases: beowulf_cluster

### ** Examples

## Not run: 
##D # Create cluster with 3 machines
##D beowulf.cluster <- beowulf_cluster(
##D   cluster.ips = c(
##D     "192.168.1.10",  # main node
##D     "192.168.1.11",
##D     "192.168.1.12"
##D   ),
##D   cluster.cores = c(7, 4, 4),
##D   cluster.user = "username",
##D   cluster.port = "11000"
##D )
##D 
##D # Register cluster for parallel processing
##D doParallel::registerDoParallel(cl = beowulf.cluster)
##D 
##D # Run parallelized code (e.g., foreach loop)
##D # your_parallel_code_here
##D 
##D # Stop cluster when done
##D parallel::stopCluster(cl = beowulf.cluster)
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("beowulf_cluster", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("case_weights")
### * case_weights

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: case_weights
### Title: Generate case weights for imbalanced binary data
### Aliases: case_weights

### ** Examples

# Imbalanced dataset: 3 zeros, 2 ones
weights <- case_weights(
  data = data.frame(
    response = c(0, 0, 0, 1, 1)
  ),
  dependent.variable.name = "response"
)

weights
# Returns: 0.333, 0.333, 0.333, 0.5, 0.5
# Zeros get weight 1/3, ones get weight 1/2




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("case_weights", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("default_distance_thresholds")
### * default_distance_thresholds

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: default_distance_thresholds
### Title: Default distance thresholds for spatial predictors
### Aliases: default_distance_thresholds

### ** Examples

data(plants_distance)

thresholds <- default_distance_thresholds(
  distance.matrix = plants_distance
)

thresholds
# Example output: c(0, 3333, 6666, 10000)
# Four evenly-spaced thresholds from 0 to max(plants_distance)/2




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("default_distance_thresholds", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("double_center_distance_matrix")
### * double_center_distance_matrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: double_center_distance_matrix
### Title: Double-center a distance matrix
### Aliases: double_center_distance_matrix

### ** Examples

data(plants_distance)

# Double-center the distance matrix
centered <- double_center_distance_matrix(
  distance.matrix = plants_distance
)

# Verify row means are zero
head(rowMeans(centered))

# Verify column means are zero
head(colMeans(centered))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("double_center_distance_matrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("filter_spatial_predictors")
### * filter_spatial_predictors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: filter_spatial_predictors
### Title: Remove redundant spatial predictors
### Aliases: filter_spatial_predictors

### ** Examples

data(
  plants_df,
  plants_predictors,
  plants_distance
)

# Generate spatial predictors using multiple distance thresholds
mem.df <- mem_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000)
)

# Filter spatial predictors to remove redundancy
# Removes spatial predictors correlated > 0.50 with each other
# or with environmental predictors
spatial.predictors.filtered <- filter_spatial_predictors(
  data = plants_df,
  predictor.variable.names = plants_predictors,
  spatial.predictors.df = mem.df,
  cor.threshold = 0.50
)

# Check dimensions
ncol(mem.df)  # original number
ncol(spatial.predictors.filtered)  # after filtering




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("filter_spatial_predictors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_evaluation")
### * get_evaluation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_evaluation
### Title: Extract evaluation metrics from cross-validated model
### Aliases: get_evaluation

### ** Examples

data(plants_rf, plants_xy)

# Evaluate model with spatial cross-validation
m_evaluated <- rf_evaluate(
  model = plants_rf,
  xy = plants_xy,
  repetitions = 5,
  n.cores = 1
)

# Extract evaluation metrics
eval_metrics <- get_evaluation(m_evaluated)
head(eval_metrics)

# Compare with other evaluation functions
plot_evaluation(m_evaluated, notch = FALSE)
print_evaluation(m_evaluated)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_evaluation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_importance")
### * get_importance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_importance
### Title: Extract variable importance from model
### Aliases: get_importance

### ** Examples

data(plants_rf)

# Extract variable importance
importance <- get_importance(plants_rf)
head(importance)

# View top 5 most important variables
importance[1:5, ]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_importance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_importance_local")
### * get_importance_local

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_importance_local
### Title: Extract local variable importance from model
### Aliases: get_importance_local

### ** Examples

data(plants_rf)

# Extract local importance scores
local_imp <- get_importance_local(plants_rf)

# View structure: rows = observations, columns = variables
dim(local_imp)
head(local_imp)

# Find which variable is most important for first observation
colnames(local_imp)[which.max(local_imp[1, ])]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_importance_local", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_moran")
### * get_moran

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_moran
### Title: Extract Moran's I test results for model residuals
### Aliases: get_moran

### ** Examples

data(plants_rf)

# Extract Moran's I test results
moran_results <- get_moran(plants_rf)
moran_results

# Check for significant spatial autocorrelation
significant <- moran_results[moran_results$p.value < 0.05, ]
significant




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_moran", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_performance")
### * get_performance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_performance
### Title: Extract out-of-bag performance metrics from model
### Aliases: get_performance

### ** Examples

data(plants_rf)

# Extract OOB performance metrics
performance <- get_performance(plants_rf)
performance

# For repeated models, median and MAD are returned
# (example would require rf_repeat model)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_performance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_predictions")
### * get_predictions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_predictions
### Title: Extract fitted predictions from model
### Aliases: get_predictions

### ** Examples

data(plants_rf)

# Extract fitted predictions
predictions <- get_predictions(plants_rf)
head(predictions)

# Check length matches number of observations
length(predictions)

# Compare with observed values to assess fit
# (observed values would be in original data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_predictions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_residuals")
### * get_residuals

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_residuals
### Title: Extract model residuals
### Aliases: get_residuals

### ** Examples

data(plants_rf)

# Extract residuals
residuals <- get_residuals(plants_rf)
head(residuals)

# Check basic statistics
summary(residuals)

# Plot distribution to check for patterns
hist(residuals, main = "Residual Distribution", xlab = "Residuals")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_residuals", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_response_curves")
### * get_response_curves

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_response_curves
### Title: Extract response curve data for plotting
### Aliases: get_response_curves

### ** Examples

data(plants_rf)

# Extract response curve data for plotting
curves <- get_response_curves(
  model = plants_rf,
  variables = NULL,  # auto-select important variables
  quantiles = c(0.1, 0.5, 0.9)
)

# View structure
head(curves)
str(curves)

# Check unique predictors included
unique(curves$predictor.name)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_response_curves", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_spatial_predictors")
### * get_spatial_predictors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_spatial_predictors
### Title: Extract spatial predictors from spatial model
### Aliases: get_spatial_predictors

### ** Examples

data(plants_rf_spatial)

# Extract spatial predictors
spatial_preds <- get_spatial_predictors(plants_rf_spatial)
head(spatial_preds)

# Check dimensions
dim(spatial_preds)

# View predictor names (ordered by importance)
colnames(spatial_preds)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_spatial_predictors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_binary")
### * is_binary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_binary
### Title: Check if variable is binary with values 0 and 1
### Aliases: is_binary

### ** Examples

# Binary variable (returns TRUE)
is_binary(
  data = data.frame(response = c(0, 0, 0, 1, 1)),
  dependent.variable.name = "response"
)

# Non-binary variable (returns FALSE)
is_binary(
  data = data.frame(response = c(1, 2, 3, 4, 5)),
  dependent.variable.name = "response"
)

# Binary but wrong values (returns FALSE)
is_binary(
  data = data.frame(response = c(1, 1, 2, 2)),
  dependent.variable.name = "response"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_binary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make_spatial_fold")
### * make_spatial_fold

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_spatial_fold
### Title: Create spatially independent training and testing folds
### Aliases: make_spatial_fold

### ** Examples

data(plants_df, plants_xy)

# Create spatial fold centered on first coordinate
fold <- make_spatial_fold(
  xy.i = plants_xy[1, ],
  xy = plants_xy,
  training.fraction = 0.6
)

# View training and testing record IDs
fold$training
fold$testing

# Visualize the spatial split (training = red, testing = blue, center = black)
if (interactive()) {
  plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
  points(plants_xy[fold$training, c("x", "y")], col = "red4", pch = 15)
  points(plants_xy[fold$testing, c("x", "y")], col = "blue4", pch = 15)
  points(plants_xy[1, c("x", "y")], col = "black", pch = 15, cex = 2)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_spatial_fold", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make_spatial_folds")
### * make_spatial_folds

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_spatial_folds
### Title: Create multiple spatially independent training and testing folds
### Aliases: make_spatial_folds

### ** Examples

data(plants_df, plants_xy)

# Thin to 10 focal points to speed up example
xy.thin <- thinning_til_n(
  xy = plants_xy,
  n = 10
)

# Create spatial folds centered on the 10 thinned points
folds <- make_spatial_folds(
  xy.selected = xy.thin,
  xy = plants_xy,
  distance.step.x = 0.05,
  training.fraction = 0.6,
  n.cores = 1
)

# Each element is a fold with training and testing indices
length(folds)  # 10 folds
names(folds[[1]])  # "training" and "testing"

# Visualize first fold (training = red, testing = blue, center = black)
if (interactive()) {
  plot(plants_xy[c("x", "y")], type = "n", xlab = "", ylab = "")
  points(plants_xy[folds[[1]]$training, c("x", "y")], col = "red4", pch = 15)
  points(plants_xy[folds[[1]]$testing, c("x", "y")], col = "blue4", pch = 15)
  points(
    plants_xy[folds[[1]]$training[1], c("x", "y")],
    col = "black",
    pch = 15,
    cex = 2
  )
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_spatial_folds", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mem")
### * mem

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mem
### Title: Compute Moran's Eigenvector Maps from distance matrix
### Aliases: mem

### ** Examples

data(plants_distance)

# Compute MEMs from distance matrix
mems <- mem(distance.matrix = plants_distance)

# View structure
head(mems)
dim(mems)

# Check column names
colnames(mems)[1:5]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mem", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mem_multithreshold")
### * mem_multithreshold

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mem_multithreshold
### Title: Compute Moran's Eigenvector Maps across multiple distance
###   thresholds
### Aliases: mem_multithreshold

### ** Examples

data(plants_distance)

# Compute MEMs for multiple distance thresholds
mems <- mem_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 5000)
)

# View structure
head(mems)
dim(mems)

# Check column names showing threshold and predictor number
colnames(mems)[1:6]

# Limit number of spatial predictors
mems_limited <- mem_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 5000),
  max.spatial.predictors = 20
)
dim(mems_limited)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mem_multithreshold", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("moran")
### * moran

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: moran
### Title: Moran's I test for spatial autocorrelation
### Aliases: moran

### ** Examples

data(plants_df, plants_distance, plants_response)

# Test for spatial autocorrelation in response variable
moran_test <- moran(
  x = plants_df[[plants_response]],
  distance.matrix = plants_distance,
  distance.threshold = 1000
)

# View test results
moran_test$test

# Access components
moran_test$test$moran.i  # Observed Moran's I
moran_test$test$p.value  # P-value
moran_test$test$interpretation  # Text interpretation




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("moran", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("moran_multithreshold")
### * moran_multithreshold

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: moran_multithreshold
### Title: Moran's I test across multiple distance thresholds
### Aliases: moran_multithreshold

### ** Examples

data(plants_df, plants_distance, plants_response)

# Test spatial autocorrelation at multiple distance thresholds
moran_multi <- moran_multithreshold(
  x = plants_df[[plants_response]],
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 5000)
)

# View results for all thresholds
moran_multi$per.distance

# Find optimal distance threshold
moran_multi$max.moran.distance.threshold
moran_multi$max.moran

# Plot shows spatial autocorrelation across scales
moran_multi$plot




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("moran_multithreshold", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("normality")
### * normality

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: residuals_test
### Title: Normality test of a numeric vector
### Aliases: residuals_test

### ** Examples


residuals_test(residuals = runif(100))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("normality", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("objects_size")
### * objects_size

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: objects_size
### Title: Display sizes of objects in current R environment
### Aliases: objects_size

### ** Examples

# Create some objects of different sizes
small_vector <- runif(100)
medium_matrix <- matrix(runif(10000), 100, 100)
large_matrix <- matrix(runif(100000), 1000, 100)

# View the 5 largest objects
objects_size(n = 5)

# Check all objects (up to 10 by default)
objects_size()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("objects_size", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("optimization_function")
### * optimization_function

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: optimization_function
### Title: Compute optimization scores for spatial predictor selection
### Aliases: optimization_function

### ** Examples

## Not run: 
##D # This function is typically called internally during spatial predictor selection
##D # Example showing the structure of input data:
##D 
##D # Simulated optimization data frame
##D opt_data <- data.frame(
##D   moran.i = c(0.5, 0.3, 0.2, 0.15),
##D   r.squared = c(0.6, 0.65, 0.68, 0.69),
##D   penalization.per.variable = c(0.1, 0.2, 0.3, 0.4),
##D   p.value.binary = c(0, 0, 1, 1)
##D )
##D 
##D # Compute optimization scores
##D scores_moran <- optimization_function(
##D   x = opt_data,
##D   weight.r.squared = 0.5,
##D   weight.penalization.n.predictors = 0.5,
##D   optimization.method = "moran.i"
##D )
##D 
##D # Compare methods
##D scores_pvalue <- optimization_function(
##D   x = opt_data,
##D   weight.r.squared = 0.5,
##D   weight.penalization.n.predictors = 0.5,
##D   optimization.method = "p.value"
##D )
##D 
##D # Higher score indicates better solution
##D which.max(scores_moran)
##D which.max(scores_pvalue)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("optimization_function", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pca")
### * pca

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pca
### Title: Compute Principal Component Analysis
### Aliases: pca

### ** Examples

data(plants_distance)

# Compute principal components from distance matrix
pca_components <- pca(x = plants_distance)

# View structure
head(pca_components)
dim(pca_components)

# Check column names
colnames(pca_components)[1:5]

# Custom column prefix
pca_custom <- pca(
  x = plants_distance,
  colnames.prefix = "distance_pc"
)
colnames(pca_custom)[1:3]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pca", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pca_multithreshold")
### * pca_multithreshold

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pca_multithreshold
### Title: Compute Principal Component Analysis at multiple distance
###   thresholds
### Aliases: pca_multithreshold

### ** Examples


data(plants_distance)

# Compute PCA spatial predictors at multiple distance thresholds
pca_predictors <- pca_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 5000)
)

# View structure
head(pca_predictors)
dim(pca_predictors)

# Check predictor names (show scale information)
colnames(pca_predictors)[1:6]

# Limit number of predictors to save memory
pca_limited <- pca_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = c(0, 1000, 5000),
  max.spatial.predictors = 20
)
ncol(pca_limited)  # At most 20 predictors




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pca_multithreshold", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_evaluation")
### * plot_evaluation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_evaluation
### Title: Visualize spatial cross-validation results
### Aliases: plot_evaluation

### ** Examples

if(interactive()){

data(plants_rf, plants_xy)

# Perform spatial cross-validation
plants_rf <- rf_evaluate(
  model = plants_rf,
  xy = plants_xy,
  repetitions = 5,
  n.cores = 1
)

# Visualize evaluation results
plot_evaluation(plants_rf)

# Without notches for simpler boxplots
plot_evaluation(plants_rf, notch = FALSE)

# Custom colors
plot_evaluation(
  plants_rf,
  fill.color = c("#E64B35FF", "#4DBBD5FF", "#00A087FF")
)

# Print summary statistics
print_evaluation(plants_rf)

# Extract evaluation data for custom analysis
evaluation_data <- get_evaluation(plants_rf)
head(evaluation_data)

}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_evaluation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_importance")
### * plot_importance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_importance
### Title: Visualize variable importance scores
### Aliases: plot_importance

### ** Examples

data(plants_rf, plants_rf_spatial)

# Plot importance from single Random Forest model
plot_importance(plants_rf)


# Plot importance from spatial Random Forest model
plot_importance(plants_rf_spatial)

# Custom colors
plot_importance(
  plants_rf,
  fill.color = viridis::viridis(20, option = "D")
)

# Return plot object for further customization
p <- plot_importance(plants_rf, verbose = FALSE)
p + ggplot2::ggtitle("Custom Title")

# Print importance values
print_importance(plants_rf)

# Extract importance data for custom analysis
importance_data <- get_importance(plants_rf)
head(importance_data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_importance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_moran")
### * plot_moran

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_moran
### Title: Plots a Moran's I test of model residuals
### Aliases: plot_moran

### ** Examples


data(plants_rf)

plot_moran(plants_rf)

plot_moran(plants_rf, option = 2)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_moran", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_optimization")
### * plot_optimization

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_optimization
### Title: Optimization plot of a selection of spatial predictors
### Aliases: plot_optimization

### ** Examples


data(plants_rf_spatial)

plot_optimization(plants_rf_spatial)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_optimization", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_residuals_diagnostics")
### * plot_residuals_diagnostics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_residuals_diagnostics
### Title: Plot residuals diagnostics
### Aliases: plot_residuals_diagnostics

### ** Examples


data(plants_rf)

plot_residuals_diagnostics(plants_rf)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_residuals_diagnostics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_response_curves")
### * plot_response_curves

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_response_curves
### Title: Plots the response curves of a model.
### Aliases: plot_response_curves

### ** Examples


data(plants_rf)

plot_response_curves(
  model = plants_rf,
  variables = "climate_bio1_average"
)

plot_response_curves(
  model = plants_rf,
  variables = "climate_bio1_average",
  show.data = TRUE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_response_curves", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_response_surface")
### * plot_response_surface

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_response_surface
### Title: Plots the response surfaces of a random forest model
### Aliases: plot_response_surface

### ** Examples


data(plants_rf)

plot_response_surface(
  model = plants_rf,
  a = "climate_bio1_average",
  b = "human_population",
  grid.resolution = 50
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_response_surface", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_training_df")
### * plot_training_df

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_training_df
### Title: Scatterplots of a training data frame
### Aliases: plot_training_df

### ** Examples


data(
  plants_df,
  plants_response,
  plants_predictors
)

plot_training_df(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors[1:4]
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_training_df", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_training_df_moran")
### * plot_training_df_moran

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_training_df_moran
### Title: Moran's I plots of a training data frame
### Aliases: plot_training_df_moran

### ** Examples


data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

plot_training_df_moran(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors[1:4],
  distance.matrix = plants_distance,
  distance.thresholds = c(1000, 2000, 4000)
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_training_df_moran", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_tuning")
### * plot_tuning

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_tuning
### Title: Plots a tuning object produced by 'rf_tuning()'
### Aliases: plot_tuning

### ** Examples


if(interactive()){
  data(
    plants_rf,
    plants_xy
  )

  plants_rf_tuned <- rf_tuning(
    model = plants_rf,
    num.trees = c(25, 50),
    mtry = c(5, 10),
    min.node.size = c(10, 20),
    xy = plants_xy,
    repetitions = 5,
    n.cores = 1
  )

  plot_tuning(plants_rf_tuned)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_tuning", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("prepare_importance_spatial")
### * prepare_importance_spatial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: prepare_importance_spatial
### Title: Prepares variable importance objects for spatial models
### Aliases: prepare_importance_spatial

### ** Examples


data(plants_rf_spatial)

prepare_importance_spatial(plants_rf_spatial) %>%
  head()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("prepare_importance_spatial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print")
### * print

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.rf
### Title: Custom print method for random forest models
### Aliases: print.rf

### ** Examples


data(plants_rf)

print(plants_rf)

#or
plants_rf




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print_evaluation")
### * print_evaluation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print_evaluation
### Title: Prints cross-validation results
### Aliases: print_evaluation

### ** Examples


data(
  plants_rf,
  plants_xy
)

m_evaluated <- rf_evaluate(
  model = plants_rf,
  xy = plants_xy,
  repetitions = 5,
  n.cores = 1
)

plot_evaluation(m_evaluated, notch = FALSE)

print_evaluation(m_evaluated)

get_evaluation(m_evaluated)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print_evaluation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print_importance")
### * print_importance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print_importance
### Title: Prints variable importance
### Aliases: print_importance

### ** Examples


data(plants_rf)

print_importance(plants_rf)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print_importance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print_moran")
### * print_moran

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print_moran
### Title: Prints results of a Moran's I test
### Aliases: print_moran

### ** Examples


data(plants_rf)

print_moran(plants_rf)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print_moran", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print_performance")
### * print_performance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print_performance
### Title: print_performance
### Aliases: print_performance

### ** Examples


data(plants_rf)

print_performance(plants_rf)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print_performance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rank_spatial_predictors")
### * rank_spatial_predictors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rank_spatial_predictors
### Title: Ranks spatial predictors
### Aliases: rank_spatial_predictors

### ** Examples


data(
  plants_df,
  plants_response,
  plants_distance
)

#subset to speed up example
idx <- 50:90
plants_distance_sub <- plants_distance[idx, idx]

y <- mem(
  distance.matrix = plants_distance_sub,
  distance.threshold = 1000
)

#rank spatial predictors by Moran's I
y_rank <- rank_spatial_predictors(
  distance.matrix = plants_distance_sub,
  distance.thresholds = 1000,
  spatial.predictors.df = y,
  ranking.method = "moran",
  n.cores = 1
)

y_rank$criteria
y_rank$ranking

#rank spatial predictors by association with response
y_rank <- rank_spatial_predictors(
  data = plants_df[idx, ],
  dependent.variable.name = plants_response,
  distance.matrix = plants_distance_sub,
  distance.thresholds = 1000,
  spatial.predictors.df = y,
  ranking.method = "effect",
  n.cores = 1
)

y_rank$criteria
y_rank$ranking



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rank_spatial_predictors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rescale_vector")
### * rescale_vector

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rescale_vector
### Title: Rescales a numeric vector into a new range
### Aliases: rescale_vector

### ** Examples


y <- rescale_vector(
  x = rnorm(100),
  new.min = 0,
  new.max = 100,
  integer = TRUE
)
y




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rescale_vector", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("residuals_diagnostics")
### * residuals_diagnostics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: residuals_diagnostics
### Title: Normality test of a numeric vector
### Aliases: residuals_diagnostics

### ** Examples


data(plants_rf)

y <- residuals_diagnostics(
  residuals = get_residuals(plants_rf),
  predictions = get_predictions(plants_rf)
)
y




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("residuals_diagnostics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rf")
### * rf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rf
### Title: Random forest models with Moran's I test of the residuals
### Aliases: rf

### ** Examples


data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance
)

m <- rf(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(100, 1000, 2000),
  ranger.arguments = list(
    num.trees = 50,
    min.node.size = 20
  ),
  verbose = FALSE,
  n.cores = 1
)

class(m)
#variable importance
m$importance$per.variable
m$importance$per.variable.plot

#model performance
m$performance

#autocorrelation of residuals
m$residuals$autocorrelation$per.distance
m$residuals$autocorrelation$plot

#model predictions
m$predictions$values

#predictions for new data (using stats::predict)
y <- stats::predict(
  object = m,
  data = plants_df[1:5, ],
  type = "response"
)$predictions

#alternative: pass arguments via ranger.arguments list
args <- list(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = c(100, 1000, 2000),
  num.trees = 50,
  min.node.size = 20,
  num.threads = 1
)

m <- rf(
  ranger.arguments = args,
  verbose = FALSE
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rf_compare")
### * rf_compare

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rf_compare
### Title: Compares models via spatial cross-validation
### Aliases: rf_compare

### ** Examples


if(interactive()){

  data(
    plants_rf,
    plants_rf_spatial,
    plants_xy
  )

  comparison <- rf_compare(
    models = list(
      `Non spatial` = plants_rf,
      Spatial = plants_rf_spatial
    ),
    repetitions = 5,
    xy = plants_xy,
    metrics = "rmse",
    n.cores = 1
  )

}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rf_compare", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rf_evaluate")
### * rf_evaluate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rf_evaluate
### Title: Evaluates random forest models with spatial cross-validation
### Aliases: rf_evaluate

### ** Examples


data(
  plants_rf,
  plants_xy
)

m_evaluated <- rf_evaluate(
  model = plants_rf,
  xy = plants_xy,
  repetitions = 5,
  n.cores = 1
)

plot_evaluation(m_evaluated, notch = FALSE)

print_evaluation(m_evaluated)

get_evaluation(m_evaluated)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rf_evaluate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rf_importance")
### * rf_importance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rf_importance
### Title: Contribution of each predictor to model transferability
### Aliases: rf_importance

### ** Examples


if(interactive()){
  data(plants_rf)

  m_importance <- rf_importance(
    model = plants_rf,
    repetitions = 5
  )
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rf_importance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rf_repeat")
### * rf_repeat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rf_repeat
### Title: Fits several random forest models on the same data
### Aliases: rf_repeat

### ** Examples


if(interactive()){

  data(plants_rf)

  m_repeat <- rf_repeat(
    model = plants_rf,
    repetitions = 5,
    n.cores = 1
  )

  #performance scores across repetitions
  m_repeat$performance
  print_performance(m_repeat)

  #variable importance
  plot_importance(m_repeat)

  #response curves
  plot_response_curves(
    model = m_repeat,
    variables = "climate_bio1_average",
    quantiles = 0.5
  )

}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rf_repeat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rf_spatial")
### * rf_spatial

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rf_spatial
### Title: Fits spatial random forest models
### Aliases: rf_spatial

### ** Examples


if (interactive()) {
  data(
    plants_df,
    plants_response,
    plants_predictors,
    plants_distance,
    plants_rf
  )

  #subset to speed up example
  idx <- 1:100
  plants_df <- plants_df[idx, ]
  plants_distance <- plants_distance[idx, idx]

  #fit spatial model from scratch
  m_spatial <- rf_spatial(
    data = plants_df,
    dependent.variable.name = plants_response,
    predictor.variable.names = plants_predictors,
    distance.matrix = plants_distance,
    distance.thresholds = c(100, 1000, 2000),
    method = "mem.moran.sequential",
    ranger.arguments = list(num.trees = 30),
    n.cores = 1
  )

  plot_residuals_diagnostics(m_spatial)

  #optimization of MEM selection
  plot_optimization(m_spatial)

  #from non-spatial to spatial model
  m_spatial <- rf_spatial(
    model = plants_rf
    )

}





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rf_spatial", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rf_tuning")
### * rf_tuning

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rf_tuning
### Title: Tuning of random forest hyperparameters via spatial
###   cross-validation
### Aliases: rf_tuning

### ** Examples


if(interactive()){
  data(
    plants_rf,
    plants_xy
  )

  plants_rf_tuned <- rf_tuning(
    model = plants_rf,
    num.trees = c(25, 50),
    mtry = c(5, 10),
    min.node.size = c(10, 20),
    xy = plants_xy,
    repetitions = 5,
    n.cores = 1
  )

  plot_tuning(plants_rf_tuned)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rf_tuning", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("root_mean_squared_error")
### * root_mean_squared_error

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: root_mean_squared_error
### Title: RMSE and normalized RMSE
### Aliases: root_mean_squared_error

### ** Examples


root_mean_squared_error(
  o = runif(10),
  p = runif(10)
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("root_mean_squared_error", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("select_spatial_predictors_recursive")
### * select_spatial_predictors_recursive

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: select_spatial_predictors_recursive
### Title: Finds optimal combinations of spatial predictors
### Aliases: select_spatial_predictors_recursive

### ** Examples


if (interactive()) {
  data(
    plants_df,
    plants_response,
    plants_predictors,
    plants_distance,
    plants_rf
  )

  #subset to speed up example
  idx <- 1:20
  plants_df <- plants_df[idx, ]
  plants_distance <- plants_distance[idx, idx]

  #generate spatial predictors
  mems <- mem_multithreshold(
    distance.matrix = plants_distance,
    distance.thresholds = 100
  )

  #rank them from higher to lower moran
  mems.rank <- rank_spatial_predictors(
    ranking.method = "moran",
    spatial.predictors.df = mems,
    reference.moran.i = plants_rf$residuals$autocorrelation$max.moran,
    distance.matrix = plants_distance,
    distance.thresholds = 100,
    n.cores = 1
  )

  #select best subset via sequential addition
  selection <- select_spatial_predictors_recursive(
    data = plants_df,
    dependent.variable.name = plants_response,
    predictor.variable.names = plants_predictors,
    distance.matrix = plants_distance,
    distance.thresholds = 0,
    spatial.predictors.df = mems,
    spatial.predictors.ranking = mems.rank,
    ranger.arguments = list(num.trees = 30),
    n.cores = 1
  )

  #names of selected spatial predictors
  selection$best.spatial.predictors

  #optimization plot
  plot_optimization(selection$optimization)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("select_spatial_predictors_recursive", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("select_spatial_predictors_sequential")
### * select_spatial_predictors_sequential

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: select_spatial_predictors_sequential
### Title: Sequential introduction of spatial predictors into a model
### Aliases: select_spatial_predictors_sequential

### ** Examples


data(
  plants_df,
  plants_response,
  plants_predictors,
  plants_distance,
  plants_rf
)

#subset to speed up example
idx <- 1:20
plants_df <- plants_df[idx, ]
plants_distance <- plants_distance[idx, idx]

#generate spatial predictors
mems <- mem_multithreshold(
  distance.matrix = plants_distance,
  distance.thresholds = 100
)

#rank them from higher to lower moran
mems.rank <- rank_spatial_predictors(
  ranking.method = "moran",
  spatial.predictors.df = mems,
  reference.moran.i = plants_rf$residuals$autocorrelation$max.moran,
  distance.matrix = plants_distance,
  distance.thresholds = 100,
  n.cores = 1
)

#select best subset via sequential addition
selection <- select_spatial_predictors_sequential(
  data = plants_df,
  dependent.variable.name = plants_response,
  predictor.variable.names = plants_predictors,
  distance.matrix = plants_distance,
  distance.thresholds = 0,
  spatial.predictors.df = mems,
  spatial.predictors.ranking = mems.rank,
  ranger.arguments = list(num.trees = 30),
  n.cores = 1
)

#names of selected spatial predictors
selection$best.spatial.predictors

#optimization plot
plot_optimization(selection$optimization)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("select_spatial_predictors_sequential", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("standard_error")
### * standard_error

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: standard_error
### Title: Standard error of the mean of a numeric vector
### Aliases: standard_error

### ** Examples


standard_error(x = runif(10))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("standard_error", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("statistical_mode")
### * statistical_mode

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: statistical_mode
### Title: Statistical mode of a vector
### Aliases: statistical_mode

### ** Examples


statistical_mode(x = c(10, 9, 10, 8))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("statistical_mode", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("the_feature_engineer")
### * the_feature_engineer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: the_feature_engineer
### Title: Suggest variable interactions and composite features for random
###   forest models
### Aliases: the_feature_engineer

### ** Examples


if (interactive()) {
  data(
    plants_df,
    plants_response,
    plants_predictors,
    plants_xy,
    plants_rf
  )

  #get five most important predictors from plants_rf to speed-up example
  predictors <- get_importance(plants_rf)[1:5, "variable"]

  #subset to speed-up example
  idx <- 1:30
  plants_df <- plants_df[idx, ]
  plants_xy <- plants_xy[idx, ]

  #data subsetted to speed-up example runtime
  y <- the_feature_engineer(
    data = plants_df,
    dependent.variable.name = plants_response,
    predictor.variable.names = predictors,
    xy = plants_xy,
    repetitions = 5,
    n.cores = 1,
    ranger.arguments = list(
      num.trees = 30
    ),
    verbose = TRUE
  )

  #all tested interactions
  y$screening

  #selected interaction (same as above in this case)
  y$selected

  #new column added to data
  head(y$data[, y$selected$interaction.name])
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("the_feature_engineer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("thinning")
### * thinning

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: thinning
### Title: Applies thinning to pairs of coordinates
### Aliases: thinning

### ** Examples


data(plants_xy)

y <- thinning(
  xy = plants_xy,
  minimum.distance = 10
)

if (interactive()) {
  plot(
    plants_xy[, c("x", "y")],
    col = "blue",
    pch = 15
  )

  points(
    y[, c("x", "y")],
    col = "red",
    pch = 15
  )
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("thinning", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("thinning_til_n")
### * thinning_til_n

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: thinning_til_n
### Title: Applies thinning to pairs of coordinates until reaching a given
###   n
### Aliases: thinning_til_n

### ** Examples


data(plants_xy)

y <- thinning_til_n(
  xy = plants_xy,
  n = 10
)

if (interactive()) {
  plot(
    plants_xy[, c("x", "y")],
    col = "blue",
    pch = 15
  )

  points(
    y[, c("x", "y")],
    col = "red",
    pch = 15,
    cex = 1.5
  )
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("thinning_til_n", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("weights_from_distance_matrix")
### * weights_from_distance_matrix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: weights_from_distance_matrix
### Title: Transforms a distance matrix into a matrix of weights
### Aliases: weights_from_distance_matrix

### ** Examples


data(plants_distance)

y <- weights_from_distance_matrix(
  distance.matrix = plants_distance
)

y[1:5, 1:5]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("weights_from_distance_matrix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
