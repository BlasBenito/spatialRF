# =============================================================================#
# Full workflow demonstration for spatialRF package
# =============================================================================#
# This script demonstrates the complete workflow of the spatialRF package
# from a user perspective, showcasing all major functions and their integration
# =============================================================================#

# Load required packages ------------------------------------------------------
library(spatialRF)
library(ggplot2)

# Set seed for reproducibility ------------------------------------------------
set.seed(123)

# Setup parallel cluster ------------------------------------------------------
# Using a single cluster for the entire workflow is more efficient than
# creating/destroying clusters for each function call
n.cores <- parallel::detectCores() - 1
cluster <- parallel::makeCluster(n.cores, type = "PSOCK")

cat("Created parallel cluster with", n.cores, "cores\n")

# Ensure cluster is stopped when script exits (even if there's an error)
on.exit({
  parallel::stopCluster(cluster)
  cat("\nCluster stopped.\n")
})

# Load example data -----------------------------------------------------------
data("plant_richness_df")
data("distance_matrix")

# Define variables ------------------------------------------------------------
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]

# Extract coordinates (needed for spatial cross-validation)
xy <- plant_richness_df[, c("x", "y")]

cat("\nDataset dimensions:\n")
cat("  Observations:", nrow(plant_richness_df), "\n")
cat("  Predictors:", length(predictor.variable.names), "\n\n")

# =============================================================================#
# 1. EXPLORATORY DATA ANALYSIS
# =============================================================================#

cat("\n========================================\n")
cat("1. EXPLORATORY DATA ANALYSIS\n")
cat("========================================\n\n")

# 1.1 Visualize relationships between response and predictors ----------------
cat("1.1 Plotting relationships between response and predictors...\n")

p1 <- plot_training_df(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  ncol = 4
)

print(p1)

# 1.2 Check spatial autocorrelation in the data ------------------------------
cat("\n1.2 Checking spatial autocorrelation in variables...\n")

# Define distance thresholds for Moran's I computation
distance.thresholds <- c(0, 1000, 2000, 4000)

p2 <- plot_training_df_moran(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance.thresholds
)

print(p2)

# =============================================================================#
# 2. BASIC RANDOM FOREST MODEL
# =============================================================================#

cat("\n========================================\n")
cat("2. BASIC RANDOM FOREST MODEL\n")
cat("========================================\n\n")

# 2.1 Fit a basic random forest model -----------------------------------------
cat("2.1 Fitting basic random forest model...\n")

model.rf <- rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance.thresholds,
  xy = xy,
  seed = 123,
  verbose = TRUE,
  cluster = cluster
)

# 2.2 Examine model performance -----------------------------------------------
cat("\n2.2 Model performance metrics:\n")
print(model.rf$performance)

# 2.3 Check variable importance -----------------------------------------------
cat("\n2.3 Variable importance (permutation-based):\n")
print(model.rf$importance$per.variable)
print(model.rf$importance$per.variable.plot)

# 2.4 Check spatial autocorrelation of residuals ------------------------------
cat("\n2.4 Spatial autocorrelation of residuals:\n")
print(model.rf$residuals$autocorrelation$per.distance)
print(model.rf$residuals$autocorrelation$plot)

# Note: If Moran's I is significant, spatial predictors may be needed

# =============================================================================#
# 3. VARIABLE IMPORTANCE VIA SPATIAL CROSS-VALIDATION
# =============================================================================#

cat("\n========================================\n")
cat("3. VARIABLE IMPORTANCE (SPATIAL CV)\n")
cat("========================================\n\n")

# Compute variable importance using spatial cross-validation
# This provides importance scores less sensitive to spatial autocorrelation
cat("3.1 Computing variable importance via spatial cross-validation...\n")
cat("    (This may take a few minutes)\n")

model.rf <- rf_importance(
  model = model.rf,
  xy = xy,
  repetitions = 10, # Increase for more robust estimates
  training.fraction = 0.75,
  metric = "r.squared",
  seed = 123,
  verbose = TRUE,
  cluster = cluster
)

cat("\n3.2 Variable importance from spatial CV:\n")
print(model.rf$importance$per.variable)
print(model.rf$importance$cv.per.variable.plot)

# =============================================================================#
# 4. MODEL EVALUATION WITH SPATIAL CROSS-VALIDATION
# =============================================================================#

cat("\n========================================\n")
cat("4. MODEL EVALUATION (SPATIAL CV)\n")
cat("========================================\n\n")

# Evaluate model performance using spatial cross-validation
cat("4.1 Evaluating model with spatial cross-validation...\n")
cat("    (This may take a few minutes)\n")

model.rf <- rf_evaluate(
  model = model.rf,
  xy = xy,
  repetitions = 10, # Increase for more robust estimates
  training.fraction = 0.75,
  metrics = c("r.squared", "pseudo.r.squared", "rmse", "nrmse"),
  seed = 123,
  verbose = TRUE,
  cluster = cluster
)

cat("\n4.2 Evaluation results:\n")
print(model.rf$evaluation$aggregated)

# Plot evaluation results
if (exists("plot_evaluation")) {
  print(plot_evaluation(model.rf))
}

# =============================================================================#
# 5. HYPERPARAMETER TUNING
# =============================================================================#

cat("\n========================================\n")
cat("5. HYPERPARAMETER TUNING\n")
cat("========================================\n\n")

# Tune random forest hyperparameters via spatial cross-validation
cat("5.1 Tuning hyperparameters via spatial cross-validation...\n")
cat("    (This may take several minutes)\n")

model.rf <- rf_tuning(
  model = model.rf,
  num.trees = c(500, 1000),
  mtry = c(3, 6, 9),
  min.node.size = c(5, 10, 20),
  xy = xy,
  repetitions = 10, # Increase for more robust estimates
  training.fraction = 0.75,
  seed = 123,
  verbose = TRUE,
  cluster = cluster
)

cat("\n5.2 Tuning results:\n")
print(head(model.rf$tuning, 10))

# Identify best hyperparameters
best_params <- model.rf$tuning$tuning.df[
  which.max(model.rf$tuning$tuning.df$r.squared),
]
cat("\n5.3 Best hyperparameters:\n")
print(best_params)

# =============================================================================#
# 6. SPATIAL RANDOM FOREST MODELS
# =============================================================================#

cat("\n========================================\n")
cat("6. SPATIAL RANDOM FOREST MODELS\n")
cat("========================================\n\n")

# Spatial random forest models incorporate spatial predictors to account for
# spatial autocorrelation in the data and residuals

# 6.1 Hengl method (RFsp) -----------------------------------------------------
cat("6.1 Fitting spatial model using Hengl method (RFsp)...\n")
cat("    Uses distance matrix columns as spatial predictors\n")

model.spatial.hengl <- rf_spatial(
  model = model.rf,
  method = "hengl",
  seed = 123,
  verbose = TRUE,
  cluster = cluster
)

cat("\n6.1.1 Hengl model performance:\n")
print(model.spatial.hengl$performance)

cat("\n6.1.2 Spatial autocorrelation of residuals:\n")
print(model.spatial.hengl$residuals$autocorrelation$per.distance)

# 6.2 MEM method with Moran-based sequential selection -----------------------
cat("\n6.2 Fitting spatial model using MEM (Moran + Sequential)...\n")
cat("    Moran's Eigenvector Maps ranked by their Moran's I\n")

model.spatial.mem.moran <- rf_spatial(
  model = model.rf,
  method = "mem.moran.sequential",
  seed = 123,
  verbose = TRUE,
  cluster = cluster
)

cat("\n6.2.1 MEM (Moran) model performance:\n")
print(model.spatial.mem.moran$performance)

cat("\n6.2.2 Number of spatial predictors selected:\n")
cat(length(model.spatial.mem.moran$spatial$names), "spatial predictors\n")

cat("\n6.2.3 Spatial autocorrelation of residuals:\n")
print(model.spatial.mem.moran$residuals$autocorrelation$per.distance)

# 6.3 MEM method with effect-based sequential selection ----------------------
cat("\n6.3 Fitting spatial model using MEM (Effect + Sequential)...\n")
cat("    MEM predictors ranked by their effect on reducing Moran's I\n")

model.spatial.mem.effect <- rf_spatial(
  model = model.rf,
  method = "mem.effect.sequential",
  seed = 123,
  verbose = TRUE,
  cluster = cluster
)

cat("\n6.3.1 MEM (Effect) model performance:\n")
print(model.spatial.mem.effect$performance)

cat("\n6.3.2 Number of spatial predictors selected:\n")
cat(length(model.spatial.mem.effect$spatial$names), "spatial predictors\n")

cat("\n6.3.3 Spatial predictor selection plot:\n")
print(model.spatial.mem.effect$spatial$plot)

# 6.4 MEM method with effect-based recursive selection -----------------------
cat("\n6.4 Fitting spatial model using MEM (Effect + Recursive)...\n")
cat("    Iteratively re-ranks and selects spatial predictors\n")
cat("    (This may take longer but produces smaller predictor sets)\n")

model.spatial.mem.recursive <- rf_spatial(
  model = model.rf,
  method = "mem.effect.recursive",
  seed = 123,
  verbose = TRUE,
  cluster = cluster
)

cat("\n6.4.1 MEM (Recursive) model performance:\n")
print(model.spatial.mem.recursive$performance)

cat("\n6.4.2 Number of spatial predictors selected:\n")
cat(length(model.spatial.mem.recursive$spatial$names), "spatial predictors\n")

# =============================================================================#
# 7. MODEL COMPARISON
# =============================================================================#

cat("\n========================================\n")
cat("7. MODEL COMPARISON\n")
cat("========================================\n\n")

# Compare all fitted models
cat("7.1 Performance comparison across models:\n\n")

models_comparison <- data.frame(
  Model = c(
    "Basic RF",
    "Tuned RF",
    "Spatial (Hengl)",
    "Spatial (MEM-Moran)",
    "Spatial (MEM-Effect)",
    "Spatial (MEM-Recursive)"
  ),
  R2_OOB = c(
    model.rf$performance$r.squared,
    model.rf$performance$r.squared,
    model.spatial.hengl$performance$r.squared,
    model.spatial.mem.moran$performance$r.squared,
    model.spatial.mem.effect$performance$r.squared,
    model.spatial.mem.recursive$performance$r.squared
  ),
  RMSE = c(
    model.rf$performance$rmse,
    model.rf$performance$rmse,
    model.spatial.hengl$performance$rmse,
    model.spatial.mem.moran$performance$rmse,
    model.spatial.mem.effect$performance$rmse,
    model.spatial.mem.recursive$performance$rmse
  ),
  Max_Moran_I = c(
    model.rf$residuals$autocorrelation$max.moran,
    model.rf$residuals$autocorrelation$max.moran,
    model.spatial.hengl$residuals$autocorrelation$max.moran,
    model.spatial.mem.moran$residuals$autocorrelation$max.moran,
    model.spatial.mem.effect$residuals$autocorrelation$max.moran,
    model.spatial.mem.recursive$residuals$autocorrelation$max.moran
  ),
  Moran_p_value = c(
    model.rf$residuals$autocorrelation$per.distance$p.value[1],
    model.rf$residuals$autocorrelation$per.distance$p.value[1],
    model.spatial.hengl$residuals$autocorrelation$per.distance$p.value[1],
    model.spatial.mem.moran$residuals$autocorrelation$per.distance$p.value[1],
    model.spatial.mem.effect$residuals$autocorrelation$per.distance$p.value[1],
    model.spatial.mem.recursive$residuals$autocorrelation$per.distance$p.value[
      1
    ]
  )
)

print(models_comparison)

cat("\n7.2 Key observations:\n")
cat("- R2 (OOB): Higher is better (model fit)\n")
cat("- RMSE: Lower is better (prediction error)\n")
cat("- Max Moran's I: Closer to 0 is better (less spatial autocorrelation)\n")
cat(
  "- Moran p-value: > 0.05 indicates no significant spatial autocorrelation\n"
)

# =============================================================================#
# 8. PREDICTIONS AND MODEL USAGE
# =============================================================================#

cat("\n========================================\n")
cat("8. MAKING PREDICTIONS\n")
cat("========================================\n\n")

# Make predictions using the best spatial model
cat("8.1 Making predictions with the best model...\n")

# Select the best model (e.g., MEM-Effect based on Moran's I reduction)
best_model <- model.spatial.mem.effect

# Make predictions on the training data (for demonstration)
predictions <- stats::predict(
  object = best_model,
  data = plant_richness_df,
  type = "response"
)$predictions

# Create a comparison data frame
prediction_comparison <- data.frame(
  Observed = plant_richness_df[[dependent.variable.name]],
  Predicted = predictions,
  Residual = plant_richness_df[[dependent.variable.name]] - predictions
)

cat("\n8.2 Prediction summary:\n")
print(summary(prediction_comparison))

# Plot observed vs predicted
cat("\n8.3 Observed vs Predicted plot:\n")
p_pred <- ggplot(prediction_comparison, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  theme_bw() +
  labs(
    title = "Observed vs Predicted Values",
    subtitle = paste0(
      "R² = ",
      round(
        cor(prediction_comparison$Observed, prediction_comparison$Predicted)^2,
        3
      )
    )
  )

print(p_pred)

# =============================================================================#
# 9. SUMMARY AND RECOMMENDATIONS
# =============================================================================#

cat("\n========================================\n")
cat("9. WORKFLOW SUMMARY\n")
cat("========================================\n\n")

cat("This workflow demonstrated:\n")
cat(
  "1. Exploratory data analysis with plot_training_df() and plot_training_df_moran()\n"
)
cat("2. Basic random forest modeling with rf()\n")
cat("3. Variable importance via spatial CV with rf_importance()\n")
cat("4. Model evaluation via spatial CV with rf_evaluate()\n")
cat("5. Hyperparameter tuning with rf_tuning()\n")
cat("6. Spatial random forest modeling with rf_spatial()\n")
cat("7. Model comparison and selection\n")
cat("8. Making predictions with the final model\n\n")

cat("Recommended workflow for your data:\n")
cat("1. Start with exploratory analysis to understand spatial patterns\n")
cat("2. Fit a basic RF model to establish baseline performance\n")
cat("3. Check for spatial autocorrelation in residuals\n")
cat(
  "4. If Moran's I is significant, use rf_spatial() with appropriate method\n"
)
cat("5. Consider tuning hyperparameters for optimal performance\n")
cat("6. Validate with spatial cross-validation to assess transferability\n\n")

cat("========================================\n")
cat("WORKFLOW COMPLETE!\n")
cat("========================================\n\n")
