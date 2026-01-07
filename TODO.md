>   interactions <- the_feature_engineer(
+     data = plants_df,
+     dependent.variable.name = "richness_species_vascular",
+     predictor.variable.names = c(
+       "human_population",
+       "bias_area_km2",
+       "climate_bio1_average",
+       "human_population_density"
+     ),
+     xy = plants_xy,
+     importance.threshold = 1000,
+     verbose = FALSE,
+     seed = 100
+   )
Warning message:
In the_feature_engineer(data = plants_df, dependent.variable.name = "richness_species_vascular",  :
  There are not enough predictors above 'importance.threshold = 2580.63346' for this analysis, returning NULL.
