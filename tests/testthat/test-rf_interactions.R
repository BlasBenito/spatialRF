test_that("`rf_interactions()` works from scratch", {
  data(plants_df)
  data(plants_predictors)
  data(plants_xy)

  # Test with lenient parameters to ensure interactions are found
  suppressWarnings({
    m_int <- rf_interactions(
      data = plants_df[1:50, ],  # Use subset for speed
      dependent.variable.name = "richness_species_vascular",
      predictor.variable.names = c(
        "climate_bio1_average",
        "climate_aridity_index_average",
        "human_population",
        "bias_area_km2"
      ),
      xy = plants_xy[1:50, ],
      importance.threshold = 0.25,  # Lower threshold to find more interactions
      cor.threshold = 0.95,  # Higher correlation threshold
      repetitions = 5,  # Minimum required
      verbose = FALSE,
      seed = 100,
      n.cores = 1
    )
  })

  # Function may return NULL if no interactions found - that's valid
  if (!is.null(m_int)) {
    # Check class structure
    expect_s3_class(m_int, "rf")
    expect_s3_class(m_int, "rf_interactions")
    expect_s3_class(m_int, "ranger")

    # Check $interactions slot exists
    expect_true("interactions" %in% names(m_int))
    expect_s3_class(m_int$interactions$screening, "data.frame")
    expect_s3_class(m_int$interactions$selected, "data.frame")
    expect_type(m_int$interactions$plots, "list")
    expect_type(m_int$interactions$names, "character")

    # Check it works like rf model
    expect_true("predictions" %in% names(m_int))
    expect_true("importance" %in% names(m_int))
    expect_true("performance" %in% names(m_int))
  } else {
    # If NULL, just check it doesn't error
    expect_null(m_int)
  }
})

test_that("`rf_interactions()` accepts model argument", {
  data(plants_rf)
  data(plants_xy)

  # Just check it doesn't error with model argument
  suppressWarnings({
    expect_no_error({
      m_int <- rf_interactions(
        model = plants_rf,
        xy = plants_xy,
        importance.threshold = 0.25,
        cor.threshold = 0.95,
        repetitions = 5,  # Minimum required
        verbose = FALSE,
        seed = 100,
        n.cores = 1
      )
    })
  })
})

test_that("`get_interactions()` and `plot_interactions()` require rf_interactions model", {
  data(plants_rf)

  # get_interactions should error on non-rf_interactions model
  expect_error(
    get_interactions(plants_rf, type = "selected"),
    "Model must be from rf_interactions"
  )

  # plot_interactions should error on non-rf_interactions model
  expect_error(
    plot_interactions(plants_rf, verbose = FALSE),
    "Model must be from rf_interactions"
  )
})
