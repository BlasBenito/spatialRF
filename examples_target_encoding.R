#loading example data
data(
  ecoregions_df,
  ecoregions_continuous_response
  )

#selecting response and two character/factor variables
df <- ecoregions_df %>%
  dplyr::select(
    !!ecoregions_continuous_response,
    dominant_landcover
  )

#levels of the variable
unique(df$dominant_landcover)

#first few lines of df
head(df)


#method = "mean"
#--------------------------------------------------------
x.mean <- target_encoding(
  data = df,
  dependent.variable.name = ecoregions_continuous_response,
  #you can pass all your numeric and character predictors to this function
  #here I only pass it the character ones to simplify the example
  predictor.variable.names = "dominant_landcover",
  method = "mean"
  )

#leakage test produced by the function
#use `verbose = FALSE` to disable
# Encoding the variables:
#   primary_productivity
# dominant_landcover
#
# Leakage test for method mean:
#
#   variable r_squared   interpretation
# 1 primary_productivity     0.485 Unlikely leakage
# 2   dominant_landcover     0.157       No leakage
#
# r_squared: correlation between the target-encoded variable and the response.

#checking the new version of the data
head(x.mean$data)

#plot spread
plot(x = sort(x.mean$data$dominant_landcover))

#checking the encoding map
lapply(x.mean$encoding_map, head)


#method = "mean" plus noise
#--------------------------------------------------------
x.mean.noise <- target_encoding(
  data = df,
  dependent.variable.name = ecoregions_continuous_response,
  predictor.variable.names = "dominant_landcover",
  method = "mean",
  noise = 0.25,
  seed = 1 #important to replicate results!
)

#checking the new version of the data
head(x.mean.noise$data)

#plot spread
plot(x = sort(x.mean.noise$data$dominant_landcover))

#checking the encoding map
lapply(x.mean.noise$encoding_map, head)


#method = "rank"
#--------------------------------------------------------
x.rank <- target_encoding(
  data = df,
  dependent.variable.name = ecoregions_continuous_response,
  predictor.variable.names = "dominant_landcover",
  method = "rank"
)

#checking the new version of the data
head(x.rank$data)

#plot spread
plot(x = sort(x.rank$data$dominant_landcover))

#checking the encoding map
lapply(x.rank$encoding_map, head)


#method = "rank" plus noise
#--------------------------------------------------------
x.rank.noise <- target_encoding(
  data = df,
  dependent.variable.name = ecoregions_continuous_response,
  predictor.variable.names = "dominant_landcover",
  method = "rank",
  noise = 0.25,
  seed = 1
)

#checking the new version of the data
head(x.rank.noise$data)

#plot spread
plot(x = sort(x.rank.noise$data$dominant_landcover))

#checking the encoding map
lapply(x.rank.noise$encoding_map, head)


#method = "loo" (leave-one-out)
#--------------------------------------------------------
x.loo <- target_encoding(
  data = df,
  dependent.variable.name = ecoregions_continuous_response,
  predictor.variable.names = "dominant_landcover",
  method = "loo"
)

#checking the new version of the data
head(x.loo$data)

#plot spread
plot(x = sort(x.loo$data$dominant_landcover))

#checking the encoding map
lapply(x.loo$encoding_map, head)


#method = "rnorm"
#--------------------------------------------------------
x.rnorm <- target_encoding(
  data = dplyr::arrange(df, dominant_landcover),
  dependent.variable.name = ecoregions_continuous_response,
  predictor.variable.names = "dominant_landcover",
  sd.width = 0.001, #experiment with this number and check the spread plot
  method = "rnorm"
)

#checking the new version of the data
head(x.rnorm$data)

#plot spread
plot(x = x.rnorm$data$dominant_landcover)

#checking the encoding map
lapply(x.rnorm$encoding_map, head)
