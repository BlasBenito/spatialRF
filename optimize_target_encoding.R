#optimize target encoding
library(spatialRF)
library(arrow)
library(tictoc)
library(magrittr)
library(foreach)

df <- arrow::read_parquet(file = "example_data/ecoregions_betadiversity_df.parquet")

load("example_data/ecoregions_betadiversity_variables.RData")

dependent.variable.name <- ecoregions_betadiversity_responses[1]

predictor.variable.names <- ecoregions_betadiversity_predictors

rm(ecoregions_betadiversity_responses, ecoregions_betadiversity_predictors)

noise <- 0
seed <- 1

character.variables <- predictor.variable.names[unlist(
  lapply(
    X = df[, predictor.variable.names, drop = FALSE],
    FUN = is.character
  )
)]

data <- df

# target_encoding_mean ----------------------------------------------------
#original time: 17 seconds
#optimized: 1.2 seconds

# target_encoding_rnorm ----------------------------------------------------
#original time: 2 seconds
#optimized: 0.5 seconds

tic()
x <- target_encoding_rnorm(
  data,
  dependent.variable.name,
  predictor.variable.names,
  seed
)
toc()
rm(x)











#optimizing with cluster
cluster <- spatialRF::start_cluster()

tic()

#HANDLING PARALLELIZATION
##########################
if("cluster" %in% class(cluster)){

  #registering cluster
  doParallel::registerDoParallel(cl = cluster)

  #parallel iterator
  `%iterator%` <- foreach::`%dopar%`

} else {

  #sequential iterator
  `%iterator%` <- foreach::`%do%`

}

data.subset <- data[, c(dependent.variable.name, character.variables)]

parallel::clusterExport(
  cl = cluster,
  varlist = c(
    "target_encoding_noise",
    "data.subset",
    "noise",
    "seed",
    "%>%"
  )
)

df.encoded <- foreach::foreach(
  character.variable = character.variables,
  .verbose = FALSE,
  .combine = "cbind"
) %iterator% {

  df.map <- tapply(
    X = data.subset[[dependent.variable.name]],
    INDEX = data.subset[[character.variable]],
    FUN = mean,
    na.rm = TRUE
  )
  df.map <- data.frame(names(df.map), df.map)
  names(df.map) <- c(character.variable, "target_encoding")

  #merge
  data.subset <- dplyr::inner_join(
    x = data.subset,
    y = df.map,
    by = character.variable
  ) %>%
    dplyr::select(
      -!!character.variable
    ) %>%
    dplyr::rename(
      !!character.variable := target_encoding
    )

  #add noise if any
  data.subset <- spatialRF::target_encoding_noise(
    data = data.subset,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = character.variable,
    noise = noise,
    seed = seed
  )

  return(data.subset[character.variable])

}

#replace encoded variables
data[, character.variables] <- df.encoded[, character.variables]

data

toc()



#iterating over character variables
for(character.variable in character.variables){

  df.map <- tapply(
    X = df[[dependent.variable.name]],
    INDEX = df[[character.variable]],
    FUN = mean,
    na.rm = TRUE
    )
  df.map <- data.frame(names(df.map), df.map)
  names(df.map) <- c(character.variable, "target_encoding")

  #merge
  data <- dplyr::inner_join(
    x = data,
    y = df.map,
    by = character.variable
  ) %>%
    dplyr::select(
      -!!character.variable
    ) %>%
    dplyr::rename(
      !!character.variable := target_encoding
    )

  #add noise if any
  data <- target_encoding_noise(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = character.variable,
    noise = noise,
    seed = seed
  )

}#end of loop over character variables

data

toc()
