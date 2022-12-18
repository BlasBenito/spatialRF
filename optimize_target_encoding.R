#optimize target encoding
library(spatialRF)
library(arrow)
library(tictoc)
library(magrittr)
library(foreach)
library(profvis)
library(microbenchmark)

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

character.variable <- character.variables[1]

data <- df

# target_encoding_mean ----------------------------------------------------
#original time: 17 seconds
#optimized: 1.2 seconds

# target_encoding_rnorm ----------------------------------------------------
#original time: 2 seconds
#optimized: 0.5 seconds

# target_encoding_rank ----------------------------------------------------
#original time: 8 seconds
#optimized: 1.3 seconds

# target_encoding_loo ----------------------------------------------------
#original WAY TOO LONG
#new: 0.58 sec
tic()
x <- target_encoding_loo(
  data,
  dependent.variable.name,
  predictor.variable.names = predictor.variable.names
)
toc()

#optimize loo per group
########################

# We can calculate the LOO mean directly: sum all the response values, subtract the current row response value, and divide by the number of rows minus 1.
#
# foo[, loow := (sum(w) - w) / (.N - 1), by = .(f, t)]

character.variable <- "ecoregion_biome"

x <- data %>%
  dplyr::group_by_at(character.variable) %>%
  dplyr::mutate(
    !!character.variable := (sum(get(dependent.variable.name), na.rm = TRUE) - get(dependent.variable.name)) / (dplyr::n() - 1)
  )

,
loo = (sum(get(dependent.variable.name), na.rm = TRUE) - {{character.variable}}) / (dplyr::n() - 1)








#optimizing for one variable
profvis::profvis({

  #new values vector
  new.values <- rep(NA, nrow(data))

  #iterate over groups to encode variable
  for(group.i in unique(data[[character.variable]]
  )){

    #get group indices
    group.i.indices <- which(data[[character.variable]] == group.i)

    #iterate over group samples
    for(sample.i in group.i.indices){

      new.values[sample.i] <- mean(
        data[[dependent.variable.name]][group.i.indices[group.i.indices != sample.i]]
      )

    } #end of iterations over group samples
  } #end of iterations over groups

  #as numeric
  data[[character.variable]] <- new.values
})



rm(x)



microbenchmark(
  dplyr = dplyr::pull(
    data,
    predictor.variable.name
  ),
  base = data[[predictor.variable.name]]
)


tic()
x <- dplyr::pull(
  data,
  predictor.variable.name
)
toc()

tic()
x <- data[[predictor.variable.name]]
toc()









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
