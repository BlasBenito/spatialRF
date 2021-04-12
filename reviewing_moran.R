#PREPARING FUNCTION ARGUMENTS
############################
library(spatialRF)

#input data
data(plant_richness_df)
data(distance_matrix)
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
distance.thresholds <- c(0, 1500, 3000)
distance.matrix <- distance_matrix

#model
m.rf <- spatialRF::rf(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance.thresholds,
  verbose = FALSE
)

moran(
  x = m.rf$residuals,
  distance.matrix = distance.matrix,
  distance.threshold = 0
)

df <- moran_multithreshold(
  x = m.rf$residuals,
  distance.matrix = distance.matrix,
  distance.thresholds = c(0, 1500, 3000)
)

plot_moran(m.rf)

#FUNCTION ARGUMENTS
############################
x = m.rf$residuals
distance.matrix = distance.matrix
distance.threshold = 0

#smaller
# x <- x[1:5]
# distance.matrix <- distance.matrix[1:5, 1:5]
# distance.threshold = 20

#FUNCTION moran() STARTS HERE
#############################

#check x and distance matrix
if(is.null(x) | !is.vector(x)){
  stop("x must be a numeric vector.")
}

#extracting weights from distance matrix
x.distance.weights <- weights_from_distance_matrix(
  x = distance.matrix,
  distance.threshold = distance.threshold
)

#length of the input vector
x.length <- length(x)

#computing expected Moran I
moran.i.expected <- -1/(x.length - 1)

#computing observed Moran I
############################

#sum of weights
x.distance.weights.sum <- sum(x.distance.weights)

#centering x
x.mean <- mean(x)
x.centered <- x - x.mean

#upper term of the Moran's I equation
#sum of cross-products of the lags
#x.centered %o% x.centered equals (xi - x.mean) * (xj - x.mean)
cross.product.sum <- sum(x.distance.weights * x.centered %o% x.centered)

#lower term of the Moran's I equation
#variance of the centered x
x.centered.variance <- sum(x.centered^2)

#observed Moran's I
moran.i.observed <- (x.length / x.distance.weights.sum) * (cross.product.sum/x.centered.variance)

#components of the expected standard deviation
s1 <- 0.5 * sum((x.distance.weights + t(x.distance.weights))^2)

s2 <- sum((apply(x.distance.weights, 1, sum) + apply(x.distance.weights, 2, sum))^2)

s3 <- x.distance.weights.sum^2

s4 <- (sum(x.centered^4) / x.length) /
     (x.centered.variance / x.length)^2

expected.standard.deviation <- sqrt(
  (x.length *
  ((x.length^2 - 3 * x.length + 3) * s1 - x.length * s2 + 3 * s3) -
  s4 *
  (x.length * (x.length - 1) * s1 - 2 * x.length*s2 + 6 * s3)) /
  ((x.length - 1) * (x.length - 2) * (x.length - 3) * s3) - 1 /
  ((x.length - 1)^2)
  )

#p.value
p.value <- pnorm(
  moran.i.observed,
  mean = moran.i.expected,
  sd = expected.standard.deviation
  )
p.value <- ifelse(
  moran.i.observed <= moran.i.expected,
  2 * p.value,
  2 * (1 - p.value)
  )

#adding interpretation
if(moran.i.observed > moran.i.expected & p.value <= 0.05){
  interpretation <- "Positive spatial correlation"
}
if(moran.i.observed < moran.i.expected & p.value <= 0.05){
  interpretation <- "Negative spatial correlation"
}
if(p.value > 0.05){
  interpretation <- "No spatial correlation"
}

#preparing output
out <- data.frame(
  moran.i = moran.i.observed,
  moran.i.null = moran.i.expected,
  p.value = p.value,
  interpretation = interpretation
)

out
