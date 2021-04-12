library(spatialRF)
library(tmap)
library(sp)
library(spdep)

#data preparation
data(plant_richness_df)
data(distance_matrix)
x <- plant_richness_df
dependent.variable.name <- "richness_species_vascular"
predictor.variable.names <- colnames(plant_richness_df)[5:21]
distance.thresholds <- c(0, 1500, 3000)
distance.matrix <- distance_matrix
random.seed <- 100

#LINEAR MODEL
formula <- as.formula(
  paste(
    dependent.variable.name,
    " ~ ",
    paste(predictor.variable.names, collapse = " + "))
)
m.lm <- lm(formula = formula, data = x)

#residuals using spdep
x.sp <- sp::SpatialPointsDataFrame(
  coords = plant_richness_df[, c("x", "y")],
  data = plant_richness_df,
  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
)

#neighbors
x.sp.neighbors <- knn2nb(knearneigh(x.sp))
x.sp.neighbors.matrix <- nb2mat(x.sp.neighbors, style = "B")
x.sp.neighbors.matrix.weights <- mat2listw(x.sp.neighbors.matrix, style = "W")

#lm.morantest
lm.morantest(
  model = m.lm,
  listw = x.sp.neighbors.matrix.weights,
  alternative = "two.sided"
)

#moran.test
moran.test(
  x = residuals(m.lm),
  listw = x.sp.neighbors.matrix.weights,
  alternative = "two.sided"
)

my.morantest <- moran_multithreshold(
  x = residuals(m.lm),
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds
)
my.morantest


#FUNCTION lm.morantest() below
model = m.lm
  listw = x.sp.neighbors.matrix.weights
  zero.policy = TRUE
  alternative = "greater" #"less", "two.sided"
  naSubset=TRUE

  # 101124 Aleksandr Andreev
  #number of observations
  x.length <- as.double(length(listw$neighbours))

  #model residuals
  x <- residuals(model)

  #makes list of weights simmetric
  listw.symmetric <- listw2U(listw)

  #sum of weights
  sum.x.distance.weights <- sum(unlist(listw.symmetric$weights))

  #squared sum of weights
  sum.weights.squared <- 0.5 * sum((2*unlist(listw.symmetric$weights))^2)

  #lags vector
  lags.vector <- lag.listw(listw.symmetric, x, zero.policy=zero.policy)

  #moran's I
  moran.i <- (x.length/sum.x.distance.weights) * ((t(x) %*% lags.vector) / (t(x) %*% x))

  #number of variables used in a model
  number.of.variables <- model$rank
  number.of.variables.seq <- seq(1, number.of.variables)

  #invert qr decomposition matrix
  qr.inverted <- chol2inv(model$qr$qr[number.of.variables.seq, number.of.variables.seq, drop = FALSE])


  predictors.matrix <- model.matrix(terms(model), model.frame(model))
  #can be replaced with as.matrix(data[, predictor.variable.names])

  #multiply values of the predictors matrix by the case weights. weights(model) can be substituted by ranger.arguments$case.weights
  if (!is.null(case.weights <- weights(model))) {
    predictors.matrix <- drop(t(sapply(1:length(case.weights),
                       function(i) sqrt(case.weights[i])*predictors.matrix[i,])))
  }

  #lags of the predictor variables
  lags.of.predictors <- lag.listw(listw.symmetric, predictors.matrix, zero.policy=zero.policy)


  C1 <- t(predictors.matrix) %*% lags.of.predictors
  trA <- (sum(diag(qr.inverted %*% C1)))
  EI <- -((x.length * trA) / ((x.length - number.of.variables) * sum.x.distance.weights))
  # minus changed from trA to EI (Luis Galvis, Dec 2, 2003)
  C2 <- t(lags.of.predictors) %*% lags.of.predictors
  C3 <- qr.inverted %*% C1
  trA2 <- sum(diag(C3 %*% C3))
  trB <- sum(diag(4*(qr.inverted %*% C2)))
  VI <- (((x.length*x.length)/((sum.x.distance.weights  * sum.x.distance.weights)*(x.length - number.of.variables)*(x.length-number.of.variables+2))) *
           (sum.weights.squared + 2*trA2 - trB - ((2*(trA^2))/(x.length - number.of.variables))))
  ZI <- (moran.i - EI) / sqrt(VI)
  if (alternative == "two.sided") pv <- 2 * pnorm(abs(ZI),
                                                  lower.tail=FALSE)
  else if (alternative == "greater")
    pv <- pnorm(ZI, lower.tail=FALSE)
  else pv <- pnorm(ZI)
  if (!is.finite(pv) || pv < 0 || pv > 1)
    warning("Out-of-range p-value: reconsider test arguments")
  statistic <- ZI
  attr(statistic, "names") <- "Moran I statistic standard deviate"
  p.value <- pv
  estimate <- c(moran.i, EI, VI)
  attr(estimate, "names") <- c("Observed Moran I", "Expectation",
                               "Variance")
  method <- "Global Moran I for regression residuals"
  data.name <- paste("\n", paste(strwrap(paste("model: ",
                                               gsub("[[:space:]]+", " ",
                                                    paste(deparse(model$call), sep="", collapse="")))), collapse="\n"),
                     "\nweights: ", listw_name, "\n", sep="")
  res <- list(statistic = statistic, p.value = p.value,
              estimate = estimate, method = method,
              alternative = alternative, data.name = data.name)
  class(res) <- "htest"
  res
