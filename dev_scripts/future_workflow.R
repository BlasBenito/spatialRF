library(spatialRF)
library(future)
library(progressr)
library(collinear)
library(ggplot2)
library(patchwork)
library(tictoc)

options(future.globals.maxSize = 3 * 1024^3) # 3 GB

data(vi, vi_predictors_numeric)

vi_xy <- vi[, c("longitude", "latitude")]
colnames(vi_xy) <- c("x", "y")

future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

progressr::handlers(global = TRUE)


m <- rf(
  data = vi,
  dependent.variable.name = "vi_numeric",
  predictor.variable.names = vi_predictors_numeric,
  n.cores = future::availableCores() - 1,
  verbose = FALSE
)

tic()
m_eval_1 <- rf_evaluate(
  model = m,
  xy = vi_xy,
  repetitions = 50,
  training.fraction = 0.5,
  metrics = c("r.squared", "rmse"),
  verbose = FALSE,
  distance.step = 1
)
toc()

future::plan(
  strategy = future::sequential
)

tic()
m_eval_2 <- rf_evaluate(
  model = m,
  xy = vi_xy,
  repetitions = 50,
  training.fraction = 0.5,
  metrics = c("r.squared", "rmse"),
  verbose = FALSE,
  distance.step = 1,
  n.cores = future::availableCores() - 1
)
toc()


m <- rf_repeat(
  model = m,
  xy = vi_xy,
  repetitions = 100,
  n.cores = 1
)

future::plan(
  strategy = future::sequential
)
