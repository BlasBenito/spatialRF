library(ggplot2)
x <- spatial.predictors.selection$optimization

ggplot(data = x) +
  aes(x = moran.i, y = r.squared, color = optimization.smooth, size = spatial.predictor.index) +
  geom_point() +
  geom_point(data=x[x$spatial.predictor.index == 28, ], aes(x = moran.i, y = r.squared), colour="red", size=5)

#CODE OF select_spatial_predictors_sequential
################################################################
optimization.df <- spatial.predictors.selection$optimization
optimization.df <- dplyr::arrange(optimization.df, spatial.predictor.index)
optimization.df$penalization.per.variable <- NULL
optimization.df$optimization <- NULL
optimization.df$optimization.smooth <- NULL

#1. IF there is a p.value higher than 0.05 get spatial predictors unitl that p.value is reachec
#get position of the first p.value higher than 0.05
good.p.values <- which(optimization.df$p.value >= 0.05)
if(!is.na(good.p.values)){

  #get value of the optimized index
  optimized.index <- good.p.values[1]

  #otherwise, optimize moran vs r.squared
} else {



}

#penalization by number of variables
optimization.df$penalization.per.variable <- (1/nrow(optimization.df)) * optimization.df$spatial.predictor.index

#binary p.values
optimization.df$p.value.binary <- ifelse(optimization.df$p.value >= 0.05, 1, 0)

#computing weighted optimization
optimization.df$optimization <- pmax(rescale_vector(1 - optimization.df$moran.i), optimization.df$p.value.binary) + (0.75 * rescale_vector(optimization.df$r.squared)) - (0.25 * rescale_vector(optimization.df$penalization.per.variable))


#reordering data by optimization
optimization.df <- dplyr::arrange(optimization.df, spatial.predictor.index)
optimization.df <- dplyr::arrange(optimization.df, dplyr::desc(optimization))
head(optimization.df)

ggplot(data = optimization.df) +
  aes(x = moran.i, y = r.squared, color = optimization, size = spatial.predictor.index) +
  geom_point() +
  geom_point(data=optimization.df[which.max(optimization.df$optimization), ], aes(x = moran.i, y = r.squared), colour="red", size=5) +
  scale_color_viridis_c() +
  ggtitle("Moran.s I vs R-squared")

#p.value vs moran.i
ggplot(data = optimization.df) +
  aes(x = moran.i, y = p.value, color = optimization, size = spatial.predictor.index) +
  geom_point() +
  geom_point(data=optimization.df[which.max(optimization.df$optimization), ], aes(x = moran.i, y = p.value), colour="red", size=5) +
  ggtitle("Moran's I vs. p-value")

ggplot(data = optimization.df) +
  aes(x = optimization, y = moran.i, color = spatial.predictor.index) +
  geom_point() +
  geom_point(data=optimization.df[which.max(optimization.df$optimization), ], aes(x = optimization, y = moran.i), colour="red", size=5)

ggplot(data = optimization.df) +
  aes(x = optimization, y = r.squared, color = spatial.predictor.index) +
  geom_point() +
  geom_point(data=optimization.df[which.max(optimization.df$optimization), ], aes(x = optimization, y = r.squared), colour="red", size=5)


ggplot(data = optimization.df) +
  aes(x = spatial.predictor.index, y = moran.i, color = optimization) +
  geom_point() +
  geom_point(data=optimization.df[which.max(optimization.df$optimization), ], aes(x = spatial.predictor.index, y = moran.i), colour="red", size=5)


ggplot(data = optimization.df) +
  aes(x = spatial.predictor.index, y = r.squared, color = optimization) +
  geom_point() +
  geom_point(data=optimization.df[which.max(optimization.df$optimization), ], aes(x = spatial.predictor.index, y = r.squared), colour="red", size=5)
