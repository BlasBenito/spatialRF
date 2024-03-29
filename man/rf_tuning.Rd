% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rf_tuning.R
\name{rf_tuning}
\alias{rf_tuning}
\title{Tuning of random forest hyperparameters via spatial cross-validation}
\usage{
rf_tuning(
  model = NULL,
  num.trees = NULL,
  mtry = NULL,
  min.node.size = NULL,
  xy = NULL,
  repetitions = 30,
  training.fraction = 0.75,
  seed = 1,
  verbose = TRUE,
  n.cores = parallel::detectCores() - 1,
  cluster = NULL
)
}
\arguments{
\item{model}{A model fitted with \code{\link[=rf]{rf()}}. If provided, the training data is taken directly from the model definition (stored in \code{model$ranger.arguments}). Default: \code{NULL}}

\item{num.trees}{Numeric integer vector with the number of trees to fit on each model repetition. Default: \code{c(500, 1000, 2000)}.}

\item{mtry}{Numeric integer vector, number of predictors to randomly select from the complete pool of predictors on each tree split. Default: \code{floor(seq(1, length(predictor.variable.names), length.out = 4))}}

\item{min.node.size}{Numeric integer, minimal number of cases in a terminal node. Default: \code{c(5, 10, 20, 40)}}

\item{xy}{Data frame or matrix with two columns containing coordinates and named "x" and "y". If \code{NULL}, the function will throw an error. Default: \code{NULL}}

\item{repetitions}{Integer, number of independent spatial folds to use during the cross-validation. Default: \code{30}.}

\item{training.fraction}{Proportion between 0.2 and 0.9 indicating the number of records to be used in model training. Default: \code{0.75}}

\item{seed}{Integer, random seed to facilitate reproduciblity. If set to a given number, the results of the function are always the same. Default: \code{1}.}

\item{verbose}{Logical. If TRUE, messages and plots generated during the execution of the function are displayed, Default: \code{TRUE}}

\item{n.cores}{Integer, number of cores to use for parallel execution. Creates a socket cluster with \code{parallel::makeCluster()}, runs operations in parallel with \code{foreach} and \verb{\%dopar\%}, and stops the cluster with \code{parallel::clusterStop()} when the job is done. Default: \code{parallel::detectCores() - 1}}

\item{cluster}{A cluster definition generated with \code{parallel::makeCluster()}. If provided, overrides \code{n.cores}. When \code{cluster = NULL} (default value), and \code{model} is provided, the cluster in \code{model}, if any, is used instead. If this cluster is \code{NULL}, then the function uses \code{n.cores} instead. The function does not stop a provided cluster, so it should be stopped with \code{parallel::stopCluster()} afterwards. The cluster definition is stored in the output list under the name "cluster" so it can be passed to other functions via the \code{model} argument, or using the \verb{\%>\%} pipe. Default: \code{NULL}}
}
\value{
A model with a new slot named \code{tuning}, with a data frame with the results of the tuning analysis.
}
\description{
Finds the optimal set of random forest hyperparameters \code{num.trees}, \code{mtry}, and \code{min.node.size} via grid search by maximizing the model's R squared, or AUC, if the response variable is binomial, via spatial cross-validation performed with \code{\link[=rf_evaluate]{rf_evaluate()}}.
}
\examples{
if(interactive()){

#loading example data
data(plant_richness_df)
data(distance_matrix)

#fitting model to tune
out <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = 0,
  n.cores = 1
)

#model tuning
tuning <- rf_tuning(
  model = out,
  num.trees = c(100, 500),
  mtry = c(2, 8),
  min.node.size = c(5, 10),
  xy = plant_richness_df[, c("x", "y")],
  n.cores = 1
)

}
}
\seealso{
\code{\link[=rf_evaluate]{rf_evaluate()}}
}
