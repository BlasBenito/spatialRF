% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_training_df_moran.R
\name{plot_training_df_moran}
\alias{plot_training_df_moran}
\title{Moran's I plots of a training data frame}
\usage{
plot_training_df_moran(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  fill.color = viridis::viridis(100, option = "F", direction = -1),
  point.color = "gray30"
)
}
\arguments{
\item{data}{Data frame with a response variable and a set of predictors. Default: \code{NULL}}

\item{dependent.variable.name}{Character string with the name of the response variable. Must be in the column names of \code{data}. If the dependent variable is binary with values 1 and 0, the argument \code{case.weights} of \code{ranger} is populated by the function \code{\link[=case_weights]{case_weights()}}. Default: \code{NULL}}

\item{predictor.variable.names}{Character vector with the names of the predictive variables. Every element of this vector must be in the column names of \code{data}. Optionally, the result of \code{\link[=auto_cor]{auto_cor()}} or \code{\link[=auto_vif]{auto_vif()}} Default: \code{NULL}}

\item{distance.matrix}{Squared matrix with the distances among the records in \code{data}. The number of rows of \code{distance.matrix} and \code{data} must be the same. If not provided, the computation of the Moran's I of the residuals is omitted. Default: \code{NULL}}

\item{distance.thresholds}{Numeric vector, distances below each value are set to 0 on separated copies of the distance matrix for the computation of Moran's I at different neighborhood distances. If \code{NULL}, it defaults to \code{seq(0, max(distance.matrix)/4, length.out = 2)}. Default: \code{NULL}}

\item{fill.color}{Character vector with hexadecimal codes (e.g. "#440154FF" "#21908CFF" "#FDE725FF"), or function generating a palette (e.g. \code{viridis::viridis(100)}). Default: \code{viridis::viridis(100, option = "F", direction = -1)}}

\item{point.color}{Character vector with a color name (e.g. "red4"). Default: \code{gray30}}
}
\value{
A ggplot2 object.
}
\description{
Plots the the Moran's I test of the response and the predictors in a training data frame.
}
\examples{
if(interactive()){

   #load example data
   data(plant_richness_df)
   data(distance_matrix)

   #plot Moran's I of training data
   plot_moran_training_data(
     data = plant_richness_df,
     dependent.variable.name = "richness_species_vascular",
     predictor.variable.names = colnames(plant_richness_df)[5:21],
     distance.matrix = distance_matrix,
     distance.thresholds = c(
       0,
       2000,
       4000,
       6000,
       8000
       )
     )
}
}
