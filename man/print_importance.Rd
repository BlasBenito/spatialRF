% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_importance.R
\name{print_importance}
\alias{print_importance}
\title{Prints variable importance}
\usage{
print_importance(
  model,
  verbose = TRUE
)
}
\arguments{
\item{model}{A model fitted with \link{rf}, \link{rf_repeat}, or \link{rf_spatial}.}

\item{verbose}{Logical, if \code{TRUE}, variable importance is returned. Default: \code{TRUE}}
}
\value{
A table printed to the standard output.
}
\description{
Prints variable importance scores from \link{rf}, \link{rf_repeat}, and \link{rf_spatial} models.
}
\examples{
if(interactive()){

#loading example data
data(plant_richness_df)
data(distance.matrix)

#fitting a random forest model
rf.model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  distance.matrix = distance_matrix,
  distance.thresholds = 0,
  n.cores = 1,
  verbose = FALSE
)

#printing variable importance scores
print_importance(model = rf.model)

}
}
\seealso{
\code{\link[=plot_importance]{plot_importance()}}, \code{\link[=get_importance]{get_importance()}}
}
