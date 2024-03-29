% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_residuals.R
\name{get_residuals}
\alias{get_residuals}
\title{Gets model residuals}
\usage{
get_residuals(model)
}
\arguments{
\item{model}{A model fitted with \code{\link[=rf]{rf()}}, \code{\link[=rf_repeat]{rf_repeat()}}, or \code{\link[=rf_spatial]{rf_spatial()}}.}
}
\value{
A vector with model residuals, or the median of model residuals across repetitions if the model was fitted with \code{\link[=rf_repeat]{rf_repeat()}}.
}
\description{
Returns the residuals of models fitted with \code{\link[=rf]{rf()}}, \code{\link[=rf_repeat]{rf_repeat()}}, or \code{\link[=rf_spatial]{rf_spatial()}}.
}
\examples{
if(interactive()){

#load example data
data(plant_richness_df)

#fit random forest model
rf.model <- rf(
  data = plant_richness_df,
  dependent.variable.name = "richness_species_vascular",
  predictor.variable.names = colnames(plant_richness_df)[5:21],
  n.cores = 1,
  verbose = FALSE
)

#getting vector with residuals
x <- get_residuals(rf.model)
x

}
}
