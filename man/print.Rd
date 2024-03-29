% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.rf}
\alias{print.rf}
\title{Custom print method for random forest models}
\usage{
\method{print}{rf}(x, ...)
}
\arguments{
\item{x}{A model fitted with \code{\link[=rf]{rf()}}, \code{\link[=rf_repeat]{rf_repeat()}}, or \code{\link[=rf_spatial]{rf_spatial()}}.}

\item{...}{Additional arguments for print methods.}
}
\value{
Prints model details to the console.
}
\description{
Custom print method for models fitted with \code{\link[=rf]{rf()}}, \code{\link[=rf_repeat]{rf_repeat()}}, and \code{\link[=rf_spatial]{rf_spatial()}}.
}
\examples{
if(interactive()){

 #loading example data
 data("plant_richness_df")
 data("distance_matrix")

 #fitting random forest model
 rf.model <- rf(
   data = plant_richness_df,
   dependent.variable.name = "richness_species_vascular",
   predictor.variable.names = colnames(plant_richness_df)[5:21],
   distance.matrix = distance_matrix,
   distance.thresholds = 0,
   n.cores = 1
 )

 #printing model summary
 print(rf.model)
}
}
\seealso{
\code{\link[=print_evaluation]{print_evaluation()}}, \code{\link[=print_importance]{print_importance()}}, \code{\link[=print_moran]{print_moran()}}, \code{\link[=print_performance]{print_performance()}}
}
