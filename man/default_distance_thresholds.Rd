% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default_distance_thresholds.R
\name{default_distance_thresholds}
\alias{default_distance_thresholds}
\title{Default distance thresholds from distance matrix}
\usage{
default_distance_thresholds(distance.matrix = NULL)
}
\arguments{
\item{distance.matrix}{Distance matrix. Default: \code{NULL}.}
}
\value{
A numeric vector with distance thresholds.
}
\description{
Generates four distance thresholds, from 0 to max(distance.matrix)/2.
}
\examples{
\dontrun{
if(interactive()){

 data(distance_matrix)
 default_distance_thresholds(distance_matrix)

 }
}
}
