% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_t_beam_I_centroid.R
\name{calculate_t_beam_I_centroid}
\alias{calculate_t_beam_I_centroid}
\title{Areal moment of inertia}
\usage{
calculate_t_beam_I_centroid(.flange, .flange_thickness, .web, .web_thickness)
}
\arguments{
\item{.flange}{Dimension of the flange \link{mm}}

\item{.flange_thickness}{Dimension of the flange thickness \link{mm}}

\item{.web}{Dimension of the web \link{mm}}

\item{.web_thickness}{Dimension of the web thickness \link{mm}}
}
\value{
data frame with the values of the areal oment of inertio along the x and y axes
}
\description{
Calculate the areal moment of inertia of a T-beam along its
centroid. The flange is centered on the y-axis.
Area moment of inertia (I in mm4)
}
\examples{
calculate_t_beam_I_centroid(.flange = 38.1,
                            .flange_thickness = 6.4,
                            .web = 38.1,
                            .web_thickness = 6.4)
}
