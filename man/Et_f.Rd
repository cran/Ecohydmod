% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Evapotranspiration.R
\name{Et_f}
\alias{Et_f}
\title{Evapotranspiration}
\usage{
Et_f(s, Emax, Ew, sh, sw, sstar)
}
\arguments{
\item{s}{Soil moisture}

\item{Emax}{Maximum evapotranspiration rate}

\item{Ew}{Minimum evapotranspiration rate}

\item{sh}{Soil moisture at hidroscopic point}

\item{sw}{Soil moisture at wilting point}

\item{sstar}{Soil moisture below field capacity point}
}
\value{
evapotranspiration
}
\description{
This function calculates the evapotranspiration based on the soil moisture, soil water retantion curve and vegetation properties.
}
\details{
Evapotranspiration function based on the soil moisture
}
\examples{
Et_f(s = 0.25, Emax = 5, Ew = 0.5, sh = 0.01, sw = 0.15, sstar = 0.40)
}
