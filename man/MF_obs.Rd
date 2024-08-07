% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{MF_obs}
\alias{MF_obs}
\title{MF_obs: Water temperature observations in the Middle Fork Basin, Idaho in 2004.}
\format{
An object of class \code{sf} (inherits from \code{data.frame}) with 45 rows and 17 columns.
}
\source{
\code{MF_obs} are unpublished United States Forest Service data.
}
\usage{
MF_obs
}
\description{
\code{MF_obs} is an \code{sf} object with POINT geometry representing water temperature observations and covariates in the Middle Fork Basin, Idaho, USA collected in 2004.
}
\details{
The \code{sf} data.frame contains 45 point features and 16 columns:
\itemize{
\item STREAMNAME: Stream name
\item COMID: Common identifier of an NHD feature or relationship
\item AREAWTMAP: Area weighted mean annual precipitation (mm) at lowermost
location on the line segment where the site resides
\item SLOPE: Slope of the line segment (cm/cm) where the site resides
\item ELEV_DEM: Elevation at the site based on a 30m DEM
\item Source: Source of the data - relates to the ID field of the source table
\item Summer_mn: Overall summer mean termperature (C) of the deployment
\item MaxOver20: Binary variable: 1 represents the maximum summer temperature
was greater than 20C and 0 indicates that it was less than 20C
\item C16: Number of times daily stream temperature exceeded 16C
\item C20: Number of times daily stream temperature exceeded 20C
\item C24: Number of times daily stream temperature exceeded 24C
\item FlowCMS: Average stream flow (cubic meters per sec) for August,
by year, from 1950-2010 across 9 USGS gauges in the region
\item AirMEANc: Average mean air temperature (C) from July 15 - August 31,
from 1980-2009 across 10 COOP air stations within the domain
\item AirMWMTc: Average maximum air temperature (C) from July 15 - August 31,
from 1980-2009 across 10 COOP air stations within the domain.
MWMT = maximum 7-day moving average of the maximum daily temperature
(i.e. maximum of all the 7-day maximums)
\item rcaAreaKm2: Reach contributing area (km2) for the downstream node of the edge feature the site resides on. RCA area is the land area draining directly into each line segment.
\item h2oAreaKm2: Watershed area (km2) for the downstream node of the edge feature the site resides on
}
}
\keyword{datasets}
