#' MF_streams: Middle Fork streams
#'
#' \code{MF_streams} is an \code{sf} object with LINESTRING geometry representing a subset of
#' streams and rivers in the Middle Fork Basin, Idaho, USA. 
#'
#' The \code{sf} data.frame contains a set of 163 features and 9 columns: 
#'   \itemize{
#'     \item COMID: Common identifier of an NHD feature or relationship
#'     \item GNIS_NAME: Feature name as found in the Geographic Names Information System
#'     \item REACHCODE: Unique identifier for a reach. The first 8 digits contain the identfier for the HUC8 and the last 6 digits are a unique within-HUC8 identifier for the reach
#'     \item FTYPE: three-digit integer used to classify hydrography features in the NHD and define subtypes
#'     \item FCODE: Numeric code that contains the feature type and its attributes as found in the NHDFCode lookup table
#'     \item AREAWTMAP: Area weighted mean annual precipitation (mm) at the lowermost location on the edge
#'     \item SLOPE: Slope of the edge (cm/cm)
#'     \item rcaAreaKm2: Reach contributing area (km2), which is the land area draining directly into each line segment.
#'     \item h2oAreaKm2: Watershed area (km2) for the lowermost location (downstream end node) on the line segment
#' }
#'
#'@source \code{MF_streams} are a modified version of the United States
#'   National Hydrography Dataset
#'   (http://nhd.usgs.gov/). 
#'
#' @docType data
#'
#' @name MF_streams
#'
"MF_streams"
#'
#' MF_obs: Water temperature observations in the Middle Fork Basin, Idaho in 2004.
#'
#' \code{MF_obs} is an \code{sf} object with POINT geometry representing water temperature observations and covariates in the Middle Fork Basin, Idaho, USA collected in 2004. 
#'
#' The \code{sf} data.frame contains 45 point features and 16 columns:
#'   \itemize{
#'     \item STREAMNAME: Stream name
#'     \item COMID: Common identifier of an NHD feature or relationship
#'     \item AREAWTMAP: Area weighted mean annual precipitation (mm) at lowermost
#'       location on the line segment where the site resides
#'     \item SLOPE: Slope of the line segment (cm/cm) where the site resides
#'     \item ELEV_DEM: Elevation at the site based on a 30m DEM
#'     \item Source: Source of the data - relates to the ID field of the source table
#'     \item Summer_mn: Overall summer mean termperature (C) of the deployment
#'     \item MaxOver20: Binary variable: 1 represents the maximum summer temperature
#'       was greater than 20C and 0 indicates that it was less than 20C
#'     \item C16: Number of times daily stream temperature exceeded 16C
#'     \item C20: Number of times daily stream temperature exceeded 20C
#'     \item C24: Number of times daily stream temperature exceeded 24C
#'     \item FlowCMS: Average stream flow (cubic meters per sec) for August,
#'       by year, from 1950-2010 across 9 USGS gauges in the region
#'     \item AirMEANc: Average mean air temperature (C) from July 15 - August 31,
#'       from 1980-2009 across 10 COOP air stations within the domain
#'     \item AirMWMTc: Average maximum air temperature (C) from July 15 - August 31,
#'       from 1980-2009 across 10 COOP air stations within the domain.
#'       MWMT = maximum 7-day moving average of the maximum daily temperature
#'       (i.e. maximum of all the 7-day maximums)
#'     \item rcaAreaKm2: Reach contributing area (km2) for the downstream node of the edge feature the site resides on. RCA area is the land area draining directly into each line segment.
#'     \item h2oAreaKm2: Watershed area (km2) for the downstream node of the edge feature the site resides on
#' }
#'
#'@source \code{MF_obs} are unpublished United States Forest Service data. 
#'
#' @docType data
#'
#' @name MF_obs
"MF_obs"
#'
#'
#' MF_CapeHorn: Prediction locations on Cape Horn Creek, in the Middle Fork Basin, Idaho.
#'
#' \code{MF_CapeHorn} is an \code{sf} object with POINT geometry representing prediction locations and covariates on Cape Horn Creek, Middle Fork Basin, Idaho, USA. 
#'
#' The \code{sf} data.frame contains 654 point features and 9 columns:
#'   \itemize{
#'     \item COMID: Common identifier of an NHD feature or relationship
#'     \item AREAWTMAP: Area weighted mean annual precipitation (mm) at lowermost
#'       location on the line segment where the site resides
#'     \item SLOPE: Slope of the line segment (cm/cm) where the site resides
#'     \item ELEV_DEM: Elevation at the site based on a 30m DEM
#'     \item Source: Source of the data - relates to the ID field of the source table
#'     \item FlowCMS: Average stream flow (cubic meters per sec) for August,
#'       by year, from 1950-2010 across 9 USGS gauges in the region
#'     \item AirMEANc: Average mean air temperature (C) from July 15 - August 31,
#'       from 1980-2009 across 10 COOP air stations within the domain
#'     \item AirMWMTc: Average maximum air temperature (C) from July 15 - August 31,
#'       from 1980-2009 across 10 COOP air stations within the domain.
#'       MWMT = maximum 7-day moving average of the maximum daily temperature
#'       (i.e. maximum of all the 7-day maximums)
#'     \item rcaAreaKm2: Reach contributing area (km2) for the downstream node of the edge feature the site resides on. RCA area is the land area draining directly into each line segment.
#'     \item h2oAreaKm2: Watershed area (km2) for the downstream node of the edge feature the site resides on
#' }
#'
#'@source \code{MF_CapeHorn} are unpublished United States Forest Service data. 
#'
#' @docType data
#'
#' @name MF_CapeHorn
"MF_CapeHorn"
#'
#' MF_pred1km: Prediction locations at 1km intervals throughout the Middle Fork Basin, Idaho.
#'
#' \code{MF_pred1km} is an \code{sf} object with POINT geometry representing prediction locations and covariates distributed at 1km intervals throughout the Middle Fork Basin, Idaho, USA. 
#'
#' The \code{sf} data.frame contains 175 point features and 9 columns:
#'   \itemize{
#'     \item COMID: Common identifier of an NHD feature or relationship
#'     \item AREAWTMAP: Area weighted mean annual precipitation (mm) at lowermost
#'       location on the line segment where the site resides
#'     \item SLOPE: Slope of the line segment (cm/cm) where the site resides
#'     \item ELEV_DEM: Elevation at the site based on a 30m DEM
#'     \item Source: Source of the data - relates to the ID field of the source table
#'     \item FlowCMS: Average stream flow (cubic meters per sec) for August,
#'       by year, from 1950-2010 across 9 USGS gauges in the region
#'     \item AirMEANc: Average mean air temperature (C) from July 15 - August 31,
#'       from 1980-2009 across 10 COOP air stations within the domain
#'     \item AirMWMTc: Average maximum air temperature (C) from July 15 - August 31,
#'       from 1980-2009 across 10 COOP air stations within the domain.
#'       MWMT = maximum 7-day moving average of the maximum daily temperature
#'       (i.e. maximum of all the 7-day maximums)
#'     \item rcaAreaKm2: Reach contributing area (km2) for the downstream node of the edge feature the site resides on. RCA area is the land area draining directly into each line segment.
#'     \item h2oAreaKm2: Watershed area (km2) for the downstream node of the edge feature the site resides on
#' }
#'
#'@source \code{MF_pred1km} are unpublished United States Forest Service data. 
#'
#' @docType data
#'
#' @name MF_pred1km
"MF_pred1km"

#' MF_preds: A small set of prediction locations found in the Middle Fork Basin, Idaho.
#'
#' \code{MF_preds} is an \code{sf} object with POINT geometry representing prediction locations and covariates distributed at 1km intervals throughout the Middle Fork Basin, Idaho, USA. 
#'
#' The \code{sf} data.frame contains 43 point features and 9 columns:
#'   \itemize{
#'     \item COMID: Common identifier of an NHD feature or relationship
#'     \item AREAWTMAP: Area weighted mean annual precipitation (mm) at lowermost
#'       location on the line segment where the site resides
#'     \item SLOPE: Slope of the line segment (cm/cm) where the site resides
#'     \item ELEV_DEM: Elevation at the site based on a 30m DEM
#'     \item FlowCMS: Average stream flow (cubic meters per sec) for August,
#'       by year, from 1950-2010 across 9 USGS gauges in the region
#'     \item AirMEANc: Average mean air temperature (C) from July 15 - August 31,
#'       from 1980-2009 across 10 COOP air stations within the domain
#'     \item AirMWMTc: Average maximum air temperature (C) from July 15 - August 31,
#'       from 1980-2009 across 10 COOP air stations within the domain.
#'       MWMT = maximum 7-day moving average of the maximum daily temperature
#'       (i.e. maximum of all the 7-day maximums)
#'     \item rcaAreaKm2: Reach contributing area (km2) for the downstream node of the edge feature the site resides on. RCA area is the land area draining directly into each line segment.
#'     \item h2oAreaKm2: Watershed area (km2) for the downstream node of the edge feature the site resides on
#' }
#'
#'@source \code{MF_preds} are unpublished United States Forest Service data. 
#'
#' @docType data
#'
#' @name MF_preds
"MF_preds"
