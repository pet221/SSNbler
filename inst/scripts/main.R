## Main script for developing and testing functions in SSNbler

setwd("c:/EPConsulting/projects/SpatialTools/SSNbler/work/SSNbler")

library(devtools)
library(SSNbler)
##library(sf)

## dat.path <- "c:/EPConsulting/projects/SpatialTools/SSNbler/data/RawMF04/"

## ---------------------------------------------------------
## lines_to_lsn: Create the landscape network
## ---------------------------------------------------------
## Import streams as sf objects
## streams <- sf::st_read("c:/temp/rawData/streams_w_attrs.shp")
## streams<- sf::st_read("c:/temp/topology/ARiver.shp")
## streams = st_read(paste0(dat.path, "MF_streams.shp"))

## Use MF_streams stored in package
names(MF_streams)

edges<- lines_to_lsn(
  streams = MF_streams,
  lsn_path = "c:/temp/mf04", 
  snap_tolerance = 1,
  check_topology = TRUE,
  topo_tolerance = 20,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)

## Notice rid has been added - can also check files in lsn_path if
## curious. help(lines_to_ssn) describes the files in the LSN and what
## they do
names(edges)

## --------------------------------------------------
## sites_to_lsn
## --------------------------------------------------
##obs = st_read("c:/temp/rawData/sites.shp")
##sites<-  sf::st_read("c:/temp/rawData/sites.shp")

names(MF_obs)

## One site is 63.654m away from the edges. The rest are close or
## exactly on an edge before snapping.
obs<- sites_to_lsn(in_sites = MF_obs,
                   edges = edges,
                   save_local = TRUE,
                   output = "c:/temp/mf04/obs.gpkg",
                   snap_tolerance = 100,
                   overwrite = TRUE,
                   verbose = TRUE)

## Notice three columns are added: rid for the edge the site resides
## on, ratio describes location on that edge, and snapdist is the
## distance the site was moved.
## names(obs)

names(MF_pred1km)

preds<- sites_to_lsn(in_sites = MF_pred1km,
                   edges = edges,
                   save_local = TRUE,
                   output = "c:/temp/mf04/preds.gpkg",
                   snap_tolerance = 1,
                   overwrite = TRUE,
                   verbose = TRUE)



## --------------------------------------------------
## updist_edges
## --------------------------------------------------

edges<- updist_edges(edges = edges,
                     lsn_path = "c:/temp/mf04",
                     ##length_col = "o_length",
                     calc_length = TRUE,
                     overwrite = TRUE,
                     verbose = TRUE)


##----------------------------------------------------
## updist_sites
##----------------------------------------------------

site.list<- updist_sites(sites = list(obs = obs,
                                pred1km = preds),
                         edges = edges,
                         length_col= "Length",
                         lsn_path = "c:/temp/mf04",
                         overwrite = TRUE)

names(site.list)

##----------------------------------------------------
## afv_edges
##----------------------------------------------------

edges<- afv_edges(edges=edges,
                  infl_col = "h2oAreaKm2", 
                  segpi_col = "areaPI",
                  lsn_path = "c:/temp/mf04",
                  afv_col = "afvArea",
                  overwrite = TRUE)



##----------------------------------------------------
## afv_sites
##----------------------------------------------------

site.list<- afv_sites(sites = site.list,
                      edges=edges,
                      afv_col = "afvArea",
                      save_local = TRUE,
                      lsn_path = "c:/temp/mf04",
                      overwrite = TRUE)


##----------------------------------------------------
## lsn_to_ssn
##----------------------------------------------------
## Adds pid, locID, and netID to points
## Adds netID to edges
## Creates netIDx.dat files
## creates binaryID.db if import = TRUE
## returns SSN object if import = TRUE
## Also look in ssn_path
ssn1<- lsn_to_ssn(edges = edges,
                   lsn_path = "c:/temp/mf04",
                   obs_sites = site.list$obs,
                   preds_list = site.list[2:4],
                   ssn_path = "c:/temp/mf04.ssn",
                   import = TRUE,
                   overwrite = TRUE)

## -----------------------------------------------------------
## ssn_import: Updated for geopackages, etc.
##             Will eventually replace ssn_import in SSN2
## -----------------------------------------------------------

ssn2<- ssn_import2(path = "c:/temp/mf04.ssn",
                  include_obs = TRUE,
                  predpts = list(pred1km ="pred1km.gpkg",
                                 knapp = "knapp.gpkg",
                                 capehorn = "capehorn.gpkg"),
                  overwrite = TRUE)
