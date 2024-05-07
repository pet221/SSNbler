##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## This script contains the R code used in the tutorial 'Correcting
## topological errors using SSNbler and ArcPro'. The example dataset
## used here is stored in topology_pro.zip, which includes a shapefile
## named ARivers.shp.
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

## Load SSNbler and other useful packages
library(SSNbler)
library(sf)
library(dplyr)
library(purrr)

## Set working directory
setwd("C:/temp/topology_pro")

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Import streams, build LSN and check for topology errors
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

## Import the streams dataset as an sf Object
river_net<- st_read("ARiver.shp")

## Build the initial LSN and check the topology
lsn_path1<- "c:/temp/topology_pro/work/lsn1"
edges<- lines_to_lsn(
  streams = river_net,
  lsn_path = lsn_path1, 
  snap_tolerance = 1,  
  check_topology = TRUE,
  topo_tolerance = 20, 
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)

## Check output files. If node_errors.gpkg exists, then there are
## potential errors to check
list.files(lsn_path1)

## Import node errors and format columns
node_errors <- st_read(paste0(lsn_path1, "/node_errors.gpkg"),
                              quiet = TRUE) %>%
  modify_if(is.character, as.factor)


## Summarise
summary(node_errors)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Move to GIS and remove complex confluences &
## downstream divergences
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Regenerate LSN and recheck network topology after removing 
## complex confluences & downstream divergences from edges in QGIS
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

## Import the edited streams dataset as an sf Object
river_net<- st_read("edgescorrected.shp")

## Build the initial LSN and check the topology
lsn_path2<- "c:/temp/topology_pro/work/lsn2"
edges<- lines_to_lsn(
  streams = river_net,
  lsn_path = lsn_path2, 
  snap_tolerance = 1,
  check_topology = TRUE,
  topo_tolerance = 20,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)

## Check output files. If node_errors.gpkg exists, then there are
## potential errors to check
list.files(lsn_path2)

## Import node errors and format columns
node_errors <- st_read(paste0(lsn_path2, "/node_errors.gpkg"),
                              quiet = TRUE) %>%
  modify_if(is.character, as.factor)

## Summarise
summary(node_errors)

## End

