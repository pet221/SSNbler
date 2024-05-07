##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## This script contains the R code used in the tutorial 'Correcting
## topological errors using SSNbler and QGIS'. The example dataset
## used here is stored in topology.zip, which includes a shapefile
## named topo_streams.shp.
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

## Load SSNbler and other useful packages
library(SSNbler)
library(sf)
library(dplyr)
library(purrr)

## Set working directory
setwd("C:/temp/topology")

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Import streams, build LSN and check for topology errors
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

## Import the streams dataset as an sf Object
river_net<- st_read("topo_streams.shp")

## Build the initial LSN and check the topology
lsn_path1<- "c:/temp/topology/work/lsn1"
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
## Move to QGIS and remove complex confluences &
## downstream divergences
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Regenerate LSN and recheck network topology after removing 
## complex confluences & downstream divergences from edges in QGIS
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

## Import edited edges 
edges1 <- st_read(paste0(lsn_path1, "/edges.gpkg"))

## Build the initial LSN and check the topology
lsn_path2<- "c:/temp/topology/work/lsn2"
edges<- lines_to_lsn(
  streams = edges1,
  lsn_path = lsn_path2, 
  snap_tolerance = 1,
  check_topology = TRUE,
  topo_tolerance = 20,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)

## Import node errors and format columns
node_errors <- st_read(paste0(lsn_path2, "/node_errors.gpkg"),
                              quiet = TRUE) %>%
  modify_if(is.character, as.factor)

## Summarise
summary(node_errors)

 #    pointid           nodecat                         error   
 # Min.   : 344   Confluence:14   Converging Node          : 5  
 # 1st Qu.:1328   Outlet    :22   Dangling Node            :20  
 # Median :1339                   Intersection Without Node: 9  
 # Mean   :1219                   Unsnapped Node           : 2  
 # 3rd Qu.:1352                                                 
 # Max.   :1372                                                 
 # NA   :12                                                   
 #            geom   
 # POINT        :36  
 # epsg:5070    : 0  
 # +proj=aea ...: 0  

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Return to QGIS to remove some topological restrictions
## (converging nodes) and errors using v.clean
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Regenerate LSN and recheck network topology after removing 
## errors and restrictions using v.clean
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

## Import edited edges 
edges2 <- st_read(paste0(lsn_path2, "/snap5m.gpkg"))

## Build the initial LSN and check the topology
lsn_path3<- "c:/temp/topology/work/lsn3"
edges<- lines_to_lsn(
  streams = edges2,
  lsn_path = lsn_path3, 
  snap_tolerance = 1,
  check_topology = TRUE,
  topo_tolerance = 20,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)

## Import node errors and format columns
node_errors <- st_read(paste0(lsn_path3, "/node_errors.gpkg"),
                              quiet = TRUE) %>%
  modify_if(is.character, as.factor)

## Summarise
summary(node_errors)

 #    pointid           nodecat                    error              geom  
 # Min.   : 473   Confluence:3   Converging Node      :3   POINT        :9  
 # 1st Qu.:1315   Outlet    :6   Dangling Node        :5   epsg:5070    :0  
 # Median :1330                  Downstream Divergence:1   +proj=aea ...:0  
 # Mean   :1207                                                             
 # 3rd Qu.:1336                                                             
 # Max.   :1347                                                             
 # NA   :2       

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Return to QGIS to remove the remaining errors
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
## Regenerate LSN and recheck network topology after removing 
## errors and restrictions 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####

## Import edited edges 
edges3 <- st_read(paste0(lsn_path3, "/snap2m.gpkg"))

## Build the initial LSN and check the topology
lsn_path4<- "c:/temp/topology/work/lsn4"
edges<- lines_to_lsn(
  streams = edges3,
  lsn_path = lsn_path4, 
  snap_tolerance = 1,
  check_topology = TRUE,
  topo_tolerance = 20,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)

## Notice the message in the console indicates that 'No obvious
## topological errors detected and node_errors.gpkg was NOT created.'

## Alternatively, you can check to see if node_errors.gpkg was saved
## to the local directory, lsn_path4
"node_errors.gpkg" %in% list.files(lsn_path4)

## Congratulations - the LSN is free of topological errors!

