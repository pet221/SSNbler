##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## This script contains the R code used in the tutorial 'Correcting
## topological errors using SSNbler and QGIS'. The example dataset
## used here is stored in topology.zip, which includes a shapefile
## named topo_streams.shp.
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Set up ----
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

## Load SSNbler and other useful packages
library(SSNbler)
library(sf)
library(dplyr)
library(purrr)

## Set working directory
setwd("C:/temp/topology")

## Import the streams dataset as an sf object
river_net<- st_read("topo_streams.shp")

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 1. Generate LSN ----
##    Initial check for topology errors 
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

## Check the minimum line feature length 
min(st_length(river_net))

## Build the initial LSN and check the topology
lsn_path1<- "c:/temp/topology/work/lsn1"
edges<- lines_to_lsn(
  streams = river_net,
  lsn_path = lsn_path1, 
  snap_tolerance = 1,    ## Must be < min line length
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

# summary(node_errors)
# pointid           nodecat                         error   
# Min.   : 275   Confluence:13   Complex Confluence       : 1  
# 1st Qu.:1328   Outlet    :21   Converging Node          : 5  
# Median :1339                   Dangling Node            :19  
# Mean   :1186                   Downstream Divergence    : 1  
# 3rd Qu.:1352                   Intersection Without Node: 8  
# Max.   :1371                                                 
# NA's   :11                                                   
#             geom   
#  POINT        :34  
#  epsg:5070    : 0  
#  +proj=aea ...: 0  

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 2. Move to QGIS for editing---- 
##    Remove complex confluences & downstream divergences
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 3. Regenerate LSN ----
##    Recheck network topology removing complex confluences & 
##    downstream divergences
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

## Import edited edges 
edges1 <- st_read(paste0(lsn_path1, "/edges.gpkg"))

## Build the LSN and check the topology
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

# pointid           nodecat                         error   
# Min.   : 344   Confluence:12   Converging Node          : 5  
# 1st Qu.:1330   Outlet    :22   Dangling Node            :20  
# Median :1341                   Intersection Without Node: 9  
# Mean   :1249                                                 
# 3rd Qu.:1354                                                 
# Max.   :1372                                                 
# NA's   :12                                                   
#             geom   
#  POINT        :34  
#  epsg:5070    : 0  
#  +proj=aea ...: 0   

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 4. Return to QGIS ----
##    Remove some topological restrictions (converging nodes) 
##    and errors using v.clean
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 5. Regenerate LSN ----
##    Recheck network topology after removing errors and 
##    restrictions using v.clean
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

## Import edited edges 
edges2 <- st_read(paste0(lsn_path2, "/snap5m.gpkg"))

## Build the LSN and check the topology
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

# pointid           nodecat                    error  
# Min.   : 473   Confluence:3   Converging Node      :3  
# 1st Qu.:1315   Outlet    :6   Dangling Node        :5  
# Median :1330                  Downstream Divergence:1  
# Mean   :1207                                           
# 3rd Qu.:1336                                           
# Max.   :1347                                           
# NA's   :2                                              
#             geom  
#  POINT        :9  
#  epsg:5070    :0  
#  +proj=aea ...:0  


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 6. Return to QGIS ----
##    Remove the remaining errors
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## 7. Generate Final LSN ----
##    Recheck network topology after removing errors and 
##    restrictions 
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

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

## Notice the message in the console indicates that '0 topology 
## errors identified. node_errors.gpkg not written to file.'

## Alternatively, you can check to see if node_errors.gpkg was saved
## to the local directory, lsn_path4
"node_errors.gpkg" %in% list.files(lsn_path4)

## Congratulations - the LSN is free of topological errors!

