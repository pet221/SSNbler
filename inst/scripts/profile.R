############# profiling

# load software
library(devtools)
library(sf)
library(profvis)
load_all()
## library(SSNbler) can load if wanting to test installed version

# store a base path to where I have ssn data stored
base_path <- "C:/Users/MDUMELLE/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ssn_test"

##################################
########### Lower Snake
##################################
lsn_path <- paste0(base_path, "/LowerSnake")
edges <- read_sf(paste0(lsn_path, "/edges.shp"))
sites <- read_sf(paste0(lsn_path, "/sites.shp"))
# preds <- read_sf(paste0(lsn_path, "/pred1km.shp"))

## --------------------------------------------------
## lines_to_lsn()
## --------------------------------------------------
# lines_to_lsn_start <- proc.time()
# p1 <- profvis(edges <- lines_to_lsn(
#   streams = edges,
#   lsn_path = lsn_path, 
#   snap_tolerance = 1,
#   check_topology = FALSE,
#   topo_tolerance = 20,
#   overwrite = TRUE,
#   verbose = TRUE,
#   remove_ZM = TRUE))
# lines_to_lsn_end <- proc.time()
# lines_to_lsn_end - lines_to_lsn_start
# 
# # profiled results
# p1
## total time 42 seconds
## 1. st_line_sample ~ 20 seconds
## 2. sqrt ~ 10 seconds

## --------------------------------------------------
## lines_to_lsn (toplogy check)
## --------------------------------------------------
lines_to_lsn_start <- proc.time()
edges <- lines_to_lsn(
  streams = edges,
  lsn_path = lsn_path, 
  snap_tolerance = 1,
  check_topology = FALSE,
  topo_tolerance = 20,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)
lines_to_lsn_end <- proc.time()
lines_to_lsn_end - lines_to_lsn_start

# profiled results
# p2
## total time 100 seconds
## 1. profiling crashed computer



## --------------------------------------------------
## sites_to_lsn
## --------------------------------------------------
sites_to_lsn_start <- proc.time()
obs <- sites_to_lsn(in_sites = sites,
                    edges = edges,
                    save_local = TRUE,
                    output = paste0(lsn_path, "/obs.gpkg"),
                    snap_tolerance = 100,
                    overwrite = TRUE,
                    verbose = TRUE)
sites_to_lsn_end <- proc.time()
sites_to_lsn_end - sites_to_lsn_start
# profiled results
# p3
## total time 135 seconds
## 1. various sf functions


## --------------------------------------------------
## updist_edges()
## --------------------------------------------------
updist_edges_start <- proc.time()
edges <- updist_edges(edges = edges,
                     lsn_path = lsn_path,
                     ##length_col = "o_length",
                     calc_length = TRUE,
                     overwrite = TRUE,
                     verbose = TRUE)
updist_edges_end <- proc.time()
updist_edges_end - updist_edges_start
# profiled results
# p4
## total time 35 seconds
## 1. igraph subsetting
## 2. igraph shortest_paths


updist_sites_start <- proc.time()
sites <- updist_sites(sites = list(obs = obs),
                                    edges = edges,
                                    lsn_path = lsn_path,
                                    length_col= "Length",
                                    overwrite = TRUE)
updist_sites_end <- proc.time()
updist_sites_end - updist_sites_start
# profiled results
# p5
## total time 35 seconds
## 1. igraph subsetting
## 2. igraph shortest_paths















# preds_to_lsn_start <- proc.time()
# preds <- sites_to_lsn(in_sites = preds,
#                                   edges = edges,
#                                   save_local = TRUE,
#                                   output = paste0(lsn_path, "/preds.gpkg"),
#                                   snap_tolerance = 100,
#                                   overwrite = TRUE,
#                                   verbose = TRUE)
# preds_to_lsn_end <- proc.time()
# preds_to_lsn_end - preds_to_lsn_start
# profiled results
# p4
## total time 135 seconds
## 1. profiling crashed computer
