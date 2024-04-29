# load software
library(devtools)
load_all()
library(sf)
## library(SSNbler) can load if wanting to test installed version

# store a base path to where I have ssn data stored
base_path <- "C:/Users/MDUMELLE/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/ssn_test"

##################################
########### Middle Fork
##################################
lsn_path <- paste0(base_path, "/MiddleFork")
# stored in data folder as
# MF_streams <- read_sf(paste0(lsn_path, "/edges.shp"))
# MF_obs <- read_sf(paste0(lsn_path, "/sites.shp"))
# MF_pred1km <- read_sf(paste0(lsn_path, "/pred1km.shp"))

edges <- MF_streams
sites <- MF_obs
preds <- MF_pred1km

## --------------------------------------------------
## lines_to_lsn
## --------------------------------------------------

edges <- lines_to_lsn(
  streams = edges,
  lsn_path = lsn_path, 
  snap_tolerance = 1,
  check_topology = TRUE,
  topo_tolerance = 20,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)

## --------------------------------------------------
## sites_to_lsn
## --------------------------------------------------

obs <- sites_to_lsn(in_sites = sites,
                    edges = edges,
                    save_local = TRUE,
                    output = paste0(lsn_path, "/obs.gpkg"),
                    snap_tolerance = 100,
                    overwrite = TRUE,
                    verbose = TRUE)


preds<- sites_to_lsn(in_sites = preds,
                     edges = edges,
                     save_local = TRUE,
                     output = paste0(lsn_path, "/preds.gpkg"),
                     snap_tolerance = 1,
                     overwrite = TRUE,
                     verbose = TRUE)



## --------------------------------------------------
## updist_edges
## --------------------------------------------------

edges<- updist_edges(edges = edges,
                     lsn_path = lsn_path,
                     ##length_col = "o_length",
                     calc_length = TRUE,
                     overwrite = TRUE,
                     verbose = TRUE)


##----------------------------------------------------
## updist_sites
##----------------------------------------------------

site.list<- updist_sites(sites = list(obs = obs,
                                      pred = preds),
                         edges = edges,
                         length_col= "Length",
                         lsn_path = lsn_path,
                         overwrite = TRUE)

##----------------------------------------------------
## afv_edges
##----------------------------------------------------

edges<- afv_edges(edges=edges,
                  infl_col = "AREAWTMAP", 
                  segpi_col = "areaPI",
                  lsn_path = lsn_path,
                  afv_col = "afvArea",
                  overwrite = TRUE)



##----------------------------------------------------
## afv_sites
##----------------------------------------------------

site.list<- afv_sites(sites = site.list,
                      edges=edges,
                      afv_col = "afvArea",
                      save_local = TRUE,
                      lsn_path = lsn_path,
                      overwrite = TRUE)


##----------------------------------------------------
## lsn_to_ssn
##----------------------------------------------------
ssn1<- lsn_to_ssn(edges = edges,
                    lsn_path = lsn_path,
                    obs_sites = site.list$obs,
                    preds_list = list(pred = site.list$pred),
                    # site.list[2] works; site.list$pred DOES NOT work; list(site.list$pred) DOES NOT work
                    ssn_path = paste0(lsn_path, "/MF.ssn"),
                    import = TRUE,
                    overwrite = TRUE)




##################################
########### rawData
##################################
lsn_path <- paste0(base_path, "/rawData")
# stored in data folder as
edges <- read_sf(paste0(lsn_path, "/streams_w_attrs.shp"))
sites <- read_sf(paste0(lsn_path, "/sites.shp"))
preds <- read_sf(paste0(lsn_path, "/preds.shp"))


## --------------------------------------------------
## lines_to_lsn
## --------------------------------------------------

edges <- lines_to_lsn(
  streams = edges,
  lsn_path = lsn_path, 
  snap_tolerance = 1,
  check_topology = TRUE,
  topo_tolerance = 20,
  overwrite = TRUE,
  verbose = TRUE,
  remove_ZM = TRUE)

## --------------------------------------------------
## sites_to_lsn
## --------------------------------------------------

obs <- sites_to_lsn(in_sites = sites,
                    edges = edges,
                    save_local = TRUE,
                    output = paste0(lsn_path, "/obs.gpkg"),
                    snap_tolerance = 100,
                    overwrite = TRUE,
                    verbose = TRUE)


preds<- sites_to_lsn(in_sites = preds,
                     edges = edges,
                     save_local = TRUE,
                     output = paste0(lsn_path, "/preds.gpkg"),
                     snap_tolerance = 1,
                     overwrite = TRUE,
                     verbose = TRUE)



## --------------------------------------------------
## updist_edges
## --------------------------------------------------

edges<- updist_edges(edges = edges,
                     lsn_path = lsn_path,
                     ##length_col = "o_length",
                     calc_length = TRUE,
                     overwrite = TRUE,
                     verbose = TRUE)


##----------------------------------------------------
## updist_sites
##----------------------------------------------------

site.list<- updist_sites(sites = list(obs = obs,
                                      pred = preds),
                         edges = edges,
                         length_col= "Length",
                         lsn_path = lsn_path,
                         overwrite = TRUE)

##----------------------------------------------------
## afv_edges
##----------------------------------------------------

edges<- afv_edges(edges=edges,
                  infl_col = "o_h2oArea", 
                  segpi_col = "areaPI",
                  lsn_path = lsn_path,
                  afv_col = "afvArea",
                  overwrite = TRUE)



##----------------------------------------------------
## afv_sites
##----------------------------------------------------

site.list<- afv_sites(sites = site.list,
                      edges=edges,
                      afv_col = "afvArea",
                      save_local = TRUE,
                      lsn_path = lsn_path,
                      overwrite = TRUE)


##----------------------------------------------------
## ssn_assemble
##----------------------------------------------------
ssn1<- ssn_assemble(edges = edges,
                    lsn_path = lsn_path,
                    obs_sites = site.list$obs,
                    preds_list = list(site.list$pred),
                    ssn_path = paste0(lsn_path, "/streams.ssn"),
                    import = TRUE,
                    overwrite = TRUE)
