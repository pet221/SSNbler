#################################################################
## R script for SSNbler_vignette.pdf ####
##
## Includes additional comments and R code not included in the 
## SSNbler vignete
#################################################################

## --------------------------------------------------------------
## Install and load SSNbler ####
## --------------------------------------------------------------

##install.packages("remotes") ## Install remotes if needed
library(remotes)

## Install the latest version of SSNbler from github
remotes::install_github("pet221/SSNbler", ref = "main")

## Load SSNbler library
library(SSNbler)

## --------------------------------------------------------------
## Import and view the input data ####
## --------------------------------------------------------------

## Copy the example dataset to a temporary directory
copy_streams_to_temp()
path <- paste0(tempdir(), "/streamsdata")


## Load the sf package and import the streams, observation sites, and
## two prediction datasets
library(sf)
MF_streams <- st_read(paste0(path, "/MF_streams.gpkg"))
MF_obs <- st_read(paste0(path, "/MF_obs.gpkg"))
MF_pred1km <- st_read(paste0(path, "/MF_pred1km.gpkg"))
MF_CapeHorn <- st_read(paste0(path, "/MF_CapeHorn.gpkg"))

## Notice that the imported data are of class sf data.frame
class(MF_obs)
names(MF_obs)

## Plot the data using ggplot2
library(ggplot2)
ggplot() +
  geom_sf(data = MF_streams) +
  geom_sf(data = MF_CapeHorn, color = "gold", size = 1.7) +
  geom_sf(data = MF_pred1km, colour = "purple", size = 1.7) +
  geom_sf(data = MF_obs, color = "blue", size = 2)

## --------------------------------------------------------------
## Build the LSN ####
## --------------------------------------------------------------
## Set the lsn.path variable
lsn.path <- paste0(tempdir(),"/mf04")

## Build the LSN and take note of the messages printed to the console
edges <- lines_to_lsn(
  streams = MF_streams,
  lsn_path = lsn.path,
  check_topology = TRUE,
  snap_tolerance = 1,
  topo_tolerance = 20,
  overwrite = TRUE
)
## list new files saved in lsn.path
list.files(lsn.path)

## --------------------------------------------------------------
## Incorporate sites into LSN ####
## --------------------------------------------------------------

## Incorporate observations. Pay attention to output messages in R
## console to ensure all sites are snapped successfully.
obs <- sites_to_lsn(
  sites = MF_obs,
  edges = edges,
  lsn_path = lsn.path,
  file_name = "obs",
  snap_tolerance = 100,
  save_local = TRUE,
  overwrite = TRUE
)

## Predictions at 1km intervals
preds <- sites_to_lsn(
  sites = MF_pred1km,
  edges = edges,
  lsn_path = lsn.path,
  file_name = "pred1km.gpkg",
  snap_tolerance = 100,
  save_local = TRUE,
  overwrite = TRUE
)

## Prediction sites in Cape Horn Creek
capehorn <- sites_to_lsn(
  sites = MF_CapeHorn,
  edges = edges,
  lsn_path = lsn.path,
  file_name = "CapeHorn.gpkg",
  snap_tolerance = 100,
  save_local = TRUE,
  overwrite = TRUE
)

## list files saved in lsn.path. Notice that geopackages for the sites
## have been added
list.files(lsn.path)

## Notice the new columns rid, ratio, and snapdist
names(obs)

## Summarise snapdist, looking for sites that were snapped large
## distances
summary(obs$snapdist)

## Look at ratio values to ensure they range from 0-1
summary(obs$ratio)

## --------------------------------------------------------------
## Calculate upstream distance for edges ####
## --------------------------------------------------------------
edges <- updist_edges(
  edges = edges,
  lsn_path = lsn.path,
  calc_length = TRUE,
  save_local = TRUE
)

## View edges column names
names(edges)

## Summarise upDist
summary(edges$upDist)

## --------------------------------------------------------------
## Calculate upstream distance for sites ####
## --------------------------------------------------------------

site.list <- updist_sites(
  sites = list(obs = obs,             ## Input is a named list of sf
               pred1km = preds,       ## objects
               CapeHorn = capehorn),
  edges = edges,
  length_col= "Length",
  lsn_path = lsn.path,
  save_local = TRUE
)

## Notice that length of site.list = 3
length(site.list)

## View output site.list names
names(site.list)

## View column names in obs
names(site.list$obs)       

## Plot the upstream distances for edges and obs
ggplot() +
  geom_sf(data = edges, aes(color = upDist)) +
  geom_sf(data = site.list$obs, aes(color = upDist)) +
  scale_color_viridis_c()

## --------------------------------------------------------------
## Calculate AFV for edges ####
## --------------------------------------------------------------

edges <- afv_edges(
  edges = edges,
  infl_col = "h2oAreaKm2", 
  segpi_col = "areaPI",
  afv_col = "afvArea",
  lsn_path = lsn.path,
  save_local = TRUE
)
## Look at edges column names. Notice the two new columns, areaPI and
## afvArea
names(edges)

## Summarize the AFV column to ensure it ranges from 0-1
summary(edges$afvArea)

## --------------------------------------------------------------
## Calculate AFV for sites ####
## --------------------------------------------------------------

site.list <- afv_sites(
  sites = site.list,       ## Input is a named list
  edges = edges,
  afv_col = "afvArea",
  save_local = TRUE,
  lsn_path = lsn.path
)

## View column names in pred1km. Notice the new afvArea column
names(site.list$pred1km)

## Summarise AFVs in pred1km to ensure they range from 0-1
summary(site.list$pred1km$afvArea)


## Plot the afvArea for edges and pred1km
ggplot() +
  geom_sf(data = edges, aes(color = afvArea)) +
  geom_sf(data = site.list$obs, aes(color = afvArea)) +
  scale_color_viridis_c()

## --------------------------------------------------------------
## Assemble the SSN object ####
## --------------------------------------------------------------

mf04_ssn <- ssn_assemble(
  edges = edges,
  lsn_path = lsn.path,
  obs_sites = site.list$obs,          ## Input is an sf data.frame
  preds_list = site.list[c("pred1km", ## Input is a named list
                           "CapeHorn")],
  ssn_path = paste0(lsn.path, "/MiddleFork04.ssn"),
  import = TRUE,
  overwrite = TRUE
)

## Look at files in MiddleFork04.ssn
list.files(paste0(lsn.path, "/MiddleFork04.ssn"))

## Check class for mf04_ssn
class(mf04_ssn)

## Get SSN object names
names(mf04_ssn)

## Print path to local .ssn
mf04_ssn$path

## Print names of prediction datasets in SSN object
names(mf04_ssn$preds)

## Look at the observations in the SSN object. Notice the new columns
## added when the SSN was assembled
class(mf04_ssn$obs)
names(mf04_ssn$obs)
View(mf04_ssn$obs)

## Plot mf04_ssn
ggplot() +
  geom_sf(
    data = mf04_ssn$edges,
    color = "medium blue",
    aes(linewidth = h2oAreaKm2)
  ) +
  scale_linewidth(range = c(0.1, 2.5)) + 
  geom_sf(
    data = mf04_ssn$preds$pred1km,
    size = 1.5,
    shape = 21,
    fill = "white",
    color = "dark grey"
  ) +
  geom_sf(
    data = mf04_ssn$obs, 
    size = 1.7,
    aes(color = Summer_mn)
  ) +
  scale_color_viridis_c() +
  labs(color = "Temperature", linewidth = "WS Area") +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

## --------------------------------------------------------------
## Create distance matrices ####
## --------------------------------------------------------------
library(SSN2)

## Create the distance matrices
ssn_create_distmat(mf04_ssn, predpts = "pred1km",
                   overwrite = TRUE)

## Look at distance matrix files for obs
list.files(paste0(mf04_ssn$path, "/distance/obs"))

## Look at distance matrix files for pred1km
list.files(paste0(mf04_ssn$path, "/distance/pred1km"))

## Output a list of distance matrices for obs
obs.distmat<- ssn_get_stream_distmat(mf04_ssn,
                                     name = "obs")
names(obs.distmat)
View(obs.distmat[[1]])

## --------------------------------------------------------------
## Fit a spatial stream-network model model ####
## --------------------------------------------------------------

## Fit a spatial linear model to Summer mean temperature with a
## mixture of TU/TD/EUC covariance models
ssn_mod <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04_ssn,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_type = "gaussian",
  additive = "afvArea"
)
## Summarise model results
summary(ssn_mod)

## Get class for model output
class(ssn_mod)

## Notice that the ssn.object is stored in ssn_mod
## for reproducibility
summary(ssn_mod$ssn.object)



