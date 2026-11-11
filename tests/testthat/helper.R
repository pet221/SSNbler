# when loading, use load_all(helpers = FALSE) to avoid running all this code

################################
#### copy_streams_to_temp() ####
################################

copy_streams_to_temp()
path <- paste0(tempdir(), "/streamsdata")

################################
#### read in data ####
################################

MF_streams <- sf::read_sf(paste0(path, "/MF_streams.gpkg"))
MF_obs <- sf::read_sf(paste0(path, "/MF_obs.gpkg"))
MF_pred1km <- sf::read_sf(paste0(path, "/MF_pred1km.gpkg"))
MF_CapeHorn <- sf::read_sf(paste0(path, "/MF_CapeHorn.gpkg"))
MF_preds <- sf::read_sf(paste0(path, "/MF_preds.gpkg"))

################################
#### lines_to_lsn ####
################################

edges_lsn <- lines_to_lsn(
	streams = MF_streams,
	lsn_path = path,
	snap_tolerance = 1,
	check_topology = TRUE,
	topo_tolerance = 20,
	overwrite = TRUE,
	verbose = TRUE,
	remove_ZM = TRUE
)

################################
#### sites_to_lsn ####
################################

obs_lsn <- sites_to_lsn(
	sites = MF_obs,
	edges = edges_lsn,
	save_local = TRUE,
	lsn_path = path,
	file_name = "obs.gpkg",
	snap_tolerance = 100,
	overwrite = TRUE,
	verbose = TRUE
)

preds_lsn <- sites_to_lsn(
	sites = MF_pred1km,
	edges = edges_lsn,
	save_local = TRUE,
	lsn_path = path,
	file_name = "pred1km.gpkg",
	snap_tolerance = 1,
	overwrite = TRUE,
	verbose = TRUE
)

################################
#### updist_edges ####
################################

edges_updist <- updist_edges(
	edges = edges_lsn,
	lsn_path = path,
	calc_length = TRUE,
	overwrite = TRUE,
	verbose = TRUE
)

################################
#### updist_sites ####
################################

site.list_updist <- updist_sites(
	sites = list(obs = obs_lsn, pred1km = preds_lsn),
	edges = edges_updist,
	length_col = "Length",
	lsn_path = path,
	overwrite = TRUE
)

################################
#### afv_edges ####
################################

edges_afv <- afv_edges(
	edges = edges_updist,
	infl_col = "h2oAreaKm2",
	segpi_col = "areaPI",
	lsn_path = path,
	afv_col = "afvArea",
	overwrite = TRUE
)

################################
#### afv_sites ####
################################

site.list_afv <- afv_sites(
	sites = site.list_updist,
	edges = edges_afv,
	afv_col = "afvArea",
	save_local = TRUE,
	lsn_path = path,
	overwrite = TRUE
)

################################
#### ssn_assemble ####
################################

ssn_object <- ssn_assemble(
	edges = edges_afv,
	lsn_path = path,
	obs_sites = site.list_afv$obs,
	preds_list = list(pred1km = site.list_afv$pred1km),
	ssn_path = paste0(path, "/MF.ssn"),
	import = TRUE,
	overwrite = TRUE,
	check = FALSE
)

################################
#### read in output data ####
################################

edges <- sf::read_sf(paste0(path, "/edges.gpkg"))
nodes <- sf::read_sf(paste0(path, "/nodes.gpkg"))
obs <- sf::read_sf(paste0(path, "/obs.gpkg"))
pred1km <- sf::read_sf(paste0(path, "/pred1km.gpkg"))
noderelationships <- read.csv(paste0(path, "/noderelationships.csv"))
relationships <- read.csv(paste0(path, "/relationships.csv"))