test_that("workflow works", {
  
  copy_streams_to_temp()
  path <- paste0(tempdir(), "/streamsdata")
  MF_streams <- sf::read_sf(paste0(path, "/MF_streams.gpkg"))
  MF_obs <- sf::read_sf(paste0(path, "/MF_obs.gpkg"))
  MF_pred1km <- sf::read_sf(paste0(path, "/MF_pred1km.gpkg"))
  
################## landscape network
  edges <- lines_to_lsn(
    streams = MF_streams,
    lsn_path = path, 
    snap_tolerance = 1,
    check_topology = TRUE,
    topo_tolerance = 20,
    overwrite = TRUE,
    verbose = TRUE,
    remove_ZM = TRUE
  )
  expect_s3_class(edges, "sf")
  
  obs <- sites_to_lsn(
    sites = MF_obs,
    edges = edges,
    save_local = TRUE,
    lsn_path = path,
    file_name = "obs.gpkg",
    snap_tolerance = 100,
    overwrite = TRUE,
    verbose = TRUE
  )
  expect_s3_class(obs, "sf")
  
  preds <- sites_to_lsn(
    sites = MF_pred1km,
    edges = edges,
    save_local = TRUE,
    lsn_path = path,
    file_name = "pred1km.gpkg",
    snap_tolerance = 1,
    overwrite = TRUE,
    verbose = TRUE
  )
  expect_s3_class(obs, "sf")
  
  
  ################## upstream distance
  edges <- updist_edges(
    edges = edges,
    lsn_path = path,
    calc_length = TRUE,
    overwrite = TRUE,
    verbose = TRUE
  )
  expect_s3_class(edges, "sf")
  
  
  site.list <- updist_sites(
    sites = list(obs = obs, pred1km = preds),
    edges = edges,
    length_col= "Length",
    lsn_path = path,
    overwrite = TRUE
  )
  expect_equal(names(site.list), c("obs", "pred1km"))
  expect_s3_class(site.list$obs, "sf")
  expect_s3_class(site.list$pred1km, "sf")
  
  ################## additive function value
  edges <- afv_edges(
    edges = edges,
    infl_col = "AREAWTMAP", 
    segpi_col = "areaPI",
    lsn_path = path,
    afv_col = "afvArea",
    overwrite = TRUE
  )
  expect_true(all(c("areaPI", "afvArea") %in% names(edges)))
  expect_s3_class(edges, "sf")
  
  site.list <- afv_sites(
    sites = site.list,
    edges = edges,
    afv_col = "afvArea",
    save_local = TRUE,
    lsn_path = path,
    overwrite = TRUE
  )
  expect_equal(names(site.list), c("obs", "pred1km"))
  expect_s3_class(site.list$obs, "sf")
  expect_s3_class(site.list$pred1km, "sf")
  
  ################## additive function value
  ssn_object <- ssn_assemble(
    edges = edges,
    lsn_path = path,
    obs_sites = site.list$obs,
    preds_list = list(pred1km = site.list$pred1km),
    ssn_path = paste0(path, "/MF.ssn"),
    import = TRUE,
    overwrite = TRUE
  )
  expect_s3_class(ssn_object, "SSN")
  
})
