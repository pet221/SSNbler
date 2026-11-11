test_that("Relevant files exist", {
	expect_equal(length(list.files(path)), 12)
	expect_false(file.exists(file.path(path, "nodexy.csv")))
})

test_that("MF_streams", {
  expect_equal(dim(MF_streams), c(163, 10))
	expect_true(all(sf::st_geometry_type(MF_streams) == "LINESTRING"))
	expect_s3_class(MF_streams, "sf")
})

test_that("MF_obs", {
	expect_equal(dim(MF_obs), c(45, 17))
	expect_true(all(sf::st_geometry_type(MF_obs) == "POINT"))
	expect_s3_class(MF_obs, "sf")
})


test_that("MF_pred1km", {
	expect_equal(dim(MF_pred1km), c(175, 10))
	expect_true(all(sf::st_geometry_type(MF_pred1km) == "POINT"))
	expect_s3_class(MF_pred1km, "sf")
})

test_that("MF_CapeHorn", {
	expect_equal(dim(MF_CapeHorn), c(654, 10))
	expect_true(all(sf::st_geometry_type(MF_CapeHorn) == "POINT"))
	expect_s3_class(MF_CapeHorn, "sf")
})

test_that("MF_preds", {
	expect_equal(dim(MF_preds), c(43, 10))
	expect_true(all(sf::st_geometry_type(MF_preds) == "POINT"))
	expect_s3_class(MF_preds, "sf")
})

test_that("edges", {
	expect_equal(dim(edges), c(163, 15))
	expect_true(all(sf::st_geometry_type(edges) == "LINESTRING"))
	expect_s3_class(edges, "sf")
})

test_that("nodes", {
	expect_equal(dim(nodes), c(165, 3))
	expect_true(all(sf::st_geometry_type(nodes) == "POINT"))
	expect_equal(nodes$pointid, 1:165)
	expect_true(all(sf::st_coordinates(nodes)[, "X"] < -1450000))
	expect_true(all(sf::st_coordinates(nodes)[, "Y"] > 2500000))
	expect_true(all(nodes$nodecat %in% c("Pseudonode", "Confluence", "Source", "Outlet")))
	expect_s3_class(nodes, "sf")
})


test_that("obs", {
	expect_equal(dim(obs), c(45, 22))
	expect_true(all(sf::st_geometry_type(obs) == "POINT"))
	expect_s3_class(obs, "sf")
})

test_that("pred1km", {
	expect_equal(dim(pred1km), c(175, 15))
	expect_true(all(sf::st_geometry_type(pred1km) == "POINT"))
	expect_s3_class(pred1km, "sf")
})

test_that("noderelationships", {
	expect_equal(dim(noderelationships), c(163, 3))
	expect_equal(names(noderelationships), c("rid", "fromnode", "tonode"))
	expect_equal(noderelationships$rid[1:2], c(1, 2))
	expect_equal(noderelationships$fromnode[1:2], c(1, 3))
	expect_equal(noderelationships$tonode[1:2], c(2, 1))
	expect_s3_class(noderelationships, "data.frame")
})


test_that("relationships", {
	expect_equal(dim(relationships), c(161, 2))
	expect_equal(names(relationships), c("fromedge", "toedge"))
	expect_equal(relationships$fromedge[1:2], c(1, 2))
	expect_equal(relationships$toedge[1:2], c(16, 1))
	expect_s3_class(relationships, "data.frame")
})


test_that("SSN files exist", {
	expect_true(file.exists(paste0(path, "/MF.ssn")))
	expect_true(file.exists(paste0(path, "/MF.ssn", "/binaryID.db")))
	expect_true(file.exists(paste0(path, "/MF.ssn", "/edges.gpkg")))
	expect_true(file.exists(paste0(path, "/MF.ssn", "/sites.gpkg")))
	expect_true(file.exists(paste0(path, "/MF.ssn", "/pred1km.gpkg")))
	expect_true(file.exists(paste0(path, "/MF.ssn", "/netID1.dat")))
	expect_true(file.exists(paste0(path, "/MF.ssn", "/netID2.dat")))
	
})