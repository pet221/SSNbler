test_that("sites_to_lsn", {
	
	# MF_obs
	expect_equal(dim(obs_lsn), c(45, 20))
	expect_true("rid" %in% names(obs_lsn))
	expect_equal(obs_lsn$rid[1:2], c(33, 34))
	expect_true("ratio" %in% names(obs_lsn))
	expect_equal(as.vector(obs_lsn$ratio[1:2]), c(0.321, 0.069), tolerance = 0.001)
	expect_true(all(obs_lsn$ratio >= 0))
	expect_true(all(obs_lsn$ratio <= 1))
	expect_true("snapdist" %in% names(obs_lsn))
	expect_true(all(obs_lsn$snapdist >= 0))
	expect_true(all(obs_lsn$snapdist <= 1))
	expect_s3_class(obs_lsn, "sf")
	
	# MF_pred1km
	expect_equal(dim(preds_lsn), c(175, 13))
	expect_true("rid" %in% names(preds_lsn))
	expect_equal(preds_lsn$rid[1:2], c(5, 5))
	expect_true("ratio" %in% names(preds_lsn))
	expect_equal(as.vector(preds_lsn$ratio[1:2]), c(0.637, 0.275), tolerance = 0.001)
	expect_true(all(preds_lsn$ratio >= 0))
	expect_true(all(preds_lsn$ratio <= 1))
	expect_true("snapdist" %in% names(preds_lsn))
	expect_true(all(preds_lsn$snapdist >= 0))
	expect_true(all(preds_lsn$snapdist <= 1))
	expect_s3_class(preds_lsn, "sf")
})

test_that("sites_to_lsn checks for Z and M dimensions", {
	# Copies of sites or edges with an extra dimension
	add_dim <- function(x, dim) {
		g <- sf::st_geometry(x)
		extra <- if (dim == "XYZM") c(1200, 0) else 1200
		if (inherits(g, "sfc_POINT")) {
			new.g <- lapply(g, function(p) sf::st_point(c(unclass(p), extra), dim = dim))
		} else {
			new.g <- lapply(g, function(l) {
				m <- unclass(l)
				sf::st_linestring(cbind(m, matrix(extra, nrow(m), length(extra),
					byrow = TRUE)), dim = dim)
			})
		}
		sf::st_geometry(x) <- sf::st_sfc(new.g, crs = sf::st_crs(x))
		x
	}
	snap <- function(sites, edges, ...) {
		sites_to_lsn(sites = sites, edges = edges, snap_tolerance = 100,
			save_local = FALSE, overwrite = TRUE, verbose = FALSE, ...)
	}

	for (d in c("XYZ", "XYM", "XYZM")) {
		sites.d <- add_dim(MF_obs, d)
		edges.d <- add_dim(edges_lsn, d)

		expect_error(snap(sites.d, edges_lsn), "sites has XYZ, XYM or XYZM")
		expect_error(snap(MF_obs, edges.d), "edges has XYZ, XYM or XYZM")

		# remove_ZM = TRUE gives the same result as XY input
		out <- snap(sites.d, edges.d, remove_ZM = TRUE)
		expect_equal(colnames(sf::st_coordinates(out)), c("X", "Y"))
		expect_equal(out$rid, obs_lsn$rid)
		expect_equal(out$ratio, obs_lsn$ratio)
		expect_equal(out$snapdist, obs_lsn$snapdist)
	}
})
