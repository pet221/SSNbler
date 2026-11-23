test_that("accum_edges", {
	edges_accum <- accum_edges(
	  edges = edges_lsn,
	  lsn_path = path,
	  sum_col = "rcaAreaKm2",
	  acc_col = "WArea_km2",
	  save_local = FALSE,
	  overwrite = TRUE,
	  verbose = FALSE
	)
	expect_true("WArea_km2" %in% names(edges_accum))
	expect_equal(edges_accum$WArea_km2, edges_accum$h2oAreaKm2)
	expect_true(all(edges_accum$WArea_km2 >= edges_accum$rcaAreaKm2))
})

test_that("accum_edges stops when a Self-loop is present", {
	# Inside a network (also a downstream divergence) and at the outlet
	# (otherwise silently wrong results)
	e <- self_loop_lsn("inside")
	expect_error(accum_edges(e, attr(e, "lsn_path"), sum_col = "area",
			acc_col = "areaAcc", save_local = FALSE, verbose = FALSE),
		"1 Self-loop edge\\(s\\) \\(rid 2\\)")
	e <- self_loop_lsn("outlet")
	expect_error(accum_edges(e, attr(e, "lsn_path"), sum_col = "area",
			acc_col = "areaAcc", save_local = FALSE, verbose = FALSE),
		"1 Self-loop edge\\(s\\) \\(rid 4\\)")
})

test_that("accum_edges stops when a network has no outlet", {
	# Lines 1-3 form a loop and line 4 flows into it; lines 5-6 are valid.
	# Nothing is calculated or written, so edges.gpkg keeps all 6 edges
	e <- no_outlet_lsn()
	expect_error(accum_edges(e, attr(e, "lsn_path"), sum_col = "area",
		acc_col = "areaAcc", save_local = TRUE, overwrite = TRUE, verbose = FALSE),
		"4 edge\\(s\\) \\(rid 1, 2, 3, 4\\) are in a network without an outlet")
	expect_equal(nrow(sf::st_read(file.path(attr(e, "lsn_path"), "edges.gpkg"),
		quiet = TRUE)), 6)})
