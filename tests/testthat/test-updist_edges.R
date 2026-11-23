test_that("updist_edges", {
	expect_equal(dim(edges_updist), c(163, 13))
	expect_true("Length" %in% names(edges_updist))
	expect_equal(edges_updist$Length[1:2], c(3208.97, 898.94), tolerance = 0.01)
	expect_true(all(edges_updist$Length >= 0))
	expect_true("upDist" %in% names(edges_updist))
	expect_equal(edges_updist$upDist[1:2], c(17458.27, 18357.20), tolerance = 0.01)
	expect_true(all(edges_updist$upDist >= 0))
	expect_true(all(edges_updist$upDist >= edges_updist$Length))
	expect_s3_class(edges_updist, "sf")
})

test_that("updist_edges stops when a Self-loop is present", {
	# Inside a network (also a downstream divergence) and at the outlet
	# (otherwise silently wrong results)
	e <- self_loop_lsn("inside")
	expect_error(updist_edges(e, attr(e, "lsn_path"), calc_length = TRUE,
			save_local = FALSE, verbose = FALSE),
		"1 Self-loop edge\\(s\\) \\(rid 2\\)")
	e <- self_loop_lsn("outlet")
	expect_error(updist_edges(e, attr(e, "lsn_path"), calc_length = TRUE,
			save_local = FALSE, verbose = FALSE),
		"1 Self-loop edge\\(s\\) \\(rid 4\\)")
})

test_that("updist_edges stops when a network has no outlet", {
	# Lines 1-3 form a loop and line 4 flows into it; lines 5-6 are valid.
	# Nothing is calculated or written, so edges.gpkg keeps all 6 edges
	e <- no_outlet_lsn()
	expect_error(updist_edges(e, attr(e, "lsn_path"), calc_length = TRUE,
		save_local = TRUE, verbose = FALSE),
		"4 edge\\(s\\) \\(rid 1, 2, 3, 4\\) are in a network without an outlet")
	expect_equal(nrow(sf::st_read(file.path(attr(e, "lsn_path"), "edges.gpkg"),
		quiet = TRUE)), 6)
	# Backstop: stops if any edge would be dropped, whatever the cause
	expect_error(check_edges_kept(1:3, c(1, 3), "upDist"),
		"upDist could not be calculated for 1 edge\\(s\\) \\(rid 2\\)")
	expect_no_error(check_edges_kept(1:3, 3:1, "upDist"))
})
