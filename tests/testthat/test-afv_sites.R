test_that("afv_sites", {
	expect_true(is.list(site.list_afv))
	expect_equal(length(site.list_afv), 2)
	expect_true("obs" %in% names(site.list_afv))
	expect_equal(dim(site.list_afv$obs), c(45, 22))
	expect_true("afvArea" %in% names(site.list_afv$obs))
	expect_equal(site.list_afv$obs$afvArea[1:2], c(0.171, 0.171), tolerance = 0.001)
	expect_true(all(site.list_afv$obs$afvArea >= 0))
	expect_s3_class(site.list_afv$obs, "sf")
	expect_true("pred1km" %in% names(site.list_afv))
	expect_equal(dim(site.list_afv$pred1km), c(175, 15))
	expect_true("afvArea" %in% names(site.list_afv$pred1km))
	expect_equal(site.list_afv$pred1km$afvArea[1:2], c(0.171, 0.171), tolerance = 0.001)
	expect_true(all(site.list_afv$pred1km$afvArea >= 0))
	expect_s3_class(site.list_afv$pred1km, "sf")
})

test_that("afv_sites stops when a site is on an edge with no AFV", {
	e <- no_outlet_lsn()
	sites <- no_outlet_sites(e)
	# Edges in the loop are missing, as in updist_edges() output before
	# the network outlet check was added
	e.valid <- e[e$rid %in% 5:6, ]
	e.valid$afvArea <- 1
	expect_error(afv_sites(list(obs = sites), e.valid, afv_col = "afvArea",
		save_local = FALSE),
		"1 site\\(s\\) in obs are on edges with no afvArea \\(rid 1\\)")
})
