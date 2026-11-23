test_that("updist_sites", {
  expect_true(is.list(site.list_updist))
	expect_equal(length(site.list_updist), 2)
	expect_true("obs" %in% names(site.list_updist))
	expect_equal(dim(site.list_updist$obs), c(45, 21))
	expect_true("upDist" %in% names(site.list_updist$obs))
	expect_equal(site.list_updist$obs$upDist[1:2], c(14295.20, 16258.19), tolerance = 0.01)
	expect_true(all(site.list_updist$obs$upDist >= 0))
	expect_s3_class(site.list_updist$obs, "sf")
	expect_true("pred1km" %in% names(site.list_updist))
	expect_equal(dim(site.list_updist$pred1km), c(175, 14))
	expect_true("upDist" %in% names(site.list_updist$pred1km))
	expect_equal(site.list_updist$pred1km$upDist[1:2], c(16458.27, 15458.27), tolerance = 0.01)
	expect_true(all(site.list_updist$pred1km$upDist >= 0))
	expect_s3_class(site.list_updist$pred1km, "sf")
})



test_that("updist_sites stops when a site is on an edge with no upDist", {
	e <- no_outlet_lsn()
	sites <- no_outlet_sites(e)
	e$Length <- as.numeric(sf::st_length(e))
	e$upDist <- ifelse(e$rid %in% 5:6, 100, NA)
	expect_error(updist_sites(list(obs = sites), e, length_col = "Length",
		lsn_path = attr(e, "lsn_path"), save_local = FALSE),
		"1 site\\(s\\) in obs are on edges with no upDist \\(rid 1\\)")
})
