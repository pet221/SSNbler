## Builds an LSN (check_topology = FALSE) with a network that has no
## outlet: lines 1-3 form a loop and line 4 flows into it. Lines 5 and 6
## are a separate valid river (5 -> 6). Returns the edges with an area
## column; the LSN is in attr(, "lsn_path")
no_outlet_lsn <- function() {
	x0 <- 878000
	y0 <- 1261000
	coords <- list(rbind(c(x0, y0), c(x0 + 100, y0)),
		rbind(c(x0 + 100, y0), c(x0 + 50, y0 + 80)),
		rbind(c(x0 + 50, y0 + 80), c(x0, y0)),
		rbind(c(x0 - 100, y0 - 100), c(x0, y0)),
		rbind(c(x0, y0 + 500), c(x0 + 100, y0 + 500)),
		rbind(c(x0 + 100, y0 + 500), c(x0 + 200, y0 + 500)))
	streams <- sf::st_sf(id = seq_along(coords),
		geometry = sf::st_sfc(lapply(coords, sf::st_linestring), crs = 3005))
	streams$area <- 1
	lsn.path <- tempfile()
	edges <- suppressWarnings(lines_to_lsn(streams, lsn_path = lsn.path,
		check_topology = FALSE, snap_tolerance = 0.3, overwrite = TRUE,
		verbose = FALSE))
	attr(edges, "lsn_path") <- lsn.path
	edges
}

## Sites on rid 1 (in the loop) and rid 5 (on the valid river) of
## no_outlet_lsn()
no_outlet_sites <- function(edges) {
	x0 <- 878000
	y0 <- 1261000
	sites <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(
		sf::st_point(c(x0 + 50, y0 + 1)), sf::st_point(c(x0 + 50, y0 + 501)),
		crs = 3005))
	sites_to_lsn(sites, edges, snap_tolerance = 5, save_local = FALSE,
		verbose = FALSE)
}
