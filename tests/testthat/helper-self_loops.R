## Builds an LSN with a Self-loop line (check_topology = FALSE) for the
## Self-loop checks in updist_edges(), afv_edges() and accum_edges().
## where = "inside": line 2 is a loop at the node where line 1 ends and
## line 3 starts. where = "outlet": line 4 is a loop at the outlet node,
## below lines 1-3. Returns the edges with an area column; the LSN is in
## attr(, "lsn_path")
self_loop_lsn <- function(where = c("inside", "outlet")) {
	where <- match.arg(where)
	x0 <- 878000
	y0 <- 1261000
	coords <- if (where == "inside") {
		list(rbind(c(x0 - 100, y0), c(x0, y0)),
			rbind(c(x0, y0), c(x0 + 50, y0 + 50), c(x0 + 50, y0 - 50), c(x0, y0)),
			rbind(c(x0, y0), c(x0, y0 - 100)))
	} else {
		list(rbind(c(x0, y0 + 300), c(x0, y0 + 100)),
			rbind(c(x0 - 100, y0 + 200), c(x0, y0 + 100)),
			rbind(c(x0, y0 + 100), c(x0, y0)),
			rbind(c(x0, y0), c(x0 + 50, y0 - 50), c(x0 - 50, y0 - 50), c(x0, y0)))
	}
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
