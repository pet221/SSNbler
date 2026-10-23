
grid_pairs <- function(xy, tol) {

	## Returns a two-column matrix (i, j), i != j, of row indices of xy
	## for points <= tol apart. Each unordered pair is returned once.
	## Points are binned on a grid so each point is only compared with
	## points in its own and neighbouring cells.
	n <- nrow(xy)
	if (tol > 0) {
		## Cells slightly wider than tol, so floating-point error in
		## floor() can't put two points within tol more than one cell
		## apart. + 0 turns -0 into 0 so the sprintf() keys below match.
		h <- tol * 1.0001
		cx <- floor(xy[, 1] / h) + 0
		cy <- floor(xy[, 2] / h) + 0
		## Own cell plus half of the 8 neighbours, so each pair of cells
		## is visited once
		offsets <- rbind(c(0, 0), c(1, -1), c(1, 0), c(1, 1), c(0, 1))
	} else {
		## tol = 0: a cell is an exact coordinate
		cx <- xy[, 1]
		cy <- xy[, 2]
		offsets <- rbind(c(0, 0))
	}

	## Group points by cell: radix sort, new cell wherever cx or cy changes
	o <- order(cx, cy, method = "radix")
	scx <- cx[o]
	scy <- cy[o]
	new.cell <- c(TRUE, scx[-1] != scx[-n] | scy[-1] != scy[-n])
	cell.start <- which(new.cell)
	cell.size <- diff(c(cell.start, n + 1))
	cell.x <- scx[cell.start]
	cell.y <- scy[cell.start]

	## Cell indices can exceed R's integer maximum, so use exact string
	## keys for the neighbour lookup
	if (nrow(offsets) > 1) cell.key <- sprintf("%.0f_%.0f", cell.x, cell.y)

	pairs <- vector("list", nrow(offsets))
	for (k in seq_len(nrow(offsets))) {
		if (k == 1) {
			a <- b <- seq_along(cell.start)
		} else {
			b.all <- match(sprintf("%.0f_%.0f", cell.x + offsets[k, 1],
														 cell.y + offsets[k, 2]), cell.key)
			a <- which(!is.na(b.all))
			b <- b.all[a]
		}

		## Expand each pair of cells (a, b) into all pairs of their points
		n.pair <- cell.size[a] * cell.size[b]
		blk <- rep(seq_along(a), n.pair)
		pos <- sequence(n.pair) - 1
		sb <- cell.size[b][blk]
		i.pos <- pos %/% sb
		j.pos <- pos %% sb
		if (k == 1) {
			## Within a cell keep each unordered pair once, no self pairs
			keep <- i.pos < j.pos
			blk <- blk[keep]
			i.pos <- i.pos[keep]
			j.pos <- j.pos[keep]
		}
		pairs[[k]] <- cbind(i = o[cell.start[a][blk] + i.pos],
												j = o[cell.start[b][blk] + j.pos])
	}
	pairs <- do.call(rbind, pairs)

	## Exact double-precision distance
	d <- sqrt((xy[pairs[, 1], 1] - xy[pairs[, 2], 1])^2 +
							(xy[pairs[, 1], 2] - xy[pairs[, 2], 2])^2)

	return(pairs[d <= tol, , drop = FALSE])
}
