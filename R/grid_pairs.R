## Returns a two-column matrix (i, j), where i != j, of row indices of xy
## where end nodes <= tol apart. Each node pair is returned only once.
## End nodes are binned on a grid so each node is only compared with
## nodes in its own cell and neighbouring cells.

grid_pairs <- function(xy, tol) {

	## Get total number of node coordinates
	n.xy <- nrow(xy)
	
	## Set up grid cells to search when tol > 0
	if (tol > 0) {
		## Set cell size slightly wider than tol to ensure two nodes
		## within tol aren't more than one cell apart.
		cell.width <- tol * 1.0001
		node.col <- floor(xy[, 1] / cell.width) + 0 ## + 0 turns -0 to 0
		node.row <- floor(xy[, 2] / cell.width) + 0
		## Direction to move from source cell (0,0) to one of its neighbours, 
		## measured in cells (x, y directions). Limit directional
		## comparisons to ensure pairs aren't counted twice.
		offsets <- rbind(c(0, 0), c(1, -1), c(1, 0), c(1, 1), c(0, 1))
		
	## No snapping, a cell is an exact coordinate when tol == 0
	} else {
		node.col <- xy[, 1]
		node.row <- xy[, 2]
		offsets <- rbind(c(0, 0))
	}
	
	## Group nodes by cell ##
	
	## Sort nodes by cell (column, then row)
	o <- order(node.col, node.row, method = "radix")
	sorted.node.col <- node.col[o]
	sorted.node.row <- node.row[o]
	
	## Create indicator vector of where node's cell location
	## differs from previous node. First cell always TRUE.
	new.cell.ind <- c(TRUE, sorted.node.col[-1] != sorted.node.col[-n.xy] | 
									sorted.node.row[-1] != sorted.node.row[-n.xy])
	
	## Get first position of each new cell
	cell.start <- which(new.cell.ind)
	
	## Get number of end nodes in each cell
	cell.size <- diff(c(cell.start, n.xy + 1))
	
	## Get row and column for each new cell
	cell.col <- sorted.node.col[cell.start]
	cell.row <- sorted.node.row[cell.start]

	## Give cells a text name to avoid exceeding R's integer maximum
	if (nrow(offsets) > 1) {
		cell.key <- sprintf("%.0f_%.0f", cell.col, cell.row)
	}
 
	## Create empty list to store pairs per offset
	pairs <- vector("list", nrow(offsets))
	
	## For each neighbouring cell (source cell + 4 neighbours)
	for (k in seq_len(nrow(offsets))) {
		
		## Compare each cell with itself
		if (k == 1) {
			from.cell <- to.cell <- seq_along(cell.start)
		
		## Compare each cell to its neighbour
		} else {
			to.cell.all <- match(sprintf("%.0f_%.0f", cell.col + offsets[k, 1],
														 cell.row + offsets[k, 2]), cell.key)
			from.cell <- which(!is.na(to.cell.all))
			to.cell <- to.cell.all[from.cell]
		}

		## Get number of node pairs in the two cells
		n.pair <- cell.size[from.cell] * cell.size[to.cell]
		
		## Create a vector with an element for each node pair, 
		## indicating which cell pair block (from.cell, to.cell) it belongs to
		cell.pair.id <- rep(seq_along(from.cell), n.pair)
		
		## Get pair position within the cell.pair.id block, starting at 0
		pair.pos <- sequence(n.pair) - 1
		
		## Get number of nodes in to.cell
		to.cell.n <- cell.size[to.cell][cell.pair.id]
		
		## Get node position within from.cell and to.cell
		from.node.pos <- pair.pos %/% to.cell.n ## Which node in from.cell
		to.node.pos <- pair.pos %% to.cell.n  ## Which node in to.cell
		
		## Within from.cell keep each node pair once, no self pairs
		if (k == 1) {
			keep <- from.node.pos < to.node.pos
			cell.pair.id <- cell.pair.id[keep]
			from.node.pos <- from.node.pos[keep]
			to.node.pos <- to.node.pos[keep]
		}

		## Convert node pairs' cell relative positions into row numbers in xy ##
		## Get the sorted position of the first node in each from/to cell.
		## Expand from one value per cell pair to one value per node pair.
		## Add the from/to node's position within its cell to get its sorted position.
		## Use o to convert sorted positions to row numbers in xy.
		pairs[[k]] <- cbind(i = o[cell.start[from.cell][cell.pair.id] + from.node.pos],
												j = o[cell.start[to.cell][cell.pair.id] + to.node.pos])
	}
	## Convert list to matrix
	pairs <- do.call(rbind, pairs)

	## Calculate exact double-precision distance
	d <- sqrt((xy[pairs[, 1], 1] - xy[pairs[, 2], 1])^2 +
							(xy[pairs[, 1], 2] - xy[pairs[, 2], 2])^2)

	return(pairs[d <= tol, , drop = FALSE])
}
