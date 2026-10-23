
split_self_loops <- function(end.pairs, end.xy, memb) {

	## Re-groups line ends so the from and to ends of a line are never
	## in the same node. Only groups (memb) that contain both ends of a
	## line are re-grouped; all other groups are returned unchanged.
	##
	## end.pairs: two-column matrix of end indices <= snap_tolerance
	##   apart (from grid_pairs())
	## end.xy: end coordinates in the order from1, to1, from2, to2, ...
	## memb: group id of each end (from igraph::components())
	##
	## Within the affected groups, pairs are merged closest first and a
	## merge is skipped if it would put both ends of one line in the
	## same node. Lines whose two ends have identical coordinates can't
	## be separated and are left as self-loops.
	##
	## Returns a list with memb (new group ids) and blocked (indices of
	## ends in pairs that were within snap_tolerance but not merged).
	n.ends <- nrow(end.xy)
	ind.from <- seq(1, n.ends, by = 2)
	ind.to <- seq(2, n.ends, by = 2)
	edge.of.end <- (seq_len(n.ends) + 1) %/% 2

	same.xy <- end.xy[ind.from, 1] == end.xy[ind.to, 1] &
		end.xy[ind.from, 2] == end.xy[ind.to, 2]
	loop.edges <- which(memb[ind.from] == memb[ind.to] & !same.xy)

	if (length(loop.edges) == 0) {
		return(list(memb = memb, blocked = integer(0)))
	}

	## Ends and pairs in the affected groups
	loop.groups <- unique(memb[ind.from[loop.edges]])
	local.ends <- which(memb %in% loop.groups)
	pairs <- end.pairs[memb[end.pairs[, 1]] %in% loop.groups, , drop = FALSE]
	d <- sqrt((end.xy[pairs[, 1], 1] - end.xy[pairs[, 2], 1])^2 +
							(end.xy[pairs[, 1], 2] - end.xy[pairs[, 2], 2])^2)
	pairs <- pairs[order(d, pairs[, 1], pairs[, 2]), , drop = FALSE]

	## Union-find over the affected ends. edge.sets holds the lines with
	## an end in each group; lines with identical ends are left out so
	## they never block a merge.
	pair.i <- match(pairs[, 1], local.ends)
	pair.j <- match(pairs[, 2], local.ends)
	parent <- seq_along(local.ends)
	edge.sets <- as.list(edge.of.end[local.ends])
	edge.sets[same.xy[edge.of.end[local.ends]]] <- list(integer(0))
	blocked <- logical(nrow(pairs))

	find_root <- function(a) {
		while (parent[a] != a) a <- parent[a]
		a
	}

	for (k in seq_len(nrow(pairs))) {
		ri <- find_root(pair.i[k])
		rj <- find_root(pair.j[k])
		if (ri == rj) next
		if (any(edge.sets[[ri]] %in% edge.sets[[rj]])) {
			blocked[k] <- TRUE
			next
		}
		parent[rj] <- ri
		edge.sets[[ri]] <- c(edge.sets[[ri]], edge.sets[[rj]])
		edge.sets[rj] <- list(NULL)
	}

	## New group ids for the affected ends, after the existing ids
	roots <- vapply(seq_along(local.ends), find_root, integer(1))
	memb[local.ends] <- max(memb) + match(roots, unique(roots))

	return(list(memb = memb,
							blocked = unique(c(pairs[blocked, 1], pairs[blocked, 2]))))
}
