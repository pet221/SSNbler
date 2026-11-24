## Re-groups nodes so snapping never makes lines flow in a loop: the
## from and to nodes of a line, or the two ends of a chain of short lines,
## are never snapped. Only groups (memb) that contain both ends of a
## line are re-grouped here; all other groups are returned unchanged.
## 
## Inputs:
## end.pairs: two-column matrix of end node indices <= snap_tolerance
##   apart (from grid_pairs)
## end.xy: line end node coordinates in the order from1, to1, from2, 
##   to2, ... 
## memb: group id of each end node (from igraph::components)
##
## Returns a list of length 2 with memb (new group/node ids) and blocked
## (indices of end nodes in pairs that were within snap_tolerance but 
## not snapped). 

prevent_snapped_loops <- function(end.pairs, end.xy, memb) {

	## Get total number of node coordinates
	n.xy <- nrow(end.xy)

	## Rows of end.xy containing from (upstream) and to (downstream)
	## end nodes
	ind.from <- seq(1, n.xy, by = 2)
	ind.to <- seq(2, n.xy, by = 2)

	## Create indicator of edges with identical from/to node coords
	same.xy <- end.xy[ind.from, 1] == end.xy[ind.to, 1] &
		end.xy[ind.from, 2] == end.xy[ind.to, 2]

	## Identify edges with from/to nodes in the same group, 
	## excluding the self-loops from same.xy
	loop.edges <- which(memb[ind.from] == memb[ind.to] & !same.xy)

	## Nothing to split: return the groups unchanged, no blocked ends
	if (length(loop.edges) == 0) {
		return(list(memb = memb, blocked = integer(0)))
	}

	## Find snapped groups/nodes that create looped edges
	loop.groups <- unique(memb[ind.from[loop.edges]])

	## Find line end nodes associated with nodes in loop.groups. 
	local.ends <- which(memb %in% loop.groups)

	## Keep line end node pairs that need to be rebuilt and 
	## ignore the rest
	pairs <- end.pairs[memb[end.pairs[, 1]] %in% loop.groups, , drop = FALSE]

	## Calculate distance between end nodes in each pair
	d <- sqrt((end.xy[pairs[, 1], 1] - end.xy[pairs[, 2], 1])^2 +
							(end.xy[pairs[, 1], 2] - end.xy[pairs[, 2], 2])^2)

	## Sort node pairs by distance, so closest end nodes are merged first.
	## Ties are broken by the coordinates of the pair's end nodes 
	## (smaller x, then y, first)
	end.a <- pairs[, 1]
	end.b <- pairs[, 2]
	a.first.ind <- end.xy[end.a, 1] < end.xy[end.b, 1] |
		(end.xy[end.a, 1] == end.xy[end.b, 1] &
			end.xy[end.a, 2] <= end.xy[end.b, 2])
	low.end <- ifelse(a.first.ind == TRUE, end.a, end.b)
	high.end <- ifelse(a.first.ind == TRUE, end.b, end.a)
	pair.order <- order(d, end.xy[low.end, 1], end.xy[low.end, 2],
		end.xy[high.end, 1], end.xy[high.end, 2])
	pairs <- pairs[pair.order, , drop = FALSE]
	d <- d[pair.order]

	## Renumber the line end nodes in each pair so they match 
	## numbering used inside merge loop below
	pair.i <- match(pairs[, 1], local.ends)
	pair.j <- match(pairs[, 2], local.ends)

	## Put each line end node in its own node group to start,
	## before merging takes place
	snapped <- seq_along(local.ends)

	## Lines with both end nodes in these groups, renumbered to match
	## local.ends. Self-loop lines are left out, so they never block a
	## merge
	local.from <- match(ind.from, local.ends)
	local.to <- match(ind.to, local.ends)
	local.edges <- which(is.na(local.from) == FALSE &
		is.na(local.to) == FALSE & same.xy == FALSE)

	## Group end nodes that are connected through lines. 
	## Initially, each group = two end nodes of a line. 
	## Groups will be combined below each time a snap is made. 
	linked <- seq_along(local.ends)
	for (e in local.edges) {
		linked <- join_node_labels(local.from[e], local.to[e], linked)
	}

	## Group end nodes with identical coordinates (prior to snapping). 
	## Include both end nodes for the two associated lines in the group. 
	exact <- linked
	for (k in which(d == 0)) {
		exact <- join_node_labels(pair.i[k], pair.j[k], exact)
	}

	## Create a vector of logical values (all initially FALSE) that 
	## records whether a pair is not merged and needs to be reported
	blocked <- logical(nrow(pairs))

	## Merge pairs in order, closest first
	for (k in seq_len(nrow(pairs))) {

		## Get group labels associated with two end nodes in pair k
		node.label.i <- find_node_label(pair.i[k], snapped)
		node.label.j <- find_node_label(pair.j[k], snapped)

		## Already in the same group (merged through other pairs)
		if (node.label.i == node.label.j) next

		## Snapping end nodes that are already linked would make lines flow
		## in a loop: skip the merge. End nodes with identical coordinates
		## (d == 0) are always joined. Report the pair unless the end nodes
		## are already exact
		linked.ind <- find_node_label(pair.i[k], linked) ==
			find_node_label(pair.j[k], linked)
		if (d[k] > 0 && linked.ind == TRUE) {
			blocked[k] <- find_node_label(pair.i[k], exact) !=
				find_node_label(pair.j[k], exact)
			next
		}

		## Merge group node.label.j into group node.label.i
		snapped[node.label.j] <- node.label.i
		linked <- join_node_labels(pair.i[k], pair.j[k], linked)
	}

	## Create final group/node label for affected end nodes
	node.labels <- vapply(seq_along(local.ends), 
												find_node_label, 
												integer(1),
												joined.to = snapped)

	## Add max(memb) to node label to ensure node labels are unique
	memb[local.ends] <- max(memb) + match(node.labels, unique(node.labels))

	## Return new node labels and end nodes for reported blocked pairs.
  ## To be reported as unsnapped node errors
	return(list(memb = memb,
							blocked = unique(c(pairs[blocked, 1], pairs[blocked, 2]))))
}
