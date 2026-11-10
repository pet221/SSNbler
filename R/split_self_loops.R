## Re-groups nodes so the from and to nodes of a line are never
## assigned the same node. Only groups (memb) that contain both ends of a
## line are re-grouped; all other groups are returned unchanged.
## 
## Inputs:
## end.pairs: two-column matrix of end node indices <= snap_tolerance
##   apart (from grid_pairs())
## end.xy: line end node coordinates in the order from1, to1, from2, 
##   to2, ...
## memb: group id of each end node (from igraph::components())
##
## Returns a list of length 2 with memb (new group/node ids) and blocked
## (indices of end nodes in pairs that were within snap_tolerance but 
## not merged). 

split_self_loops <- function(end.pairs, end.xy, memb) {

	## Get total number of node coordinates
	n.xy <- nrow(end.xy)

	## Rows of end.xy containing from (upstream) and to (downstream)
	## end nodes
	ind.from <- seq(1, n.xy, by = 2)
	ind.to <- seq(2, n.xy, by = 2)

	## Create vector of edge ids based on row
	edge.id <- (seq_len(n.xy) + 1) %/% 2

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
	## Ties are broken by pairs$i and then pairs$j. 
	pairs <- pairs[order(d, pairs[, 1], pairs[, 2]), , drop = FALSE]

	## Union-find operation over nodes associated with loop.groups. 
	## edge.sets holds the lines with a node in each group; lines with
	## identical ends are omitted so they never block a merge. --- ##
  
	## Renumber the line end nodes in each pair so they match 
  ## numbering used inside merge loop below.
	pair.i <- match(pairs[, 1], local.ends)
	pair.j <- match(pairs[, 2], local.ends)

	## Put each line end node in its own node group to start,
	## before merging takes place
	parent <- seq_along(local.ends)

	## For each node, keep a list of lines that are associated with it. 
	## The first step is to add the line the node belongs to.
	edge.sets <- as.list(edge.id[local.ends])

	## Self-loop lines are left out of edge.sets, so they don't block
	## a merge
	edge.sets[same.xy[edge.id[local.ends]]] <- list(integer(0))

	## Create a vector of logical values (all initially FALSE) that 
	## records whether a pair is not merged and needs to be reported
	blocked <- logical(nrow(pairs))

	## Find which node a line end node is currently associated with
	find_node_label <- function(a) {
		while (parent[a] != a) a <- parent[a]
		a
	}

	## Check whether a blocked set of end nodes is a copy of a line's
	## own two end nodes. If so, it has been correctly excluded and 
	## the pair is not reported as unsnapped node
	matches_line_ends <- function(end.a, end.b, edges) {
		from.ends <- 2 * edges - 1
		to.ends <- 2 * edges
		same_xy <- function(end, ends) {
			end.xy[ends, 1] == end.xy[end, 1] & end.xy[ends, 2] == end.xy[end, 2]
		}
		any((same_xy(end.a, from.ends) & same_xy(end.b, to.ends)) |
				(same_xy(end.a, to.ends) & same_xy(end.b, from.ends)))
	}

	## Merge pairs in order, closest first
	for (k in seq_len(nrow(pairs))) {

		## Get group labels associated with two end nodes in pair k
		node.label.i <- find_node_label(pair.i[k])
		node.label.j <- find_node_label(pair.j[k])

		## Already in the same group (merged through other pairs)
		if (node.label.i == node.label.j) next

		## Does the line have an end node in each node label group?
		blocking.edges <- edge.sets[[node.label.i]][edge.sets[[node.label.i]] %in%
			edge.sets[[node.label.j]]]

		## Merging would put both ends of a line in one group: skip the
		## merge. Report the pair unless it is equivalent to that line's
		## own two end nodes
		if (length(blocking.edges) > 0) {
			blocked[k] <- !matches_line_ends(pairs[k, 1], pairs[k, 2], 
																			 blocking.edges)
			next
		}

		## Merge group node.label.j into group node.label.i, and combine
		## their lines
		parent[node.label.j] <- node.label.i
		edge.sets[[node.label.i]] <- c(edge.sets[[node.label.i]],
			edge.sets[[node.label.j]])
		edge.sets[node.label.j] <- list(NULL)
	}

	## Create final group/node label for affected end nodes
	node.labels <- vapply(seq_along(local.ends), find_node_label, integer(1))

	## Add max(memb) to node label to ensure node labels are unique
	memb[local.ends] <- max(memb) + match(node.labels, unique(node.labels))

	## Return new node labels and the end nodes for reported blocked pairs.
  ## To be reported as Unsnapped Node errors
	return(list(memb = memb,
							blocked = unique(c(pairs[blocked, 1], pairs[blocked, 2]))))
}
