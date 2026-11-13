## Lists the rids of the edges at each error node, as comma-separated
## text (e.g. "4, 7"). Used by lines_to_lsn() to add the up_rid and
## down_rid columns to node_errors.gpkg.
##
## Inputs:
## node: node id at one end of each edge (tonode for edges flowing in,
##   fromnode for edges flowing out), one value per row of node.relate
## rid: rid of each edge, in the same order as node
## error.nodes: unique pointids of the error nodes
## error.pointid: pointid of each row in the errors table (NA for errors
##   without a node)
##
## Returns a character vector with one value per row of the errors
## table: the rids, or NA where no edges meet the node or pointid is NA.

rids_at_nodes <- function(node, rid, error.nodes, error.pointid) {
	ind <- node %in% error.nodes
	rid.list <- split(rid[ind], factor(node[ind], levels = error.nodes))
	rid.text <- vapply(rid.list, paste, character(1), collapse = ", ")
	rid.text[lengths(rid.list) == 0] <- NA
	unname(rid.text[match(error.pointid, error.nodes)])
}
