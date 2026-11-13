## Checks whether a blocked pair of end nodes is a line's own
## two end nodes (identical coordinates). If so, the pair was correctly
## left unsnapped and is not reported as an Unsnapped Node. Used by
## split_self_loops().
##
## Inputs:
## end.a, end.b: Integer representing rows of end.xy for two end nodes in a pair
## edges: integer vector of row numbers for line(s) in streams that 
##        blocked the merge, in input order
## end.xy: matrix of line end node coordinates in the order from1, to1, from2,
##   to2, ...
##
## Returns TRUE if the pair matches the from and to end nodes of any of
## the lines in edges, otherwise FALSE.

matches_line_ends <- function(end.a, end.b, edges, end.xy) {
	from.ends <- 2 * edges - 1
	to.ends <- 2 * edges

	## Does end.a / end.b have identical coordinates to each line's
	## from / to end node?
	a.from <- end.xy[from.ends, 1] == end.xy[end.a, 1] & 
		end.xy[from.ends, 2] == end.xy[end.a, 2]
	a.to   <- end.xy[to.ends, 1] == end.xy[end.a, 1] & 
		end.xy[to.ends, 2] == end.xy[end.a, 2]
	b.from <- end.xy[from.ends, 1] == end.xy[end.b, 1] & 
		end.xy[from.ends, 2] == end.xy[end.b, 2]
	b.to   <- end.xy[to.ends, 1] == end.xy[end.b, 1] & 
		end.xy[to.ends, 2] == end.xy[end.b, 2]

	## The pair matches a line if one end node matches its from end node
	## and the other matches its to end node
	any((a.from & b.to) | (a.to & b.from))
}
