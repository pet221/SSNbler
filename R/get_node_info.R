get_node_info <- function(node_coords, from_point, to_point, snap_tolerance) {
  ## Find distances between nodes and the edge to/from end nodes.
  ## Returns rectangular distance matrix between nodes (rows)
  ## and edge end nodes (columns)
  from_x <- do.call(c, lapply(from_point, function(x) x[1]))
  from_y <- do.call(c, lapply(from_point, function(x) x[2]))
  to_x <- do.call(c, lapply(to_point, function(x) x[1]))
  to_y <- do.call(c, lapply(to_point, function(x) x[2]))
  node_coords_split <- split(node_coords, seq_len(NROW(node_coords)))
  names(node_coords_split) <- NULL
  node_info <- lapply(node_coords_split, function(x) {
    get_each_node_info(x, from_x, from_y, to_x, to_y, snap_tolerance)
  })
  node_info

}

get_each_node_info <- function(node_coords_split, from_x, from_y, to_x, to_y, snap_tolerance) {
  dist_vector_from_x <- (node_coords_split[[1]] - from_x)^2
  dist_vector_from_y <- (node_coords_split[[2]] - from_y)^2
  nodes_vs_from <- sqrt(dist_vector_from_x + dist_vector_from_y)
  dist_vector_to_x <- (node_coords_split[[1]]  - to_x)^2
  dist_vector_to_y <- (node_coords_split[[2]]  - to_y)^2
  nodes_vs_to <- sqrt(dist_vector_to_x + dist_vector_to_y)
  ## Summarise the number of node-edge intersections based on this distance matrix
  ## Returns a vector with length==nrow(node_coords) containing number of line
  ## segments flowing in/out
  nodes_vs_from_le_snap <- nodes_vs_from <= snap_tolerance
  nodes_vs_to_le_snap <- nodes_vs_to <= snap_tolerance
  n_outflow <- sum(nodes_vs_from_le_snap)
  n_inflow <- sum(nodes_vs_to_le_snap)
  ## Find any unsnapped intersections for line endpoints and line
  ## start points based on snap_tolerance. Apply to each row. Value
  ## of 0 means already at same location
  snap_check_1 <- sum(nodes_vs_to > 0 & nodes_vs_to_le_snap)
  snap_check_2 <- sum(nodes_vs_from > 0 & nodes_vs_from_le_snap)
  unsnapped_connection <- (snap_check_1 + snap_check_2) > 0
  list(
    n_outflow = n_outflow,
    n_inflow = n_inflow,
    unsnapped_connection = unsnapped_connection
  )
}