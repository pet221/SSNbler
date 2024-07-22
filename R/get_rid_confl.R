get_rid_confl <- function(from_point, to_point, snap_tolerance) {
  ## First and last point of each last segment, stored as an SF object
  # from_xy <- do.call(rbind, lapply(from_point, function(x) x[1:2]))
  from_x <- do.call(c, lapply(from_point, function(x) x[1]))
  from_y <- do.call(c, lapply(from_point, function(x) x[2]))
  # to_xy <- do.call(rbind, lapply(to_point, function(x) x[1:2]))

  rid_confl <- lapply(to_point, function(x) {
    get_each_rid_confl(x[1:2], from_x, from_y, snap_tolerance)
  })
  rid_confl
}

get_each_rid_confl <- function(to_xy_element, from_x, from_y, snap_tolerance) {
  ## Find the distance between the from_xy of each line segment and
  ## the to_xy of every line segment. from in the rows, tos in the columns
  # only relevant with matrices, can just do vector here because one row/column
  # dist_vector_x <- as.vector(outer(X = from_xy[, 1], Y = to_xy_element[1], FUN = function(X, Y) (X - Y)^2))
  # dist_vector_y <- as.vector(outer(X = from_xy[, 2], Y = to_xy_element[2], FUN = function(X, Y) (X - Y)^2))
  dist_vector_x <- (to_xy_element[1] - from_x)^2
  dist_vector_y <- (to_xy_element[2] - from_y)^2
  dist_vector <- sqrt(dist_vector_x + dist_vector_y)
  ## Returns a list == length(edges) with dist_matrix names containing
  ## the rid value for other flow-connected edges connecting to the
  ## same node. Does not capture flow-unconnected
  dist_vector <- which(dist_vector <= snap_tolerance)
}
