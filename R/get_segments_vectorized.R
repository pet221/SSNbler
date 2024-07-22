## Creates list of LINESTRING sfg for each sub-segment in an edge using
## the xy coordinates of the line vertices as input

get_segments_vectorized <- function(x) {
  ## Get number of vertices
  nx <- nrow(x)

  ## Displace matrix of coordinates by one position and remove column L1
  x1 <- x[1:(nx - 1), -3]
  x2 <- x[2:nx, -3]

  ## In a vectorised fashion, create new matrices of sub-segments and
  ## store in a list
  segments <- lapply(1:(nx - 1), function(x) st_linestring(as.matrix(rbind(x1[x, ], x2[x, ]))))

  return(segments)
}
