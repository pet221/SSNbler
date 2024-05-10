v_snapping <- function(x, sites, nearest_subsegs){
  sg.col <- attributes(sites)$sf_column
  st_nearest_points(sites[x, sg.col], nearest_subsegs[[x]])
}
