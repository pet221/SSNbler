v_snapping <- function(x, sites, nearest_subsegs){
  st_nearest_points(sites$geometry[x], nearest_subsegs[[x]]) 
}