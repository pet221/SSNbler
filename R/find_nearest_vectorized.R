find_nearest_vectorized <- function(x, sites, edges){
  st_nearest_feature(sites[x,], edges[[x]])
}