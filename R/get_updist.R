get_updist <- function(x, sites, sub_idx, sub_near, lengths){
  up_to_nearest <- lengths[[x]][sub_idx[[x]]]
  from_nearest_to_site <- sqrt(sum((sites[x,] - sub_near[[x]][1, 1:2])^2))
  length_to_site <- up_to_nearest + from_nearest_to_site
  return(length_to_site)
}