identify_outlet_segment <- function(relationship_table, edges){
  
  # Not defensively programmed yet...
  if(is.character(relationship_table)) relationship_table <- read.csv(relationship_table)
  if(is.character(edges)) edges <- st_read(edges)
  
  # Search for segments that don't flow anywhere
  all_rids <- edges$rid
  outlet <- all_rids[!all_rids %in% relationship_table$fromedge] 
  return(outlet)
  
}
