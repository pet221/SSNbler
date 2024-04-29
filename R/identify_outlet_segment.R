#' @title Get outlet segment identifiers
#' @description Get the rid values for the outlet segment(s) of an edge network created using \code{lines_to_lsn}
#' @param relationship_table File path to the \code{relationships.csv} file created \code{lines_to_lsn}. 
#' @param edges File path to shapefile of stream segments for the stream network, or this as an \code{sf} object.
#' @return Numeric vector. RID value(s) for the outlet segment.
#' @export
identify_outlet_segment <- function(relationship_table, edges){
  
  # Not defensively programmed yet...
  if(is.character(relationship_table)) relationship_table <- read.csv(relationship_table)
  if(is.character(edges)) edges <- st_read(edges)
  
  # Search for segments that don't flow anywhere
  all_rids <- edges$rid
  outlet <- all_rids[!all_rids %in% relationship_table$fromedge] 
  return(outlet)
  
}
