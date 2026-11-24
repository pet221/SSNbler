## Stop if any edge would be dropped from the results, 
## whatever the cause. 
## Input arguments
##   input.rids: numeric vector of rids for all edges; 
##   output.rids: numeric vector of rids for edges with results; 
##   value.name: character string of value being calculated

check_edges_kept <- function(input.rids, output.rids, value.name) {
	
  missing.rids <- input.rids[!(input.rids %in% output.rids)]
  
  if (length(missing.rids) > 0) {
    stop(value.name, " could not be calculated for ", length(missing.rids),
      " edge(s) (rid ", format_rids(missing.rids),
      "). edges.gpkg has not been changed. Use lines_to_lsn() with check_topology = TRUE to check the LSN for topology errors.",
      call. = FALSE)
  }
}
