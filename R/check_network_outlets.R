## Stop if any edge is in a network without an outlet (e.g. a loop of
## edges, with or without edges flowing into it). 

check_network_outlets <- function(lsn_path) {
  node.relate <- read.csv(paste0(lsn_path, "/noderelationships.csv"))
  edge.relate <- read.csv(paste0(lsn_path, "/relationships.csv"))
  all.rids <- node.relate$rid

  ## Outlets edges don't flow into another edge
  outlet.rids <- all.rids[!(all.rids %in% edge.relate$fromedge)]

  ## Traverse upstream from all outlets: graph edges point from each
  ## edge to the upstream edges, plus a start vertex ("start")
  ## pointing to every outlet. Edges not reached have no outlet
  
  ## Build a data.frame of edges found in path upstream of 
  ## every outlet node ("start")
  flow.links <- data.frame(
    from = c(rep("start", 
    						 length(outlet.rids)), 
    				 as.character(edge.relate$toedge)),
    to = c(as.character(outlet.rids), 
    			 as.character(edge.relate$fromedge))
  )
  
  ## Convert  to an igraph network. vertices ensures all edges 
  ## are in network, even if missing from flow.links
  flow.graph <- graph_from_data_frame(
  	flow.links, 
  	directed = TRUE,
    vertices = data.frame(name = c("start", as.character(all.rids))))
  
  ## find rids that were not in an upstream path
  reached <- names(subcomponent(flow.graph, "start", mode = "out"))
  no.outlet.rids <- all.rids[!(as.character(all.rids) %in% reached)]

  if (length(no.outlet.rids) > 0) {
    stop(length(no.outlet.rids), " edge(s) (rid ", 
    		 format_rids(no.outlet.rids),
      ") are in a network without an outlet (e.g. loop of edges). Use lines_to_lsn() with check_topology = TRUE to locate Network Without Outlet errors.",
      call. = FALSE)
  }
}
