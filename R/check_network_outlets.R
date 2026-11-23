## Stop if any edge is in a network without an outlet (e.g. a loop of
## edges, with or without edges flowing into it). 
check_network_outlets <- function(lsn_path) {
  node.relate <- read.csv(paste0(lsn_path, "/noderelationships.csv"))
  edge.relate <- read.csv(paste0(lsn_path, "/relationships.csv"))
  all.rids <- node.relate$rid

  ## Outlets are edges that don't flow into another edge
  outlet.rids <- all.rids[!(all.rids %in% edge.relate$fromedge)]

  ## Walk upstream from all outlets at once: graph edges point from each
  ## edge to the edges flowing into it, plus a start vertex ("start")
  ## pointing to every outlet. Edges not reached have no outlet
  flow.links <- data.frame(
    from = c(rep("start", length(outlet.rids)), as.character(edge.relate$toedge)),
    to = c(as.character(outlet.rids), as.character(edge.relate$fromedge))
  )
  flow.graph <- graph_from_data_frame(flow.links, directed = TRUE,
    vertices = data.frame(name = c("start", as.character(all.rids))))
  reached <- names(subcomponent(flow.graph, "start", mode = "out"))
  no.outlet.rids <- all.rids[!(as.character(all.rids) %in% reached)]

  if (length(no.outlet.rids) > 0) {
    stop(length(no.outlet.rids), " edge(s) (rid ", format_rids(no.outlet.rids),
      ") are in a network without an outlet (e.g. a loop of edges), so values can't be calculated for them. Use lines_to_lsn() with check_topology = TRUE to locate Network Without Outlet errors, fix them and re-create error free edges.",
      call. = FALSE)
  }
}
