#' @title Convert lines to a landscape network
#' @description Convert an \code{sf} object containing features with
#'   LINESTRING geometry to a landscape network (LSN), which is a
#'   topological data model of streams/rivers represented as a
#'   directional graph embedded in 2-D geographic space. Relationship
#'   tables are created and topological relationships are checked.
#' @param streams An \code{sf} object with LINESTING geometry
#'   representing streams.
#' @param lsn_path Pathname to a directory in character format specifying
#'   where to store the outputs. The directory will be created if it
#'   does not already exist.
#' @param snap_tolerance Distance in map units. Two nodes separated by
#'   a distance less than or equal to \code{snap_tolerance} are
#'   assumed to be connected. Defaults to 0.
#' @param check_topology Logical. If \code{TRUE}, the edges will be
#'   checked for topological errors and potential node errors will be
#'   flagged in an output file called
#'   \code{node_errors.gpkg}. Defaults to \code{TRUE}.
#' @param topo_tolerance Distance in map units. Only used if
#'   \code{check_topology = TRUE}. Two nodes on the network separated
#'   by a distance less than or equal to \code{topo_tolerance} are
#'   flagged as potential topological errors in the network. Defaults
#'   to 0.
#' @param remove_ZM Logical. If \code{TRUE}, Z and M dimensions are
#'   removed from streams, if they exist. Default is \code{FALSE}.
#' @param overwrite Logical. If \code{TRUE}, output files will
#'   overwrite existing files with the same names. If \code{FALSE} and
#'   files sharing the same names as the outputs exist in the
#'   \code{lsn_path}, the function will exit early with an error.
#' @param verbose Logical. If \code{TRUE}, messages describing
#'   function progress will be printed to the console. Default is
#'   \code{TRUE}.
#' 
#' @details \code{lines_to_lsn} converts an \code{sf} object
#'   representing streams to a landscape network
#'   (LSN), which is a directional graph used to represent
#'   the topological and geographic relationships between line
#'   features, along with additional attribute information. 
#'   \code{streams} must have LINESTRING geometry and a projected coordinate system, rather
#'   than geographic coordinate system (i.e. not Longitude and Latitude).
#'
#' The LSN is saved to a local directory defined by \code{lsn_path} and has 5 components:
#' \itemize{ 
#'   \item{ nodes.gpkg: A geopackage of features with POINT geometry representing topologic breaks in the stream network such as pseudonodes, confluences, stream sources, or stream outlets. A column containing a unique node identifier, pointid, is included. }
#'   \item{ edges.gpkg: A geopackage of features with LINESTRING geometry representing flow paths (i.e. line features) from node to node. A new column named rid is added that contains a unique identifier for each line feature. }
#'   \item{nodexy.csv: a table with three columns: the pointid (unique node identifier), and the x-, y-coordinates for each node. }
#'   \item{noderelationships.csv: a table describing the relationship between the nodes and their associated edges, as well as directionality in the LSN. The table contains three columns: the rid (for each edge), fromnode (pointid for the upstream node of each edge), and tonode (pointid for the downstream node of each edge). Directionality is defined based on the digitized direction of the line features. }
#'   \item{relationships.csv: a table representing the downstream flow path from edge to edge. The table contains two columns: fromedge (upstream edge) and toedge (downstream edge). }
#' }
#' 
#' Topological errors are common in spatial data and must be corrected
#' to ensure that the LSN accurately represents direction and
#' connectivity within the stream network. When \code{check_topology =
#' TRUE}, the edges are checked for topological errors. Two nodes on
#' the network separated by a distance less than or equal to
#' \code{topo_tolerance} are flagged as potential topological errors
#' in the network saved in an output file called
#' \code{node_errors.gpkg}, which is also saved to \code{lsn_path}. In
#' addition to the pointid, \code{node_errors.gpkg} contains a column
#' containing the node class (nodecat), which can take on values of
#' Pseudonode, Confluence, Source, or Outlet. A second column (error)
#' defines the potential error type and can take on values of Complex
#' Confluence, Converging Node, Dangling Node, Intersection Without
#' Node, Downstream Divergence and Unsnapped Node. The nodecat column
#' is also added to nodes.gpkg. A node_errors.gpkg file is not
#' produced if no obvious errors are identified. There is no guarantee
#' that all topological errors will be identified and included in
#' node_errors.gpkg. Therefore, potential errors *and* node classes
#' found in node_errors.gpkg and nodes.gpkg must be checked in a GIS
#' and topological errors in \code{streams} corrected before
#' rebuilding the LSN using \code{lines_to_lsn()}. This process is
#' iterative and must continue until the LSN is free of topological
#' errors.
#' 
#' @return An \code{sf} object representing edges in the LSN. The LSN, including edges.gpkg, nodes.gpkg, nodexy.csv, noderelationships.csv, and relationships.csv files, are saved locally to a directory defined by \code{lsn_path}. If \code{check_topology = TRUE} and topological errors are identified, then node_errors.gpkg is also saved to \code{lsn_path}.
#' 
#' @export
lines_to_lsn_temp <- function(streams, lsn_path,
                         snap_tolerance = 0,
                         topo_tolerance = 0, 
                         check_topology = TRUE,
                         remove_ZM = FALSE,
                         overwrite = FALSE,
                         verbose = TRUE) {
  
  ## bind nodecat variable to function
  nodecat <- NULL
  
  in_edges <- streams
  lst_crs <-st_crs(in_edges)
  
  ## Check that data are projected.
  if(st_is_longlat(in_edges) == TRUE) {
    stop("Streams data have geographic coordinates. Re-project streams to a cartesian coordinate system.")
  }
  
  ## Get geometry type as text
  edge_geom<- st_as_text(st_geometry(in_edges)[[1]])
  
  ## Check geometry type
  ##if(str_detect(edge_geom, "LINESTRING") == FALSE) {
  if(grepl("LINESTRING", edge_geom) == FALSE) {
    stop("Input streams must have LINESTRING geometry") }
  
  if(grepl("M", edge_geom) == TRUE & remove_ZM == FALSE) {
    ##if(str_detect(edge_geom, "M") == TRUE & remove_ZM == FALSE) {
    stop("XYM or XYZM geometries are not supported. Set remove_ZM = TRUE to drop Z and/or M dimensions from feature geometries.")
  }
  ## Remove ZM dimensions if they exist
  if(grepl("M", edge_geom) == TRUE & remove_ZM == TRUE) {
    ##if(str_detect(edge_geom, "M") == TRUE & remove_ZM == TRUE) {
    in_edges<- st_zm(in_edges)
  }     
  
  ## if output directory doesn't exist, print warning and create it now
  if (!file.exists(lsn_path)){
    ##message("\nOutput directory does not exist. Trying to create ", lsn_path,"\n\n")
    dir.create(lsn_path)
    # old and deprecated below
    # tryCatch(dir.create(lsn_path), error = function(e) e)   ## create directory
    ##cat("\nUsing output directory", lsn_path)
  }
  
  if (inherits(in_edges, "sf")) n_edges = nrow(in_edges)
  if (inherits(in_edges, "sfc")) n_edges = length(in_edges)
  
  ## Add the field 'rid' (meaning reach identifier) and use it in all scripts as the rid / edgeid
  in_edges$rid <- seq.int(from=1, to=n_edges) ## add and populate 1-based index rid column to edges
  
  ## Write edges to geopackage
  if(overwrite == FALSE & file.exists(paste0(lsn_path, "/edges.gpkg"))) {
    stop(paste0(lsn_path, "/edges.gpkg"), " exists and overwrite == FALSE")
  } else {
    st_write(in_edges, paste0(lsn_path, "/edges.gpkg"), layer = "edges", quiet = TRUE,
             delete_dsn = TRUE)
  }
  
  if(verbose == TRUE) message(paste0("\nSaved ", lsn_path, "/edges.gpkg", "\n"))
  
  ## create empty nodexy_mat matrix to be filled with  pointid, xcoord, ycoord  
  nodexy_mat <- matrix(data = NA, nrow = 3*n_edges, ncol = 3, byrow = TRUE)
  colnames(nodexy_mat) <- c("pointid","xcoord","ycoord") 
  
  ## Create empty noderelationships matrix to be filled with rid, fromnode and tonode pointid.
  ##cat("\n    Creating Relationship Tables....\n")
  node_relate <- matrix(data = NA, nrow = 3*n_edges, ncol = 3, byrow = TRUE)
  colnames(node_relate) <- c("rid","fromnode","tonode") 
  
  ## Create empty relationships matrix that rids for fromfeat and tofeat
  edge_relate <- matrix(data = NA, nrow = 3*n_edges, ncol = 2, byrow = TRUE)
  colnames(edge_relate) <- c("fromfeat","tofeat") 
  
  if(verbose == TRUE) message("Building Edge Relationships ...\n")
  
  ##### SLOW VERSION WITHOUT ERROR
  ## First and last point of each last segment, stored as an SF object
  ##from_point <- st_line_sample(st_transform(in_edges, epsg), sample = 0)
  # from_point <- st_line_sample(in_edges, sample = 0)
  # to_point <- st_line_sample(in_edges, sample = 1)
  # from_xy <- do.call(rbind, lapply(from_point, function(x) x[1:2]))
  # to_xy <- do.call(rbind, lapply(to_point, function(x) x[1:2]))
  # 
  # ## Find the distance between the from_xy of each line segment and
  # ## the to_xy of every line segment. from in the rows, tos in the columns
  # node_dist <- pdist(from_xy, to_xy)
  # 
  # ## convert to matrix for easier searching. row/column elements
  # ## labelled by rid
  # dist_matrix <- as.matrix(node_dist)
  # colnames(dist_matrix) <- rownames(dist_matrix) <- in_edges$rid
  # 
  # ## Returns a list == length(edges) with dist_matrix names containing
  # ## the rid value for other flow-connected edges connecting to the
  # ## same node. Does not capture flow-unconnected
  # rid_confl <- apply(dist_matrix, 2, function(x) which(x <= snap_tolerance))
  
  ##### FAST VERSION WITH ERROR
  from_point <- st_line_sample(in_edges, sample = 0)
  to_point <- st_line_sample(in_edges, sample = 1)
  rid_confl <- get_rid_confl(from_point, to_point, snap_tolerance)
  # each element does not have names on the vector here, but they do for previous
  names(rid_confl) <- in_edges$rid
  
  
  
  
  ## Find all outlet edges -- these are just edges that do not flow into another edge
  ## Returns a vector of TRUE/FALSE
  outlets <- unlist(lapply(rid_confl, function(x) length(x) == 0)) ## logical vector
  
  ##message("\n\nCreating nodes....\n")
  
  ## Get sfc of edge end node coordinates
  all_nodes <- st_line_sample(in_edges, sample = c(0, 1))
  
  ## Convert from MULTIPOINT to POINT
  all_nodes_as_single <- st_cast(all_nodes, "POINT")
  
  ## Get table of all edge end node coordinates
  to_from_coords <- st_coordinates(all_nodes)
  
  ## Remove to/from nodes with identical coordinates
  node_coords<- as.data.frame(unique(to_from_coords[,1:2]))
  
  ## Create nodexy table: get coords and assign pointid
  nodexy_mat <- node_coords
  pid <- seq(1, nrow(node_coords), by = 1)
  nodexy_mat<- as.data.frame(cbind(pid, nodexy_mat))
  colnames(nodexy_mat)<- c("pointid", "xcoord", "ycoord")
  
  ## Remove points with very very close coordinates
  ndec <- get_decimals(snap_tolerance / 1e4)
  tmp<- as.data.frame(apply(nodexy_mat[,2:3], 2, round, ndec))
  tmp<- data.frame(cbind(pointid=nodexy_mat$pointid, tmp))
  ind.dup <- duplicated(tmp[,2:3])
  
  ## Save table of removed nodes, new node pointid and coords
  removed_nodes <- tmp[ind.dup,]
  
  if(nrow(removed_nodes) > 0) {
    tmp<- tmp[!ind.dup,]
    colnames(tmp)[1]<- c("new.pointid")
    
    ## Get corresponding pointid (new.pointid) and remove rounded coords
    removed_nodes<- merge(removed_nodes, as.data.frame(tmp), by = c("xcoord", "ycoord"),
                          all.x = TRUE)
    removed_nodes <- removed_nodes[,!colnames(removed_nodes) %in% c("xcoord", "ycoord")]
    
    ## get unrounded coords for new.pointid (new.X, new.Y)
    removed_nodes <- merge(removed_nodes, nodexy_mat, by.x = "new.pointid", by.y = "pointid",
                           all.x = TRUE)
    colnames(removed_nodes)[3:4]<- c("new.X", "new.Y")
    
    ## get unrounded coords for old pointid (pointid)
    removed_nodes <- merge(removed_nodes, nodexy_mat, by.x = "pointid", by.y = "pointid",
                           all.x = TRUE)
    rm(tmp)
  }
  
  node_coords <- node_coords[!ind.dup,]
  
  ## Finish creating the sf object for all edge endpoints
  nodexy_sf <- st_as_sf(as.data.frame(nodexy_mat[!ind.dup,]),
                        coords = c("xcoord", "ycoord"), crs = lst_crs)
  
  ## Check topology at this step, if it's been asked for
  if(check_topology){
    
    ## Print message
    if(verbose == TRUE) message("Checking network topology\n")
    
    ##### SLOW VERSION WITHOUT ERROR
    # # Find distances between nodes and the edge to/from end nodes.
    # # Returns rectangular distance matrix between nodes (rows)
    # # and edge end nodes (columns)
    # from_xy <- do.call(rbind, lapply(from_point, function(x) x[1:2]))
    # to_xy <- do.call(rbind, lapply(to_point, function(x) x[1:2]))
    # nodes_vs_from <- as.matrix(pdist(node_coords, from_xy))
    # nodes_vs_to <- as.matrix(pdist(node_coords, to_xy))
    # 
    # ## Summarise the number of node-edge intersections based on this distance matrix
    # ## Returns a vector with length==nrow(node_coords) containing number of line
    # ## segments flowing in/out
    # n_inflow <- apply(nodes_vs_to, 1, function(x){sum(x <= snap_tolerance)})
    # n_outflow <- apply(nodes_vs_from, 1, function(x){sum(x <= snap_tolerance)})
    # 
    # ## Find any unsnapped intersections for line endpoints and line
    # ## start points based on snap_tolerance. Apply to each row. Value
    # ## of 0 means already at same location
    # snap_check_1 <- apply(nodes_vs_to, 1, function(x){sum(x > 0 & x <= snap_tolerance)})
    # snap_check_2 <- apply(nodes_vs_from, 1, function(x){sum(x > 0 & x <= snap_tolerance)})
    # unsnapped_connection <- (snap_check_1 + snap_check_2) > 0
    
    ##### FAST VERSION WITH ERROR
    node_info <- get_node_info(node_coords, from_point, to_point, snap_tolerance)
    n_inflow <- unlist(lapply(node_info, function(x) x$n_inflow))
    n_outflow <- unlist(lapply(node_info, function(x) x$n_outflow))
    unsnapped_connection <- unlist(lapply(node_info, function(x) x$unsnapped_connection))
    
    ## Categorise nodes based on the number of inflow/outflow edges
    ## Returns a vector with T/F length = nrow(node_coords)
    
    ## outlet nodes definition would also get converging confluences
    ##outlet_nodes <- (n_inflow > 0 & n_outflow == 0)
    outlet_nodes <- (n_inflow == 1 & n_outflow == 0)
    ##outlet_nodes <- (n_inflow > 0 & n_inflow <=2 & n_outflow == 0)
    
    headwaters <- (n_inflow == 0 & n_outflow == 1)
    confluences <- (n_inflow == 2 & n_outflow == 1)
    pseudo_node <- (n_inflow == 1 & n_outflow == 1)
    
    ## Identify errors
    complex_confluences <- n_inflow > 2 & n_outflow == 1
    ##converging_nodes <- n_inflow > 1 & n_outflow == 0
    converging_nodes <- n_inflow == 2 & n_outflow == 0
    downstream_divergence <- n_outflow > 1
    
    ## LOOK FOR UNCLASSIFIED NODES
    
    ## Add column of node categories
    nodexy_sf$nodecat <- NA
    nodexy_sf$nodecat[outlet_nodes] <- "Outlet"
    nodexy_sf$nodecat[pseudo_node] <- "Pseudonode"
    nodexy_sf$nodecat[headwaters] <- "Source"
    nodexy_sf$nodecat[confluences] <- "Confluence"
    ##nodexy_sf$nodecat[complex_confluences]<- "Complex Confluences"
    nodexy_sf$nodecat[complex_confluences]<- "Confluence"
    ##nodexy_sf$nodecat[converging_nodes]<- "Converging Node"
    nodexy_sf$nodecat[converging_nodes]<- "Outlet"
    ##nodexy_sf$nodecat[downstream_divergence]<- "Downstream Divergence"
    nodexy_sf$nodecat[downstream_divergence]<- "Confluence"
    
    ## Find dangling edges -- these are another kind of unsnapped connection
    ## Figure this out when using data with topo errors
    ##outlets_only <- filter(nodexy_sf, nodecat == "Outlet")
    outlets_only<- subset(nodexy_sf, nodecat == "Outlet")
    buff_outlets <- st_buffer(outlets_only, topo_tolerance) 
    buff_cross <- st_crosses(in_edges, buff_outlets)
    buff_lines <- sapply(buff_cross, length)
    buffer_ids<-  buff_cross[which(buff_lines > 0)]
    buffer_ids<- unlist(buffer_ids)
    
    crossings <- table(buffer_ids) 
    hanging_nodes <- unname(which(crossings > 1))
    
    ## Subset by node error, add an error column, combine at end
    unsnapped <- nodexy_sf[unsnapped_connection, ]
    if(nrow(unsnapped) > 0) unsnapped$error <- "Unsnapped Node"
    
    comp_conf <- nodexy_sf[complex_confluences, ]
    if(nrow(comp_conf) > 0) comp_conf$error <-  "Complex Confluence"
    
    conv_node <-nodexy_sf[converging_nodes, ]
    if(nrow(conv_node) > 0) conv_node$error = "Converging Node"
    
    down_dive <- nodexy_sf[downstream_divergence, ]
    if(nrow(down_dive) > 0) down_dive$error = "Downstream Divergence"
    
    hanging <- outlets_only[hanging_nodes, ]
    if(nrow(hanging) > 0) hanging$error = "Dangling Node"
    
    ## Combine errors - suppress warnings if there are no errors
    suppressWarnings(errors <-rbind(unsnapped, comp_conf, conv_node,
                                    down_dive, hanging))
    
    ## Find edge intersections without nodes
    ##edges.in <- in_edges
    sf::st_agr(in_edges)<- "constant"
    intersections <- st_intersection(in_edges, in_edges)
    points_only<- subset(intersections, st_geometry_type(intersections) == "POINT")
    
    ## Want all_nodes as single here because looking for edge intersections
    int_vs_nodes <- st_intersects(points_only, all_nodes_as_single)
    node_checks<- sapply(int_vs_nodes, length)
    illegal_intersections <- which(node_checks == 0)
    
    ill_int <- points_only[illegal_intersections, ]
    if(nrow(ill_int) > 0) {
      ill_int<- ill_int[!duplicated(ill_int$geometry),]
      ill_int$pointid <- NA
      ill_int$nodecat <- "Confluence"
      ill_int$error = "Intersection Without Node"
      ill_int<-
        ill_int[,c("pointid", "nodecat", "error")]
    }
    
    ## Identify additional dangling nodes
    ## This is where it identifies the removed dangle node
    ind.dangle <- which(node_checks == 1)
    dangle<- points_only[ind.dangle, ]
    if(nrow(dangle) > 0) {
      dangle$pointid <- NA
      dangle$nodecat = "Confluence"
      dangle$error = "Dangling Node"
      dangle <-
        dangle[,c("pointid", "nodecat", "error")]
      
      ## Remove removed_nodes from dangle error list
      if(nrow(removed_nodes) > 0) {
        dangle_xy<- as.data.frame(st_coordinates(dangle))
        colnames(dangle_xy)<- c("xcoord", "ycoord")
        dangle_xy$ord <- seq(1, nrow(dangle_xy), by = 1)
        removed_nodes$removed <- 1
        tmp<- merge(dangle_xy, removed_nodes[,c("xcoord", "ycoord", "removed")],
                    by = c("xcoord", "ycoord"), all.x = TRUE)
        tmp<- tmp[order(tmp$ord),]
        ind <- tmp$removed == 1 & !is.na(tmp$removed)
        dangle <- dangle[!ind,]
      }
    }
    
    ## Add to errors table   
    suppressWarnings(errors <- rbind(errors, ill_int, dangle))
    
    ## Writing node_errors
    if(overwrite == FALSE & file.exists(paste0(lsn_path, "/node_errors.gpkg"))) {
      stop(paste0(lsn_path, "/node_errors.gpkg"), " exists and overwrite == FALSE")
    } else {
      if(nrow(errors) == 0) {
        message("No obvious topological errors detected and node_errors.gpkg was NOT created.")
      }
      
      if(nrow(errors) > 0) {
        if(verbose == TRUE) message(paste0("Saving ", lsn_path, "/node_errors.gpkg"), "\n")
        st_write(errors, paste0(lsn_path, "/node_errors.gpkg"), layer = "node_errors",
                 quiet = TRUE,
                 delete_dsn = TRUE) 
      }
    }
  }
  
  ### Write nodes.gpkg 
  if(verbose == TRUE) message(paste0("Saving ", lsn_path, "/nodes.gpkg"), "\n")
  
  if(overwrite == FALSE & file.exists(paste0(lsn_path, "/nodes.gpkg"))) {
    stop(paste0(lsn_path, "/nodes.gpkg"), " exists and overwrite == FALSE")
  } else {
    st_write(nodexy_sf, paste0(lsn_path, "/nodes.gpkg"), layer = "nodes",
             quiet = TRUE,
             delete_dsn = TRUE)
  }
  
  ### Relationship tables
  if(verbose == TRUE) message("Building relationship tables....\n")
  
  ### Create noderelationships table
  first_indices <- seq(1, nrow(to_from_coords), 2)
  last_indices <- seq(2, nrow(to_from_coords), 2)
  
  ## From node pointid: should be no duplicates
  ## When there are
  ## duplicates in nodexy, left_join only keeps the first match,
  ## but merge keeps both so that the result has more rows than
  ## nrow(to_from_coords[last_indices,]
  from_pointids <- as.data.frame(to_from_coords[first_indices, c("X","Y")])
  from_pointids <- left_join(from_pointids, nodexy_mat, by = c("X" = "xcoord", "Y" = "ycoord"))
  from_pointids<- from_pointids$pointid
  
  ## To node pointid: There will be valid duplicates b/c of confluences
  to_pointids <- as.data.frame(to_from_coords[last_indices, c("X","Y")])
  to_pointids <- left_join(to_pointids, nodexy_mat, by = c("X" = "xcoord", "Y" = "ycoord"))
  to_pointids<- to_pointids$pointid
  
  node_relate <- data.frame(rid = in_edges$rid,
                            fromnode = from_pointids,
                            tonode = to_pointids)
  
  ## Replace removed nodes pointid with new node pointid value
  if(nrow(removed_nodes) > 0) {
    for(i in 1:nrow(removed_nodes)) {
      old.pid <- removed_nodes$pointid[i]
      ind<- node_relate$tonode == old.pid
      node_relate$tonode[ind]<- removed_nodes$new.pointid[i]
      ind<- node_relate$fromnode == old.pid
      node_relate$fromnode[ind]<- removed_nodes$new.pointid[i]
    }
  }
  
  ## Write noderelationships to file
  if(overwrite == FALSE & file.exists(paste0(lsn_path, "/noderelationships.csv"))) {
    stop(paste0(lsn_path, "/noderelationships.csv"), " exists and overwrite == FALSE")
  } else {
    ## Export noderelationshipsto a file.
    write.table(node_relate,
                file = paste(lsn_path,"noderelationships.csv", sep = "/"),
                sep = ",", col.names = TRUE, row.names = FALSE)
  }
  
  ## Remove duplicated nodes in nodexy matrix
  nodexy_mat<- nodexy_mat[!ind.dup,]
  ## Write nodexy to file
  write.table(nodexy_mat, file = paste(lsn_path, "nodexy.csv", sep = "/"),
              sep = ",", col.names = TRUE, row.names = FALSE)
  
  ## Create relationships table
  to_rids <- rid_confl  
  n_repetitions <- unlist(lapply(to_rids, length)) 
  n_repetitions[outlets] <- 1 ## these are outlets
  from_rids_base <- as.integer(names(to_rids))
  from_rids <- c()
  
  from_rids<- rep.int(from_rids_base, n_repetitions) 
  to_rids[outlets] <- NA 
  to_rids <- unname(unlist(to_rids))
  edge_relate <- data.frame(fromedge = from_rids, toedge = to_rids)
  ## Remove outlet segment records with missing toedge values
  edge_relate <- edge_relate[!is.na(edge_relate$toedge),]
  
  ## Export relationships to file.
  write.table (edge_relate, file = paste(lsn_path,"relationships.csv", sep = "/"),
               sep = ",", col.names = TRUE, row.names = FALSE)
  
  ## Final message
  if(verbose == TRUE) {
    message(
      paste0("FINISHED successfully. Don't forget to manually check for and correct errors in ",
             lsn_path, "/edges.gpkg before continuing."), "\n")
  }
  
  return(in_edges)
  
} 




