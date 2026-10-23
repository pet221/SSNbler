#' @title Convert lines to a landscape network (LSN)
#' @description Convert an `sf` object containing features with
#'   LINESTRING geometry to a landscape network (LSN), which is a
#'   topological data model of streams/rivers represented as a
#'   directional graph embedded in 2-D geographic space. Relationship
#'   tables are created and topological relationships are checked.
#' @param streams An `sf` object with LINESTRING geometry
#'   representing streams.
#' @param lsn_path Pathname to a directory in character format specifying
#'   where to store the outputs. The directory will be created if it
#'   does not already exist.
#' @param check_topology Logical. If \code{TRUE}, the edges will be
#'   checked for topological errors and potential node errors will be
#'   flagged in an output file called
#'   \code{node_errors.gpkg}. Defaults to \code{TRUE}.
#' @param snap_tolerance Distance in map units >= 0. Two nodes separated by
#'   a distance less than or equal to \code{snap_tolerance} are
#'   assumed to be connected. Defaults to 0.
#' @param topo_tolerance Distance in map units >= 0. Only used if
#'   \code{check_topology = TRUE}. Two nodes on the network separated
#'   by a distance less than or equal to \code{topo_tolerance} are
#'   flagged as potential topological errors in the network. Must be
#'   greater than \code{snap_tolerance} when \code{check_topology =
#'   TRUE} and \code{snap_tolerance} > 0. Defaults to 0.
#' @param remove_ZM Logical. If \code{TRUE}, Z and M dimensions are
#'   removed from streams, if they exist. If \code{FALSE} and
#'   \code{streams} has Z and/or M dimensions, the function stops with
#'   an error. Default is \code{FALSE}.
#' @param overwrite Logical. If \code{TRUE}, output files will
#'   overwrite existing files with the same names, and a
#'   node_errors.gpkg file from an earlier run is deleted. If
#'   \code{FALSE} and files sharing the same names as the outputs exist
#'   in the \code{lsn_path}, the function will exit early with an
#'   error.
#' @param verbose Logical. If \code{TRUE}, messages describing
#'   function progress will be printed to the console. Default is
#'   \code{TRUE}.
#'
#' @details \code{lines_to_lsn} converts an `sf` object
#'   representing streams to a landscape network
#'   (LSN), which is a directional graph used to represent
#'   the topological and geographic relationships between line
#'   features, along with additional attribute information.
#'   \code{streams} must have LINESTRING geometry and a projected coordinate system, rather
#'   than geographic coordinate system (i.e. not Longitude and Latitude).
#'
#' The LSN is saved to a local directory defined by \code{lsn_path} and has 5 components:
#' \itemize{
#'   \item{ nodes.gpkg: A GeoPackage of features with POINT geometry representing topologic breaks in the stream network such as pseudonodes, confluences, stream sources, or stream outlets. A column containing a unique node identifier, pointid, is included. }
#'   \item{ edges.gpkg: A GeoPackage of features with LINESTRING geometry representing flow paths (i.e. line features) from node to node. A new column named rid is added that contains a unique identifier for each line feature. }
#'   \item{nodexy.csv: a table with three columns: the pointid (unique node identifier), and the x-, y-coordinates for each node. }
#'   \item{noderelationships.csv: a table describing the relationship between the nodes and their associated edges, as well as directionality in the LSN. The table contains three columns: the rid (for each edge), fromnode (pointid for the upstream node of each edge), and tonode (pointid for the downstream node of each edge). Directionality is defined based on the digitized direction of the line features. }
#'   \item{relationships.csv: a table representing the downstream flow path from edge to edge. The table contains two columns: fromedge (upstream edge) and toedge (downstream edge). }
#' }
#'
#' Line end nodes separated by a Euclidean distance <=
#' \code{snap_tolerance} are snapped together into a single node, and
#' nodes, noderelationships.csv and relationships.csv are all built
#' from this rule. Snapping is transitive: if end A is within
#' \code{snap_tolerance} of end B, and B is within \code{snap_tolerance}
#' of end C, then A, B and C form one node even if A and C are further
#' apart. The upstream and downstream ends of the same line feature
#' are never snapped into one node. The node takes the coordinates of
#' the first line end assigned to it. There is no limit on the number
#' of line features in \code{streams}.
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
#' defines the potential error type and can take on values of:
#' \itemize{
#'   \item{Complex Confluence: three or more edges flow into the node.}
#'   \item{Converging Node: two edges flow into the node and none flow out.}
#'   \item{Dangling Node: an outlet within \code{topo_tolerance} of another edge, or an edge end that touches another edge without a node.}
#'   \item{Downstream Divergence: two or more edges flow out of the node.}
#'   \item{Intersection Without Node: two edges cross without a node.}
#'   \item{Self-loop: the upstream and downstream ends of a line feature have identical coordinates. The line feature is not included in relationships.csv.}
#'   \item{Unsnapped Node: a line end <= \code{snap_tolerance} from another line end was not snapped to it, because this would have put both ends of a line feature into one node (e.g. a line whose ends nearly meet, or a short line whose ends are both near the same node).}
#' }
#' The nodecat column
#' is also added to nodes.gpkg. A node_errors.gpkg file is not
#' produced if no obvious errors are identified. There is no guarantee
#' that all topological errors will be identified and included in
#' node_errors.gpkg. For example, a line end that stops just short of
#' another line (an undershoot) is not always reported. Therefore, potential errors *and* node classes
#' found in node_errors.gpkg and nodes.gpkg must be checked in a GIS
#' and topological errors in \code{streams} corrected before
#' rebuilding the LSN using \code{lines_to_lsn()}. This process is
#' iterative and must continue until the LSN is free of topological
#' errors.
#'
#' The arguments \code{snap_tolerance} and \code{topo_tolerance} are
#' both provided in map units and must be >= 0. When
#' \code{check_topology = TRUE} and \code{snap_tolerance} > 0,
#' \code{topo_tolerance} must be greater than \code{snap_tolerance}. It is generally a good
#' idea to set the \code{snap_tolerance} to a relatively small value
#' compared to the \code{topo_tolerance} argument. Nodes separated by
#' a Euclidean distance <= \code{snap_tolerance} are assumed to be
#' connected and are snapped together. When snap_tolerance < distance <= topo_tolerance, nodes are flagged as potential errors.
#'
#' WARNING: \code{snap_tolerance} must always be less than the length of the shortest line feature found in \code{streams}. Use the \code{\link[sf]{st_length}} to obtain and check the minimum length of the line features.
#'
#' @return An `sf` object representing edges in the LSN. The LSN, including edges.gpkg, nodes.gpkg, nodexy.csv, noderelationships.csv, and relationships.csv files, are saved locally to a directory defined by \code{lsn_path}. If \code{check_topology = TRUE} and topological errors are identified, then node_errors.gpkg is also saved to \code{lsn_path}.
#'
#' @export
#'
lines_to_lsn <- function(streams,
                         lsn_path,
                         check_topology = TRUE,
                         snap_tolerance = 0,
                         topo_tolerance = 0,
                         remove_ZM = FALSE,
                         overwrite = FALSE,
                         verbose = TRUE) {

	##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
	## Check Inputs ----
	##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

	# check sf object
  if (!inherits(streams, "sf")) {
    stop("streams must be an sf object.", call. = FALSE)
  }

  # check empty geometries
  if (any(st_is_empty(streams))) {
    stop("streams has at least one empty geometry. Check and/or use sf::st_is_empty() to remove.",
      call. = FALSE
    )
  }

  ## bind nodecat variable to function
  nodecat <- NULL

  in_edges <- streams
  lst_crs <- st_crs(in_edges)

  ## Check that data are projected.
  if (st_is_longlat(in_edges) == TRUE) {
    stop("Streams data have geographic coordinates. Re-project streams to a cartesian coordinate system.")
  }

  ## Get geometry type as text
  edge_geom <- st_as_text(st_geometry(in_edges)[[1]])

  ## Check geometry type
  if (grepl("LINESTRING", edge_geom) == FALSE | grepl("MULTILINESTRING", edge_geom) == TRUE) {
    if (grepl("MULTILINESTRING", edge_geom) == TRUE) {
      stop("Input streams must have LINESTRING geometry. Use sf::st_cast to convert from MULTILINESTRING to LINESTRING.")
    } else {
      stop("Input streams must have LINESTRING geometry.")
    }
  }

  edge.geom.name <- attributes(in_edges)$sf_column

  if (grepl("Z|M", edge_geom) == TRUE & remove_ZM == FALSE) {
    stop("XYZ, XYM or XYZM geometries are not supported. Set remove_ZM = TRUE to drop Z and/or M dimensions from feature geometries.")
  }

  ## Remove Z and/or M dimensions if they exist
  if (grepl("Z|M", edge_geom) == TRUE & remove_ZM == TRUE) {
    in_edges <- st_zm(in_edges)
  }

  ## if output directory doesn't exist, create it now
  if (!file.exists(lsn_path)) {
    dir.create(lsn_path)
  }

  ## Check for negative tolerances
  if (snap_tolerance < 0 | topo_tolerance < 0) {
    stop("snap_tolerance and topo_tolerance must be >= 0")
  }

  ## Check that topo_tolerance > snap_tolerance
  if (check_topology == TRUE & snap_tolerance > 0 & topo_tolerance <= snap_tolerance) {
    stop("topo_tolerance must be > snap_tolerance when check_topology = TRUE and snap_tolerance > 0")
  }

  ## Check that snap_tolerance < length of shortest line
  if(check_topology == TRUE & snap_tolerance > 0) {
  	sm_edges<- sum(as.numeric(st_length(in_edges)) < snap_tolerance)

  	if(sm_edges > 0) {
  		stop("snap_tolerance must be < than the length of the smallest line feature in streams. Use sf::st_length() to obtain line feature lengths.")
  	}
  }

  n_edges <- nrow(in_edges)

  # check column names
  if ("rid" %in% colnames(in_edges)) {
    if (overwrite) {
      in_edges$rid <- NULL
    } else {
      stop("rid already exists in edges and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(in_edges), "rid", "streams")

  ## If fid column exists and overwrite is TRUE
  if ("fid" %in% colnames(in_edges)) {
    if (overwrite) {
      in_edges$fid <- NULL
    } else {
      stop("fid already exists in edges and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(in_edges), "fid", "streams")

  ## Add the field rid
  in_edges$rid <- seq.int(from = 1, to = n_edges) ## add and populate 1-based index rid column to edges

  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  ## Build nodes ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

  ## Write edges to GeoPackage
  if (overwrite == FALSE & file.exists(paste0(lsn_path, "/edges.gpkg"))) {
    stop(paste0(lsn_path, "/edges.gpkg"), " exists and overwrite == FALSE")
  } else {
    st_write(in_edges, paste0(lsn_path, "/edges.gpkg"),
      layer = "edges", quiet = TRUE,
      delete_dsn = TRUE
    )
  }

  if (verbose == TRUE) message(paste0("\nSaved ", lsn_path, "/edges.gpkg", "\n"))

  ## Remove node_errors.gpkg from an earlier run, so it isn't mistaken
  ## for errors in this LSN when this run finds none
  if (overwrite == TRUE & file.exists(paste0(lsn_path, "/node_errors.gpkg"))) {
    unlink(paste0(lsn_path, "/node_errors.gpkg"))
  }

  if (verbose == TRUE) message("Building nodes and edge relationships ...\n")

  ## Edge end coordinates (first and last vertex of each line), in the
  ## order from1, to1, from2, to2, ... Taken straight from the vertices,
  ## which is much faster than st_line_sample()
  ind.from <- seq(1, 2 * n_edges, by = 2)
  ind.to <- seq(2, 2 * n_edges, by = 2)
  line.xy <- st_coordinates(in_edges)
  first.v <- which(!duplicated(line.xy[, "L1"]))
  if (length(first.v) != n_edges) {
  	stop("Could not extract exactly two end nodes for every line feature.")
  }
  last.v <- c(first.v[-1] - 1, nrow(line.xy))
  end.xy <- matrix(NA_real_, nrow = 2 * n_edges, ncol = 2,
  									dimnames = list(NULL, c("X", "Y")))
  end.xy[ind.from, ] <- line.xy[first.v, c("X", "Y")]
  end.xy[ind.to, ] <- line.xy[last.v, c("X", "Y")]
  rm(line.xy, first.v, last.v)

  ## End nodes as POINTs, used to find intersections without nodes
  all_nodes_as_single <- st_geometry(st_as_sf(as.data.frame(end.xy),
  																									coords = c("X", "Y"), crs = lst_crs))

  ## Pairs of ends <= snap_tolerance apart, grouped into nodes
  end.pairs <- grid_pairs(end.xy, snap_tolerance)
  g <- igraph::make_empty_graph(n = 2 * n_edges, directed = FALSE)
  g <- igraph::add_edges(g, as.vector(t(end.pairs)))
  memb <- igraph::components(g)$membership
  rm(g)

  ## Never put the from and to ends of a line in the same node
  split.res <- split_self_loops(end.pairs, end.xy, memb)
  memb <- split.res$memb
  blocked.ends <- split.res$blocked
  rm(end.pairs, split.res)

  ## pointid numbered in order of first appearance; each node takes the
  ## coordinates of its first end
  first <- !duplicated(memb)
  node.of.end <- match(memb, memb[first])
  nodexy_mat <- data.frame(pointid = seq_len(sum(first)),
  												 xcoord = end.xy[first, 1],
  												 ycoord = end.xy[first, 2])
  n.nodes <- nrow(nodexy_mat)

  ## Ends that were moved to snap them to their node
  snapped <- end.xy[, 1] != nodexy_mat$xcoord[node.of.end] |
  	end.xy[, 2] != nodexy_mat$ycoord[node.of.end]
  snapped_xy <- data.frame(xcoord = end.xy[snapped, 1],
  												 ycoord = end.xy[snapped, 2])

  nodexy_sf <- st_as_sf(nodexy_mat,
    coords = c("xcoord", "ycoord"),
    crs = lst_crs,
    sf_column_name = edge.geom.name
  )

  node_relate <- data.frame(
    rid = in_edges$rid,
    fromnode = node.of.end[ind.from],
    tonode = node.of.end[ind.to]
  )

  ## Edges whose from and to ends are in the same node
  self_loop_rids <- node_relate$rid[node_relate$fromnode == node_relate$tonode]

  ## Edge A flows into edge B where A's tonode is B's fromnode.
  ## Self-loops are excluded.
  edge_relate <- merge(data.frame(fromedge = node_relate$rid, node = node_relate$tonode),
  										 data.frame(toedge = node_relate$rid, node = node_relate$fromnode),
  										 by = "node", sort = FALSE)
  edge_relate <- edge_relate[edge_relate$fromedge != edge_relate$toedge,
  													 c("fromedge", "toedge")]
  edge_relate <- edge_relate[order(edge_relate$fromedge, edge_relate$toedge), ]

  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  ## Check topology ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

  if (check_topology) {

  	if (verbose == TRUE) message("Checking network topology\n")

  	## Number of edges flowing in/out of each node
  	n_inflow <- tabulate(node_relate$tonode, n.nodes)
  	n_outflow <- tabulate(node_relate$fromnode, n.nodes)

    ## Categorise nodes based on the number of inflow/outflow edges
    ## Returns a vector with T/F length = n.nodes
    outlet_nodes <- (n_inflow >= 1 & n_outflow == 0)
    headwaters <- (n_inflow == 0 & n_outflow >= 1)
    confluences <- (n_inflow >= 2 & n_outflow >= 1)
    pseudo_node <- (n_inflow == 1 & n_outflow == 1)

    ## Identify errors
    complex_confluences <- n_inflow >= 3 & n_outflow >=0
    converging_nodes <- n_inflow == 2 & n_outflow == 0
    downstream_divergence <- n_outflow > 1

    ## Add column of node categories. Includes errors
    nodexy_sf$nodecat <- NA
    nodexy_sf$nodecat[outlet_nodes] <- "Outlet"
    nodexy_sf$nodecat[pseudo_node] <- "Pseudonode"
    nodexy_sf$nodecat[headwaters] <- "Source"
    nodexy_sf$nodecat[confluences] <- "Confluence"
    nodexy_sf$nodecat[complex_confluences] <- "Confluence"
    nodexy_sf$nodecat[downstream_divergence] <- "Confluence"
    nodexy_sf$nodecat[converging_nodes] <- "Outlet"

    ## Find dangling edges: these are another kind of unsnapped connection
    outlets_only <- subset(nodexy_sf, nodecat == "Outlet")
    buff_outlets <- st_buffer(outlets_only, topo_tolerance)
    buff_cross <- st_crosses(in_edges, buff_outlets)
    buff_lines <- sapply(buff_cross, length)
    buffer_ids <- buff_cross[which(buff_lines > 0)]
    buffer_ids <- unlist(buffer_ids)

    crossings <- table(buffer_ids)

    hanging_nodes <- as.numeric(names(crossings)[which(crossings > 1)])

    ## Subset by node error, add an error column, combine at end
    comp_conf <- nodexy_sf[complex_confluences, ]
    if (nrow(comp_conf) > 0) comp_conf$error <- "Complex Confluence"

    conv_node <- nodexy_sf[converging_nodes, ]
    if (nrow(conv_node) > 0) conv_node$error <- "Converging Node"

    down_dive <- nodexy_sf[downstream_divergence, ]
    if (nrow(down_dive) > 0) down_dive$error <- "Downstream Divergence"

    ## One error per node where an edge flows into itself
    self_loop <- nodexy_sf[unique(node_relate$fromnode[self_loop_rids]), ]
    if (nrow(self_loop) > 0) self_loop$error <- "Self-loop"

    ## Nodes with ends <= snap_tolerance from an end in another node,
    ## left unsnapped so a line's from and to ends stay in separate nodes
    unsnapped <- nodexy_sf[unique(node.of.end[blocked.ends]), ]
    if (nrow(unsnapped) > 0) unsnapped$error <- "Unsnapped Node"

    hanging <- outlets_only[hanging_nodes, ]
    ind <- hanging$pointid %in% conv_node$pointid
    hanging <- hanging[!ind, ]
    if (nrow(hanging) > 0) {
      hanging$error <- "Dangling Node"
    }

    ## Combine errors - suppress warnings if there are no errors
    suppressWarnings(errors <- rbind(
      unsnapped, comp_conf, conv_node,
      down_dive, self_loop, hanging
    ))

    ## Find edge intersections without nodes. Only two kinds of pair
    ## can give an error: lines whose interiors cross, and a line end on
    ## another line's interior. Pairs that only touch at shared ends
    ## (confluences, pseudonodes) can't, so st_intersection() is only
    ## run on the lines in candidate pairs.
    sf::st_agr(in_edges) <- "constant"

    ## Lines whose interiors cross
    cross_edges <- which(lengths(st_crosses(in_edges, in_edges)) > 0)

    ## Line ends on, or within eps of, a line they aren't an end of.
    ## st_intersection() snaps crossings within rounding error of a line
    ## end onto that end, but the exact predicates miss them, so each end
    ## is a small square. eps scales with the coordinates to stay well
    ## above double-precision rounding; a larger eps only adds candidates.
    eps <- max(1e-6, 1e-10 * max(abs(sf::st_bbox(in_edges))))
    end_boxes <- st_buffer(all_nodes_as_single, eps, endCapStyle = "SQUARE")
    end_on_line <- st_intersects(end_boxes, in_edges)
    rm(end_boxes)
    k <- rep(seq_len(2 * n_edges), lengths(end_on_line))
    j <- unlist(end_on_line)
    is_end_of_j <- (end.xy[k, 1] == end.xy[ind.from[j], 1] & end.xy[k, 2] == end.xy[ind.from[j], 2]) |
    	(end.xy[k, 1] == end.xy[ind.to[j], 1] & end.xy[k, 2] == end.xy[ind.to[j], 2])
    k_edge <- (k + 1) %/% 2
    on_interior <- k_edge != j & !is_end_of_j
    t_edges <- c(k_edge[on_interior], j[on_interior])
    rm(end_on_line, k, j, is_end_of_j, k_edge, on_interior)

    cand_edges <- sort(unique(c(cross_edges, t_edges)))
    cand <- in_edges[cand_edges, ]
    intersections <- st_intersection(cand, cand)
    points_only <- subset(intersections,
                          st_geometry_type(intersections) == "POINT")

    ## Want all_nodes as single here because looking for edge intersections
    int_vs_nodes <- st_intersects(points_only, all_nodes_as_single)
    node_checks <- sapply(int_vs_nodes, length)
    illegal_intersections <- which(node_checks == 0)

    ill_int <- points_only[illegal_intersections, ]
    if (nrow(ill_int) > 0) {
      g.col <- attributes(ill_int)$sf_column
      ill_int <- ill_int[!duplicated(ill_int[, g.col]), ]
      ill_int$pointid <- NA
      ill_int$nodecat <- "Confluence"
      ill_int$error <- "Intersection Without Node"
      ill_int <-
        ill_int[, c("pointid", "nodecat", "error")]
    }

    ## Identify additional dangling nodes
    ind.dangle <- which(node_checks == 1)
    dangle <- points_only[ind.dangle, ]
    if (nrow(dangle) > 0) {
      dangle$pointid <- NA
      dangle$nodecat <- "Confluence"
      dangle$error <- "Dangling Node"
      dangle <-
        dangle[, c("pointid", "nodecat", "error")]

      ind <- duplicated(dangle)
      dangle <- dangle[!ind, ]

      ## Remove ends that were snapped to a node from dangle error list
      if (nrow(snapped_xy) > 0) {
        dangle_xy <- as.data.frame(st_coordinates(dangle))
        colnames(dangle_xy) <- c("xcoord", "ycoord")
        dangle_xy$ord <- seq(1, nrow(dangle_xy), by = 1)
        snapped_xy$removed <- 1
        tmp <- merge(dangle_xy, unique(snapped_xy),
          by = c("xcoord", "ycoord"), all.x = TRUE
        )
        tmp <- tmp[order(tmp$ord), ]
        ind <- tmp$removed == 1 & !is.na(tmp$removed)
        dangle <- dangle[!ind, ]
      }
    }

    ## Add to errors table
    suppressWarnings(errors <- rbind(errors, ill_int, dangle))

    ## Writing node_errors
    if (overwrite == FALSE & file.exists(paste0(lsn_path, "/node_errors.gpkg"))) {
      stop(paste0(lsn_path, "/node_errors.gpkg"), " exists and overwrite == FALSE")
    } else {
      if (nrow(errors) > 0) {
        if (verbose == TRUE) message(paste0("Saving ", lsn_path, "/node_errors.gpkg"), "\n")
        st_write(errors, paste0(lsn_path, "/node_errors.gpkg"),
          layer = "node_errors",
          quiet = TRUE,
          delete_dsn = TRUE
        )
      }
    }
  }

  ### Write nodes.gpkg
  if (verbose == TRUE) message(paste0("Saving ", lsn_path, "/nodes.gpkg"), "\n")

  if (overwrite == FALSE & file.exists(paste0(lsn_path, "/nodes.gpkg"))) {
    stop(paste0(lsn_path, "/nodes.gpkg"), " exists and overwrite == FALSE")
  } else {
    n.outlets <- sum(nodexy_sf$nodecat == "Outlet")

    st_write(nodexy_sf, paste0(lsn_path, "/nodes.gpkg"),
      layer = "nodes",
      quiet = TRUE,
      delete_dsn = TRUE
    )
  }

  ### Relationship tables
  if (verbose == TRUE) message("Building relationship tables....\n")

  ## Write noderelationships to file
  if (overwrite == FALSE & file.exists(paste0(lsn_path, "/noderelationships.csv"))) {
    stop(paste0(lsn_path, "/noderelationships.csv"), " exists and overwrite == FALSE")
  } else {
    write.table(node_relate,
      file = paste(lsn_path, "noderelationships.csv", sep = "/"),
      sep = ",", col.names = TRUE, row.names = FALSE
    )
  }

  ## Write nodexy to file
  write.table(nodexy_mat,
    file = paste(lsn_path, "nodexy.csv", sep = "/"),
    sep = ",", col.names = TRUE, row.names = FALSE
  )

  ## Export relationships to file.
  write.table(edge_relate,
    file = paste(lsn_path, "relationships.csv", sep = "/"),
    sep = ",", col.names = TRUE, row.names = FALSE
  )

  ## Final message
  if (verbose == TRUE) {
    if(check_topology == TRUE){
      if (nrow(errors) == 0) {
        message(paste0(
          "\n0 topology errors identified. node_errors.gpkg not written to file.\n\n",
          n.outlets,
          " outlets found. Visually check nodecat == Outlet locations in ",
          lsn_path, "/nodes.gpkg and correct errors if found.\n"
        ))
      } else {
      	message(paste0("\n", nrow(errors),
      		" topology errors identified. node_errors.gpkg written to ",
      		lsn_path, "/node_errors.gpkg. Correct errors before re-running lines_to_lsn().\n\n",
      		n.outlets,
      		" outlets found. Visually check nodecat == Outlet locations in ",
      		lsn_path, "/nodes.gpkg and correct errors if found.\n"
      	))
      }
    } else {
        message("LSN created. Topology not checked.")
    }
  }
  return(in_edges)
}
