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
#' @param snap_tolerance Distance in map units >= 0. Required; there is
#'   no default. Two nodes separated by a distance less than or equal to
#'   \code{snap_tolerance} are assumed to be connected. Use 0 to connect
#'   only line end nodes with \emph{identical} coordinates.
#' @param topo_tolerance Distance in map units. Required when
#'   \code{check_topology = TRUE}, and ignored otherwise.
#'   Two nodes on the network separated by a distance less
#'   than or equal to \code{topo_tolerance} are flagged as potential
#'   topological errors in the network. Must be greater than
#'   \code{snap_tolerance} (and therefore > 0) when
#'   \code{check_topology = TRUE}.
#' @param remove_ZM Logical. If \code{TRUE}, Z and M dimensions are
#'   removed from streams, if they exist. If \code{FALSE} and
#'   \code{streams} has Z and/or M dimensions, the function stops with
#'   an error. Default is \code{FALSE}.
#' @param overwrite Logical. If \code{TRUE}, output files will
#'   overwrite existing files with the same names, and a
#'   node_errors.gpkg file from an earlier run is deleted. Default is \code{FALSE}.
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
#' The LSN is saved to a local directory defined by \code{lsn_path} and has four components:
#' \itemize{
#'   \item{ nodes.gpkg: A GeoPackage of features with POINT geometry representing topologic breaks in the stream network. A column named nodecat specifies the type of node (e.g. pseudonodes, confluences, stream sources, or stream outlets). A column containing a unique node identifier, pointid, is also included. }
#'   \item{ edges.gpkg: A GeoPackage of features with LINESTRING geometry representing flow paths (i.e. line features) from node to node. A new column named rid is added that contains a unique identifier for each line feature. }
#'   \item{noderelationships.csv: a table describing the relationship between the nodes and their associated edges, as well as directionality in the LSN. The table contains three columns: the rid (for each edge), fromnode (pointid for the upstream node of each edge), and tonode (pointid for the downstream node of each edge). Directionality is defined based on the digitized direction of the line features. }
#'   \item{relationships.csv: a table representing the downstream flow path from edge to edge. The table contains two columns: fromedge (upstream edge) and toedge (downstream edge). }
#' }
#'
#' Line end nodes separated by a Euclidean distance <=
#' \code{snap_tolerance} are snapped together into a single node, and
#' nodes, noderelationships.csv and relationships.csv are all built
#' from this rule. Snapping is transitive: if end node A is within
#' \code{snap_tolerance} of end node B, and B is within
#' \code{snap_tolerance} of end node C, then A, B and C are snapped
#' even if A and C are further apart. The upstream and downstream end
#' nodes of the same line feature are never snapped into one node. The
#' node takes the coordinates of the first line end node assigned to it.
#'
#' Topological errors are common in spatial data and must be corrected
#' to ensure the LSN accurately represents direction and
#' connectivity within the stream network. When \code{check_topology =
#' TRUE}, the edges are checked for topological errors and potential errors are saved to \code{lsn_path} in an output file called
#' \code{node_errors.gpkg}. In
#' addition to the pointid, \code{node_errors.gpkg} contains a column
#' containing the node category (nodecat), which like \code{nodes.gpkg} can take on values of
#' Pseudonode, Confluence, Source, or Outlet. Another column (error)
#' defines the potential error type and can take on values of:
#' \itemize{
#'   \item{Complex Confluence: three or more edges flow into the node.}
#'   \item{Converging Node: two edges flow into the node and none flow out.}
#'   \item{Dangling Node: an outlet within \code{topo_tolerance} of an edge in a different network. }
#'   \item{Downstream Divergence: two or more edges flow out of the node.}
#'   \item{Intersection Without Node: two edges cross without a node. A crossing within \code{snap_tolerance} of a node that both edges flow into or out of is not reported.}
#'   \item{Confluence Without Node: an edge end node touches another edge somewhere other than the end node (e.g. forms a T junction where the other edge is not split).}
#'   \item{Network Without Outlet: the node is part of a loop of edges with no outlet (e.g. edges digitised in a circle, with or without edges flowing into it). Every node on the loop is reported. }
#'   \item{Self-loop: the upstream and downstream end nodes of a line feature have identical coordinates. The line feature is not included in relationships.csv.}
#'    \item{Chained Node: the node contains line end nodes more than \code{snap_tolerance} apart. Because snapping is transitive, this happens when end nodes are linked by a chain of end nodes, each within \code{snap_tolerance} of the next.}
#'   \item{Unsnapped Node: a line end node was <= \code{snap_tolerance} from another end node and was not snapped to it. This typically occurs when snapping would result in a line feature's to and from nodes being combined into one node (e.g. a line end node close to both end nodes of a short line). }
#' }
#' Two additional columns are added to \code{node_errors.gpkg} containing the rids of the edges flowing into (up_rid)
#' and out of (down_rid) each node, in comma separated text format (e.g. "4, 7"). This allows the user to visually check flow relationships at each potential error more
#' quickly. Note that up_rid is NA for a node without edges that flow in (e.g. a
#' Source) and down_rid is NA for a node without edges flowing out
#' (e.g. an Outlet). Both are NA for Intersection Without Node errors, along with pointid and nodecat, 
#' because these do not coincide with topological breaks in the edges. A numeric column (node_width) gives the largest distance, in
#' map units, between two line end nodes in each Chained Node, so the
#' widest can be checked first. Note that node_width is NA for all other error
#' types.
#'
#' There is no guarantee
#' that all topological errors will be identified and included in
#' node_errors.gpkg. Therefore, potential errors *and* node classes
#' found in node_errors.gpkg and nodes.gpkg must be checked
#' and topological errors in \code{streams} corrected before
#' rebuilding the LSN using \code{lines_to_lsn()}. This process is
#' iterative and must continue until the LSN is free of topological
#' errors. A node_errors.gpkg file is not
#' produced if no obvious errors are identified. 
#'
#' The arguments \code{snap_tolerance} and \code{topo_tolerance} are
#' both provided in map units and neither has a default.
#' \code{snap_tolerance} must always be provided and must be >= 0. When
#' \code{check_topology = TRUE}, \code{topo_tolerance} must also be
#' provided and must be greater than \code{snap_tolerance}. It is generally a good
#' idea to set the \code{snap_tolerance} to a distance shorter than the smallest line segment length. It should also be relatively small
#' compared to the distance specified in the \code{topo_tolerance} argument. Nodes separated by
#' a Euclidean distance <= \code{snap_tolerance} are assumed to be
#' connected and are snapped together. When snap_tolerance < distance <= topo_tolerance, nodes are flagged as potential errors. If \code{snap_tolerance} is too large, end nodes that should remain
#' separate will be snapped together in error. If \code{topo_tolerance}
#' is too large, more false errors will be flagged in
#' \code{node_errors.gpkg}.
#'
#' WARNING: A warning is given if the length of any line feature in \code{streams} is <= \code{snap_tolerance}, because snapping near that line feature may not give the intended connections. Use \code{\link[sf]{st_length}} to obtain and check the minimum length of the line features prior to running \code{lines_to_lsn}. When \code{check_topology = FALSE}, a warning is also given if any line end nodes within \code{snap_tolerance} of another line end node were not snapped. This typically occurs when snapping would put both end nodes of a line feature into one node. If this warning is returned, re-run \code{lines_to_lsn} with \code{check_topology = TRUE} to locate these nodes. Similarly, when \code{check_topology = FALSE}, a warning is given if any snapped nodes include Chained Nodes that were more than \code{snap_tolerance} apart because of transitive snapping. Again, use \code{check_topology = TRUE} to locate and check these nodes.
#'
#' @return An `sf` object representing edges in the LSN. The LSN, including edges.gpkg, nodes.gpkg, noderelationships.csv, and relationships.csv files, are saved locally to a directory defined by \code{lsn_path}. If \code{check_topology = TRUE} and topological errors are identified, then node_errors.gpkg is also saved to \code{lsn_path}.
#'
#' @export
#'
lines_to_lsn <- function(streams,
                         lsn_path,
                         check_topology = TRUE,
                         snap_tolerance,
                         topo_tolerance,
                         remove_ZM = FALSE,
                         overwrite = FALSE,
                         verbose = TRUE) {

	##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
	# Check Inputs ----
	##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

	# check sf object
  if (inherits(streams, "sf") == FALSE) {
    stop("streams must be an sf object.", call. = FALSE)
  }

  # check streams has at least one line feature
  if (nrow(streams) == 0) {
    stop("streams has no features (zero rows).", call. = FALSE)
  }

  ## Check that logical arguments are single TRUE/FALSE values
  logical.args <- list(check_topology = check_topology, 
  										 remove_ZM = remove_ZM,
                       overwrite = overwrite, 
  										 verbose = verbose)
  
  for (arg.name in names(logical.args)) {
    arg.value <- logical.args[[arg.name]]
    if (is.logical(arg.value) == FALSE || length(arg.value) != 1 ||
        is.na(arg.value) == TRUE) {
      stop(arg.name, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  rm(logical.args, arg.name, arg.value)

  # check empty geometries
  if (any(st_is_empty(streams)) == TRUE) {
    stop("streams has at least one empty geometry. Check and/or use sf::st_is_empty() to remove.",
      call. = FALSE
    )
  }

  in.edges <- streams
  lst.crs <- st_crs(in.edges)

  ## Check that data have a coordinate reference system and are projected
  if (is.na(lst.crs) == TRUE) {
    stop("streams has no coordinate reference system. Set it with sf::st_set_crs(), then re-project streams to a cartesian coordinate system if needed.", call. = FALSE)
  }
  if (st_is_longlat(in.edges) == TRUE) {
    stop("Streams data have geographic coordinates. Re-project streams to a cartesian coordinate system.", call. = FALSE)
  }

  ## Get geometry type as text
  edge.geom <- st_as_text(st_geometry(in.edges)[[1]])

  ## Check geometry type 
  if (inherits(st_geometry(in.edges), "sfc_LINESTRING") == FALSE) {
    if (any(st_geometry_type(in.edges) == "MULTILINESTRING") == TRUE) {
      stop("Input streams must have LINESTRING geometry. Use sf::st_cast to convert from MULTILINESTRING to LINESTRING.", call. = FALSE)
    } else {
      stop("Input streams must have LINESTRING geometry.", call. = FALSE)
    }
  }

  edge.geom.name <- attributes(in.edges)$sf_column

  if (grepl("Z|M", edge.geom) == TRUE && remove_ZM == FALSE) {
    stop("XYZ, XYM or XYZM geometries are not supported. Set remove_ZM = TRUE to drop Z and/or M dimensions from feature geometries.", call. = FALSE)
  }

  ## Remove Z and/or M dimensions if they exist
  if (grepl("Z|M", edge.geom) == TRUE && remove_ZM == TRUE) {
    in.edges <- st_zm(in.edges)
  }

  ## Check that every line has at least two vertices. line.xy 
  ## used later to get line end nodes
  line.xy <- st_coordinates(in.edges)
  n.vert <- tabulate(line.xy[, "L1"], nrow(in.edges))
  if (any(n.vert < 2) == TRUE) {
    stop("Each line feature in streams must have at least two vertices. ",
      sum(n.vert < 2), " line feature(s) have fewer.", call. = FALSE)
  }
  rm(n.vert)

  ## Check tolerances are provided
  if (missing(snap_tolerance) == TRUE) {
    stop("snap_tolerance must be provided, in map units (>= 0). Use snap_tolerance = 0 to connect only line end nodes with identical coordinates.", call. = FALSE)
  }
  if (check_topology == TRUE && missing(topo_tolerance) == TRUE) {
    stop("topo_tolerance must be provided, in map units (> snap_tolerance), when check_topology = TRUE.", call. = FALSE)
  }

  ## Check that tolerances are single, finite numbers >= 0
  if (is.numeric(snap_tolerance) == FALSE || length(snap_tolerance) != 1 ||
      is.finite(snap_tolerance) == FALSE || snap_tolerance < 0) {
    stop("snap_tolerance must be a single number and must be >= 0", call. = FALSE)
  }
  if (missing(topo_tolerance) == FALSE &&
      (is.numeric(topo_tolerance) == FALSE || length(topo_tolerance) != 1 ||
       is.finite(topo_tolerance) == FALSE || topo_tolerance < 0)) {
    stop("topo_tolerance must be a single number and must be >= 0", call. = FALSE)
  }

  ## Check that topo_tolerance > snap_tolerance (so also > 0)
  if (check_topology == TRUE && topo_tolerance <= snap_tolerance) {
    stop("topo_tolerance must be > snap_tolerance when check_topology = TRUE",
      call. = FALSE)
  }

  ## Warn if any line is <= snap_tolerance
  if(snap_tolerance > 0) {
  	sm.edges<- sum(as.numeric(st_length(in.edges)) <= snap_tolerance)

  	if(sm.edges > 0) {
  		warning(sm.edges, " line feature(s) in streams have a length <= snap_tolerance. Snapping near these lines may not give the intended connections. Check them or use a smaller snap_tolerance. Use sf::st_length() to obtain line feature lengths.", call. = FALSE)
  	}
  }

  # check column names
  if ("rid" %in% colnames(in.edges)) {
    if (overwrite == TRUE) {
      in.edges$rid <- NULL
    } else {
      stop("rid already exists in edges and overwrite = FALSE", 
      		 call. = FALSE)
    }
  }
  check_names_case(names(in.edges), "rid", "streams")

  ## If fid column exists and overwrite is TRUE
  if ("fid" %in% colnames(in.edges)) {
    if (overwrite == TRUE) {
      in.edges$fid <- NULL
    } else {
      stop("fid already exists in edges and overwrite = FALSE", 
      		 call. = FALSE)
    }
  }
  check_names_case(names(in.edges), "fid", "streams")

  ## lsn_path must be a folder, not a file
  if (file.exists(lsn_path) == TRUE && dir.exists(lsn_path) == FALSE) {
    stop("lsn_path must be a folder, but ", lsn_path, " is a file.",
      call. = FALSE)
  }

  ## Stop if output files exist and overwrite = FALSE 
  if (overwrite == FALSE) {
    out.files <- c("edges.gpkg", "nodes.gpkg", "noderelationships.csv",
                   "relationships.csv")
    
    if (check_topology == TRUE) out.files <- c(out.files, "node_errors.gpkg")
    
    existing.files <- out.files[file.exists(file.path(lsn_path, out.files))]
    
    if (length(existing.files) > 0) {
      stop("These files already exist in ", lsn_path, 
      		 " and overwrite = FALSE: ",
        paste(existing.files, collapse = ", "),
        ". Use overwrite = TRUE to replace them, or specify a different lsn_path.",
        call. = FALSE)
    }
  }
  
  ## With overwrite = TRUE, delete the LSN files before anything
  ## is written. Stop if a file can't be deleted
  if (overwrite == TRUE) {
    old.files <- c("edges.gpkg", "nodes.gpkg", "noderelationships.csv",
                   "relationships.csv", "node_errors.gpkg", "nodexy.csv")
    old.files <- old.files[file.exists(file.path(lsn_path, old.files))]
    unlink(file.path(lsn_path, old.files))
    
    locked.files <- old.files[file.exists(file.path(lsn_path, old.files))]
    
    if (length(locked.files) > 0) {
      stop("These files in ", lsn_path, " could not be deleted: ",
        paste(locked.files, collapse = ", "),
        ". If they are open in other software, files may be locked.",
        call. = FALSE)
    }
  }
  
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  # Save edges ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

  ## Add the field rid
  n.edges <- nrow(in.edges)
  in.edges$rid <- seq.int(from = 1, to = n.edges) 

  ## if output directory doesn't exist, create it now 
  if (file.exists(lsn_path) == FALSE) {
    dir.create(lsn_path, recursive = TRUE)
  }

  ## Write edges to GeoPackage
  st_write(in.edges, paste0(lsn_path, "/edges.gpkg"),
    layer = "edges", quiet = TRUE,
    delete_dsn = TRUE
  )

  if (verbose == TRUE) message(paste0("\nSaved ", lsn_path, 
  																		"/edges.gpkg", "\n"))
  
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  # Build nodes and edge relationships ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

  if (verbose == TRUE) message("Building nodes and edge relationships ...\n")

  ## Create matrix of edge end node coords ordered from1, to1, 
  ## from2, to2 ...
  ind.from <- seq(1, 2 * n.edges, by = 2)
  ind.to <- seq(2, 2 * n.edges, by = 2)
  
  first.v <- which(!duplicated(line.xy[, "L1"]))
  last.v <- c(first.v[-1] - 1, nrow(line.xy))
  
  end.xy <- matrix(NA_real_, nrow = 2 * n.edges, ncol = 2,
  									dimnames = list(NULL, c("X", "Y")))
  end.xy[ind.from, ] <- line.xy[first.v, c("X", "Y")]
  end.xy[ind.to, ] <- line.xy[last.v, c("X", "Y")]
  rm(line.xy, first.v, last.v)
	
  ## Find line end nodes <= snap_tolerance apart
  end.pairs <- grid_pairs(end.xy, snap_tolerance)
  
  ## Create graph with one vertex per line end node
  g <- igraph::make_empty_graph(n = 2 * n.edges, directed = FALSE)
  
  ## Add graph edges/links between connected nodes
  g <- igraph::add_edges(g, as.vector(t(end.pairs)))
  
  ## Assign group number to snapped/shared nodes
  memb <- igraph::components(g)$membership
  rm(g)

  ## Prevent a line's from/to nodes being snapped together
  split.res <- split_self_loops(end.pairs, end.xy, memb)

  memb <- split.res$memb
  blocked.ends <- split.res$blocked
  rm(split.res)

  ## Produce warning if not checking topology
  if (check_topology == FALSE && length(blocked.ends) > 0) {
    warning(length(blocked.ends), " line end node(s) (may be shared nodes) within snap_tolerance were not snapped, because this would force end nodes of a single line feature into one node. Connections at these locations may be incorrect. Use check_topology = TRUE to locate and check Unsnapped Node errors.", call. = FALSE)
  }

  ## pointid numbered in order of first appearance; each node takes the
  ## coordinates of its first associated end node
  first <- !duplicated(memb)
  node.of.end <- match(memb, memb[first])
  nodexy.mat <- data.frame(pointid = seq_len(sum(first)),
  												 xcoord = end.xy[first, 1],
  												 ycoord = end.xy[first, 2])
  n.nodes <- nrow(nodexy.mat)

  ## Chained Nodes: nodes whose end nodes are not all within
  ## snap_tolerance of each other, because snapping is transitive
  chained.nodes <- find_chained_nodes(end.pairs, node.of.end, n.nodes)
  rm(end.pairs)

  ## With check_topology = TRUE these are reported as Chained Node errors
  if (check_topology == FALSE && length(chained.nodes) > 0) {
    warning(length(chained.nodes), " node(s) contain line end nodes more than snap_tolerance apart. Snapping is transitive, so end nodes linked by a chain of end nodes, each within snap_tolerance of the next, were snapped into one node. Connections at these locations may be incorrect. Use check_topology = TRUE to locate and check Chained Nodes.", call. = FALSE)
  }
  
  ## Create nodes sf object
  nodexy.sf <- st_as_sf(nodexy.mat,
    coords = c("xcoord", "ycoord"),
    crs = lst.crs,
    sf_column_name = edge.geom.name
  )
  
  ## Create noderelationships table
  node.relate <- data.frame(
    rid = in.edges$rid,
    fromnode = node.of.end[ind.from],
    tonode = node.of.end[ind.to]
  )

  ## Edges whose from and to end nodes are the same node
  self.loop.rids <- 
  	node.relate$rid[node.relate$fromnode == node.relate$tonode]

  ## Create (edge) relationships table. Exclude self-loops
  edge.relate <- merge(data.frame(fromedge = node.relate$rid, 
  																node = node.relate$tonode),
  										 data.frame(toedge = node.relate$rid, 
  										 					 node = node.relate$fromnode),
  										 by = "node", sort = FALSE)
  edge.relate <- edge.relate[edge.relate$fromedge != edge.relate$toedge,
  													 c("fromedge", "toedge")]
  edge.relate <- edge.relate[order(edge.relate$fromedge, 
  																 edge.relate$toedge), ]

  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  # Create node categories ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

  ## Number of edges flowing in/out of each node
  n.inflow <- tabulate(node.relate$tonode, n.nodes)
  n.outflow <- tabulate(node.relate$fromnode, n.nodes)

  ## Categorise nodes based on the number of inflow/outflow edges
  headwaters <- n.inflow == 0
  outlet.nodes <- n.inflow >= 1 & n.outflow == 0
  pseudo.node <- n.inflow == 1 & n.outflow == 1
  confluences <- (n.inflow >= 2 & n.outflow >= 1) |
    (n.inflow == 1 & n.outflow >= 2)

  ## Add column of node categories
  nodexy.sf$nodecat <- NA
  nodexy.sf$nodecat[headwaters] <- "Source"
  nodexy.sf$nodecat[outlet.nodes] <- "Outlet"
  nodexy.sf$nodecat[pseudo.node] <- "Pseudonode"
  nodexy.sf$nodecat[confluences] <- "Confluence"

  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  # Check topology ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

  if (check_topology == TRUE) {

  	if (verbose == TRUE) message("Checking network topology\n")

    ## Errors by inflow/outflow count ---- 
    complex.confluences <- n.inflow >= 3
    converging.nodes <- n.inflow == 2 & n.outflow == 0
    downstream.divergence <- n.outflow > 1

    ## Subset by node error, add an error column, combine at end
    comp.conf <- nodexy.sf[complex.confluences, ]
    if (nrow(comp.conf) > 0) comp.conf$error <- "Complex Confluence"

    conv.node <- nodexy.sf[converging.nodes, ]
    if (nrow(conv.node) > 0) conv.node$error <- "Converging Node"

    down.dive <- nodexy.sf[downstream.divergence, ]
    if (nrow(down.dive) > 0) down.dive$error <- "Downstream Divergence"

    ## One error per node where an edge flows into itself
    self.loop <- nodexy.sf[unique(node.relate$fromnode[self.loop.rids]), ]
    if (nrow(self.loop) > 0) self.loop$error <- "Self-loop"
    
    ## Unsnapped nodes: from split_self_loops ----
    unsnapped <- nodexy.sf[unique(node.of.end[blocked.ends]), ]
    if (nrow(unsnapped) > 0) unsnapped$error <- "Unsnapped Node"
    
    ## Chained nodes: from find_chained_nodes  ----
    chained <- nodexy.sf[chained.nodes, ]
    if (nrow(chained) > 0) chained$error <- "Chained Node"

    ## Calc largest distance b/t end nodes in each chained node
    chained.width <- chained_node_width(chained.nodes, 
    																		node.of.end, 
    																		end.xy)
    
    ## Create end nodes as POINTs, used to find intersections and 
    ## confluences w/out nodes
    all.nodes.as.single <- st_geometry(st_as_sf(as.data.frame(end.xy),
    																						coords = c("X", "Y"), 
    																						crs = lst.crs))
    
    ## Tolerance for rounding error, used to find intersections. Scales
    ## w/coords to stay above double-precision rounding
    eps <- max(1e-6, 1e-10 * max(abs(st_bbox(in.edges))))

    ## Confluence w/out node ----
    confluence.nodes <- find_confluence_no_node(all.nodes.as.single,
                                                in.edges, 
    																						node.of.end, 
    																						eps)
    confluence.no.node <- nodexy.sf[confluence.nodes, ]
    if (nrow(confluence.no.node) > 0) {
      confluence.no.node$error <- "Confluence Without Node"
    }

    ## Intersection w/out node ----
    ill.int <- find_intersection_no_node(in.edges, all.nodes.as.single,
                                         end.xy, 
    																		 node.of.end,
                                         snap_tolerance, 
    																		 eps)
    if (nrow(ill.int) > 0) {
      ill.int$pointid <- NA
      ill.int$nodecat <- NA_character_
      ill.int$error <- "Intersection Without Node"
      ill.int <- ill.int[, c("pointid", "nodecat", "error")]
    }

    ## Dangling Node Outlets ----
    ## Outlets within topo_tolerance of a line from a different network
    
    ## Network number assigned to nodes
    node.graph <- igraph::make_graph(
      as.vector(t(node.relate[, c("fromnode", "tonode")])),
      n = n.nodes, directed = FALSE)
    node.network <- igraph::components(node.graph)$membership
    rm(node.graph)

    ## Subset outlets
    outlets.only <- nodexy.sf[which(nodexy.sf$nodecat == "Outlet"), ]
    
    # Find outlets within topo_tolerance of lines on other networks
    dangle.outlet.rows <- find_dangling_outlets(
    	outlets.only, 
    	in.edges,
      topo_tolerance,
      outlet.network = node.network[outlets.only$pointid],
      edge.network = node.network[node.relate$fromnode]
    	)
    dangle.outlet <- outlets.only[dangle.outlet.rows, ]
    
    ## Remove converging nodes, complex confluences and confluence 
    ## without node errors from dangle.outlet
    ind <- dangle.outlet$pointid %in% c(conv.node$pointid, comp.conf$pointid,
                                        confluence.no.node$pointid)
    dangle.outlet <- dangle.outlet[!ind, ]
    
    ## Label remaining dangling nodes
    if (nrow(dangle.outlet) > 0) {
      dangle.outlet$error <- "Dangling Node"
    }

    ## Network w/out outlet ----
    ## Nodes on a loop in a network with no outlet
    no.outlet.nodes <- find_network_no_outlet(node.relate, n.nodes,
                                              node.network, outlet.nodes)
    network.no.outlet <- nodexy.sf[no.outlet.nodes, ]
    if (nrow(network.no.outlet) > 0) {
      network.no.outlet$error <- "Network Without Outlet"
    }

    ## Combine errors - suppress warnings if there are no errors
    suppressWarnings(errors <- rbind(
      unsnapped, chained, comp.conf, conv.node, down.dive, self.loop,
      dangle.outlet, ill.int, confluence.no.node, network.no.outlet
    ))

    ## Add up_rid/down_rid columns to errors, as comma separated text. 
    ## NA where no edges flow in (source) or out (outlet), and 
    ## errors w/out a node 
    if (nrow(errors) > 0) {
      error.nodes <- unique(errors$pointid[!is.na(errors$pointid)])
      errors$up_rid <- rids_at_nodes(node.relate$tonode, 
      															 node.relate$rid,
                                     error.nodes, 
      															 errors$pointid)
      errors$down_rid <- rids_at_nodes(node.relate$fromnode, 
      																 node.relate$rid,
                                       error.nodes, 
      																 errors$pointid)

      ## Add node_width column for chained nodes only
      errors$node_width <- NA_real_
      chained.row.ind <- errors$error == "Chained Node"
      errors$node_width[chained.row.ind] <-
        chained.width[match(errors$pointid[chained.row.ind], chained.nodes)]
    }
  }
  
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  # Save outputs ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  
  ## Save errors to node_errors.gpkg (if required)
  if(check_topology == TRUE && nrow(errors) > 0) {
  	if (verbose == TRUE) message(paste0("Saving ", lsn_path, 
  																			"/node_errors.gpkg"), "\n")
  	
  	st_write(errors, paste0(lsn_path, "/node_errors.gpkg"),
  					 layer = "node_errors",
  					 quiet = TRUE,
  					 delete_dsn = TRUE)
  }

  ### Write nodes.gpkg
  if (verbose == TRUE) message(paste0("Saving ", lsn_path, "/nodes.gpkg"), "\n")

  n.outlets <- sum(nodexy.sf$nodecat == "Outlet")

  st_write(nodexy.sf, paste0(lsn_path, "/nodes.gpkg"),
    layer = "nodes",
    quiet = TRUE,
    delete_dsn = TRUE
  )

  ### Relationship tables
  if (verbose == TRUE) message("Saving tables....\n")

  ## Write noderelationships to file
  write.table(node.relate,
    file = paste(lsn_path, "noderelationships.csv", sep = "/"),
    sep = ",", col.names = TRUE, row.names = FALSE
  )

  ## Export relationships to file.
  write.table(edge.relate,
    file = paste(lsn_path, "relationships.csv", sep = "/"),
    sep = ",", col.names = TRUE, row.names = FALSE
  )
  
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##
  # Final message ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##

  ## Final message
  if (verbose == TRUE) {
    outlet.msg <- paste0(n.outlets,
      " outlets found. Visually check nodecat == Outlet locations in ",
      lsn_path, "/nodes.gpkg and correct errors if found.\n")

    if (check_topology == FALSE) {
      message("\nLSN created. Topology not checked.\n\n", outlet.msg)
    } else if (nrow(errors) == 0) {
      message("\n0 topology errors identified. node_errors.gpkg not written to file.\n\n",
        outlet.msg)
    } else {
      message("\n", nrow(errors), " topology errors identified. node_errors.gpkg written to ",
        lsn_path, "/node_errors.gpkg. Correct errors before re-running lines_to_lsn().\n\n",
        outlet.msg)
    }
  }
  return(in.edges)
}
