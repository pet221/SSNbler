#' @title Get upstream distance for edges in a LSN
#' @description Calculate the distance from the stream outlet
#'   (i.e. the most downstream location on the stream network) to the
#'   upstream node of each edge feature (i.e. upstream distance) in
#'   the Landscape Network (LSN)
#' @param edges An \code{sf} object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}}.
#' @param lsn_path Local pathname to a directory in character format
#'   specifying where relationships.csv resides, which is created
#'   using \code{\link[SSNbler]{lines_to_lsn}}.
#' @param calc_length A logical indicating whether a column
#'   representing line length should be calculated and added to
#'   \code{edges}. Default = \code{FALSE}.
#' @param length_col Optional. If \code{calc_length = FALSE},
#'   \code{length_col} is the name of the column in the \code{edges}
#'   attribute table that contains the length of the edge
#'   feature. When \code{calc_length = FALSE}, \code{length_col} is
#'   required. If \code{calc_length = TRUE}, \code{length_col} is the
#'   name of the new column created in \code{edges} that will store
#'   the new length values for each feature, in character format.
#'   When \code{calc_length = TRUE} and \code{length_col = NULL}, the default for \code{length_col}
#'   is \code{"Length"}.
#' @param save_local Logical indicating whether the updated
#'   \code{edges} should be saved to \code{lsn_path} in geopackage
#'   format. Defaults to \code{TRUE}.
#' @param overwrite A logical indicating whether results should be
#'   overwritten if the upDist column already exists in \code{edges}
#'   or edges.gpkg already exists in \code{lsn_path} and
#'   \code{save_local = TRUE}. Default = TRUE
#' @param verbose Logical. Indicates whether messages about the
#'   function progress should be printed to the console. Defaults to
#'   \code{TRUE}.
#'
#' @details \code{updist_edges()} calculates the total hydrologic distance from
#'   the uppermost location on each edge feature (upstream node) to
#'   the stream outlet (i.e. the most downstream location in the
#'   stream network), when movement is restricted to the stream
#'   network. We refer to this as the upstream distance. Upstream distances are
#'   measured in the map projection units for the \code{sf} object edges and stored in
#'   a new column in edges named \code{upDist}.
#'
#' The upstream distances stored in \code{upDist} are used to calculate the upstream distance for
#' sites in [updist_sites()] and the pairwise hydrologic distances used to fit spatial stream network models in
#' the `SSN2` package. Do not modify the name of the column in any way or the values the \code{upDist}
#' column contains.
#' 
#' @return An \code{sf} object representing edges in the LSN, with a new \code{upDist} column. When \code{calc_length = TRUE} an additional column named \code{length_col}
#' @export
#'
#' @examples
#' # Get temporary directory, where the example LSN will be stored
#' # locally. 
#' temp_dir <- tempdir()

#' # Build the LSN. When working with your own data, lsn_path will be 
#' # a local folder of your choice rather than a temporary directory.
#' edges<- lines_to_lsn(
#'    streams = MF_streams,
#'    lsn_path = temp_dir, 
#'    snap_tolerance = 1,
#'    check_topology = FALSE,
#'    overwrite = TRUE,
#'    verbose = FALSE
#' )
#'
#' # Calculate upstream distance for edges
#' edges<- updist_edges(
#'    edges = edges,
#'    lsn_path = temp_dir,
#'    calc_length = TRUE,
#'    length_col = "Length",
#'    overwrite = TRUE,
#'    save_local = FALSE,
#'    verbose = FALSE
#' )
#'
#' # Notice that two new columns have been added to edges containing
#' # line feature length (Length) and the upstream distance (upDist)
#' names(edges)
#' summary(edges[,c("Length", "upDist")])
#' 
 
updist_edges <- function(edges, lsn_path = NULL, calc_length = FALSE, length_col = NULL,
                         save_local = TRUE, overwrite = TRUE, verbose = TRUE){


  # check sf object
  if (!inherits(edges, "sf")) {
    stop("edges must be an sf object.", call. = FALSE)
  }
  
  ## Check inputs ------------------------------------------
  ## Get geometry type as text
  edge_geom<- st_as_text(st_geometry(edges)[[1]])

  ## Check geometry type
  if(grepl("LINESTRING", edge_geom) == FALSE) {
    stop("Input edges must have LINESTRING geometry") }
  
  ## Check lsn_path exists
  if(save_local == TRUE) {
    if (!file.exists(lsn_path)){
      stop("\n lsn_path does not exist.\n\n")
    }
    ## Can we overwrite edges.gpkg if necessary
    if(overwrite == FALSE & file.exists(paste0(lsn_path, "/edges.gpkg"))) {
      stop("edges.gpkg already exists in lsn_path and overwrite = FALSE")
    }
  }

  ## Can we overwrite upDist column if necessary
  # if(sum(colnames(edges) == "upDist") > 0) {
  #    if(overwrite == FALSE) {
  #      stop("upDist already exists in edges and overwrite = FALSE")
  #    } else { ## Remove upDist
  #      edges$upDist<- NULL
  #    }
  # }
  # ## Check for duplicate names
  # check_names_case(names(edges), "upDist", "edges")
  
  ## If upDist file exists and overwrite is TRUE
  if ("upDist" %in% colnames(edges)) {
    if (overwrite) {
      edges$upDist <- NULL
    } else {
      stop("upDist already exists in edges and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(edges), "upDist", "edges")
  
  ## If fid file exists and overwrite is TRUE
  if ("fid" %in% colnames(edges)) {
    if (overwrite) {
      edges$fid <- NULL
    } else {
      stop("fid already exists in edges and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(edges), "fid", "edges")
  
  
  ## Does length_col contain NAs
  if (calc_length == FALSE) {
    
    if (is.null(length_col)) {
      stop("If calc_length is FALSE then length_col must be specified.", call. = FALSE)
    }
    
    ## Check whether length_col exists
    if(!length_col %in% names(edges)) {
      stop(paste0("The edges do not contain ", length_col), call. = FALSE)
    }
    
    if(sum(is.na(edges[,length_col])) > 0) {
      stop(paste0("NA values in length_col, ", length_col,
                  ", are not allowed"), call. = FALSE)
    }
  }

  relate_table <- paste0(lsn_path, "/relationships.csv")
    
  ## Add length column if necessary
  if(calc_length == TRUE){
    if (is.null(length_col)) {
      length_col <- "Length"
    }

    ## If overwrite is FALSE & length_col exists, stop
    if(overwrite == FALSE & length_col %in% colnames(edges)) {
        stop(paste0(length_col, " exists in edges and overwrite == FALSE"))
    }
    ## if overwrite == TRUE and length column exists, delete it
    if(overwrite == TRUE & length_col %in% colnames(edges)) {
      edges[, length_col] <- NULL
    }
    
    ## Check for duplicate names
    check_names_case_add(names(edges), length_col, "edges", "length_col")

    ## Calculate length
    edges[[length_col]] <- as.vector(st_length(edges))
  }

  ## Import relationship table
  if(verbose == TRUE) message("\n\nImporting relationships.csv table")
  rel <- read.csv(relate_table)
  
  ## Get vector of rid values for outlet segment(s)
  if(verbose == TRUE) message("\nIdentifying outlet segments\n")
  outlet <- identify_outlet_segment(rel, edges)
  n_outlets <- length(outlet) ## no. of outlets == no. of networks
  
  ## Construct graph (igraph) of edges from relationship table
  # EDGES ARE REPRESENTED AS VERTICES, with name == rid
  if(verbose == TRUE) message("Linking edge networks and outlets\n")
  names(rel) <- c("from", "to") 
  rel_as_graph <- graph_from_data_frame(rel)
  vertex_names <- vertex_attr(rel_as_graph, 'name') 
  
  ## Create an igraph subgraph for each network and return as list
  sub_graphs <- decompose(rel_as_graph)
  ## Get a list of vectors containing rid values in each subgraph 
  sub_graph_rids <- lapply(sub_graphs, function(x) as.numeric(vertex_attr(x, 'name')))
  
  ## Find outlet associated with each subgraph. Assign in_subgraph
  ## index to outlet_vs_subgraph
  outlet_vs_subgraph <- numeric(n_outlets)
  for(k in 1:n_outlets){
    ## Return logical vector indicating whether outlet rid is
    ## contained in either sub_graph_rids vector
    in_subgraph <- unlist(lapply(sub_graph_rids, function(x) outlet[k] %in% x))
    if(any(in_subgraph)){
      outlet_vs_subgraph[k] <- which(in_subgraph)
    } else {
      outlet_vs_subgraph[k] <- NA
    }
  }
  
  ## Loop through the outlets; find paths through network from each
  ## edge/vertex to the outlet edge
  if(verbose == TRUE) message("Calculating upstream distance\n")
  results_list <- vector('list', n_outlets)
  for(k in 1:n_outlets){
      
    if(is.na(outlet_vs_subgraph[k])){
      results_df <- data.frame(rid = outlet[k])
      results_df <- left_join(results_df, edges, "rid")[, c("rid", length_col)]
      results_df$upDist <- results_df[, length_col]
    } else {
      current_subgraph <- sub_graphs[[outlet_vs_subgraph[k]]]
      n_vertices <- length(current_subgraph) ## no. edges in network     
      results <- vector("list", n_vertices) ## empty list for results
      
      ## Can this be vectorized?  from = full igraph sub_graph for
      ## network i to = number of vertices vpath is a list with length
      ## == length(to). Contains vertex ids (rids) on path. Outputs a list of
      ## igraph.vs objects, with one element per edge (except outlet),
      ## containing the rid values in path between outlet and
      ## edge. Does not include to edge in rid values
      
      for(i in 1:(n_vertices - 1)){
        results[[i]] <- shortest_paths(current_subgraph, from = current_subgraph[[i]],
                                       to = length(current_subgraph))$vpath[[1]]
      }
      
      rids <- sub_graph_rids[[outlet_vs_subgraph[k]]] ## Get all rids in subgraph
      results_df <- data.frame(rid = rids) 

      ## Merge orders by rid and left_join does not
      results_df <- merge(results_df, edges, by = "rid", sort = FALSE)[, c("rid", length_col)]
      
      ## Calculate upstream distance, not including to rid
      UPDISTS <- unlist(lapply(results,
                               function(x) sum(results_df[results_df$rid %in% attributes(x)$names,
                                                          length_col])))
      results_df$upDist <- UPDISTS
      ## Add to rid updist to upDist value
      results_df$upDist <- results_df$upDist + results_df[, length_col]
    }

    results_list[[k]] <- results_df
  }

  all_results_df <- do.call(rbind, results_list)
  ## Join result to existing sf object
  results_sf <- merge(edges, all_results_df[, c("rid", "upDist")], "rid", sort = FALSE)

    ## Write out the in_sites if save_local == TRUE
  if(save_local == TRUE) {
    if(verbose == TRUE) {
      message(paste0("Saving updated edges in ",
                     lsn_path, "\n"))
    }
    suppressMessages(write_sf(results_sf, paste0(lsn_path, "/edges.gpkg"), delete_dsn = TRUE,
                                quiet = TRUE))
  }

  if(verbose == TRUE) message("FINISHED updist_edges successfully\n")

  return(results_sf) 
}
