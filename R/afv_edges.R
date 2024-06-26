#' @title Calculate the additive function value for edges in a LSN
#' @description Calculate the additive function value for each edge
#'   feature in a Landscape Network (LSN)
#' @param edges An \code{sf} object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}}.
#' @param lsn_path Local pathname to a directory in character format
#'   specifying where relationships.csv resides, which is created
#'   using \code{link[SSNbler]{lines_to_lsn}}.
#' @param infl_col Name of the existing column in the edges data.frame
#'   that will be used to generate the segment proportional influence
#'   (segment PI) for each line feature, in character format.
#' @param segpi_col Name of the new column created in \code{edges}
#'   that will store the new segment proportional influence values for
#'   each feature, in character format.
#' @param afv_col Name of the new column created in \code{edges} to
#'   store the additive function value for each feature, in character
#'   format.
#' @param save_local Logical indicating whether the updated
#'   \code{edges} should be saved to \code{lsn_path} in geopackage
#'   format. Defaults to \code{TRUE}.
#' @param overwrite A logical indicating whether results should be
#'   overwritten if \code{segpi_col} and/or \code{afv_col} already
#'   exists in \code{edges}, or edges.gpkg already exists in
#'   \code{lsn_path} and \code{save_local = TRUE}. Default = TRUE.
#' @return An \code{sf} object representing edges in the LSN, with new
#'   \code{segpi_col} and \code{afv_col} columns. If \code{save_local
#'   = TRUE}, the updated version of \code{edges} will be saved as
#'   \code{edges.gpkg} in \code{lsn_path}.
#' @export
afv_edges <- function(edges, lsn_path, infl_col, segpi_col, afv_col,
                      save_local = TRUE, overwrite = TRUE){

  ## Check inputs -------------------------------------------------
  ## Check geometry type
  edge_geom<- st_as_text(st_geometry(edges)[[1]]) 
  if(grepl("LINESTRING", edge_geom) == FALSE) {
    stop("Input edges must have LINESTRING geometry") }

  ## Check lsn_path exists
  if (!file.exists(lsn_path)){
    stop("\n lsn_path does not exist.\n\n")
  }
  ## Can we overwrite edges.gpkg if necessary
  if(!overwrite & save_local & file.exists(paste0(lsn_path, "/edges.gpkg"))) {
    stop("Cannot save edges to local file because edges.gpkg already exists in lsn_path and overwrite = FALSE")
  }

  ## Does segpi_col already exist in edges
  if(overwrite == FALSE & sum(colnames(edges) == segpi_col) > 0) {
    stop(paste0(segpi_col, " already exists in edges and overwrite = FALSE"))
  } else {
    edges[, segpi_col]<- NULL
  }
   
  ## Does afv_col already exist in edges when overwrite = FALSE
  if(overwrite == FALSE & afv_col %in% names(edges)) {
    stop(paste0(afv_col, " already exists in edges and overwrite is FALSE."))
  } else {
    edges[, afv_col]<- NULL
  }
  
  ## Check for duplicate names
  check_names_case_add(names(edges), segpi_col, "edges", "segpi_col")
  check_names_case_add(names(edges), afv_col, "edges", "afv_col")
  

  ## Check infl_col column
  if(!infl_col %in% colnames(edges)) {
    stop(paste0(infl_col, " not found in edges"))
  }
  
  ind<- st_drop_geometry(edges[, infl_col]) < 0  
  if(sum(ind) > 0) {
    stop(paste0(infl_col, " contains negative values, which is not allowed"))
  }
  ind <- st_drop_geometry(edges[,infl_col]) == 0
  sum.zeros <- sum(ind)
   

  ## calculate segment proportional infuence column
  edges<- get_segment_pi(edges = edges, lsn_path = lsn_path,
                         infl_col=infl_col, segpi_col= segpi_col,
                         overwrite = overwrite)

  ## Format input data
  edges_sf <- edges[, c(segpi_col, "rid")]
  relate_table <- read.csv(paste0(lsn_path, "/relationships.csv"))

  ## Find the outlet segment(s)
  outlet <- identify_outlet_segment(relate_table, edges_sf)
  n_outlets <- length(outlet) ## number of outlets is the same as the number of networks

  ## Import relationship table
  names(relate_table) <- c("from", "to")  
  
  ## Construct graph (igraph) from relationship table
  rel_as_graph <- graph_from_data_frame(relate_table)
  vertex_names <- vertex_attr(rel_as_graph, 'name')
  
  ## Get lists of sub-graphs for each outlet and sub graph rids
  sub_graphs <- decompose(rel_as_graph)
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
  ## segment to the outlet segment. Here, vertices = edges
  results_list <- vector('list', n_outlets)
  for(k in 1:n_outlets){
    if(is.na(outlet_vs_subgraph[k])){      
      matching_rid <- which(edges_sf$rid == outlet[k])
      results_df <- data.frame(rid = outlet[k])
      results_df[, segpi_col] <- 1
      results_df[, afv_col] <- 1
         
    } else {
      current_subgraph <- sub_graphs[[outlet_vs_subgraph[k]]]
      n_vertices <- length(current_subgraph)
      results <- vector("list", n_vertices)

      ## Create a list of igraph.vs elements that store the rid values
      ## in path to outlet. rid values are ordered from upstream to
      ## downstream (outlet) segment. Does not include to edge in rid values
      for(i in 1:(n_vertices - 1)){
        results[[i]] <- shortest_paths(current_subgraph,
                                       from = current_subgraph[[i]],
                                       to = length(current_subgraph))$vpath[[1]]
      }

      rids <- sub_graph_rids[[outlet_vs_subgraph[k]]]
      results_df <- data.frame(rid = rids) 
      results_df <-  merge(results_df, edges_sf, by = "rid",
                           sort = FALSE)[, c("rid", segpi_col)]

      ## Get the AFV value for the edge directly down stream of each edge
      AFV.ds <- unlist(lapply(results,
                              function(x) prod(results_df[results_df$rid %in%
                                                          attributes(x)$names,
                                                          segpi_col])))     
      results_df[, afv_col] <- AFV.ds

      ## Multiply the AFV.ds * segpi_col for to segment to get AFV for each edge
      results_df[, afv_col] <- results_df[, afv_col] * results_df[, segpi_col]
    }
    results_list[[k]] <- results_df
  }
  ## Convert list to single data.frame
  all_results_df <-do.call(rbind, results_list)

  ## Join result to existing sf object
  results_sf <- merge(edges, all_results_df[, c("rid", afv_col)],
                      "rid", sort = FALSE)
 
  ## Write out results
  if(save_local) {
    st_write(results_sf,
             dsn = paste0(lsn_path, "/edges.gpkg"),
             delete_dsn = TRUE,
             quiet = TRUE)
  }

  ## Print warning

  sum.afv.0 <- sum(st_drop_geometry(results_sf[,afv_col]) == 0)
  if(sum.afv.0 > 0) {
    warning(paste0("\n", infl_col, " contains ", sum.zeros, " zeros and ", afv_col,
                   " contains ", sum.afv.0,
                   " zeros. If a large number of SITES (not edges) have AFV==0, it will impact the tail-up autocovariance function."))
  }
  
   return(results_sf)
  
}
