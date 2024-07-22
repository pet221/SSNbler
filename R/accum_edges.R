#' @title Accumulate edge values downstream
#' @description Accumulate (sum) edge values downstream in a
#'   Landscape Network (LSN)
#'
#' @param edges An \code{sf} object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}}.
#' @param lsn_path Local pathname to a directory in character format
#'   specifying where relationships.csv resides, which is created
#'   using \code{link{lines_to_lsn}}.
#' @param sum_col Name of an existing column in \code{edges}
#'   (character format) that contains the value to sum downstream.
#' @param acc_col Name of the new column in \code{edges} (character
#'   format) where the accumulated \code{sum_col} values will be
#'   stored (see details).
#' @param save_local Logical indicating whether the updated
#'   \code{edges} should be saved to \code{lsn_path} in geopackage
#'   format. Defaults to \code{TRUE}.
#' @param overwrite A logical indicating whether results should be
#'   overwritten if \code{acc_col} already exists in \code{edges}
#'   or edges.gpkg already exists in \code{lsn_path} and
#'   \code{save_local = TRUE}.
#' @param verbose Logical. Indicates whether messages about the
#'   function progress should be printed to the console. Defaults to
#'   \code{TRUE}.
#' @details \code{accum_edges} sums (i.e. accumulates) numeric
#'   values in \code{edges} downstream, from headwater or source
#'   features to each network outlet feature (i.e. most downstream
#'   line feature in each sub-network). Missing values are not allowed in
#'   \code{sum_col} and should be replaced with 0, or any other
#'   meaningful numeric value, prior to running
#'   \code{accum_edges}. The \code{acc_col} returned contains the
#'   sum of \code{sum_col} found in upstream edges, in addition the
#'   the \code{sum_col} for the edge itself. As such, \code{acc_col}
#'   represents the cumulative upstream sum of \code{sum_col} for the
#'   downstream node of each line feature in \code{edges}.
#'
#' @return An \code{sf} object representing edges in the LSN, with a
#'   new \code{acc_col} column. If \code{save_local = TRUE},
#'   \code{edges} is saved to \code{lsn_path} in geopackage (.gpkg)
#'   format.
#' @export
#' @examples
#' # Get temporary directory, where the example LSN will be stored
#' # locally.
#' temp_dir <- tempdir()
#'
#' # Build the LSN. When working with your own data, lsn_path will be
#' # a local folder of your choice rather than a temporary directory.
#' edges <- lines_to_lsn(
#'   streams = MF_streams,
#'   lsn_path = temp_dir,
#'   snap_tolerance = 1,
#'   check_topology = FALSE,
#'   overwrite = TRUE,
#'   verbose = FALSE
#' )
#'
#' # Accumulate RCA area (rcaAreaKm2) downstream and store in a new
#' # column named WArea_km2
#' edges <- accum_edges(
#'   edges = edges,
#'   lsn_path = temp_dir,
#'   sum_col = "rcaAreaKm2",
#'   acc_col = "WArea_km2",
#'   save_local = FALSE,
#'   overwrite = TRUE,
#'   verbose = FALSE
#' )
#'
#' summary(edges$WArea_km2)
accum_edges <- function(edges, lsn_path, sum_col, acc_col,
                        save_local = TRUE, overwrite = FALSE,
                        verbose = TRUE) {
  # check sf object
  if (!inherits(edges, "sf")) {
    stop("edges must be an sf object.", call. = FALSE)
  }

  ## Check inputs ------------------------------------------
  ## Get geometry type as text
  edge_geom <- st_as_text(st_geometry(edges)[[1]])

  ## Check geometry type
  if (grepl("LINESTRING", edge_geom) == FALSE) {
    stop("Input edges must have LINESTRING geometry")
  }

  ## Make sure geometry column is named geometry rather than geom
  ## if(!"geometry" %in% colnames(edges)) {
  ##   edges <- st_geometry(edges, rename = "geometry")
  ## }

  ## Check lsn_path exists
  if (!file.exists(lsn_path)) {
    stop("\n lsn_path does not exist.\n\n")
  }
  ## Can we overwrite edges.gpkg if necessary
  if (overwrite == FALSE & save_local == TRUE & file.exists(paste0(lsn_path, "/edges.gpkg"))) {
    stop("edges.gpkg already exists in lsn_path and overwrite = FALSE")
  }
  ## Delete acc_col column if necessary
  ind <- colnames(edges) == acc_col
  if (sum(ind) > 0) {
    if (overwrite == FALSE) {
      stop(paste(acc_col, " already exists in edges and overwrite = FALSE"))
    } else {
      edges <- edges[, !ind]
    }
  }

  # ## Does sum_col exist
  # if(sum(colnames(edges) == sum_col) == 0) {
  #   stop(paste0(sum_col," not found in edges"))
  # }

  ## If acc_col file exists and overwrite is TRUE
  if (acc_col %in% colnames(edges)) {
    if (overwrite) {
      edges[, acc_col] <- NULL
    } else {
      stop(paste0(acc_col, " already exists in edges and overwrite = FALSE"), call. = FALSE)
    }
  }
  check_names_case(names(edges), acc_col, "edges")

  ## If fid file exists and overwrite is TRUE
  if ("fid" %in% colnames(edges)) {
    if (overwrite) {
      edges$fid <- NULL
    } else {
      stop("fid already exists in edges and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(edges), "fid", "edges")

  ## Does sum_col contain NAs
  if (sum(is.na(edges[, sum_col])) > 0) {
    stop(paste0("NA values found in ", sum_col, ". Replace these NAs with a numeric value."))
  }
  ## Import relationship table
  if (verbose == TRUE) message("\n\nImporting relationships.csv table")
  relate_table <- paste0(lsn_path, "/relationships.csv")
  rel <- read.csv(relate_table)

  ## Get vector of rid values for outlet outlet segment(s)
  if (verbose == TRUE) message("\nIdentifying outlet segments\n")
  outlet <- identify_outlet_segment(rel, edges)
  ## sources <- identify_source_segment(rel, edges)
  n_outlets <- length(outlet) ## no. of outlets == no. of networks

  ## Construct graph (igraph) of edges from relationship table
  # EDGES ARE REPRESENTED AS VERTICES, with name == rid
  if (verbose == TRUE) message("Linking edge networks and outlets\n")
  names(rel) <- c("from", "to")
  rel_as_graph <- graph_from_data_frame(rel)
  vertex_names <- vertex_attr(rel_as_graph, "name")

  ## Create an igraph subgraph for each network and return as list
  sub_graphs <- decompose(rel_as_graph)
  ## Get a list of vectors containing rid values in each subgraph
  sub_graph_rids <- lapply(sub_graphs, function(x) as.numeric(vertex_attr(x, "name")))

  ## Find outlet associated with each subgraph. Assign in_subgraph
  ## index to outlet_vs_subgraph
  outlet_vs_subgraph <- numeric(n_outlets)
  for (k in 1:n_outlets) {
    ## Return logical vector indicating whether outlet rid is
    ## contained in either sub_graph_rids vector
    in_subgraph <- unlist(lapply(sub_graph_rids, function(x) outlet[k] %in% x))
    if (any(in_subgraph)) {
      outlet_vs_subgraph[k] <- which(in_subgraph)
    } else {
      outlet_vs_subgraph[k] <- NA
    }
  }

  ## Loop through the outlets; find all upstream line features for each
  ## edge
  if (verbose == TRUE) message("Accumulating values\n")
  results_list <- vector("list", n_outlets)
  for (k in 1:n_outlets) {
    if (is.na(outlet_vs_subgraph[k])) {
      results_df <- data.frame(rid = outlet[k])
      results_df <- left_join(results_df, edges, "rid")[, c("rid", sum_col)]
      results_df[, acc_col] <- results_df[, sum_col]
    } else {
      current_subgraph <- sub_graphs[[outlet_vs_subgraph[k]]]
      n_vertices <- length(current_subgraph) ## no. edges in network
      results <- vector("list", n_vertices) ## empty list for results

      ## Find all vertices upstream of a vertex in a directed graph
      for (i in 1:(n_vertices)) {
        results[[i]] <- subcomponent(current_subgraph, v = i, mode = "in")
      }

      rids <- sub_graph_rids[[outlet_vs_subgraph[k]]]
      results_df <- data.frame(rid = rids)

      ## Merge orders by rid and left_join does not
      results_df <- merge(results_df, edges, by = "rid", sort = FALSE)[, c("rid", sum_col)]

      ## Calculate acc_col, not including to rid
      rca_sums <- unlist(lapply(
        results,
        function(x) {
          sum(results_df[
            results_df$rid %in% attributes(x)$names,
            sum_col
          ])
        }
      ))
      results_df[, acc_col] <- rca_sums

      ## Add to rid sum_col to acc_col value
      ## results_df[, acc_col] <- results_df[, acc_col] + results_df[, sum_col]
    }

    results_list[[k]] <- results_df
  }

  all_results_df <- do.call(rbind, results_list)
  ## Join result to existing sf object
  results_sf <- merge(edges, all_results_df[, c("rid", acc_col)], "rid", sort = FALSE)

  ## Write out the in_sites if save_local == TRUE
  if (save_local == TRUE) {
    if (verbose == TRUE) {
      message(paste0(
        "Saving updated edges in ",
        lsn_path, "\n"
      ))
    }
    suppressMessages(write_sf(results_sf, paste0(lsn_path, "/edges.gpkg"),
      delete_dsn = TRUE,
      quiet = TRUE
    ))
  }

  if (verbose == TRUE) message("FINISHED accum_edges successfully\n")

  return(results_sf)
}
