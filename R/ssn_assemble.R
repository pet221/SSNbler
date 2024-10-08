#' @title Assemble an `SSN` object from an LSN
#'
#' @description Create an `SSN` (spatial stream network) object from a Landscape Network (LSN).
#'
#' @param edges An `sf` object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}} (see Details).
#' @param lsn_path Local pathname to a directory in character format
#'   specifying where relationships.csv resides, which is created
#'   using \code{link{lines_to_lsn}}.
#' @param obs_sites Optional. A single `sf` object with POINT
#'   geometry created using \code{link{sites_to_lsn}} that represents
#'   the observation locations (i.e. where data were
#'   collected). Default = NULL (see Details).
#' @param preds_list Optional. A list of one or more `sf` objects
#'   representing prediction sites.
#' @param ssn_path Pathname to an output directory where output files
#'   will be stored. A .ssn extension will be added if it is not
#'   included.
#' @param import Logical indicating whether the output files should be
#'   returned as an `SSN` object. Defaults to \code{TRUE}.
#' @param check Logical indicating whether the validity of the
#'   `SSN` should be checked using \code{[ssn_check]} when both
#'   \code{import = TRUE} and \code{verbose = TRUE}. Default = \code{TRUE}.
#' @param afv_col Character vector containing the names of the
#'   additive function value columns that will be checked when
#'   \code{check = TRUE}. Columns must be present in \code{edges},
#'   \code{obs_sites} and \code{preds_list}, if all are
#'   included. Default is \code{NULL}.
#' @param overwrite Logical. If \code{TRUE} and \code{ssn_path}
#'   already exists, the contents of \code{ssn_path} will be
#'   overwritten. Defaults to \code{FALSE}.
#' @param verbose Logical. Indicates whether messages about the
#'   function progress and object validity check (when \code{check = TRUE} should be printed to the console. Defaults to
#'   \code{TRUE}.
#'
#' @details The \code{SSNbler} package is used to generate the
#'   spatial, topological, and attribute information needed to fit
#'   spatial stream-network models using the 'SSN2' package. The
#'   \code{ssn_assemble} function will often be the final step in the
#'   'SSNbler' data-processing workflow and it is important that
#'   the previous processing steps have been followed. Prior to
#'   running \code{ssn_assemble}, the \code{edges} must be processed
#'   using \code{link{lines_to_lsn}}, \code{link{updist_edges}}, and
#'   \code{link{afv_edges}}. The \code{obs_sites} and prediction site
#'   datasets in \code{preds_list} must be processed with
#'   \code{link{sites_to_lsn}}, \code{link{updist_sites}}, and
#'   \code{link{afv_sites}}. In addition, the \code{edges},
#'   \code{obs_sites}, and all of the `sf` objects in
#'   \code{preds_list} must be part of the same LSN.
#'
#'   The \code{obs_sites} and \code{preds_list} are optional arguments,
#'   with the Default = NULL. If \code{obs_sites = NULL}, an
#'   `SSN` object will be returned with NA stored in
#'   \code{ssn.object$obs} and a warning returned that
#'   \code{ssn.object$obs} is required for fitting spatial statistical
#'   models in 'SSN2'.
#'
#'   \code{ssn_assemble} stores the output locally in \code{ssn_path}. If
#'   \code{ssn_path} does not include the .ssn extension, it is added
#'   before the new directory is created. This directory contains:
#'   \itemize{
#'      \item edges.gpkg: edges in GeoPackage format. A new network identifier, netID, is added that is unique to each subnetwork.
#'      \item sites.gpkg: observed sites in GeoPackage format (if present). Three new ID columns are added that are unique to the measurement (pid), the location (locID), and the network (netID).
#'      \item prediction datasets in GeoPackage format (if present). The prediction sites also contain pid, locID, and netID. The naming convention is taken from the names provided in \code{preds_list}.
#'      \item netID.dat files for each distinct network, which store the binaryID values for line segments in edges.
#'   }
#'   A more detailed description of the .ssn directory and its contents is provided in Peterson and Ver Hoef (2014).
#'
#' @return The components of an `SSN` object are written to
#'     \code{ssn_path} (see Details). When \code{import = TRUE}, the
#'     function also returns an object of class `SSN`. If
#'     \code{check = TRUE} and \code{verbose = TRUE}, the validity of the returned `SSN`
#'     object is checked using \code{[ssn_check]} and results are
#'     printed to the console.
#'
#' @export
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
#' # Incorporate observed sites, MF_obs, into LSN
#' obs<- sites_to_lsn(
#'    sites = MF_obs,
#'    edges = edges,
#'    save_local = FALSE,
#'    snap_tolerance = 100,
#'    overwrite = TRUE,
#'    verbose = FALSE
#' )
#'
#' # Incorporate prediction dataset, MF_preds, into LSN
#' preds<- sites_to_lsn(sites = MF_preds,
#'    edges = edges,
#'    save_local = FALSE,
#'    snap_tolerance = 1,
#'    overwrite = TRUE,
#'    verbose = FALSE
#' )
#'
#' # Calculate the AFV for the edges using
#' # a column representing watershed area (h2oAreaKm2).
#' edges<- afv_edges(
#'    edges=edges,
#'    infl_col = "h2oAreaKm2",
#'    segpi_col = "areaPI",
#'    lsn_path = temp_dir,
#'    afv_col = "afvArea",
#'    overwrite = TRUE,
#'    save_local = FALSE
#' )
#'
#' # Calculate the AFV for observed sites (obs) and prediction
#' # dataset, preds.
#' site.list<- afv_sites(
#'    sites = list(obs = obs,
#'                 preds = preds),
#'    edges=edges,
#'    afv_col = "afvArea",
#'    save_local = FALSE,
#'    overwrite = TRUE
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
#' # Calculate upstream distance for observed sites (obs) and one
#' # prediction dataset (preds)
#' site.list<- updist_sites(
#'    sites = site.list,
#'    edges = edges,
#'    length_col= "Length",
#'    lsn_path = temp_dir,
#'    save_local = FALSE,
#'    overwrite = TRUE
#' )
#'
#' # Assemble SSN object
#' ssn.obj<- ssn_assemble(
#'    edges = edges,
#'    lsn_path = temp_dir,
#'    obs_sites = site.list[["obs"]],
#'    preds_list = site.list[c("preds")],
#'    ssn_path = paste0(temp_dir, "/example.ssn"),
#'    import = TRUE,
#'    overwrite = TRUE
#' )
#'
#' # Summarise SSN object
#' summary(ssn.obj)
#'
ssn_assemble <- function(edges, lsn_path = NULL, obs_sites = NULL,
                         preds_list = NULL, ssn_path, import = TRUE,
                         check = TRUE, afv_col = NULL, overwrite = FALSE,
                         verbose = TRUE) {
  ## Check all inputs ------------------------------------------------
  if (verbose == TRUE) {
    message("\nChecking inputs\n")
  }

  obs.exist <- !is.null(obs_sites)
  preds.exist <- !is.null(preds_list)

  ## check if edges is sf object
  if (!inherits(edges, "sf")) {
    stop("edges must be an sf object.", call. = FALSE)
  }

  ## Stop if obs_sites is not a single sf data.frame
  if (obs.exist) {
    if (!inherits(obs_sites, "sf")) {
      if (is.list(obs_sites)) {
        stop("obs_sites must be a single sf object, not a list.", call. = FALSE)
      } else {
        stop("obs_sites must be an sf object.", call. = FALSE)
      }
    }
  }

  ## check preds_list contains sf object(s)
  if (preds.exist == TRUE) {
    if (any(vapply(preds_list, function(x) !inherits(x, "sf"), logical(1)))) {
      stop("All preds objects must be sf objects.", call. = FALSE)
    }
  }

  ## Edges
  ## geometry type
  edge_geom <- st_as_text(st_geometry(edges)[[1]])
  if (grepl("LINESTRING", edge_geom) == FALSE) {
    stop("Input edges must have LINESTRING geometry")
  }

  ## Can we overwrite edges.gpkg if necessary
  if (overwrite == FALSE & file.exists(paste0(ssn_path, "/edges.gpkg"))) {
    stop("edges.gpkg already exists in ssn_path and overwrite = FALSE")
  }

  ## If netID file exists and overwrite is TRUE
  if ("netID" %in% colnames(edges)) {
    if (overwrite) {
      edges$netID <- NULL
    } else {
      stop("netID already exists in edges and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(edges), "netID", "edges")

  ## If fid file exists and overwrite is TRUE
  if ("fid" %in% colnames(edges)) {
    if (overwrite) {
      edges$fid <- NULL
    } else {
      stop("fid already exists in edges and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(edges), "fid", "edges")

  ## lsn_path
  if (!file.exists(lsn_path)) {
    stop("\n lsn_path does not exist.\n\n")
  }

  ## Check for .ssn extension
  if (substr(ssn_path, nchar(ssn_path) - 3, nchar(ssn_path)) != ".ssn") {
    ssn_path <- paste0(ssn_path, ".ssn")
    message(
      "ssn_path folder must have an .ssn extension. ssn_path changed to ",
      paste0(ssn_path), "\n"
    )
  }

  ## Check whether .ssn path exists
  if (file.exists(ssn_path) & overwrite == FALSE) {
    stop("\n ssn_path exists and overwrite = FALSE")
  }

    ## Print warning if check == TRUE and verbose == FALSE
  if (import == TRUE & check == TRUE & verbose == FALSE) {
      warning("check == TRUE and verbose == FALSE. SSN object will not be checked. Set verbose = TRUE to check SSN object or run ssn_check() separately.")

  }

  ## obs_sites
  if (obs.exist) {
    obs.geom <- st_as_text(st_geometry(obs_sites)[[1]])
    if (class(obs_sites)[1] != "sf" | grepl("POINT", obs.geom) == FALSE) {
      stop("obs_sites must be an sf object with POINT geometry")
    }

    if (file.exists(paste0(ssn_path, "/obs.gpkg")) & overwrite == FALSE) {
      stop("obs.gpkg exists in ssn_path and overwrite == FALSE")
    }

    if (sum(colnames(obs_sites) %in% c("pid", "locID", "netID")) > 0 & overwrite == FALSE) {
      stop(paste0("Columns pid, locID, and/or netID exist in obs_sites and overwrite = FALSE"))
    }

    ## If pid file exists and overwrite is TRUE
    if ("pid" %in% colnames(obs_sites)) {
      if (overwrite) {
        obs_sites$pid <- NULL
      } else {
        stop("pid already exists in obs_sites and overwrite = FALSE", call. = FALSE)
      }
    }
    check_names_case(names(obs_sites), "pid", "obs_sites")

    ## If locID file exists and overwrite is TRUE
    if ("locID" %in% colnames(obs_sites)) {
      if (overwrite) {
        obs_sites$locID <- NULL
      } else {
        stop("locID already exists in obs_sites and overwrite = FALSE", call. = FALSE)
      }
    }
    check_names_case(names(obs_sites), "locID", "obs_sites")

    ## If netID file exists and overwrite is TRUE
    if ("netID" %in% colnames(obs_sites)) {
      if (overwrite) {
        obs_sites$netID <- NULL
      } else {
        stop("netID already exists in obs_sites and overwrite = FALSE", call. = FALSE)
      }
    }
    check_names_case(names(obs_sites), "netID", "obs_sites")

    ## If fid file exists and overwrite is TRUE
    if ("fid" %in% colnames(obs_sites)) {
      if (overwrite) {
        obs_sites$fid <- NULL
      } else {
        stop("fid already exists in obs_sites and overwrite = FALSE", call. = FALSE)
      }
    }
    check_names_case(names(obs_sites), "fid", "obs_sites")

    if (attr(obs_sites, "sf_column") != attr(edges, "sf_column")) {
      st_geometry(obs_sites) <- attr(edges, "sf_column")
    }
  }
  #################################################
  ## Check each set of preds in predlist
  #################################################

  if (preds.exist) {
    for (p in 1:length(preds_list)) {
      p.geom <- st_as_text(st_geometry(preds_list[[p]])[[1]])

      if (class(preds_list[[p]])[1] != "sf" | grepl("POINT", p.geom) == FALSE) {
        stop(paste0(names(preds_list)[p], " must be an sf object with POINT geometry"))
      }

      if (file.exists(paste0(ssn_path, "/", names(preds_list)[p], ".gpkg")) & overwrite == FALSE) {
        stop(paste0(names(preds_list), ".gpkg exists in ssn_path and overwrite == FALSE"))
      }

      if (sum(colnames(preds_list[[p]]) %in% c("pid", "locID", "netID")) > 0 & overwrite == FALSE) {
        stop(paste0(
          "Columns pid, locID, and/or netID exist in ", names(preds_list)[p],
          " and overwrite = FALSE"
        ))
      }

      ## If pid file exists and overwrite is TRUE
      if ("pid" %in% colnames(preds_list[[p]])) {
        if (overwrite) {
          preds_list[[p]]$pid <- NULL
        } else {
          stop(paste0("pid already exists in ", names(preds_list)[p], " and overwrite = FALSE", call. = FALSE))
        }
      }
      check_names_case(colnames(preds_list[[p]]), "pid", names(preds_list)[p])

      ## If locID file exists and overwrite is TRUE
      if ("locID" %in% colnames(preds_list[[p]])) {
        if (overwrite) {
          preds_list[[p]]$locID <- NULL
        } else {
          stop(paste0("locID already exists in ", names(preds_list)[p], " and overwrite = FALSE", call. = FALSE))
        }
      }
      check_names_case(colnames(preds_list[[p]]), "locID", names(preds_list)[p])

      ## If netID file exists and overwrite is TRUE
      if ("netID" %in% colnames(preds_list[[p]])) {
        if (overwrite) {
          preds_list[[p]]$netID <- NULL
        } else {
          stop(paste0("netID already exists in ", names(preds_list)[p], " and overwrite = FALSE", call. = FALSE))
        }
      }
      check_names_case(colnames(preds_list[[p]]), "pid", names(preds_list)[p])

      ## If fid file exists and overwrite is TRUE
      if ("fid" %in% colnames(preds_list[[p]])) {
        if (overwrite) {
          preds_list[[p]]$fid <- NULL
        } else {
          stop(paste0("fid already exists in ", names(preds_list)[p], " and overwrite = FALSE", call. = FALSE))
        }
      }
      check_names_case(colnames(preds_list[[p]]), "fid", names(preds_list)[p])

      if (attr(preds_list[[p]], "sf_column") != attr(edges, "sf_column")) {
        st_geometry(preds_list[[p]]) <- attr(edges, "sf_column")
      }
    }
  }

  ## Create a .ssn folder for outputs
  if (!file.exists(ssn_path)) {
    try(dir.create(ssn_path)) ## create directory
    if (verbose == TRUE) {
      message("\n", paste0(ssn_path, " created.\n"), appendLF = FALSE)
    }
  } else {
    ## output directory exists, print warning, delete folder and re-create it now
    unlink(ssn_path, recursive = TRUE)
    dir.create(ssn_path) ## re-create directory
    if (verbose == TRUE) {
      message("\nOutput .ssn directory exists. Directory and contents are being deleted and recreated.\n",
        appendLF = FALSE
      )
    }
  }

  ## ----------------------------------------------------------------
  ## Create binary IDs
  ## ----------------------------------------------------------------
  ## Create Binary Segment ID
  if (verbose == TRUE) {
    message("\nCreating binaryID.db\n")
  }

  ## Read relationships.csv
  if (file.exists(paste(lsn_path, "relationships.csv", sep = "/"))) {
    rel <- read.csv(paste(lsn_path, "relationships.csv", sep = "/"))
  } else {
    stop(paste0(
      lsn_path, "/relationships.csv does not exist. Is lsn_path = ",
      lsn_path, " correct?"
    ))
  }

  ## Get outlet rid values
  all_rids <- edges$rid
  outlets <- which(!all_rids %in% rel$fromedge)

  ## Create igraph object from the relationship tables and reverse direction
  rel_graph <- graph_from_data_frame(rel, directed = T)
  inv_rel_graph <- reverse_edges(rel_graph)

  ## Get subgraphs
  networks <- decompose(graph = inv_rel_graph)
  n_networks <- length(outlets)
  ## outlets is the correct vector to derive the number of networks from
  ## single-edge networks will not show up in the networks list

  ## Identify the rids in each network
  rids_net <- lapply(networks, function(x) as.numeric(vertex_attr(x, "name")))

  #####################################################
  ## For loop: Identify outlets for sub-networks
  #####################################################

  ## Find which outlet is in which subgraph
  outlet_vs_subgraph <- numeric(n_networks)
  for (k in 1:n_networks) {
    in_subgraph <- unlist(lapply(rids_net, function(x) outlets[k] %in% x))
    if (any(in_subgraph)) {
      outlet_vs_subgraph[k] <- which(in_subgraph)
    } else {
      outlet_vs_subgraph[k] <- NA
    }
  } ################################################
  ## Set of binary labels
  binary_label <- c("0", "1")

  ## Initialise list to store data.frames of binaryID, netID, rid
  result_list <- vector("list", n_networks)

  ############################################################
  ## Assign binaryID by sub-network
  ############################################################
  ## Loop through each of the networks,
  ## Assign binary labels to each edge
  for (j in 1:n_networks) {
    ## If only one edge in network
    if (is.na(outlet_vs_subgraph[j])) {
      results_frame <- data.frame(rid = outlets[j], bid = 1)
    } else {
      ## Create empty vectors to store binary labels
      rid_record <- c()
      bin_record <- c()
      subgraph_j <- networks[[outlet_vs_subgraph[j]]]

      ############################################################
      ## Nested for loop: Each vertex/edge in each network
      ## Investigate for removal later
      ############################################################
      for (i in 1:length(subgraph_j)) {
        num_connections <- length(subgraph_j[[i]][[1]])
        rid_i <- attributes(subgraph_j[[i]][[1]])$names
        if (num_connections > 2) stop("Topological error: more than two edges flowing into this confluence.")
        if (num_connections == 0) next
        if (num_connections == 2) {
          rid_record <- c(rid_record, rid_i)
          bin_record <- c(bin_record, binary_label)
        } else {
          ## Case where no connections (outlet) or one connection (pseudo-node)
          ## These just need to be assigned a value of "1"
          rid_record <- c(rid_record, rid_i)
          bin_record <- c(bin_record, "1")
        }
      }

      bin_frame <- data.frame(rid = rid_record, bin = bin_record)

      ## Find number of "vertices" in the graph (excluding outlet segment)
      ## N.B. edges = vertices
      n_vertices <- length(subgraph_j)
      results <- vector("list", n_vertices - 1)
      for (k in 1:(n_vertices - 1)) {
        results[[k]] <- shortest_paths(subgraph_j,
          from = length(subgraph_j),
          to = attributes(subgraph_j[[k]])$name
        )$vpath[[1]]
      }
      results_frame <- data.frame(rid = vertex_attr(subgraph_j, "name"))

      bids <- unlist(lapply(results, function(x) {
        first_pass <- bin_frame$bin[match(attributes(x)$names, bin_frame$rid)]
        last_pass <- gsub("NA", "1", paste(first_pass, collapse = ""))
        return(last_pass)
      }))


      ## Join this to the results frame
      results_frame$bid <- c(bids, "1")
    }

    ## Rename columns
    names(results_frame) <- c("rid", "binaryID")

    ## Write binary id file
    dat_filename <- paste(ssn_path, "/netID", j, ".dat", sep = "")
    write.table(results_frame, dat_filename,
      row.names = FALSE,
      sep = ",", quote = FALSE
    )

    ## Add this to the list of results
    results_frame$netID <- j
    result_list[[j]] <- results_frame
  }

  ## Collapse results list into a single data frame
  netid_df <- do.call(rbind.data.frame, result_list)
  netid_df$netID <- as.numeric(netid_df$netID)
  netid_df$rid <- as.numeric(netid_df$rid)

  if (verbose == TRUE) {
    message("Adding NetID and netgeom to edges \n")
  }

  ## ## Drop column for binary ids and join to the edges attribute table
  if ("netID" %in% colnames(edges)) {
    edges <- subset(edges, select = -get("netID"))
  }
  edges <- merge(edges, subset(netid_df, select = -get("binaryID")), all.x = TRUE, by = "rid")

  ## Add netgeom column
  edges <- create_netgeom(edges, type = "LINESTRING", overwrite = TRUE)

  #############################################################################
  ## Add locID and pid to obs_sites attribute table
  #############################################################################
  if (verbose == TRUE) {
    message("\npid, locID, netID, and netgeom added to ...")
  }
  ## ---------------------------------------------------------
  ## Assign pid values and get unique point locations for
  ## assigning locID
  ## ---------------------------------------------------------
  if (obs.exist) {
    n_pid <- nrow(obs_sites)
    obs_sites$pid <- seq_len(n_pid)
    all_pids <- obs_sites[, "pid"]

    ## Create master locID table - using base R unique instead of
    ## dplyr::distinct will be too slow for large datasets
    obs_sites$locID <- NA
    all_geoms <- obs_sites[, c("locID")]
    obs_sites <- subset(obs_sites, select = -get("locID"))
  } else {
    n_pid <- 0
    all_pids <- NULL
    all_geoms <- NULL
  }

  if (preds.exist) {
    for (z in 1:length(preds_list)) {
      ## Assign pid values
      n_preds <- nrow(preds_list[[z]])
      preds_list[[z]]$pid <- n_pid + seq_len(n_preds)
      n_pid <- n_pid + n_preds

      ## Save to pid table for later
      all_pids <- rbind(all_pids, preds_list[[z]][, "pid"])

      ## Remove locID if it exists
      if ("locID" %in% colnames(preds_list[[z]])) {
        preds_list[[z]] <- subset(preds_list[[z]], select = -get("locID"))
      }

      ## Get geometry
      pred.tmp <- preds_list[[z]]
      pred.tmp$locID <- NA
      all_geoms <- rbind(all_geoms, pred.tmp[, "locID"])
    }
  }

  if (!is.null(all_geoms)) {
    ## Get unique locations using distinct.sf. Using base R unique()
    ## instead of dplyr::distinct.sf will be too slow for large datasets
    unq_geoms <- distinct(all_geoms)

    ## Add unique locID
    n_locs <- nrow(unq_geoms)
    unq_geoms$locID <- seq_len(n_locs)

    ## Add locID to all_pids based on geometry
    all_ids <- st_join(all_pids, unq_geoms, join = st_equals)
    all_ids <- st_drop_geometry(all_ids)

    ## Add locIDs and netIDs to obs_sites
    if (obs.exist) {
      obs_sites <- merge(obs_sites, all_ids, all.x = TRUE, by = "pid")

      ## Add netID to obs_sites
      if ("netID" %in% colnames(obs_sites)) {
        obs_sites <- subset(obs_sites, select = -get("netID"))
      }
      obs_sites <- merge(obs_sites, subset(netid_df, select = -get("binaryID")),
        all.x = TRUE, by = "rid"
      )

      ## Add netgeom column
      obs_sites <- create_netgeom(obs_sites, type = "POINT", overwrite = TRUE)

      if (verbose == TRUE) {
        message("\nobs_sites")
      }
    }
  }

  ############################################################
  ## For loop: For each set of pred sites, assign locID, netID
  ##   and pid
  ############################################################

  if (preds.exist) {
    ## Loop through pred sites datasets
    for (p in 1:length(preds_list)) {
      ## Assign locIDs to the obs_sites - using unique instead of dplyr::distinct
      ## may be too slow for large datasets
      pred.tmp <- preds_list[[p]]

      pred.tmp <- merge(pred.tmp, all_ids, all.x = TRUE, by = "pid")

      if ("netID" %in% colnames(pred.tmp)) {
        pred.tmp <- subset(pred.tmp, select = -get("netID"))
      }
      pred.tmp <- merge(pred.tmp, subset(netid_df, select = -get("binaryID")),
        all.x = TRUE, by = "rid"
      )

      ## Add netgeom column
      pred.tmp <- create_netgeom(pred.tmp, type = "POINT", overwrite = TRUE)

      preds_list[[p]] <- pred.tmp
      rm(pred.tmp)

      if (verbose == TRUE) {
        message(paste0("\n", names(preds_list)[p]))
      }
    }
  }

  ###########################################################################
  ## Export sf objects to GeoPackages
  ###########################################################################

  if (verbose == TRUE) {
    message(paste("\nSaving files to", ssn_path))
  }

  st_write(edges, paste(ssn_path, "edges.gpkg", sep = "/"),
    quiet = TRUE
  )

  ## Do not export obs_sites if they do not exist
  if (obs.exist) {
    st_write(obs_sites, paste(ssn_path, "sites.gpkg", sep = "/"),
      quiet = TRUE
    )
  } else {
    obs_sites <- NA
  }

  if (preds.exist) {
    ############################################
    ## For loop: write files
    ############################################
    for (w in 1:length(preds_list)) {
      st_write(preds_list[[w]],
        dsn = paste0(ssn_path, "/", names(preds_list)[w], ".gpkg"),
        quiet = TRUE
      )
    }
  } else {
    preds_list <- list()
  }

  ## ---------------------------------------------------------------
  ## If import = TRUE, construct and return SSN object
  ## ---------------------------------------------------------------
  if (import == TRUE) {
    if (verbose == TRUE) {
      message("\nCreating SSN object")
    }

    ## Create SSN object
    ssnlist <- list(edges = edges, obs = obs_sites, preds = preds_list, path = ssn_path)
    class(ssnlist) <- "SSN"

    ## Create Binary ID database
    createBinaryID(ssnlist, overwrite = overwrite)

    ## ## Check the SSN object
    if (check == TRUE & verbose == TRUE) {

        message("\nChecking the SSN object")


      if (obs.exist) {
          check.msg <- ssn_check(ssnlist,
                                 afv_col = afv_col,
                                 verbose = verbose)
      } else {
        check.msg <- ssn_check(ssnlist,
          check_obs = FALSE,
          afv_col = afv_col,
          verbose = verbose
        )
      }
        cat(check.msg)

    }

    return(ssnlist)
  }
}
