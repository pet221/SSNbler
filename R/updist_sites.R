#' @title Get upstream distance for sites in a Landscape Network (LSN)
#'
#' @param sites A named list of one or more `sf` objects with
#'   POINT geometry that have been snapped to the LSN using
#'   \code{\link[SSNbler]{sites_to_lsn}}.
#' @param edges An `sf` object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}} and
#'   \code{link[SSNbler]{updist_edges}}.
#' @param length_col The name of the column in \code{edges} that
#'   contains the length of each edge feature.
#' @param save_local Logical indicating whether the updated
#'   \code{sites} should be saved to \code{lsn_path} in GeoPackage
#'   format. File basenames are taken from the names assigned to the
#'   \code{sites} list. Default is \code{TRUE}.
#' @param lsn_path Local pathname to a directory in
#'   character format specifying where the LSN resides, which is
#'   created using \code{link[SSNbler]{lines_to_lsn}}. Must be
#'   specified if \code{save_local = TRUE}.
#' @param overwrite A logical indicating whether results should be
#'   overwritten if the upDist column already exists in \code{sites}
#'   or sites.gpkg already exists in \code{lsn_path} and
#'   \code{save_local = TRUE}. Default = TRUE.
#'
#' #' @details \code{updist_sites()} calculates the total hydrologic distance from
#'   each observed or prediction point feature to
#'   the stream outlet (i.e. the most downstream location in the
#'   stream network), when movement is restricted to the stream
#'   network. We refer to this as the upstream distance.
#'
#' Upstream distances are measured in the map projection units for the
#'   `sf` object containing the point features and stored in a
#'   new column named \code{upDist}.
#'
#' The upstream distances stored in \code{upDist} are used to
#' calculate the pairwise hydrologic distances used to fit spatial
#' stream network models in the 'SSN2' package. Do not modify the name
#' of the column in any way or the values the \code{upDist} column
#' contains.
#'
#' @return One or more `sf` object(s) with all the original
#'   data from \code{sites}, along with a new \code{upDist} column in
#'   each sites `sf` object. A named list is returned. If
#'   \code{save_local = TRUE}, a GeoPackage for each `sf` object
#'   is saved in \code{lsn_path}. Output file names are assigned based
#'   on the input \code{sites} attribute \code{names}.
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
#' preds<- sites_to_lsn(
#'    sites = MF_preds,
#'    edges = edges,
#'    save_local = FALSE,
#'    snap_tolerance = 1,
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
#' # Calculate upstream distance for observed sites (obs) and one
#' # prediction dataset (preds)
#' site.list<- updist_sites(
#'    sites = list(obs = obs,
#'                 preds = preds),
#'    edges = edges,
#'    length_col= "Length",
#'    lsn_path = temp_dir,
#'    save_local = FALSE,
#'    overwrite = TRUE
#' )
#'
#' # Summarize the new column upDist in obs
#' summary(site.list$obs$upDist)
#'
#' # Summarize the new column upDist in preds
#' summary(site.list$preds$upDist)
#'
updist_sites <- function(sites, edges, length_col, lsn_path, save_local = TRUE,
                         overwrite = TRUE) {
  # check sf object
  if (any(vapply(sites, function(x) !inherits(x, "sf"), logical(1)))) {
    stop("All sites objects must be sf objects.", call. = FALSE)
  }

  # check sf object
  if (!inherits(edges, "sf")) {
    stop("edges must be an sf object.", call. = FALSE)
  }

  ## Check inputs -------------------------------------------
  ## Check lsn_path exists when save_local = TRUE
  if (save_local == TRUE & !file.exists(lsn_path)) {
    stop("\n lsn_path does not exist.\n\n")
  }

  ## Can we overwrite sites GeoPackage files if necessary
  if (save_local == TRUE & overwrite == FALSE) {
    s.exists <- vector()
    for (e in 1:length(sites)) {
      if (file.exists(paste0(lsn_path, "/", names(sites)[e], ".gpkg"))) {
        s.exists[e] <- TRUE
      } else {
        s.exists[e] <- FALSE
      }
    }
    ## Do some sites GeoPackage files already exist when overwrite = FALSE and save_local = TRUE
    if (sum(s.exists) > 0) {
      stop(paste0(
        "Cannot save sites to local files because at least one file already exists in ",
        lsn_path, " and overwrite = FALSE"
      ))
    }
  }

  ## Stop if sites is a single sf data.frame instead of a list
  if (is.list(sites) && !all(sapply(sites, inherits, "sf"))) {
    stop("sites must be a named list of one or more sf objects")
  }

  ## Check if upDist is present in edges
  if (!"upDist" %in% names(edges)) {
    stop("upDist column not present in edges. Run updist_edges() to add this column.")
  }

  ## Format edges
  edges <- edges[, c("rid", "upDist", length_col)]
  ind <- colnames(edges) == "upDist"
  colnames(edges)[ind] <- "uDist"
  edges <- st_drop_geometry(edges)

  ## Loop over the sites list
  out_sites <- list()
  n_sites <- length(sites)
  for (i in 1:n_sites) {
    ## Get output site name and sf object
    sites_i_name <- names(sites)[i]
    sites_i_sf <- sites[[i]]



    ## Check if upDist column already exists and overwrite is FALSE
    ## If so, skip this iteration in the loop
    if ("upDist" %in% names(sites_i_sf) & !overwrite) {
      message(
        "A column called 'upDist' already exists in", sites_i_name,
        "and overwrite is set to FALSE. This set of sites will be skipped."
      )
      next
    }

    # ## Remove the upDist column if it exists before the join with edges
    # if("upDist" %in% names(sites_i_sf) & overwrite) {
    #   ## ind<- colnames(sites_i_sf) == "upDist"
    #   ## sites_i_sf <- sites_i_sf[, !ind]
    #
    #   sites_i_sf$upDist <- NULL
    # }
    #
    # ## Check for duplicate names
    # check_names_case(names(sites_i_sf), "upDist", sites_i_name)

    ## If upDist file exists and overwrite is TRUE
    if ("upDist" %in% colnames(sites_i_sf)) {
      if (overwrite) {
        sites_i_sf$upDist <- NULL
      } else {
        stop(paste0("upDist already exists in ", sites_i_sf, " and and overwrite = FALSE"), call. = FALSE)
      }
    }
    check_names_case(names(sites_i_sf), "upDist", sites_i_name)

    ## If fid file exists and overwrite is TRUE
    if ("fid" %in% colnames(sites_i_sf)) {
      if (overwrite) {
        sites_i_sf$fid <- NULL
      } else {
        stop(paste0("fid already exists in ", sites_i_sf, " and and overwrite = FALSE"), call. = FALSE)
      }
    }
    check_names_case(names(sites_i_sf), "fid", sites_i_name)

    ## Get the Length and upDist fields from the edges
    sites_i_sf <- merge(sites_i_sf, edges, by = "rid", all.x = TRUE)
    ## Compute the new upDist for sites based on the ratio, updist and length of edge
    sites_i_sf$upDist <- sites_i_sf[, "uDist", drop = TRUE] -
      sites_i_sf[, length_col, drop = TRUE] +
      (sites_i_sf[, "ratio", drop = TRUE] * sites_i_sf[, length_col, drop = TRUE])
    ## Remove uDist and length column from edges
    ind <- colnames(sites_i_sf) %in% c("uDist", length_col)
    sites_i_sf <- sites_i_sf[, !ind]

    if (save_local == TRUE) {
      ## Write out the result
      write_sf(sites_i_sf, paste0(lsn_path, "/", sites_i_name, ".gpkg"),
        delete_layer = overwrite
      )
    }

    out_sites[[i]] <- sites_i_sf
  }

  ## Add names out out_sites list
  names(out_sites) <- names(sites)

  return(out_sites)

  message("\nFINISHED updist_sites script successfully\n")
}
