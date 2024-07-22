#' Incorporate sites into a Landscape Network
#' @description Incorporates an \code{sf} object containing features
#'   with POINT geometry into a landscape network (LSN), which is a
#'   topological data model of streams/rivers represented as a
#'   directional graph embedded in 2-D geographic space. Point
#'   locations are 'snapped' to the closest edge location and new
#'   information is generated describing the geographic and
#'   topological location relative to other features in the LSN.
#' @param sites An \code{sf} object with POINT geometery, typically
#'   representing observed data or prediction locations.
#' @param edges An \code{sf} object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}}.
#' @param snap_tolerance Numeric distance in map units >= 0. Sites that
#'   are <= this distance from an edge feature will snapped to the
#'   closest location on the edge. When the distance between the site
#'   and all edges is > \code{snap_tolerance}, the point feature is not
#'   snapped or included in the lsn_path. 
#' @param save_local Logical indicating whether the outputs should be
#'   saved to a local directory in geopackage format. Defaults to
#'   \code{TRUE}.
#' @param lsn_path Pathname to the LSN. This is typically a directory created by
#'   \code{\link{lines_to_lsn}}. Required if \code{save_local = TRUE}.
#' @param file_name Filename for output sites, which are saved to \code{lsn_path}
#'   in geopackage format (must include the .gpkg extension). Required if \code{save_local = TRUE}.
#' @param overwrite Logical indicating whether the outputs saved to
#'   \code{lsn_path} should overwrite existing files if they
#'   exist. Defaults to \code{FALSE}.
#' @param verbose Logical indicating whether progress messages should
#'   be printed to the console. Defaults to \code{TRUE}
#' 
#' @details The \code{sites_to_lsn} function is used to incorporate
#'   observed and prediction sites into the LSN. The output is an
#'   \code{sf} object with POINT geometry, which contains only the
#'   point features from \code{sites} that were found less than the
#'   \code{snap_tolerance} distance from the closest edge
#'   feature. When a \code{sites} point feature meets these
#'   requirements, it is moved (i.e. snapped) to the closest location
#'   on an edge feature. Three new columns are also added: rid, ratio,
#'   and snapdist. The rid column contains the rid for the edge the
#'   site has been snapped to. The second column, ratio,
#'   represents the proportional length of the edge found between the
#'   downstream end node for the edge and the updated/snapped site
#'   location. The snapdist is the distance in map units that the site
#'   was moved. If the distance between a point feature and the
#'   closest edge is greater than or equal to the
#'   \code{snap_tolerance}, the feature is not included in the output.
#'
#'   The \code{snap_tolerance} must always be \eqn{\ge 0} and must be large
#' enough to snap all of the point features to the edges. Using
#' \code{snap_tolerance = 0} is not recommended, even when the
#' \code{sites} features intersect the edge features. Instead, a
#' very small \code{snap_tolerance} value is recommended to ensure
#' that differences in the precision of the x and y coordinates and
#' the line location do not result in unsnapped point features. 
#'
#' Note that the \code{sites} and \code{edges}
#'   must have the same projection.
#' 
#' @return An \code{sf} object with POINT geometry containing the
#'   features from \code{sites} that were found within the
#'   \code{snap_tolerance} distance to the closest edge. In addition to
#'   the original columns in \code{sites}, three new columns are
#'   added: rid, ratio, and snapdist (see Details). If
#'   \code{save_local = TRUE}, the \code{sf} object is also saved to
#'   \code{lsn_path}.
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
#' # # Incorporate observed sites, MF_obs, into LSN
#' obs<- sites_to_lsn(
#'    sites = MF_obs,
#'    edges = edges,
#'    save_local = FALSE,
#'    snap_tolerance = 100,
#'    overwrite = TRUE,
#'    verbose = FALSE
#' )
#' # Notice that 3 new columns have been added to obs
#' names(obs)
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
sites_to_lsn <- function(sites, edges, snap_tolerance, save_local = TRUE,
                         lsn_path = NULL, file_name = NULL, overwrite = FALSE, 
                         verbose = TRUE){
  
  
  # check sf object
  if (!inherits(sites, "sf")) {
    stop("sites must be an sf object.", call. = FALSE)
  }
  
  # check sf object
  if (!inherits(edges, "sf")) {
    stop("edges must be an sf object.", call. = FALSE)
  }
  
  
  ## Check some inputs --------------------------------------------
  ## Remove column names assigned by sites_to_lsn if they exist &
  ## overwrite == TRUE
  # if(overwrite == TRUE & sum(c("rid", "ratio", "snapdist", ) %in%
  #                            colnames(sites) > 0)) {
  #   sites$rid <- NULL
  #   sites$snapdist <- NULL
  #   sites$ratio<-NULL
  # }
  
  # ## Check for duplicate column names
  # check_names_case(names(sites), "rid", "sites")
  # check_names_case(names(sites), "ratio", "sites")
  # check_names_case(names(sites), "snapdist", "sites")
  
  # check bad column names
  ## If rid file exists and overwrite is TRUE
  if ("rid" %in% colnames(sites)) {
    if (overwrite) {
      sites$rid <- NULL
    } else {
      stop("rid already exists in sites and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(sites), "rid", "sites")
  
  ## If ratio file exists and overwrite is TRUE
  if ("ratio" %in% colnames(sites)) {
    if (overwrite) {
      sites$ratio <- NULL
    } else {
      stop("ratio already exists in sites and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(sites), "ratio", "sites")
  
  ## If snapdist file exists and overwrite is TRUE
  if ("snapdist" %in% colnames(sites)) {
    if (overwrite) {
      sites$snapdist <- NULL
    } else {
      stop("snapdist already exists in sites and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(sites), "snapdist", "sites")
  
  ## If fid file exists and overwrite is TRUE
  if ("fid" %in% colnames(sites)) {
    if (overwrite) {
      sites$fid <- NULL
    } else {
      stop("fid already exists in sites and overwrite = FALSE", call. = FALSE)
    }
  }
  check_names_case(names(sites), "fid", "sites")

  # save local
  if(save_local == TRUE & is.null(lsn_path)) {
    stop(paste("lsn_path argument must be defined if save_local = TRUE"))
  }

  ## Check and fix file_name format if necessary
  if(save_local == TRUE) {
    if(!is.null(file_name)) {
      
      ## Check file_name
      f.ext.shp <- substr(file_name, nchar(file_name)-3, nchar(file_name)) == ".shp"
      f.ext.gpkg <- substr(file_name, nchar(file_name)-4, nchar(file_name)) == ".gpkg"
      
      if(f.ext.shp == TRUE) {
        file_name<- paste0(substr(file_name, 1, nchar(file_name)-4), ".gpkg")
        warning(paste0("file_name changed to ", file_name))
      }
      if(f.ext.shp == FALSE & f.ext.gpkg == FALSE) {
        file_name <- paste0(file_name, ".gpkg")
      }
      
      ## store "new" lsn path that combines lsn path with lsn extension
      ## old lsn path had both together
      lsn_path <- paste0(lsn_path, "/", file_name)
      ## if needing absolute paths
      ## lsn_path <- normalizePath(paste0(lsn_path, "/", file_name), mustWork = FALSE)
      
      ## stop if lsn_path file exists and overwrite == FALSE
      if(file.exists(lsn_path) & overwrite == FALSE) {
        stop(paste(lsn_path,
                   "exists and overwrite == FALSE. Delete this file or set overwrite == TRUE"))
      }
    } else {
      stop("file_name cannot be NULL when save_local = TRUE")
    }
  }
  
  ## Stop if sites and/or edges are not sf objects
  if( (!inherits(sites, "sf")) || (!inherits(edges, "sf"))) {
    stop("Both sites and edges must an sf object.", call. = FALSE)
  }
  
  # check empty geometries
  if (any(st_is_empty(edges))) {
    stop("edges has at least one empty geometry. Check and/or use sf::st_is_empty() to remove.",
         call. = FALSE)
  }
  
  # check empty geometries
  if (any(st_is_empty(sites))) {
    stop("sites has at least one empty geometry. Check and/or use sf::st_is_empty() to remove.",
         call. = FALSE)
  }
  
  ## Stop if sites and edges do not have the correct geometry types
  ## Get geometry type as text
  site_geom<- st_as_text(st_geometry(sites)[[1]])
  edge_geom<- st_as_text(st_geometry(edges)[[1]])
  
  ## Check geometry type
  if(grepl("POINT", site_geom) == FALSE) {
    stop("Input sites must have POINT geometry") }
  if(grepl("LINESTRING", edge_geom) == FALSE) {
    stop("Input edges must have LINESTRING geometry") }
  
  ## Stop if the crs for edges and sites does not match
  if(st_crs(edges) != st_crs(sites)) {
    stop("The CRS for edges and sites does not match. Reproject sites or edges using sf::st_transform()")
  }
  
  if(snap_tolerance < 0) {
    stop("snap_tolerance must be >= 0")
  }
  
  if(snap_tolerance == 0) {
    warning("Using a snap_tolerance = 0 may result in a large number of sites that are not snapped. Check the output carefully.")
  }  
  
  ## Get number of input sites
  site_no <- nrow(sites)
  if(site_no == 0) {
    stop("sites does not contain any features")
  }
  
  ## Get nearest edge feature for each site
  if(verbose == TRUE) message("\nFinding locations on nearest edge segments\n")
  
  near_edge <- st_nearest_feature(sites, edges)
  
  ## Get rids of nearest feature
  site_rid <- edges$rid[near_edge]
  
  ## Try to extract coordinates and use geometry to project points to
  ## lines and extract relative lengths
  g.col <- attributes(edges)$sf_col
  near_edge_coords<- st_coordinates(edges[near_edge,g.col]) 
  ##old_edge_coords<- st_coordinates(edges$geom[near_edge])
  IDs <- near_edge_coords[,3]
  
  ## Same for the sites
  ## site_coords <- st_coordinates(sites)
  
  ## Get index of near_edge elements
  index <- 1:length(near_edge)

  coords<- as.data.frame(st_coordinates(edges[near_edge,g.col]))
  ##old_coords <- as.data.frame(st_coordinates(edges$geom[near_edge]))
  
  ## Get list of sfc for each edge segment associated with a site
  segments_list <- lapply(split(coords, coords$L1), get_segments_vectorized)
  
  # tidy
  segments_sfc_list <- lapply(segments_list, st_sfc)
  
  ## Assign CRS to new sfc
  for(i in 1:length(segments_sfc_list)) st_crs(segments_sfc_list[[i]]) <- st_crs(edges)
  
  ## Find nearest subsegment on closest edge and return a vector of
  ## sub-segment identifiers
  subseg_list <- lapply(1:nrow(sites), find_nearest_vectorized, sites = sites, edges = segments_sfc_list)
  
  # tidy
  # subseg_nearest <- subseg_list %>% unlist
  # untidy
  subseg_nearest <- unlist(subseg_list)
  
  ## Do the snapping----
  ## Get closest line sub-segment to each site
  ## returns a list of length nrow(sites).
  ## Each list element is a list of length 1 containing the
  ## sfg for the closest sub-segment 
  if(verbose == TRUE) message("Snapping points to edges\n")
  the_nearest <- lapply(index, extract_nearest, subsegs = segments_sfc_list,
                        nearest = subseg_nearest)
  
  ## v_snapping finds the nearest point on subsegment to original site
  ## locations. Returns a list of length
  ## nrow(sites). Each element contains an sfc LINESTRING object with
  ## 1 sfg LINESTRING containing endnode coordinates for the old site location
  ## and the new site location.
  
  the_result <- lapply(index, v_snapping, sites = sites, nearest_subsegs = the_nearest)
  
  ## Get the vector of snap distance by calculating the length of the
  ## line
  snapping_distances <- as.numeric(unlist(lapply(the_result, st_length)))
  
  ## Returns a list of length nrow(sites), where each element is an
  ## sfc POINT feature geometry for the new site location
  the_result2 <- lapply(the_result, v_snapextract)
  
  ## Convert to an sfc POINT geometry set with nrow(sites) 
  snapped_sf <- do.call(c, the_result2)

  ## Overwrite geometry column if lengths < snap_tolerance
  ## this means the point *has been snapped*
  meets_condition <- as.numeric(snapping_distances) <= snap_tolerance
  sg.col <- attributes(sites)$sf_column
  sites[meets_condition, sg.col] <- as.data.frame(snapped_sf[meets_condition])
  ##sites$geometry[meets_condition] <- snapped_sf[meets_condition]
  
  ## Get matrix of snapped sites as coords
  snapped_sites_as_coords <- st_coordinates(snapped_sf)
  
  ## Add snapping lengths column to sites
  ## NA values for snap length when no snapping has occurred 
  snap_dists <- snapping_distances
  snap_dists[!meets_condition] <- NA
  
  ## NA rid vales for sites where no snapping occurred
  site_rid[!meets_condition] <- NA
  
  ## Get ratio value for snapped sites ----------------------
  if(verbose == TRUE) message("Calculating ratio values\n")
  
  ## Compute lengths of all subsegments and return the cumulative sum
  ## of length moving from the most upstream subsegment to the most
  ## downstream subsegment?  Returns a list of length nrow(sites), with
  ## each element containing a vector of cumulative lengths upstream
  ## for the associated edge
  length_at_start_of_segment <- lapply(lapply(segments_list, v_length), v_cumsum)
  
  ## Get list containing total length of the associated line segments
  max_length <- lapply(length_at_start_of_segment, max)
  
  ## Return list of matrices containing the coordinates for the
  ## endnodes for the nearest subsegment associated with each site
  near_edge_coords <- lapply(the_nearest, st_coordinates)
  
  #######################################################################
  ## Calculate the distance along the line segment from most
  ## upstream node to the snapped site location. Returns a numeric
  ## vector of length nrow(sites)
  site_along_length <- unlist(lapply(1:nrow(sites), get_updist,
                                     sites = snapped_sites_as_coords,
                                     sub_idx = subseg_list, sub_near = near_edge_coords,
                                     lengths = length_at_start_of_segment))
  
  ## Calculate ratio of (distance from downstream line segment node to
  ## site location):(total length of the line segment)
  ratios <- 1 - unlist(site_along_length)/unlist(max_length)
  
  #######################################################################
  
  ## Add columns to sites
  sites$rid <- site_rid
  sites$ratio <- ratios
  sites$snapdist <- snap_dists
  
  ## Remove sites that were not snapped
  sites <- sites[!is.na(sites$rid) & !is.na(sites$snapdist), , drop = FALSE]
  
  
  ## Write out the sites if save_local == TRUE
  if(save_local == TRUE) {
    if(verbose == TRUE) message(paste0("Saving snapped sites ",
                                       lsn_path, "\n"))
    
    if(overwrite == TRUE){
      suppressMessages(write_sf(sites, lsn_path, delete_dsn = overwrite,
                                quiet = TRUE))
    } else {
      suppressMessages(write_sf(sites, lsn_path, quiet = TRUE))
    }
  }
  
  if(verbose == TRUE) message("FINISHED sites_to_lsn script successfully")
  if(verbose == TRUE) message(paste0("Snapped ", nrow(sites), " out of ", site_no, " sites to LSN\n"))
  
  ## Return result  
  return(sites)  
}
