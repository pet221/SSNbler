#' Snap sites to edges in a Landscape Network
#' @description Incorporates an \code{sf} object containing features
#'   with POINT geometry into a landscape network (LSN), which is a
#'   topological data model of streams/rivers represented as a
#'   directional graph embedded in 2-D geographic space. Point
#'   locations are 'snapped' to the closest edge location and new
#'   information is generated describing the geographic and
#'   topological location relative to other features in the LSN.
#' @param in_sites An \code{sf} object with POINT geometery, typically
#'   representing observed data or prediction locations.
#' @param edges An \code{sf} object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}}.
#' @param snap_tolerance Numeric distance in map units >= 0. Sites that
#'   are <= this distance from an edge feature will snapped to the
#'   closest location on the edge. When the distance between the site
#'   and all edges is > \code{snap_tolerance}, the point feature is not
#'   snapped or included in the output. 
#' @param save_local Logical indicating whether the outputs should be
#'   saved to a local directory in geopackage format. Defaults to
#'   \code{TRUE}.
#' @param output Pathname in character format, including the .gpkg extension,
#'   specifying where to store the snapped sites in geopackage
#'   format. This is typically a directory created by
#'   \code{\link{lines_to_lsn}}. Required if \code{save_local = TRUE}.
#' @param overwrite Logical indicating whether the outputs saved to
#'   \code{output} should overwrite existing files if they
#'   exist. Defaults to \code{FALSE}.
#' @param verbose Logical indicating whether progress messages should
#'   be printed to the console. Defaults to \code{TRUE}
#' 
#' @details The \code{sites_to_lsn} function is used to incorporate
#'   observed and prediction sites into the LSN. The output is an
#'   \code{sf} object with POINT geometry, which contains only the
#'   point features from \code{in_sites} that were found less than the
#'   \code{snap_tolerance} distance from the closest edge
#'   feature. When an \code{in_sites} point feature meets these
#'   requirements, it is moved (i.e. snapped) to the closest location
#'   on an edge feature. Two new columns are also added: rid and
#'   ratio. The rid column containst the rid for the edge the site has been
#'   snapped to. The second column, ratio, represents the proportional
#'   length of the edge found between the downstream end node for
#'   the edge and the updated/snapped site location. If the distance
#'   between a point feature and the closest edge is greater than or
#'   equal to the \code{snap_tolerance}, the feature is not included
#'   in the output.
#'
#'   The \code{snap_tolerance} must always be >= 0 and must be large
#' enough to snap all of the point features to the edges. Using
#' \code{snap_tolerance = 0} is not recommended, even when the
#' \code{in_sites} features intersect the edge features. Instead, a
#' very small \code{snap_tolerance} value is recommended to ensure
#' that differences in the precision of the x and y coordinates and
#' the line location do not result in unsnapped point features. 
#'
#' Note that the \code{in_sites} and \code{edges}
#'   must have the same projection.
#' 
#' @return An \code{sf} object with POINT geometry containing the
#'   features from \code{in_sites} that were found within the
#'   \code{snap_tolerance} distance to the closest edge. In addition to
#'   the original columns in \code{in_sites}, two new columns are
#'   added. The first, rid, is a unique identifier for the edge the
#'   site is snapped to. The second column, ratio, represents the
#'   proportional length of the edge that lies between the downstream
#'   end node for the edge and the updated site location. If
#'   \code{save_local = TRUE}, the \code{sf} object is also saved to
#'   \code{output}.
#' 
#' @export
sites_to_lsn <- function(in_sites, edges, snap_tolerance, save_local = TRUE,
                         output = NULL, overwrite = FALSE, 
                         verbose = TRUE){
 


  ## Check some inputs --------------------------------------------
  if(save_local == TRUE & is.null(output)) {
    stop(paste("output argument must be defined if save_local = TRUE"))
  }  
  ## stop if output file exists and overwrite == FALSE
  if(file.exists(output) & overwrite == FALSE) {
    stop(paste(output,
                "exists and overwrite == FALSE. Delete this file or set overwrite == TRUE"))
  }

  ## Stop if in_sites and/or edges are not sf objects
  if(class(in_sites)[1] != "sf" | class(edges)[1] != "sf") {
    stop("Both in_sites and edges must be of class sf")
  }

  ## Stop if in_sites and edges do not have the correct geometry types
  ## Get geometry type as text
  site_geom<- st_as_text(st_geometry(in_sites)[[1]])
  edge_geom<- st_as_text(st_geometry(edges)[[1]])

  ## Make sure geometry column is named geometry rather than geom
  if(!"geometry" %in% colnames(in_sites)) {
    sf::st_geometry(in_sites) <- "geometry"
  }
  if(!"geometry" %in% colnames(edges)) {
    sf::st_geometry(edges) <- "geometry"
  }

  ## Check geometry type
  if(grepl("POINT", site_geom) == FALSE) {
    stop("Input in_sites must have POINT geometry") }
  if(grepl("LINESTRING", edge_geom) == FALSE) {
    stop("Input edges must have LINESTRING geometry") }

  ## Stop if the crs for edges and in_sites does not match
  if(st_crs(edges) != st_crs(in_sites)) {
    stop("The CRS for edges and in_sites does not match. Reproject in_sites or edges using sf::st_transform()")
  }

  if(snap_tolerance < 0) {
    stop("snap_tolerance must be >= 0")
  }

  if(snap_tolerance == 0) {
    warning("Using a snap_tolerance = 0 may result in a large number of in_sites that are not snapped. Check the output carefully.")
    }
  

  ## Get number of input in_sites
  site_no <- nrow(in_sites)
  if(site_no == 0) {
    stop("in_sites does not contain any features")
  }
  
  ## Get nearest edge feature for each site
  if(verbose == TRUE) message("\nFinding locations on nearest edge segments\n")

  near_edge <- st_nearest_feature(in_sites, edges)
  
  ## Get rids of nearest feature
  site_rid <- edges$rid[near_edge]
  
  ## Try to extract coordinates and use geometry to project points to
  ## lines and extract relative lengths
  near_edge_coords <- st_coordinates(edges$geometry[near_edge])
  IDs <- near_edge_coords[,3]
  
  ## Same for the in_sites
  ## site_coords <- st_coordinates(in_sites)
  
  ## Get index of near_edge elements
  index <- 1:length(near_edge)
  
  ## Get coordinates for nearest features
  # tidy
  # coords <- edges %>% 
  #   dplyr::pull(geometry) %>% 
  #   .[near_edge] %>% 
  #   st_coordinates() %>% 
  #   as.data.frame
  # untidy
  coords <- as.data.frame(st_coordinates(edges$geometry[near_edge]))
  
  ## Get list of sfc for each edge segment associated with a site
  # tidy
  # segments_list <- coords %>% 
  #   split(.$L1) %>%
  #   map(get_segments_vectorized) 
  # untidy
  segments_list <- lapply(split(coords, coords$L1), get_segments_vectorized)
  
  # tidy
  # segments_sfc_list <- segments_list %>% map(st_sfc)
  # untidy
  segments_sfc_list <- lapply(segments_list, st_sfc)
  
  ## Assign CRS to new sfc
  for(i in 1:length(segments_sfc_list)) st_crs(segments_sfc_list[[i]]) <- st_crs(edges)
  
  ## Find nearest subsegment on closest edge and return a vector of
  ## sub-segment identifiers
  # tidy
  # subseg_list <- 1:nrow(in_sites) %>%
  #   map(find_nearest_vectorized, sites = in_sites, edges = segments_sfc_list)
  # untidy
  subseg_list <- lapply(1:nrow(in_sites), find_nearest_vectorized, sites = in_sites, edges = segments_sfc_list)
  
  # tidy
  # subseg_nearest <- subseg_list %>% unlist
  # untidy
  subseg_nearest <- unlist(subseg_list)
  
  ## Do the snapping----

  ## Get closest line sub-segment to each site
  ## returns a list of length nrow(in_sites).
  ## Each list element is a list of length 1 containing the
  ## sfg for the closest sub-segment 
  if(verbose == TRUE) message("Snapping points to edges\n")
  # tidy
  # the_nearest <- index %>%
  #   map(extract_nearest, subsegs = segments_sfc_list, nearest = subseg_nearest)
  # untidy
  the_nearest <- lapply(index, extract_nearest, subsegs = segments_sfc_list, nearest = subseg_nearest)

  ## v_snapping finds the nearest point on subsegment to original site
  ## locations. Returns a list of length
  ## nrow(in_sites). Each element contains an sfc LINESTRING object with
  ## 1 sfg LINESTRING containing endnode coordinates for the old site location
  ## and the new site location.
  # tidy
  # the_result <- index %>%
  #   map(v_snapping, sites = in_sites, nearest_subsegs = the_nearest)
  # untidy
  the_result <- lapply(index, v_snapping, sites = in_sites, nearest_subsegs = the_nearest)

  ## Get the vector of snap distance by calculating the length of the
  ## line
  # tidy
  # snapping_distances <- the_result %>%
  #   map(st_length) %>%
  #   unlist %>%
  #   as.numeric
  # untidy
  snapping_distances <- as.numeric(unlist(lapply(the_result, st_length)))

  ## Returns a list of length nrow(in_sites), where each element is an
  ## sfc POINT feature geometry for the new site location
  # tidy
  # the_result2 <- the_result %>%
  #   map(v_snapextract)
  # untidy
  the_result2 <- lapply(the_result, v_snapextract)
  
  ## Convert to an sfc POINT geometry set with nrow(in_sites) 
  snapped_sf <- do.call(c, the_result2)
  
  ## Overwrite geometry column if lengths < snap_tolerance
  ## this means the point *has been snapped*
  meets_condition <- as.numeric(snapping_distances) <= snap_tolerance
  in_sites$geometry[meets_condition] <- snapped_sf[meets_condition]
  
  ## Get matrix of snapped in_sites as coords
  snapped_sites_as_coords <- st_coordinates(snapped_sf)
  
  ## Add snapping lengths column to in_sites
  ## NA values for snap length when no snapping has occurred 
  snap_dists <- snapping_distances
  snap_dists[!meets_condition] <- NA
  
  ## NA rid vales for sites where no snapping occurred
  site_rid[!meets_condition] <- NA

  ## Get ratio value for snapped sites ----------------------
  if(verbose == TRUE) message("Calculating ratio values\n")
  
  ## Compute lengths of all subsegments and return the cumulative sum
  ## of length moving from the most upstream subsegment to the most
  ## downstream subsegment?  Returns a list of length nrow(in_sites), with
  ## each element containing a vector of cumulative lengths upstream
  ## for the associated edge
  # tidy
  # length_at_start_of_segment <- map(segments_list, v_length) %>%
  #   map(v_cumsum)
  # untidy
  length_at_start_of_segment <- lapply(lapply(segments_list, v_length), v_cumsum)

  ## Get list containing total length of the associated line segments
  # tidy
  # max_length <- map(length_at_start_of_segment, max)
  # untidy
  max_length <- lapply(length_at_start_of_segment, max)

  ## Return list of matrices containing the coordinates for the
  ## endnodes for the nearest subsegment associated with each site
  # tidy
  # near_edge_coords <- the_nearest %>%
  #   map(st_coordinates)
  # untidy
  near_edge_coords <- lapply(the_nearest, st_coordinates)

  #######################################################################
  ## Calculate the distance along the line segment from most
  ## upstream node to the snapped site location. Returns a numeric
  ## vector of length nrow(in_sites)
  # tidy
  # site_along_length <- 1:nrow(in_sites) %>% 
  #   map(get_updist, sites = snapped_sites_as_coords,
  #       sub_idx = subseg_list, sub_near = near_edge_coords,
  #       lengths = length_at_start_of_segment) %>%
  #   unlist
  # untidy
  site_along_length <- unlist(lapply(1:nrow(in_sites), get_updist, sites = snapped_sites_as_coords,
                                     sub_idx = subseg_list, sub_near = near_edge_coords,
                                     lengths = length_at_start_of_segment))

  ## Calculate ratio of (distance from downstream line segment node to
  ## site location):(total length of the line segment)
  ratios <- 1 - unlist(site_along_length)/unlist(max_length)

  #######################################################################
  
  ## Add columns to in_sites
  # tidy
  # in_sites <- in_sites %>% 
  #   mutate(rid = site_rid, ratio = ratios, snapdist = snap_dists)
  # untidy
  in_sites$rid <- site_rid
  in_sites$ratio <- ratios
  in_sites$snapdist <- snap_dists

  ## Remove in_sites that were not snapped
  # tidy
  # in_sites <- in_sites %>%
  #   dplyr::filter(!is.na(rid) & !is.na(snapdist))
  # untidy
  in_sites <- in_sites[!is.na(in_sites$rid) & !is.na(in_sites$snapdist), , drop = FALSE]
  
  
  ## Write out the in_sites if save_local == TRUE
  if(save_local == TRUE) {
    if(verbose == TRUE) message(paste0("Saving snapped sites ",
                 output, "\n"))

    if(overwrite == TRUE){
      suppressMessages(write_sf(in_sites, output, delete_dsn = overwrite,
                                quiet = TRUE))
    } else {
      suppressMessages(write_sf(in_sites, output, quiet = TRUE))
    }
  }
   
  if(verbose == TRUE) message("FINISHED sites_to_lsn script successfully")
  if(verbose == TRUE) message(paste0("Snapped ", nrow(in_sites), " out of ", site_no, " sites to LSN\n"))
  
  ## Return result  
  return(in_sites)  
}
