#' @title Calculate additive function values for sites in a Landscape Network (LSN)
#'
#' @param sites A named list of one or more `sf` objects with
#'   POINT geometry that have been snapped to the LSN using
#'   \code{\link[SSNbler]{sites_to_lsn}}.
#' @param edges `sf` object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}}.
#' @param afv_col Name of the column in \code{edges} containing
#'   the additive function value for each feature, in character
#'   format. Created using \code{\link{afv_edges}}.
#' @param save_local Logical indicating whether the updated
#'   \code{sites} should be saved to \code{lsn_path} in GeoPackage
#'   format. File basenames are taken from the names assigned to the
#'   \code{sites} list. Default is \code{TRUE}.
#' @param lsn_path Optional. Local pathname to a directory in
#'   character format specifying where the LSN resides, which is
#'   created using \code{link[SSNbler]{lines_to_lsn}}. Must be
#'   specified if \code{save_local = TRUE}.
#' @param overwrite A logical indicating whether results should be
#'   overwritten if \code{afv_col} already exists in \code{sites}
#'   or sites.gpkg already exists in \code{lsn_path} and
#'   \code{save_local = TRUE}. Default = TRUE.
#'
#' @details Spatial weights are used when fitting statistical models
#'   with 'SSN2' to split the tail up covariance function upstream of
#'   network confluences, which allows for the disproportionate
#'   influence of one upstream edge over another (e.g., a large stream
#'   channel converges with a smaller one) on downstream
#'   values. Calculating the spatial weights is a four step process:
#' 1) calculating the segment proportional influence (PI) values for
#'   the edges,
#' 2) calculating the additive function values (AFVs) for
#'   the edges,
#' 3) calculating the AFVs for the
#'   observed and prediction sites, and
#' 4) calculating the spatial
#'   weights for observed and prediction sites.
#'
#' Steps 1) and 2) are undertaken in [afv_edges()], Step 3) is
#' calculated in \code{afv_sites()}, and Step 4) is calculated in the
#' package 'SSN2' when spatial stream network models that include the
#' tail up covariance function are fit using \code{\link[SSN2]{ssn_lm}}
#' or \code{\link[SSN2]{ssn_glm}}.
#'
#' The additive function value (AFV) for an observed or
#'   prediction site is equal to the AFV of the edge the site resides
#'   on. Therefore, \eqn{0 \le AFV \le 1}. See Peterson and Ver Hoef
#'   (2010) for a more detailed description of AFVs, how they are
#'   calculated, and how they are used in the tail up covariance function.
#'
#' @return One or more `sf` object(s) with all the original data
#'   from \code{sites}, along with a new \code{afv_col} column in each
#'   \code{sites sf} object. A named list is returned. If
#'   \code{save_local = TRUE}, a GeoPackage for each `sf` object
#'   is saved in \code{lsn_path}. Output file names are assigned based
#'   on the input \code{sites} attribute \code{names}.
#'
#' @references
#' Peterson, E.E. and Ver Hoef, J.M. (2010) A
#'   mixed model moving average approach to geostatistical modeling in
#'   stream networks. Ecology 91(3), 644–651.
#' @export
#'
#' @examples
#' #' # Get temporary directory, where the example LSN will be stored
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
#' # Incorporate observed sites into the LSN
#' obs<- sites_to_lsn(
#'    sites = MF_obs,
#'    edges = edges,
#'    save_local = FALSE,
#'    snap_tolerance = 100,
#'    overwrite = TRUE,
#'    verbose = FALSE
#' )
#'
#' # Incorporate the prediction dataset, preds, into the LSN
#' preds<- sites_to_lsn(sites = MF_preds,
#'    edges = edges,
#'    save_local = FALSE,
#'    snap_tolerance = 1,
#'    overwrite = TRUE,
#'    verbose = FALSE
#' )
#'
#' # Calculate the AFVs for the edges using a column representing
#' # watershed area (h2oAreaKm2) for the downstream node of each edge
#' # feature.
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
#' # Calculate AFVs for observed sites (obs) and the prediction
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
#' # Get names of sites in site.list
#' names(site.list)
#'
#' # Check AFVs stored in new column afvArea to ensure that 0 <= AFV
#' # <= 1 and that there are no NULL values.
#' summary(site.list$obs$afvArea)
#' summary(site.list$preds$afvArea)

afv_sites <- function(sites, edges, afv_col, save_local = TRUE,
                      lsn_path = NULL, overwrite = TRUE) {
  ## check sf object
  if (any(vapply(sites, function(x) !inherits(x, "sf"), logical(1)))) {
    stop("All sites objects must be sf objects.", call. = FALSE)
  }

  ## check sf object
  if (!inherits(edges, "sf")) {
    stop("edges must be an sf object.", call. = FALSE)
  }

  ## Check inputs -------------------------------------------
  ## if(is.null(lsn_path) & save_local == TRUE) {
  ##   stop("lsn_path is required when save_local = TRUE")
  ## }

  ## ## Check lsn_path exists
  ## if (save_local == TRUE & !file.exists(lsn_path)){
  ##   stop("\n lsn_path does not exist.\n\n")
  ## }

  if (save_local == TRUE) {
    ## Check lsn_path
    if (is.null(lsn_path)) {
      stop("lsn_path is required when save_local = TRUE")
    }
    if (!file.exists(lsn_path)) {
      stop("\n lsn_path does not exist.\n\n")
    }

    ## Can we overwrite sites GeoPackage files if necessary
    if (overwrite == FALSE) {
      s.exists <- vector()
      for (e in 1:length(sites)) {
        if (file.exists(paste0(lsn_path, "/", names(sites)[e], ".gpkg"))) {
          s.exists[e] <- TRUE
        } else {
          s.exists[e] <- FALSE
        }
      }
      ## Do some sites GeoPackage files already exist
      if (sum(s.exists) > 0) {
        stop(paste0(
          "Cannot save sites to local files because at least one file already exists in ",
          lsn_path, " and overwrite = FALSE"
        ))
      }
    }
  }

  ## Stop if sites is a single sf data.frame instead of a list
  if (is.list(sites) && !all(sapply(sites, inherits, "sf"))) {
    stop("sites must be a named list of one or more sf objects")
  }

  if (is.null(names(sites))) {
    stop("sites list is missing names attribute")
  }

  ## Check for afv_col in edges and subset df if found
  if (!afv_col %in% colnames(edges)) {
    stop(paste(afv_col, "not found in edges"))
  } else {
    ## Convert edges to df
    edges_df <- edges[, c("rid", afv_col)]
    edges_df <- st_drop_geometry(edges_df)
  }

  ## Loop over sites, saving afv_col after extracting from the corresponding edge
  n_sites <- length(sites)
  for (i in 1:n_sites) {
    sites_i <- sites[[i]]

    # ## Remove afv_col if it exists and overwrite == TRUE
    # if(overwrite == TRUE & afv_col %in% colnames(sites_i)) {
    #   sites_i[, afv_col]<- NULL
    # }
    # ## Check for duplicate names
    # check_names_case_add(names(sites_i), afv_col, names(sites)[i], "afv_col")

    if (afv_col %in% names(sites_i)) {
      if (overwrite == FALSE) {
        message(
          "A column called", afv_col, "already exists in", names(sites)[i],
          "and overwrite is set to FALSE. Skipping this set of sites."
        )
        next ## skip to next iteration after message
      } else {
        ind <- colnames(sites_i) == afv_col
        sites_i <- sites_i[, !ind]
      }
    }

    ## If afv_col file exists and overwrite is TRUE (seg_pi is NOT needed in this function)
    if (afv_col %in% colnames(sites_i)) {
      if (overwrite) {
        sites_i[, afv_col] <- NULL
      } else {
        stop(paste0(afv_col, " already exists in ", sites_i, " edges and overwrite = FALSE"), call. = FALSE)
      }
    }
    check_names_case_add(names(sites_i), afv_col, names(sites)[i], "afv_col")

    ## If fid file exists and overwrite is TRUE
    if ("fid" %in% colnames(sites_i)) {
      if (overwrite) {
        sites_i$fid <- NULL
      } else {
        stop(paste0("fid", " already exists in ", sites_i, " edges and overwrite = FALSE"), call. = FALSE)
      }
    }
    check_names_case(names(sites_i), "fid", names(sites)[i])

    sites_i <- merge(sites_i, edges_df, by = "rid", sort = FALSE)

    ind.neg <- st_drop_geometry(sites_i[, afv_col]) < 0
    if (sum(ind.neg) > 0) {
      stop("negative values produced for afv_col. Go back to edgs_afv() and check infl_col, segpi_col, and afv_col")
    }

    ## Write to local file
    if (save_local) {
      st_write(sites_i,
        dsn = paste0(lsn_path, "/", names(sites)[i], ".gpkg"),
        delete_dsn = TRUE, quiet = TRUE
      )
    }

    ind.zeros <- st_drop_geometry(sites_i[, afv_col]) == 0
    sum.zeros <- sum(ind.zeros)

    if (sum.zeros > 0) {
      warning(paste0(
        sum.zeros, "/", nrow(sites_i), " AFV values equal to zero in ",
        names(sites)[i],
        ". Sites with AFV==0 will have no influence in the tail-up model.\n"
      ))
    }

    sites[[i]] <- sites_i
  }

  return(sites)
}
