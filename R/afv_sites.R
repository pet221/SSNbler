#' @title Calculate additive function values for sites in a LSN
#'
#' @param sites A named list of one or more \code{sf} objects with
#'   POINT geometry that have been snapped to the LSN using
#'   \code{\link[SSNbler]{sites_to_lsn}}.
#' @param edges An \code{sf} object with LINESTING geometry created
#'   using \code{\link{lines_to_lsn}}.
#' @param afv_col Name of the column in \code{edges} containing
#'   the additive function value for each feature, in character
#'   format. Created using \code{\link{afv_edges}}.
#' @param save_local Logical indicating whether the updated
#'   \code{sites} should be saved to \code{lsn_path} in geopackage
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
#' @return One or more \code{sf} object(s) with all the original data
#'   from \code{sites}, along with a new \code{afv_col} column in each
#'   \code{sites sf} object. A named list is returned. If
#'   \code{save_local = TRUE}, a geopackage for each \code{sf} object
#'   is saved in \code{lsn_path}. Output file names are assigned based
#'   on the input \code{sites} attribute \code{names}.
#' @export

afv_sites <- function(sites, edges, afv_col, save_local = TRUE,
                      lsn_path=NULL, overwrite = TRUE){

  ## Check inputs -------------------------------------------
  if(is.null(lsn_path) & save_local == TRUE) {
    stop("lsn_path is required when save_local = TRUE")
  }
    
  ## Check lsn_path exists
  if (save_local == TRUE & !file.exists(lsn_path)){
    stop("\n lsn_path does not exist.\n\n")
  } 
  
  ## Can we overwrite sites geopackage files if necessary
  if(save_local == TRUE & overwrite == FALSE) {
    s.exists<- vector()
    for(e in 1:length(sites)) {
      if(file.exists(paste0(lsn_path, "/", names(sites)[e], ".gpkg"))){
        s.exists[e] <- TRUE
      } else {
        s.exists[e]<-FALSE
      }
    }
    ## Do some sites geopackage files already exist 
    if(sum(s.exists) > 0) {
      stop(paste0("Cannot save sites to local files because at least one file already exists in ",
                  lsn_path, " and overwrite = FALSE"))
    }
  }

  ## Stop if sites is a single sf data.frame instead of a list
  if (is.list(sites) && !all(sapply(sites, inherits, "sf"))) {
    stop("sites must be a named list of one or more sf objects")
  }

  if(is.null(names(sites))) {
    stop("sites list is missing names attribute")
  }

  ## Check for afv_col in edges and subset df if found
  if(!afv_col %in% colnames(edges)) {
    stop(paste(afv_col, "not found in edges"))
  } else { 
    ## Convert edges to df
    edges_df <- edges[, c("rid", afv_col)]
    edges_df <- st_drop_geometry(edges_df)
  }
    
  ## Loop over sites, saving afv_col after extracting from the corresponding edge
  n_sites <- length(sites)
  for(i in 1:n_sites){

    sites_i <- sites[[i]]
    
    check_names_case_add(names(sites_i), afv_col, names(sites)[i], "afv_col")

    if(afv_col %in% names(sites_i)){
      if(overwrite == FALSE) {
        message("A column called", afv_col, "already exists in", names(sites)[i],
                "and overwrite is set to FALSE. Skipping this set of sites.")
        next ## skip to next iteration after message
      } else {
        ind<- colnames(sites_i) == afv_col
        sites_i<- sites_i[,!ind]
      }   
    } 

    sites_i<- merge(sites_i, edges_df, by = "rid", sort = FALSE)

    ind.neg<- st_drop_geometry(sites_i[,afv_col]) <0
    if(sum(ind.neg) > 0) {
      stop("negative values produced for afv_col. Go back to edgs_afv() and check infl_col, segpi_col, and afv_col")
    }

    ## Write to local file
    if(save_local) {
      st_write(sites_i, dsn = paste0(lsn_path, "/", names(sites)[i], ".gpkg"),
                                     delete_dsn = TRUE, quiet = TRUE)
    }
   
    ind.zeros <- st_drop_geometry(sites_i[,afv_col]) == 0
    sum.zeros<-sum(ind.zeros)
    
    if(sum.zeros > 0) {
      warning(paste0(sum.zeros, "/", nrow(sites_i), " AFV values equal to zero in ",
                     names(sites)[i],
                     ". Sites with AFV==0 will have no influence in the tail-up model.\n"))
    }
    
    sites[[i]]<- sites_i
  }
  
  return(sites) 
}
