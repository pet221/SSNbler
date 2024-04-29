#' @title Create netgeom column in SSN object
#' 
#' @description Create netgeom column for edges, observed sites,
#'   and/or prediction sites in a Landscape Network (LSN).
#' 
#' @param sf_data An \code{sf} object with LINESTING or POINT geometry
#'   created using \code{link{lsn_to_ssn}} (see Details).
#' @param type Character string defining geometry type of
#'   \code{sf_data}. Default = \code{NULL}.
#' @param overwrite Logical indicating whether existing data should be
#'   overwritten if present. Default = \code{TRUE}.
#'
#' @details Most users will not need to run \code{create_netgeom2}
#'   themselves because it is called internally when \code{lsn_to_ssn}
#'   is run or an \code{SSN} is imported using
#'   \code{link[SSN2]{ssn_import}} found in the \code{SSN2}
#'   package. For users who do wish to run \code{create_netgeom2}, the
#'   \code{sf_data} object must represent edges, observed sites, or
#'   prediction sites in a \code{SSN} object created using
#'   \code{link{lsn_to_ssn}}.
#'
#'   The netgeom column contains information in character format used
#'   to describe the topology of the LSN. The format and content of
#'   the netgeom column differs depending on whether \code{sf_data}
#'   contains LINESTRING (edges) or POINT (observed or prediction
#'   sites) geometry. For edges, the netgeom format is:
#'   \itemize{
#'       \item{\code{'ENETWORK (netID, rid, upDist)'}}
#'   } 
#'
#' The rid, upDist and netID columns must already be present in edges
#'   before netgeom is added. These columns are created usining
#'   \code{link{lines_to_lsn}}, \code{\link{updist_edges}}, and
#'   \code{link{lsn_to_ssn}}, respectively.
#' 
#'   For observed or prediction sites, the netgeom format is:
#'   \itemize{
#'       \item{\code{'SNETWORK (netID, rid, upDist, ratio, pid, locID)'}}
#'   }
#'
#' The rid, ratio, upDist, netID, pid, and locID columns must be
#' present in \code{sf_data} and are created using
#' \code{link{sites_to_lsn}}, \code{link{updist_sites}}, and
#' \code{link{lsn_to_ssn}}, respectively.
#'   
#' If \code{overwrite = TRUE} and a column named netgeom is present in
#' \code{sf_data}, the data will be overwritten. Default = FALSE. 
#' 
#' @return An \code{sf} object containing the original data from
#'   \code{sf_data} and an additional column named netgeom.
#' 
#' @export

create_netgeom2 <- function(sf_data, type = NULL, overwrite = TRUE) {
  if (type == "POINT") {
    sf_data[, "netgeom"] <- paste0("SNETWORK (", paste(
      sf_data$netID, sf_data$rid, sf_data$upDist,
      sf_data$ratio, sf_data$pid, sf_data$locID
    ), ")", sep = "")
  }
  if(type == "LINESTRING") {
    sf_data[, "netgeom"] <- paste0("ENETWORK (", paste(
      sf_data$netID,
      sf_data$rid,
      sf_data$upDist
    ),
    ")",
    sep = ""
    )
  }
  return(sf_data) ## Return sf data.frame with netgeom column added
}
