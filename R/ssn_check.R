#' @title Check an `SSN` object
#'
#' @description Check an `SSN` (spatial stream network) object to ensure that it contains valid spatial, topological, and attribute information needed to fit spatial statistical stream network models using the 'SSN2' package.
#'
#' @param ssn.object An `SSN` object created using \code{\link[SSNbler]{ssn_assemble}} or imported using \code{\link[SSN2]{ssn_import}}.
#' @param check_obs A logical indicating whether the observations should be checked. Default = \code{TRUE}.
#' @param afv_col Character vector containing  names of columns containing additive function values. The Default = \code{NULL}.
#' @param verbose Logical \code{TRUE/FALSE} indicating whether details describing the checks are printed to the console. If \code{verbose = FALSE}, a logical \code{TRUE/FALSE} is returned indicating whether the `SSN` object passed all of the checks. Default = \code{TRUE}.
#'
#' @return Boolean indicating whether the `SSN` object is valid. If \code{verbose = TRUE}, additional messages are printed to the console describing potential issues with the `SSN` object.
#'
#' @export
#'
#' @examples
#' ## Create local temporary copy of MiddleFork04.ssn found in
#' ## the SSN2 package. Only necessary for this example.
#' SSN2::copy_lsn_to_temp()
#'
#' # Import the SSN object with prediction points, pred1km
#' mf04 <- SSN2::ssn_import(
#'   paste0(tempdir(), "/MiddleFork04.ssn"),
#'   predpts = c("pred1km"),
#'   overwrite = TRUE
#' )
#'
#' # Check the SSN object, including the additive function column,
#' # afvArea
#' ssn_check(mf04, afv_col = "afvArea")
#'
ssn_check <- function(ssn.object, check_obs = TRUE, afv_col = NULL, verbose = TRUE) {
  out.message <- "\n"
  valid <- TRUE

  ## check that ssn.object == SSN
  if (!inherits(ssn.object, "SSN")) {
    stop("ssn.object is not of class SSN", call. = FALSE)
  }

  ## check that .ssn folder exists
  if (!file.exists(ssn.object$path)) {
    stop(paste0(
      out.message, ssn.object$path,
      " does not exist. Use SSN2:ssn_update_path() to update the path to the local .ssn directory.\n"
    ))
  }

  ## check that binaryID.db exists in .ssn directory
  if (!file.exists(paste0(ssn.object$path, "/binaryID.db"))) {
    out.message <- paste0(
      "binaryID.db file is missing from ",
      ssn.object$path, ".\n"
    )
    valid <- FALSE
  }

  ## --------------------------------------------------------
  ## check edges
  ## --------------------------------------------------------

  ## Check class of edges
  if (!inherits(ssn.object$edges, "sf")) {
    stop("edges are not of class sf")
  }

  ## Check geometry of edges
  edge_geom <- st_as_text(st_geometry(ssn.object$edges)[[1]])
  if (grepl("LINESTRING", edge_geom) == FALSE) {
    out.message <- paste0(out.message, "edges do not have LINESTRING geometry\n")
    valid <- FALSE
  }

  ## check for empty geometries
  empty.edges <- sum(st_is_empty(ssn.object$edges))

  if (empty.edges > 0) {
    out.message <- paste0(out.message, "edges contain missing geometries. Use sf:st_is_empty() to identify these line features.\n")
    valid <- FALSE
  }

  ## Check netgeom----------------------------------------------
  if ("netgeom" %in% colnames(ssn.object$edges)) {
    ## Check contents of netgeom
    ng.message <- check_netgeom(ssn.object$edges, type = "edges", verbose = TRUE)
    if (!is.null(ng.message)) {
      out.message <- paste0(out.message, ng.message)
      valid <- FALSE
    }
  } else {
    out.message <- paste0(out.message, "edges netgeom column not found\n")
    valid <- FALSE
  }

  ## Check AFV cols-----------------------------------------
  if (!is.null(afv_col)) {
    edges.df <- st_drop_geometry(ssn.object$edges)

    for (i in 1:length(afv_col)) {
      afv_i <- afv_col[i]

      ## Does AFV column exist
      if (afv_i %in% colnames(ssn.object$edges)) {
        if (sum(is.na(edges.df[, afv_i])) > 0) {
          out.message <- paste0(
            out.message, "edges AFV column, ",
            afv_i, ", contains NAs\n"
          )
          valid <- FALSE
        }

        if (sum(edges.df[, afv_i] < 0) > 0 |
          sum(edges.df[, afv_i] > 1) > 0) {
          out.message <- paste0(
            out.message, "edges AFV column, ",
            afv_i,
            ", contains values < 0 and/or > 1.\n"
          )
          valid <- FALSE
        }
      } else {
        out.message <- paste0(
          out.message, "AFV column, ", afv_i,
          ", not found in edges\n"
        )
        valid <- FALSE
      }
    }
  }

  ## --------------------------------------------------------
  ## check obs
  ## --------------------------------------------------------

  if (check_obs == TRUE) {
    ## Check class of obs
    if (!inherits(ssn.object$obs, "sf")) {
      stop("obs are not of class sf")
    }

    ## Check geometry of obs
    obs_geom <- st_as_text(st_geometry(ssn.object$obs)[[1]])
    if (grepl("POINT", obs_geom) == FALSE) {
      out.message <- paste0(out.message, "obs do not have POINT geometry\n")
      valid <- FALSE
    }

    ## check for empty geometries ----------------
    empty.obs <- sum(st_is_empty(ssn.object$obs))

    if (empty.obs > 0) {
      out.message <- paste0(
        out.message,
        "obs contain missing geometries. Use sf:st_is_empty() to identify these point features.\n"
      )
      valid <- FALSE
    }

    ## Check netgeom----------------------------------------------
    if ("netgeom" %in% colnames(ssn.object$obs)) {
      ## Check contents of netgeom
      ng.message <- check_netgeom(ssn.object$obs, type = "obs", verbose = TRUE)
      if (!is.null(ng.message)) {
        out.message <- paste0(out.message, ng.message)
        valid <- FALSE
      }
    } else {
      out.message <- paste0(out.message, "obs netgeom column not found\n")
      valid <- FALSE
    }

    ## Check AFV cols ---------------------------------
    if (!is.null(afv_col)) {
      obs.df <- st_drop_geometry(ssn.object$obs)

      for (i in 1:length(afv_col)) {
        afv_i <- afv_col[i]

        ## Does AFV column exist
        if (afv_i %in% colnames(ssn.object$obs)) {
          if (sum(is.na(obs.df[, afv_i])) > 0) {
            out.message <- paste0(out.message, "obs AFV column, ", afv_i, ", contains NAs\n")
            valid <- FALSE
          }

          if (sum(obs.df[, afv_i] < 0) > 0 |
            sum(obs.df[, afv_i] > 1) > 0) {
            out.message <- paste0(
              out.message, "obs AFV column, ",
              afv_i,
              ", contains values < 0 and/or > 1\n"
            )
            valid <- FALSE
          }
        } else {
          out.message <- paste0(
            out.message, "obs AFV column, ", afv_i,
            ", not found\n"
          )
          valid <- FALSE
        }
      }
    }
  }

  ## ---------------------------------------------------------
  ## check preds, if they exist
  ## ---------------------------------------------------------

  pred.names <- names(ssn.object$preds)

  if (!is.null(pred.names)) {
    for (p in 1:length(pred.names)) {
      ## Check class of preds
      if (!inherits(ssn.object$preds[[pred.names[p]]], "sf")) {
        stop(paste0(pred.names[p], " are not of class sf"))
      }

      ## Check geometry of preds
      pred_geom <- st_as_text(st_geometry(ssn.object$preds[[pred.names[p]]])[[1]])
      if (grepl("POINT", pred_geom) == FALSE) {
        out.message <- paste0(out.message, pred.names[p], " does not have POINT geometry\n")
        valid <- FALSE
      }

      ## check for empty geometries ----------------
      empty.preds <- sum(st_is_empty(ssn.object$preds[[pred.names[p]]]))

      if (empty.preds > 0) {
        out.message <- paste0(
          out.message, pred.names[p],
          " contains missing geometries. Use sf:st_is_empty() to identify these point features.\n"
        )
        valid <- FALSE
      }

      ## Check netgeom----------------------------------------------
      if ("netgeom" %in% colnames(ssn.object$preds[[pred.names[p]]])) {
        ## Check contents of netgeom
        ng.message <- check_netgeom(ssn.object$preds[[pred.names[p]]],
          type = pred.names[p], verbose = TRUE
        )
        if (!is.null(ng.message)) {
          out.message <- paste0(out.message, ng.message)
          valid <- FALSE
        }
      } else {
        out.message <- paste0(out.message, pred.names[p], " netgeom column not found\n")
        valid <- FALSE
      }

      ## Check AFV cols ---------------------------------
      if (!is.null(afv_col)) {
        pred.df <- st_drop_geometry(ssn.object$preds[[pred.names[p]]])

        for (i in 1:length(afv_col)) {
          afv_i <- afv_col[i]

          ## Does AFV column exist
          if (afv_i %in% colnames(ssn.object$preds[[pred.names[p]]])) {
            if (sum(is.na(pred.df[, afv_i])) > 0) {
              out.message <- paste0(
                out.message, pred.names[p], " AFV column, ",
                afv_i, ", contains NAs\n"
              )
              valid <- FALSE
            }

            if (sum(pred.df[, afv_i] < 0) > 0 |
              sum(pred.df[, afv_i] > 1) > 0) {
              out.message <- paste0(
                out.message, pred.names[p],
                " AFV column, ", afv_i,
                ", contains values < 0 and/or > 1\n"
              )
              valid <- FALSE
            }
          } else {
            out.message <- paste0(
              out.message, pred.names[p], " AFV column, ", afv_i,
              ", not found\n"
            )
            valid <- FALSE
          }
        }
      }
    }
  }

  ## Output results
  if (verbose == TRUE) {
    cat(out.message)
    cat(paste0("\nSSN object is valid: ", valid, "\n"))
  } else {
    return(valid)
  }
}
