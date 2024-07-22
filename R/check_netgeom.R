check_netgeom<- function(sf.obj, type, name = NULL, verbose = TRUE) {

  valid.message <- NULL
  valid.bool <- TRUE
  netgeom.valid <- TRUE

  ## General checks --------------------------------
  ## Check class
  if(!is.character(sf.obj$netgeom)) {
    valid.message <- paste0(valid.message, 
                            "\n", type,
                            " netgeom column is not in character format\n")
    valid.bool <- FALSE
  }

  ## Check for NA values
  if(sum(is.na(sf.obj$netgeom)) > 0) {
    valid.message<- paste0(valid.message, " netgeom column in ", type,
                         " contains NAs\n")
    valid<- FALSE
  }

  if(type == "edges") {
    ## ENETWORK (netID rid upDist)   
   
    ## Extract netgeom info
    sf.obj.ng<-ssn_get_netgeom(sf.obj, reformat = TRUE)

    ## Check column number
    if(ncol(sf.obj.ng) != 3) {
      valid.message<- paste0(valid.message,
                             "edges netgeom character string does not contain the correct number of variables. Contents not checked\n")
      valid <- FALSE
      netgeom.valid <- FALSE
    } else {

      ## Check rid -------------------------------------
      if(sum(duplicated(sf.obj.ng$SegmentID)) > 0) {
        valid.message <- paste0(valid.message,
                                "duplicates found in edges netgeom SegmentID. Check edges rid column for duplicates\n")
        valid<- FALSE
      }
    }
    
  } else {

    ## ## SNETWORK (netID, rid upDist, ratio, pid, locID)

    ## Extract netgeom info
    sf.obj.ng<-ssn_get_netgeom(sf.obj, reformat = TRUE)

    ## Check column number
    if(ncol(sf.obj.ng) != 6) {
      valid.message<- paste0(valid.message, type,
                             " netgeom character string does not contain the correct number of variables. Contents not checked\n")
      valid <- FALSE
      netgeom.valid <- FALSE
    } else {

      ## Check pid -------------------------------------
      if(sum(duplicated(sf.obj.ng$pid)) > 0) {
        valid.message <- paste0(valid.message, "duplicates found in ", type,
                                " netgeom pid\n")
        valid<- FALSE
      }

      if(sum(is.na(sf.obj.ng$pid)) > 0) {
        valid.message <- paste0(valid.message, type, " netgeom pid contains NAs\n")
        valid<- FALSE
      }

      ## Check locID -----------------------------------
      if(sum(is.na(sf.obj.ng$locID)) > 0) {
        valid.message <- paste0(valid.message, "obs netgeom locID contains NAs\n")
        valid<- FALSE
      }
    
      ## Check ratio -----------------------------------
      if(sum(is.na(sf.obj.ng$obs$ratio)) > 0) {
        valid.message <- paste0(valid.message, type, " netgeom ratio contains NAs\n")
        valid<- FALSE
      }
      if(sum(sf.obj.ng$ratio < 0) > 0 |
         sum(sf.obj.ng$ratio > 1) > 0) {
        valid.message <- paste0(valid.message, type,
                                " netgeom ratio contains values < 0 and/or > 1\n")
        valid<- FALSE
      }
    }
  }

  ## Check common values
  if(netgeom.valid == TRUE) {
    ## Check netID-------------------------
    if(sum(is.na(sf.obj.ng$NetworkID)) > 0) {
      valid.message <- paste0(valid.message, type,
                              " netgeom NetworkID contains NAs\n")
      valid<- FALSE
    }
    if(sum(sf.obj.ng$NetworkID < 1) > 0 ) {
      valid.message<- paste0(valid.message, "WARNING: ", type,
                             " netgeom NetworkID contains values < 1. These values typically begin at 1\n")
    }
    ## Check upDist -----------------------
    if(sum(is.na(sf.obj.ng$DistanceUpstream)) > 0) {
      valid.message<- paste0(valid.message, type, " netgeom DistanceUpstream contains NAs. Check edges upDist column for NAs\n")
      valid<- FALSE
    }
    if(sum(sf.obj.ng$UpstreamDistance < 0) > 0 ) {
      valid.message<- paste0(valid.message, type, " netgeom UpstreamDistance contains negative distances\n")
      valid<- FALSE
    }

    ## Check rid -----------------------------
    if(sum(is.na(sf.obj.ng$NetworkID)) > 0) {
      valid.message <- paste0(valid.message, type,
                              " netgeom NetworkID contains NAs\n")
      valid<- FALSE
    }
  }

  ## ---------------------------------------------------------------
  ## Return results
  ## ---------------------------------------------------------------
  if(verbose == FALSE) {
    return(valid.bool)
  } else {
    return(valid.message)
  }
  

}




