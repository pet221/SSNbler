get_segment_pi <- function(edges, lsn_path, infl_col, segpi_col,
                           overwrite = FALSE) {
  ## Check inputs --------------------------------------------------
  ## Does the influence column exist, is it numeric, and are there any
  ## missing values
  if (!(infl_col %in% names(edges))) {
    stop(paste("Cannot find a column called", infl_col, "in edges"))
  }
  if (!is.numeric(edges[, infl_col, drop = TRUE])) {
    stop(paste0("infl_col ", infl_col, " must be numeric"))
  }
  if (sum(is.na(edges[, infl_col, drop = TRUE]) > 0)) {
    stop(paste0("Missing values are not permitted in ", infl_col))
  }

  ## ## Make sure geometry column is named geometry rather than geom
  ## if(!"geometry" %in% colnames(edges)) {
  ##   edges <- st_geometry(edges, rename = "geometry")
  ## }

  ## Add the proportional influence field in the edges attribute table
  edges[segpi_col] <- NA

  ## Create two table views - 1) noderelationships table and 2) edges
  ## attribute table
  nrelate <- try(read.csv(paste(lsn_path, "noderelationships.csv",
    sep = "/"
  )))
  ## Order nrelate by 'tonode' (column 3 in the table) and
  ## store in n_rels list
  nrelate <- nrelate[order(nrelate[[3]]), ]
  ## Reset sequential order of rownames to match sorted order of features
  rownames(nrelate) <- rep(1:nrow(nrelate))

  oldValue <- 1L
  ridList <- vector("integer") ## this list will hold reach feature IDs
  segmentInflList <- vector("numeric") ## this list will hold a list
  ## of segment infl_col values

  ## Loop through the ToNodes in the noderelationships table
  cumInfl <- 0 ## set cumInfl value

  for (k in 1:nrow(nrelate)) {
    ## get tonode of current node relationship
    newValue <- nrelate[k, "tonode"]
    if (newValue == oldValue) {
      ## get rid of current node relationship
      ridList[(length(ridList) + 1)] <- nrelate[k, "rid"]
    } else {
      ## Loop through each segment in ridList
      for (rid in ridList) {
        ## select the correct row in the edges table and infl_col
        ## which() returns row index where query matches
        int_edge_index <- which(edges$rid == rid)
        segmentInfl <- edges[[int_edge_index, infl_col]]

        ## calculate cumulative watershed influence at tonode
        cumInfl <- cumInfl + segmentInfl
        segmentInflList[length(segmentInflList) + 1] <- segmentInfl
      }

      i <- 1
      ## Calculate the segment PI
      for (segmentInfl in segmentInflList) {
        if (cumInfl > 0) {
          segmentPI <- segmentInfl / cumInfl
          ## print (ridList[i],"=",segmentPI)
          ## Assign value to PI field in edges table
          edges[which(edges$rid == ridList[i]), segpi_col] <- segmentPI
          i <- i + 1
        } else {
          segmentPI <- 0

          ## Assign 0 to PI field in edges table
          edges[which(edges$rid == ridList[i]), segpi_col] <- segmentPI
          i <- i + 1
        } ## end if (cumInfl > 0)
      } ## end for (segmentInfl in segmentInflList)

      ## start on new ToNode, reset some lists
      ## get rid of current node relationship
      ridList <- vector("integer") ## this list holds rids
      ridList[1] <- nrelate[k, "rid"]
      segmentInflList <- vector("numeric") ## this list holds a list
      ## of infl_col values
      cumInfl <- 0
      oldValue <- newValue
    } ## end if/else (newValue == oldValue)
  } ## end loop through each noderelationship

  ## --------Calculate segment PIs for last toNode-----------------
  for (rid.i in ridList) {
    ## select the correct row in the edges table and get infl_col
    segmentInfl <- edges[[which(edges$rid == rid.i), infl_col]]

    ## calculate cumulative watershed influence at tonode
    cumInfl <- cumInfl + segmentInfl
    segmentInflList[length(segmentInflList) + 1] <- segmentInfl
  }

  i <- 1
  ## Calculate the segment PI for last tonode
  for (segmentInfl.i in segmentInflList) {
    segmentPI <- segmentInfl.i / cumInfl

    ## Assign value to segment PI field in edges table
    edges[which(edges$rid == ridList[i]), segpi_col] <- segmentPI
    i <- i + 1
  }

  return(edges)
}
