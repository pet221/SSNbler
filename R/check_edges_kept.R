## Stop if any edge would be dropped from the results, whatever the
## cause. input.rids: rids of the edges passed in; output.rids: rids of
## the edges with results; value.name: the value being calculated
check_edges_kept <- function(input.rids, output.rids, value.name) {
  missing.rids <- input.rids[!(input.rids %in% output.rids)]
  if (length(missing.rids) > 0) {
    stop(value.name, " could not be calculated for ", length(missing.rids),
      " edge(s) (rid ", format_rids(missing.rids),
      "). edges.gpkg has not been changed. Use lines_to_lsn() with check_topology = TRUE to check the LSN for topology errors.",
      call. = FALSE)
  }
}
