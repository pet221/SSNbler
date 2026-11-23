## Stop if any edge in noderelationships.csv starts and ends at the same
## node (a Self-loop). 

check_self_loops <- function(lsn_path) {
  node.relate <- read.csv(paste0(lsn_path, "/noderelationships.csv"))
  self.loop.rids <- node.relate$rid[node.relate$fromnode == node.relate$tonode]
  
  if (length(self.loop.rids) > 0) {
    stop(length(self.loop.rids), " Self-loop edge(s) (rid ",
      format_rids(self.loop.rids), ") present in edges. A Self-loop starts and ends at the same node. Fix topology errors and re-create error free edges using lines_to_lsn().",
      call. = FALSE)
  }
}
