## Format rids for a message: sorted, comma-separated, and at most
## max.rids of them, followed by "..." if there are more
format_rids <- function(rids, max.rids = 10) {
  rids <- sort(unique(rids))
  rid.text <- paste(rids[seq_len(min(length(rids), max.rids))], collapse = ", ")
  if (length(rids) > max.rids) rid.text <- paste0(rid.text, ", ...")
  rid.text
}
