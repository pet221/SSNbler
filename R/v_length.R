v_length <- function(x) {
  unlist(lapply(x, seglength))
}