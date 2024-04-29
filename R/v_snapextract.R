v_snapextract <- function(x) {
  st_cast(x, to = "POINT")[2]
} 