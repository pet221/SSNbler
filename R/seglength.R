seglength <- function(x) {
  sqrt(sum(diff(x[, 1:2])^2))
}
