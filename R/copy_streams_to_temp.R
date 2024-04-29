#' @title Copy streams data to temporary directory
#'
#' @description Copies the streams data directory to R's temporary
#'   directory so the examples in SSNbler do not write to the local
#'   library or any other places.
#' @return A copy of the Middle Fork streams data residing in R's temporary directory
#'
#' @details Copies the Middle Fork data to R's temporary directory
#' @export
#' @examples
#' copy_streams_to_temp()
#' # getwd()
#' # setwd(tempdir())
#' # getwd()
#' # if unix-alike, list temporary directory contents using: system('ls')
#' # if windows, list temporary directory contents using: shell('dir')
copy_streams_to_temp <- function() {
  ## Create temporary streams directory to work with
  if (dir.exists(paste0(tempdir(), "/streamsdata"))) {
    return(invisible())
  }
  file.copy(
    from = system.file("streamsdata", package = "SSNbler"),
    to = tempdir(), recursive = TRUE,
    overwrite = FALSE, copy.mode = FALSE
  )
  if (.Platform$OS.type == "unix") {
    system(paste0("chmod -R 777 ", tempdir(), "/streamsdata"))
  }
  invisible()
}
