check_names_case <- function(names_string, pattern, type) {
  if (any(tolower(names_string) == tolower(pattern))) {
    stop(paste0("There is already a column in ", type, " named ", pattern, " (ignoring case). Please rename the column in ", type, "."), call. = FALSE)
  }
}

check_names_case_add <- function(names_string, pattern, type, names_argument) {
  if (any(tolower(names_string) == tolower(pattern))) {
    stop(paste0("There is already a column in ", type, " named ", pattern, " (ignoring case). Please change ", names_argument, " or rename the column in ", type, "."), call. = FALSE)
  }
}
