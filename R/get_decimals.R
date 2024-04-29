get_decimals <- function(x) {
    if ((x %% 1) != 0) {
    # if (abs(x - round(x)) > .Machine$double.eps^0.5) {
        nchar(strsplit(sub('0+$', '', format(x, scientific = FALSE)), ".", fixed=TRUE)[[1]][[2]])
    } else {
        return(0)
    }
}
