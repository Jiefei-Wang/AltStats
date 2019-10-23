#' @useDynLib AltStats, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom methods callGeneric callNextMethod is new
#' @import knitr
NULL

dispatchToDefault <- TRUE
debug <- function(on = TRUE){
    dispatchToDefault <- !on
}


