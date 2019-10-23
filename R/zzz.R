#' @useDynLib AltStat, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom methods callGeneric callNextMethod is new
NULL

dispatchToDefault <- TRUE
debug <- function(on = TRUE){
    dispatchToDefault <- !on
}


