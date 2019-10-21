#include "Rcpp.h"
using namespace Rcpp;
// [[Rcpp::export]]
bool C_has_pointer(SEXP x) {
	return DATAPTR_OR_NULL(x) != NULL;
}

// [[Rcpp::export]]
bool C_is_altrep(SEXP x) {
	return ALTREP(x);
}
