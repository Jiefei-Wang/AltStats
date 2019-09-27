#include "Rcpp.h"


using namespace Rcpp;
// [[Rcpp::export]]
bool C_has_ptr(SEXP x) {
	return DATAPTR_OR_NULL(x) != NULL;
}