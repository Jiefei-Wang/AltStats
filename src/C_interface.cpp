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

// [[Rcpp::export]]
void C_force_attribute_set(SEXP x, SEXP attrName, SEXP attr) {
	Rf_setAttrib(x, attrName, attr);
}


#include "macros.h"
// [[Rcpp::export]]
void C_copy_altrep_value(SEXP target, SEXP source) {
	TYPE_FREE_ITER_PARTIAL(source, ptr, idx, nbatch, 0, XLENGTH(target), {
			for (R_xlen_t i = 0; i < nbatch; i++) {
				//Rprintf("idx: %lld, i: %lld, ptr:%f, True value: %f \n", idx, i, ptr[i], REAL_ELT(target, idx + i));
				SET_ELT(target, idx + i, ptr[i]);
			}
		});
}





