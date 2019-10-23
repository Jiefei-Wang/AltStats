#include <Rcpp.h>
using namespace Rcpp;
#include <R_ext/Altrep.h>

R_altrep_class_t class_t;

R_xlen_t Length(SEXP x) {
	//Rprintf("length:%lld\n", XLENGTH(R_altrep_data1(x)));
	return XLENGTH(R_altrep_data1(x));
}
static Rboolean Inspect(SEXP x, int pre, int deep, int pvec, void (*inspect_subtree)(SEXP, int, int, int)) {
	//Rprintf("ALTREP object (len=%d)\n", Length(x));
	return TRUE;
}
void* Dataptr(SEXP x, Rboolean writeable) {
	Rf_error("cannot access data pointer");
	return Dataptr(R_altrep_data1(x),writeable);
}
const void* Dataptr_or_null(SEXP x) {
	//Rprintf("ptr or null\n");
	return NULL;
}
double get_Elt(SEXP x, R_xlen_t i) {
	return REAL_ELT(R_altrep_data1(x), i);
}
// [[Rcpp::export]]
SEXP makeExampleAltrep(SEXP x) {
	SEXP res = R_new_altrep(class_t, x, R_NilValue);
	return res;
}
// [[Rcpp::init]]
void init_altrep_double(DllInfo* dll){
	//Rprintf("initialized\n");
	class_t = R_make_altreal_class("example", "AltStat", dll);

	R_set_altrep_Length_method(class_t, Length);
	//R_set_altrep_Inspect_method(class_t, Inspect);
	//R_set_altvec_Dataptr_method(class_t, Dataptr);
	R_set_altvec_Dataptr_or_null_method(class_t, Dataptr_or_null);
	R_set_altreal_Elt_method(class_t, get_Elt);
}