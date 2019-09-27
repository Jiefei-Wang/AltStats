#include "Rcpp.h"
#include "tools.h"
#include <cstdarg>
using namespace std;



SEXP make_call(SEXP fun) {
	SEXP call = Rf_lang1(fun);
	return R_forceAndCall(call, 0, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1) {
	SEXP call = Rf_lang2(fun, x1);
	return R_forceAndCall(call, 1, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1, SEXP x2) {
	SEXP call = Rf_lang3(fun, x1, x2);
	return R_forceAndCall(call, 2, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3) {
	SEXP call = Rf_lang4(fun, x1, x2, x3);
	return R_forceAndCall(call, 3, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3, SEXP x4) {
	SEXP call = Rf_lang5(fun, x1, x2, x3, x4);
	return R_forceAndCall(call, 4, R_GlobalEnv);
}
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3, SEXP x4, SEXP x5) {
	SEXP call = Rf_lang6(fun, x1, x2, x3, x4, x5);
	return R_forceAndCall(call, 5, R_GlobalEnv);
}



int get_type_size(int type) {
	switch (type)
	{
	case RAWSXP:
		return 1;
	case LGLSXP:
		return 4;
	case INTSXP:
		return 4;
	case REALSXP:
		return 8;
	}
	Rf_error("Unexpected type %d\n", type);
	return 0;
}





