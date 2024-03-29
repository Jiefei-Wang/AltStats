// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// C_has_pointer
bool C_has_pointer(SEXP x);
RcppExport SEXP _AltStats_C_has_pointer(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_has_pointer(x));
    return rcpp_result_gen;
END_RCPP
}
// C_is_altrep
bool C_is_altrep(SEXP x);
RcppExport SEXP _AltStats_C_is_altrep(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_is_altrep(x));
    return rcpp_result_gen;
END_RCPP
}
// C_force_attribute_set
void C_force_attribute_set(SEXP x, SEXP attrName, SEXP attr);
RcppExport SEXP _AltStats_C_force_attribute_set(SEXP xSEXP, SEXP attrNameSEXP, SEXP attrSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type attrName(attrNameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type attr(attrSEXP);
    C_force_attribute_set(x, attrName, attr);
    return R_NilValue;
END_RCPP
}
// C_copy_altrep_value
void C_copy_altrep_value(SEXP target, SEXP source);
RcppExport SEXP _AltStats_C_copy_altrep_value(SEXP targetSEXP, SEXP sourceSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type target(targetSEXP);
    Rcpp::traits::input_parameter< SEXP >::type source(sourceSEXP);
    C_copy_altrep_value(target, source);
    return R_NilValue;
END_RCPP
}
// makeExampleAltrep
SEXP makeExampleAltrep(SEXP x);
RcppExport SEXP _AltStats_makeExampleAltrep(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(makeExampleAltrep(x));
    return rcpp_result_gen;
END_RCPP
}
// C_arith_binary_operator
SEXP C_arith_binary_operator(SEXP op, SEXP x, SEXP y);
RcppExport SEXP _AltStats_C_arith_binary_operator(SEXP opSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(C_arith_binary_operator(op, x, y));
    return rcpp_result_gen;
END_RCPP
}
// C_arith_unary_operator
SEXP C_arith_unary_operator(SEXP op, SEXP x);
RcppExport SEXP _AltStats_C_arith_unary_operator(SEXP opSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_arith_unary_operator(op, x));
    return rcpp_result_gen;
END_RCPP
}
// C_math_partial_operator
SEXP C_math_partial_operator(SEXP op, SEXP x);
RcppExport SEXP _AltStats_C_math_partial_operator(SEXP opSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_math_partial_operator(op, x));
    return rcpp_result_gen;
END_RCPP
}
// C_math_operator
SEXP C_math_operator(SEXP op, SEXP x);
RcppExport SEXP _AltStats_C_math_operator(SEXP opSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type op(opSEXP);
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(C_math_operator(op, x));
    return rcpp_result_gen;
END_RCPP
}
// C_range_function
SEXP C_range_function(SEXP x, bool na_rm, bool finite);
RcppExport SEXP _AltStats_C_range_function(SEXP xSEXP, SEXP na_rmSEXP, SEXP finiteSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    Rcpp::traits::input_parameter< bool >::type finite(finiteSEXP);
    rcpp_result_gen = Rcpp::wrap(C_range_function(x, na_rm, finite));
    return rcpp_result_gen;
END_RCPP
}
// test
int test(int x, int y);
RcppExport SEXP _AltStats_test(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(test(x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_AltStats_C_has_pointer", (DL_FUNC) &_AltStats_C_has_pointer, 1},
    {"_AltStats_C_is_altrep", (DL_FUNC) &_AltStats_C_is_altrep, 1},
    {"_AltStats_C_force_attribute_set", (DL_FUNC) &_AltStats_C_force_attribute_set, 3},
    {"_AltStats_C_copy_altrep_value", (DL_FUNC) &_AltStats_C_copy_altrep_value, 2},
    {"_AltStats_makeExampleAltrep", (DL_FUNC) &_AltStats_makeExampleAltrep, 1},
    {"_AltStats_C_arith_binary_operator", (DL_FUNC) &_AltStats_C_arith_binary_operator, 3},
    {"_AltStats_C_arith_unary_operator", (DL_FUNC) &_AltStats_C_arith_unary_operator, 2},
    {"_AltStats_C_math_partial_operator", (DL_FUNC) &_AltStats_C_math_partial_operator, 2},
    {"_AltStats_C_math_operator", (DL_FUNC) &_AltStats_C_math_operator, 2},
    {"_AltStats_C_range_function", (DL_FUNC) &_AltStats_C_range_function, 3},
    {"_AltStats_test", (DL_FUNC) &_AltStats_test, 2},
    {NULL, NULL, 0}
};

void init_altrep_double(DllInfo* dll);
RcppExport void R_init_AltStats(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    init_altrep_double(dll);
}
