#include "Rcpp.h"
#include <string>

#define DEBUG(x)

#define PACKAGE_NAME "AltStat"
#define PACKAGE_ENV_NAME "namespace:" PACKAGE_NAME
#define PACKAGE_NAMESPACE R_FindNamespace(Rf_mkString(PACKAGE_NAME))

//To char*
#define SYMBOL_TO_CHAR(x) CHAR(PRINTNAME(x))
#define STRSXP_TO_CHAR(x) CHAR(STRING_ELT(x, 0))

#define CHAR_EQUAL(str1,str2) (strcmp(str1,str2)==0)

#define ULLong unsigned long long

int get_type_size(int type);

//Allocate result
SEXP allocate_result(SEXP x, int type, R_xlen_t len);


//Call a function with arguments
SEXP make_call(SEXP fun);
SEXP make_call(SEXP fun, SEXP x1);
SEXP make_call(SEXP fun, SEXP x1, SEXP x2);
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3);
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3, SEXP x4);
SEXP make_call(SEXP fun, SEXP x1, SEXP x2, SEXP x3, SEXP x4, SEXP x5);


bool C_has_pointer(SEXP x);
bool C_is_altrep(SEXP x);