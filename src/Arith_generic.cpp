#include <math.h>   // std::ceil
#include <cmath>  //std::fmod
#include <algorithm>  //std::min
#include "Rcpp.h"
#include "tools.h"
#include "macros.h"
#include <type_traits>
using namespace Rcpp;




int get_arith_binary_operator_type(const char* op, SEXP x, SEXP y) {
	if (CHAR_EQUAL(op, "/")) {
		return(REALSXP);
	}
	if (CHAR_EQUAL(op, "==") ||
		CHAR_EQUAL(op, ">") ||
		CHAR_EQUAL(op, "<") ||
		CHAR_EQUAL(op, "!=") ||
		CHAR_EQUAL(op, "<=") ||
		CHAR_EQUAL(op, ">=") ||
		CHAR_EQUAL(op, "&") ||
		CHAR_EQUAL(op, "|")
		) {
		return(LGLSXP);
	}
	if (TYPEOF(x) == TYPEOF(y)) {
		if (TYPEOF(x) == LGLSXP)
			return(INTSXP);
		else
			return(TYPEOF(x));
	}
	//If the type does not match
	R_xlen_t target_size = std::max(get_type_size(TYPEOF(x)), get_type_size(TYPEOF(y)));
	//One of them must be a real SEXP
	if (target_size == 8) {
		return(REALSXP);
	}
	//One of them must be an integer SEXP
	if (target_size == 4) {
		return(INTSXP);
	}
	Rf_error("Undefined variable type\n");
}

R_xlen_t get_numeric_region(SEXP x, R_xlen_t i, R_xlen_t n, void* ptr) {
	switch (TYPEOF(x)) {
	case INTSXP:
		n = INTEGER_GET_REGION(x, i, n, (int*)ptr);
	case REALSXP:
		n = REAL_GET_REGION(x, i, n, (double*)ptr);
	case LGLSXP:
		n = LOGICAL_GET_REGION(x, i, n, (int*)ptr);
	}
	return(n);
}
//fill the region from the start of the vector if the region reach the end of the vector
R_xlen_t get_numeric_region_rep(SEXP x, R_xlen_t i, R_xlen_t n, void* ptr) {
	R_xlen_t x_length = LENGTH(x);
	//printf("get region:i=%lld, n=%lld\n", i, n);
	R_xlen_t n_new = get_numeric_region(x, i, n, ptr);
	//TODO: check if the read reaches the end of the vector
	//If the required index exceed the length of the vector, fill it from the start of the vector
	if (n > n_new) {
		//printf("get region2:i=0, n=%lld, %p, %p\n", i + n - x_length, ptr, (char*)ptr + get_type_size(TYPEOF(x)) * n_new);
		get_numeric_region(x, 0, i + n - x_length, (char*)ptr + get_type_size(TYPEOF(x)) * n_new);
	}
	return(n_new);
}

#define CHECK_NA(type,x) \
if (std::is_same<type, int>::value&&x == NA_INTEGER) {\
	return(NA_INTEGER);\
}\
if (std::is_same<type, double>::value&&x == NA_REAL) {\
		return(NA_REAL);\
}

template<class T1, class T2, class T3>
T1 binary_oprate(const char* op, T2 x, T3 y) {
	CHECK_NA(T1,x);
	CHECK_NA(T2, y);

	if (CHAR_EQUAL(op, "+")) {
		return(x + y);
	}
	if (CHAR_EQUAL(op, "-")) {
		return(x - y);
	}
	if (CHAR_EQUAL(op, "*")) {
		return(x * y);
	}
	if (CHAR_EQUAL(op, "/")) {
		return(x / y);
	}
	if (CHAR_EQUAL(op, "%%")) {
		return(std::fmod(x, y));
	}
	if (CHAR_EQUAL(op, "%/%")) {
		return(floor(x / y));
	}
	if (CHAR_EQUAL(op, "^")) {
		return(pow(x, y));
	}
	if (CHAR_EQUAL(op, "==")) {
		return(x == y);
	}
	if (CHAR_EQUAL(op, ">")) {
		return(x > y);
	}
	if (CHAR_EQUAL(op, "<")) {
		return(x < y);
	}
	if (CHAR_EQUAL(op, "!=")) {
		return(x != y);
	}
	if (CHAR_EQUAL(op, "<=")) {
		return(x <= y);
	}
	if (CHAR_EQUAL(op, ">=")) {
		return(x >= y);
	}
	if (CHAR_EQUAL(op, "&")) {
		return(x && y);
	}
	if (CHAR_EQUAL(op, "|")) {
		return(x || y);
	}
	return (T1)0;
}

template<class T>
T get_element_by_type(void* vector, R_xlen_t vector_length, bool has_ptr, R_xlen_t region_offset, int type, R_xlen_t i) {
	if (has_ptr) {
		i = (i + region_offset) % vector_length;
		//printf("%lld,%lld\n", vector_length,i);
	}
	switch (type) {
	case LGLSXP: {
		int* true_ptr = (int*)vector;
		return (T)(true_ptr[i]);
	}
	case INTSXP: {
		int* true_ptr = (int*)vector;
		return (T)(true_ptr[i]);
	}
	case REALSXP: {
		double* true_ptr = (double*)vector;
		return (T)(true_ptr[i]);
	}
	default:
		Rf_error("Unexpected data type\n");
	}
	return (T)0;
}

//#include "test.h"

SEXP C_binary_arith_operator(SEXP op, SEXP x, SEXP y) {
	const char* op_char = STRSXP_TO_CHAR(op);
	R_xlen_t x_length = XLENGTH(x);
	R_xlen_t y_length = XLENGTH(y);
	R_xlen_t result_length = std::max(x_length, y_length);

	int result_type = get_arith_binary_operator_type(op_char, x, y);
	SEXP result;
	if (x_length == result_length) {
		if (TYPEOF(x) != result_type) {
			result = PROTECT(Rf_coerceVector(x, result_type));
		}
		else {
			result = PROTECT(Rf_duplicate(x));
		}
	}
	else {
		result = PROTECT(Rf_allocVector(result_type, result_length));
	}

	BINARY_ITERATE_BY_REGION(x, y, x_ptr, y_ptr, x_i, y_i, 3, 0, result_length, {
		R_xlen_t absolute_i = outer_i + inner_i;
		switch (TYPEOF(result))
		{
		case INTSXP:
			SET_INTEGER_ELT(result, absolute_i, binary_oprate<int>(op_char, x_ptr[x_i], y_ptr[y_i]));
		break;
	case REALSXP:
		SET_REAL_ELT(result, absolute_i, binary_oprate<double>(op_char, x_ptr[x_i], y_ptr[y_i]));
		break;
	case LGLSXP:
		SET_LOGICAL_ELT(result, absolute_i, binary_oprate<int>(op_char, x_ptr[x_i], y_ptr[y_i]));
		break;
	default:
		Rf_error("Unknow data type\n");
		break;
		}
		}
	);
	UNPROTECT(1);
	return(result);
}





R_xlen_t region_size = 512;

/*
SEXP C_binary_arith_operator(SEXP op, SEXP x, SEXP y) {
	const char* op_char = STRSXP_TO_CHAR(op);

	bool x_has_ptr = DATAPTR_OR_NULL(x) != NULL;
	bool y_has_ptr = DATAPTR_OR_NULL(y) != NULL;
	R_xlen_t x_length = XLENGTH(x);
	R_xlen_t y_length = XLENGTH(y);
	R_xlen_t result_length = std::max(x_length, y_length);
	//Determine the region length, the region length should not exceed the length of x,y
	R_xlen_t desired_region_length = std::min(region_size, result_length);
	R_xlen_t x_region_len = std::min(x_length, desired_region_length);
	R_xlen_t y_region_len = std::min(y_length, desired_region_length);

	int x_type = TYPEOF(x);
	int y_type = TYPEOF(y);
	int result_type = get_arith_binary_operator_type(op_char, x, y);
	SEXP result;
	if (x_length == result_length) {
		if (TYPEOF(x) != result_type) {
			result = PROTECT(Rf_coerceVector(x, result_type));
		}
		else {
			result = PROTECT(Rf_duplicate(x));
		}
	}
	else {
		result = PROTECT(Rf_allocVector(result_type, result_length));
	}
	//DATAPTR(result);
	//TODO: Attributes should be handled
	void* x_region_ptr = NULL;
	void* y_region_ptr = NULL;

	if (x_has_ptr) {
		x_region_ptr = DATAPTR(x);
	}
	else {
		//printf("type:%d, size: %lld\n", get_type_size(x_type), get_type_size(x_type) * x_region_len);
		x_region_ptr = malloc(get_type_size(x_type) * x_region_len);
	}
	if (y_has_ptr) {
		y_region_ptr = DATAPTR(y);
	}
	else {
		y_region_ptr = malloc(get_type_size(y_type) * y_region_len);
	}
	//printf("x: has ptr:%d, length: %lld, region : %lld\n", x_has_ptr, x_length, x_region_len);
	//printf("y: has ptr:%d, length: %lld, region : %lld\n", y_has_ptr, y_length, y_region_len);
	//printf("%lld,%lld\n", desired_region_length,result_length);
	for (R_xlen_t i = 0; i < result_length; i = i + desired_region_length) {
		//printf("%lld,%lld,%lld\n", i, result_length, desired_region_length);
		if (!x_has_ptr)
			get_numeric_region_rep(x, i, x_region_len, x_region_ptr);

		if (!y_has_ptr)
			get_numeric_region_rep(y, i, y_region_len, y_region_ptr);
		R_xlen_t x_i = 0;
		R_xlen_t y_i = 0;

		//resize the length of the desired region in the last i to prevent it from out-of-range.
		if (i + desired_region_length > result_length)
			desired_region_length = result_length - i;

		for (R_xlen_t k = 0; k < desired_region_length; k++) {
			//printf("%lld,%lld,%lld,%lld\n", i, k, x_i, y_i);
			switch (result_type) {
			case INTSXP: {
				int x_int = get_element_by_type<int>(x_region_ptr, x_length, x_has_ptr, i, x_type, x_i);
				int y_int = get_element_by_type<int>(y_region_ptr, y_length, y_has_ptr, i, y_type, y_i);
				//printf("%d,%d\n", x_int, y_int);
				SET_INTEGER_ELT(result, i + k, binary_oprate(op_char, x_int, y_int));
				//Rf_PrintValue(result);
				break;
			}
			case REALSXP: {
				double x_double = get_element_by_type<double>(x_region_ptr, x_length, x_has_ptr, i, x_type, x_i);
				double y_double = get_element_by_type<double>(y_region_ptr, y_length, y_has_ptr, i, y_type, y_i);
				SET_REAL_ELT(result, i + k, binary_oprate(op_char, x_double, y_double));
				break;
			}
			case LGLSXP: {

			}
			}
			x_i++;
			y_i++;
			if (!x_has_ptr && x_i >= x_region_len)x_i = 0;
			if (!y_has_ptr && y_i >= y_region_len)y_i = 0;
		}

	}
	//printf("finished\n");
	if (!x_has_ptr) {
		//printf("free x: %p\n", x_region_ptr);
		free(x_region_ptr);
	}
	if (!y_has_ptr) {
		//printf("free y\n");
		free(y_region_ptr);
	}
	//printf("return\n");
	UNPROTECT(1);
	return(result);
}
*/

int get_arith_unary_operator_type(const char* op, SEXP x) {
	//The minus operator on logical vector results in an integer vector
	if (CHAR_EQUAL(op, "-") && TYPEOF(x) == LGLSXP) {
		return INTSXP;
	}
	if (CHAR_EQUAL(op, "!")
		) {
		return LGLSXP;
	}
	return(TYPEOF(x));
}
template<class T>
T unary_operate(const char* op, T x) {
	if (CHAR_EQUAL(op, "-")) {
		return -x;
	}
	if (CHAR_EQUAL(op, "!")) {
		return !x;
	}
	Rf_error("Unsupported unary operator: %s", op);
	return 0;
}


// [[Rcpp::export]]
SEXP C_arith_unary_operator(SEXP op, SEXP x) {
	const char* op_char = STRSXP_TO_CHAR(op);

	SEXP result;
	int result_type = get_arith_unary_operator_type(op_char, x);

	if (TYPEOF(x) != result_type) {
		result = PROTECT(Rf_coerceVector(x, result_type));
	}
	else {
		result = PROTECT(Rf_duplicate(x));
	}


	TYPE_FREE_ITER(x, ptr, ind, nbatch, {
		for (int i = 0; i < nbatch; i++) {
	SET_ELT(result, ind + i, unary_operate(op_char,ptr[i]));
	}
		});
	UNPROTECT(1);
	return(result);
}



// [[Rcpp::export]]
bool test(double x, double y) {
	return x != y;
}