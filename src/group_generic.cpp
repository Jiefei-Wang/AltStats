#include <math.h>   // std::ceil
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


template<class T1, class T2, class T3>
T1 binary_oprate(const char* op, T2 x, T3 y) {
	CHECK_NA(T1, x);
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
		return(x / (T1)y);
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


//#include "test.h"
R_xlen_t region_size = 512;

// [[Rcpp::export]]
SEXP C_arith_binary_operator(SEXP op, SEXP x, SEXP y) {
	const char* op_char = STRSXP_TO_CHAR(op);
	R_xlen_t x_length = XLENGTH(x);
	R_xlen_t y_length = XLENGTH(y);
	R_xlen_t result_length = std::max(x_length, y_length);

	int result_type = get_arith_binary_operator_type(op_char, x, y);
	SEXP result = PROTECT(allocate_result(x, result_type, result_length));

	BINARY_ITERATE_BY_REGION(x, y, x_ptr, y_ptr, x_i, y_i, region_size, 0, result_length, {
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





int get_arith_unary_operator_type(const char* op, SEXP x) {
	//The minus operator on logical vector results in an integer vector
	if (CHAR_EQUAL(op, "-") && TYPEOF(x) == LGLSXP) {
		return INTSXP;
	}
	if (CHAR_EQUAL(op, "!")
		) {
		return LGLSXP;
	}
	return TYPEOF(x);
}

template<class T1,class T2>
T1 unary_operate(const char* op, T2 x) {
	CHECK_NA(T2, x);
	if (CHAR_EQUAL(op, "-"))
		return -x;
	if (CHAR_EQUAL(op, "!"))
		return !x;
	if (CHAR_EQUAL(op, "abs"))
		return std::abs(x);
	Rf_error("Unsupported unary operator: %s", op);
	return 0;
}


// [[Rcpp::export]]
SEXP C_arith_unary_operator(SEXP op, SEXP x) {
	const char* op_char = STRSXP_TO_CHAR(op);
	int result_type = get_arith_unary_operator_type(op_char, x);
	SEXP result = PROTECT(allocate_result(x, result_type, XLENGTH(x)));

	TYPE_FREE_ITER(x, ptr, idx, nbatch, {
			for (int i = 0; i < nbatch; i++) {
				SET_ELT_TEMPLATE(result, idx + i, unary_operate,(op_char, ptr[i]));
			}
		}
	);
	UNPROTECT(1);
	return(result);
}

template<class T>
T math_partial_operate(const char* op, T& partial_result, T x2) {
	CHECK_NA(T, partial_result);
	if (CHAR_EQUAL(op, "cummax")) {
		partial_result = partial_result > x2 ? partial_result : x2;
		return partial_result;
	}
	if (CHAR_EQUAL(op, "cummin")) {
		partial_result = partial_result < x2 ? partial_result : x2;
		return partial_result;
	}
	if (CHAR_EQUAL(op, "cumsum")) {
		partial_result = partial_result + x2;
		return partial_result;
	}
	if (CHAR_EQUAL(op, "cumprod")) {
		partial_result = partial_result * x2;
		return partial_result;
	}
	Rf_error("Unsupported unary operator: %s", op);
}

// [[Rcpp::export]]
SEXP C_math_partial_operator(SEXP op, SEXP x) {
	const char* op_char = STRSXP_TO_CHAR(op);
	int result_type = TYPEOF(x);
	if (CHAR_EQUAL(op_char, "sqrt")) {
		result_type = REALSXP;
	}
	SEXP result = PROTECT(allocate_result(x, result_type, XLENGTH(x)));

	switch (TYPEOF(x)) {
	case LGLSXP:
		{
			int partial_record = LOGICAL_ELT(x, 0);
			SET_ELT(result, 0, partial_record);
			ITERATE_BY_REGION_PARTIAL(x, ptr, ind, nbatch, int, LOGICAL,1,XLENGTH(x), {
				for (R_xlen_t i = 0; i < nbatch; i++)
				SET_ELT(result, ind + i, math_partial_operate(op_char,partial_record, ptr[i]));
				}
			);
		}
		break;
	case INTSXP:
		{
			int partial_record = INTEGER_ELT(x, 0);
			SET_ELT(result, 0, partial_record);
			ITERATE_BY_REGION_PARTIAL(x, ptr, ind, nbatch, int, INTEGER, 1, XLENGTH(x), {
			for (R_xlen_t i = 0; i < nbatch; i++)
			SET_ELT(result, ind + i, math_partial_operate(op_char, partial_record, ptr[i]));
				}
			);
		}
		break;
	case REALSXP:
		{
			double partial_record = REAL_ELT(x, 0);
			SET_ELT(result, 0, partial_record);
			ITERATE_BY_REGION_PARTIAL(x, ptr, ind, nbatch, double, REAL, 1, XLENGTH(x), {
			for (R_xlen_t i = 0; i < nbatch; i++) {
				//Rprintf("ind: %lld, i: %lld, ptr:%f, True value: %f \n", ind, i, ptr[i], REAL_ELT(x, ind + i));
				SET_ELT(result, ind + i, math_partial_operate(op_char, partial_record, ptr[i]));
			}
				}
			);
		}
		break;
	default:
		Rf_error("Unknow data type\n"); 
		break; 
	}

	UNPROTECT(1);
	return result;
}

template<class T>
double math_operate(const char* op, T x) {
	CHECK_NA(T, x);
	if (CHAR_EQUAL(op, "abs")) {
		return std::abs(x);
	}
	if (CHAR_EQUAL(op, "sign")) {
		if (x == (T)0) {
			return (T)0;
		}
		else {
			return -std::signbit(x)*2+1;
		}
	}
	if (CHAR_EQUAL(op, "sqrt"))
		return std::sqrt(x);
	if (CHAR_EQUAL(op, "ceiling"))
		return std::ceil(x);
	if (CHAR_EQUAL(op, "floor"))
		return std::floor(x);
	if (CHAR_EQUAL(op, "trunc"))
		return std::trunc(x);
	if (CHAR_EQUAL(op, "log"))
		return std::log(x);
	if (CHAR_EQUAL(op, "log10"))
		return std::log10(x);
	if (CHAR_EQUAL(op, "log2"))
		return std::log2(x);
	if (CHAR_EQUAL(op, "log1p"))
		return std::log1p(x);
	if (CHAR_EQUAL(op, "acos"))
		return std::acos(x);
	if (CHAR_EQUAL(op, "acosh"))
		return std::acosh(x);
	if (CHAR_EQUAL(op, "asin"))
		return std::asin(x);
	if (CHAR_EQUAL(op, "asinh"))
		return std::asinh(x);
	if (CHAR_EQUAL(op, "atan"))
		return std::atan(x);
	if (CHAR_EQUAL(op, "atanh"))
		return std::atanh(x);
	if (CHAR_EQUAL(op, "exp"))
		return std::exp(x);
	if (CHAR_EQUAL(op, "expm1"))
		return std::expm1(x);
	if (CHAR_EQUAL(op, "cos"))
		return std::cos(x);
	if (CHAR_EQUAL(op, "cosh"))
		return std::cosh(x);
	if (CHAR_EQUAL(op, "cospi"))
		return std::cos(M_PI * x);
	if (CHAR_EQUAL(op, "sin"))
		return std::sin(x);
	if (CHAR_EQUAL(op, "sinh"))
		return std::sinh(x);
	if (CHAR_EQUAL(op, "sinpi"))
		return std::sin(M_PI * x);
	if (CHAR_EQUAL(op, "tan"))
		return std::tan(x);
	if (CHAR_EQUAL(op, "tanh"))
		return std::tanh(x);
	if (CHAR_EQUAL(op, "tanpi"))
		return std::tan(M_PI * x);
	if (CHAR_EQUAL(op, "gamma"))
		return std::tgamma(x);
	if (CHAR_EQUAL(op, "lgamma"))
		return std::lgamma(x);

	Rf_error("Unsupported unary operator: %s", op);
}

// [[Rcpp::export]]
SEXP C_math_operator(SEXP op, SEXP x) {
	const char* op_char = STRSXP_TO_CHAR(op);
	int result_type = REALSXP;
	SEXP result = PROTECT(allocate_result(x, result_type, XLENGTH(x)));
	TYPE_FREE_ITER(x, ptr, ind, nbatch, {
			for (R_xlen_t i = 0; i < nbatch; i++)
			SET_ELT(result, ind + i, math_operate(op_char, ptr[i]));
		}
	);
	UNPROTECT(1);
	return result;
}




template<class T>
void compute_range(T value_cur, void* result,bool na_rm, bool  finite) {
	T* result_ptr = (T*)result;
	if (finite && !std::isfinite(value_cur)) {
		return;
	}
	if (na_rm) {
		if (std::is_same<T, int>::value && value_cur == NA_INTEGER) {
			return; 
		}
		if (std::is_same<T, double>::value && value_cur == NA_REAL) {
			return; 
		}
	}
	else {
		if (std::is_same<T, int>::value && value_cur == NA_INTEGER) {
			result_ptr[0] = NA_INTEGER;
			result_ptr[1] = NA_INTEGER;
			return;
		}
		if (std::is_same<T, double>::value && value_cur == NA_REAL) {
			result_ptr[0] = NA_REAL;
			result_ptr[1] = NA_REAL;
			return;
		}
	}
	

	result_ptr[0] = value_cur < result_ptr[0] ? value_cur : result_ptr[0];
	result_ptr[1] = value_cur > result_ptr[1] ? value_cur : result_ptr[1];
}

// [[Rcpp::export]]
SEXP C_range_function(SEXP x, bool na_rm, bool finite) {
	//min and max of the vector x
	int data_type = TYPEOF(x);
	SEXP result = PROTECT(Rf_allocVector(data_type, 2L));
	void* result_ptr = DATAPTR(result);
	
	bool initialized = false;
	TYPE_FREE_ITER(x, ptr, ind, nbatch, {
		for (int i = 0; i < nbatch; i++) {
			if (!initialized) {
				if (std::isnan(ptr[i])) {
					if (!na_rm&& !finite) {
						SET_ELT(result, 0, ptr[i]);
						SET_ELT(result, 1, ptr[i]);
						initialized = true;
						continue;
					}
				}
				else if (std::isfinite(ptr[i])) {
					SET_ELT(result, 0, ptr[i]);
					SET_ELT(result, 1, ptr[i]);
					initialized = true;
					continue;
				}
				else if (!finite) {
					SET_ELT(result, 0, ptr[i]);
					SET_ELT(result, 1, ptr[i]);
					initialized = true;
					continue;
				}

				if (ind + i == XLENGTH(result)) {
					SET_ELT(result, 0, ptr[i]);
					SET_ELT(result, 1, ptr[i]);
				}
			}
			else {
				break;
			}
		}
		}
	);


	TYPE_FREE_ITER(x, ptr, ind, nbatch, {
		for (int i = 0; i < nbatch; i++) {
			compute_range(ptr[i], result_ptr, na_rm, finite);
		}
		}
	);
	UNPROTECT(1);
	return(result);
}





// [[Rcpp::export]]
int test(int x, int y) {
	return binary_oprate<int>("||", x, y);
}