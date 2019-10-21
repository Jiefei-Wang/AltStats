#define CHECK_NA(type,x) \
if (std::is_same<type, int>::value&&x == NA_INTEGER) {\
	return(NA_INTEGER);\
}\
if (std::is_same<type, double>::value&&x == NA_REAL) {\
		return(NA_REAL);\
}

#define IS_NA(x,type)\
(type==INTSXP?x==NA_INTEGER:(type==REALSXP?x==NA_REAL:TRUE))




#define SET_ELT_TEMPLATE(x,i,func,args)\
switch (TYPEOF(x))\
{\
	case INTSXP:\
		SET_INTEGER_ELT(x,i,func<int>args);\
		break;\
	case REALSXP:\
		SET_REAL_ELT(x,i,func<double>args);\
		break;\
	case LGLSXP:\
		SET_LOGICAL_ELT(x,i,func<int>args);\
		break;\
	default:\
		Rf_error("Unknow data type\n");\
		break;\
}
#define SET_ELT(x,i,v)\
switch (TYPEOF(x))\
{\
	case INTSXP:\
		SET_INTEGER_ELT(x,i,v);\
		break;\
	case REALSXP:\
		SET_REAL_ELT(x,i,v);\
		break;\
	case LGLSXP:\
		SET_LOGICAL_ELT(x,i,v);\
		break;\
	default:\
		Rf_error("Unknow data type\n");\
		break;\
}


#include <R_ext/Itermacros.h>
#undef ITERATE_BY_REGION_PARTIAL
#undef ITERATE_BY_REGION
#define ITERATE_BY_REGION_PARTIAL(sx, px, idx, nb, etype, vtype,	\
				  strt, nfull, expr) do {		\
	const etype *px = (const etype *)DATAPTR_OR_NULL(sx);				\
	if (px != NULL) {						\
	    R_xlen_t __ibr_n__ = strt + nfull;				\
	    R_xlen_t nb = __ibr_n__;					\
	    for (R_xlen_t idx = strt; idx < __ibr_n__; idx += nb) {	\
		expr							\
	     }								\
	}								\
	else ITERATE_BY_REGION_PARTIAL0(sx, px, idx, nb, etype, vtype,	\
					strt, nfull, expr);		\
    } while (0)

#define ITERATE_BY_REGION(sx, px, idx, nb, etype, vtype, expr) do {	\
	ITERATE_BY_REGION_PARTIAL(sx, px, idx, nb, etype, vtype,	\
				  0, XLENGTH(sx), expr);		\
    } while (0)


#define TYPE_FREE_ITER(sx, ptr, ind, nbatch,expr)\
switch(TYPEOF(sx)){\
case INTSXP:\
	ITERATE_BY_REGION(sx, ptr, ind, nbatch, int, INTEGER, expr);\
	break; \
case REALSXP:\
	ITERATE_BY_REGION(sx, ptr, ind, nbatch, double, REAL, expr);\
	break; \
case LGLSXP:\
	ITERATE_BY_REGION(sx, ptr, ind, nbatch, int, LOGICAL, expr);\
	break; \
default:\
	Rf_error("Unknow data type\n"); \
	break; \
}

#define GET_PTR_OR_REGION(x, x_ptr, batch, has_ptr, type)\
type* x_ptr; \
if (has_ptr) {\
		x_ptr = (type*)DATAPTR(x); \
}\
else {\
	x_ptr = (type*)malloc(sizeof(type) * batch); \
}

#define READ_REGION(x,x_ptr,x_length, x_start,size,x_has_ptr,R_type)\
if (!x_has_ptr) {\
R_xlen_t total_read = 0;\
while (size > total_read) {\
	total_read += R_type##_GET_REGION(x, (x_start + total_read) % x_length, size - total_read, x_ptr+total_read);\
}\
}

#define INNER_LOOP(exprs,x_ptr,y_ptr,x_i,y_i,batch,X_RTYPE_FULL, Y_RTYPE_FULL)\
R_xlen_t LOOP_BATCH=batch;\
for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {\
	x_start = x_start % x_length; \
	y_start = y_start % y_length; \
	if(outer_i + LOOP_BATCH>loop_len) LOOP_BATCH=loop_len-outer_i;\
	READ_REGION(x, x_ptr, x_length, x_start, LOOP_BATCH, x_has_ptr, X_RTYPE_FULL);\
	READ_REGION(y, y_ptr, y_length, y_start, LOOP_BATCH, y_has_ptr, Y_RTYPE_FULL);\
	R_xlen_t x_i = x_has_ptr ? x_start : 0L;\
	R_xlen_t y_i = y_has_ptr ? y_start : 0L;\
for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {\
	exprs;\
	x_i++;\
	y_i++;\
	if (x_has_ptr){\
		if(x_i >= x_length)x_i = 0L;\
	}\
	else {\
		if (x_i >= batch) x_i = 0L;\
	}\
	if (y_has_ptr){\
		if(y_i >= y_length) y_i = 0L;\
	}else{\
		if (y_i >= batch) y_i = 0L; \
	}\
}\
x_start += LOOP_BATCH;\
y_start += LOOP_BATCH;\
}



#define Y_SWITCH(exprs,x_ptr,y_ptr,x_i,y_i,batch,X_RTYPE_FULL)\
switch (TYPEOF(y)) {\
			case LGLSXP: {\
				GET_PTR_OR_REGION(y, y_ptr,batch, y_has_ptr, int);\
				INNER_LOOP(exprs,x_ptr,y_ptr,x_i,y_i,batch,X_RTYPE_FULL,LOGICAL);\
				if(!y_has_ptr) free(y_ptr);\
				break;\
			}\
			case INTSXP: {\
				GET_PTR_OR_REGION(y, y_ptr,batch, y_has_ptr, int);\
				INNER_LOOP(exprs,x_ptr,y_ptr,x_i,y_i,batch,X_RTYPE_FULL,INTEGER);\
				if(!y_has_ptr) free(y_ptr);\
				break;\
			}\
			case REALSXP: {\
				GET_PTR_OR_REGION(y, y_ptr,batch, y_has_ptr, double);\
				INNER_LOOP(exprs,x_ptr,y_ptr,x_i,y_i,batch,X_RTYPE_FULL,REAL);\
				if(!y_has_ptr) free(y_ptr);\
				break;\
			}\
}



#define X_SWITCH(exprs,x_ptr,y_ptr,x_i,y_i,batch)\
switch (TYPEOF(x)) {\
		case LGLSXP: {\
			GET_PTR_OR_REGION(x, x_ptr,batch, x_has_ptr, int);\
			Y_SWITCH(exprs,x_ptr,y_ptr,x_i,y_i,batch,LOGICAL);\
			if(!x_has_ptr) free(x_ptr);\
			break;\
		}\
		case INTSXP: {\
			GET_PTR_OR_REGION(x, x_ptr,batch, x_has_ptr, int);\
			Y_SWITCH(exprs,x_ptr,y_ptr,x_i,y_i,batch,INTEGER);\
			if(!x_has_ptr) free(x_ptr);\
			break;\
		}\
		case REALSXP: {\
			GET_PTR_OR_REGION(x, x_ptr,batch, x_has_ptr, double);\
			Y_SWITCH(exprs,x_ptr,y_ptr,x_i,y_i,batch,REAL);\
			if(!x_has_ptr) free(x_ptr);\
			break;\
		}\
}


#define BINARY_ITERATE_BY_REGION(x,y,x_ptr,y_ptr, x_i, y_i, batch, start, end,exprs) \
	do {\
		R_xlen_t x_length = XLENGTH(x);\
		R_xlen_t y_length = XLENGTH(y);\
		bool x_has_ptr = DATAPTR_OR_NULL(x) != NULL;\
		bool y_has_ptr = DATAPTR_OR_NULL(y) != NULL;\
		R_xlen_t loop_len = end - start;\
		if (loop_len == 0) break;\
		R_xlen_t x_start = start;\
		R_xlen_t y_start = start;\
		X_SWITCH(exprs, x_ptr, y_ptr, x_i, y_i, batch);\
	} while (0);


