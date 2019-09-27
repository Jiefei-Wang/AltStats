// [[Rcpp::export]]
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

	do {
		R_xlen_t x_length = XLENGTH(x);
		R_xlen_t y_length = XLENGTH(y);
		bool x_has_ptr = DATAPTR_OR_NULL(x) != NULL;
		bool y_has_ptr = DATAPTR_OR_NULL(y) != NULL;
		R_xlen_t loop_len = result_length - 0;
		if (loop_len == 0) break;
		R_xlen_t x_i = 0 % x_length;
		R_xlen_t y_i = 0 % y_length;
		switch (TYPEOF(x)) {
		case LGLSXP:
		{
			int* x_ptr;
			if (x_has_ptr) {
				x_ptr = (int*)DATAPTR(x);
			}
			else {
				x_ptr = (int*)malloc(sizeof(int) * 512);
			};
			switch (TYPEOF(y)) {
			case LGLSXP:
			{
				int* y_ptr;
				if (y_has_ptr) {
					y_ptr = (int*)DATAPTR(y);
				}
				else {
					y_ptr = (int*)malloc(sizeof(int) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += LOGICAL_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += LOGICAL_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			case INTSXP:
			{
				int* y_ptr;
				if (y_has_ptr) {
					y_ptr = (int*)DATAPTR(y);
				}
				else {
					y_ptr = (int*)malloc(sizeof(int) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += LOGICAL_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += INTEGER_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			case REALSXP:
			{
				double* y_ptr;
				if (y_has_ptr) {
					y_ptr = (double*)DATAPTR(y);
				}
				else {
					y_ptr = (double*)malloc(sizeof(double) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += LOGICAL_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += REAL_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			}; free(x_ptr);
			break;
		}
		case INTSXP:
		{
			int* x_ptr;
			if (x_has_ptr) {
				x_ptr = (int*)DATAPTR(x);
			}
			else {
				x_ptr = (int*)malloc(sizeof(int) * 512);
			};
			switch (TYPEOF(y)) {
			case LGLSXP:
			{
				int* y_ptr;
				if (y_has_ptr) {
					y_ptr = (int*)DATAPTR(y);
				}
				else {
					y_ptr = (int*)malloc(sizeof(int) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += INTEGER_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += LOGICAL_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			case INTSXP:
			{
				int* y_ptr;
				if (y_has_ptr) {
					y_ptr = (int*)DATAPTR(y);
				}
				else {
					y_ptr = (int*)malloc(sizeof(int) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += INTEGER_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += INTEGER_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			case REALSXP:
			{
				double* y_ptr;
				if (y_has_ptr) {
					y_ptr = (double*)DATAPTR(y);
				}
				else {
					y_ptr = (double*)malloc(sizeof(double) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += INTEGER_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += REAL_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			}; free(x_ptr);
			break;
		}
		case REALSXP:
		{
			double* x_ptr;
			if (x_has_ptr) {
				x_ptr = (double*)DATAPTR(x);
			}
			else {
				x_ptr = (double*)malloc(sizeof(double) * 512);
			};
			switch (TYPEOF(y)) {
			case LGLSXP:
			{
				int* y_ptr;
				if (y_has_ptr) {
					y_ptr = (int*)DATAPTR(y);
				}
				else {
					y_ptr = (int*)malloc(sizeof(int) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += REAL_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += LOGICAL_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			case INTSXP:
			{
				int* y_ptr;
				if (y_has_ptr) {
					y_ptr = (int*)DATAPTR(y);
				}
				else {
					y_ptr = (int*)malloc(sizeof(int) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += REAL_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += INTEGER_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			case REALSXP:
			{
				double* y_ptr;
				if (y_has_ptr) {
					y_ptr = (double*)DATAPTR(y);
				}
				else {
					y_ptr = (double*)malloc(sizeof(double) * 512);
				}; R_xlen_t LOOP_BATCH = 512;
				for (R_xlen_t outer_i = 0; outer_i < loop_len; outer_i = outer_i + LOOP_BATCH) {
					Rprintf("loop_len: %lld,outer_i:%lld\n", loop_len,outer_i);
					if (outer_i + LOOP_BATCH > loop_len) LOOP_BATCH = loop_len - outer_i;
					if (!x_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += REAL_GET_REGION(x, (x_i + total_read) % x_length, LOOP_BATCH - total_read, x_ptr);
						}
					};
					if (!y_has_ptr) {
						R_xlen_t total_read = 0;
						while (LOOP_BATCH > total_read) {
							total_read += REAL_GET_REGION(y, (y_i + total_read) % y_length, LOOP_BATCH - total_read, y_ptr);
						}
					};
					for (R_xlen_t inner_i = 0; inner_i < LOOP_BATCH; inner_i++) {
						{
							Rprintf("inner_i:%lld\n", inner_i);
							R_xlen_t absolute_i = outer_i + inner_i;
							switch (TYPEOF(result)) {
							case INTSXP:
								SET_INTEGER_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case REALSXP:
								SET_REAL_ELT(result, absolute_i, binary_oprate < double >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							case LGLSXP:
								SET_LOGICAL_ELT(result, absolute_i, binary_oprate < int >(op_char, x_ptr[x_i], y_ptr[y_i]));
								break;
							default:
								Rf_error("Unknow data type\n");
								break;
							}
						};
						Rprintf("Finish\n");
						x_i++;
						y_i++;
						if (x_has_ptr && x_i >= x_length) x_i = 0;
						if (y_has_ptr && y_i >= y_length) y_i = 0;
					}
				}; free(y_ptr);
				break;
			}
			}; free(x_ptr);
			break;
		}
		};
	} while (0);
	return(result);
}