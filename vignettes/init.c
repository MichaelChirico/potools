Pointers are %d bytes, greater than 8. We have not tested on any architecture greater than 64bit yet.
Checking NA_INTEGER [%d] == INT_MIN [%d] %s
Checking NA_INTEGER [%d] == NA_LOGICAL [%d] %s
Checking sizeof(int) [%d] is 4 %s
Checking sizeof(double) [%d] is 8 %s
Checking sizeof(long long) [%d] is 8 %s
Checking sizeof(pointer) [%d] is 4 or 8 %s
Checking sizeof(SEXP) [%d] == sizeof(pointer) [%d] %s
Checking sizeof(uint64_t) [%d] is 8 %s
Checking sizeof(int64_t) [%d] is 8 %s
Checking sizeof(signed char) [%d] is 1 %s
Checking sizeof(int8_t) [%d] is 1 %s
Checking sizeof(uint8_t) [%d] is 1 %s
Checking sizeof(int16_t) [%d] is 2 %s
Checking sizeof(uint16_t) [%d] is 2 %s
Checking LENGTH(allocVector(INTSXP,2)) [%d] is 2 %s
Checking TRUELENGTH(allocVector(INTSXP,2)) [%d] is 0 %s
Checking memset(&i,0,sizeof(int)); i == (int)0 %s
Checking memset(&ui, 0, sizeof(unsigned int)); ui == (unsigned int)0 %s
Checking memset(&d, 0, sizeof(double)); d == (double)0.0 %s
Checking memset(&ld, 0, sizeof(long double)); ld == (long double)0.0 %s
The ascii character '/' is not just before '0'
The C expression (uint_fast8_t)('/'-'0')<10 is true. Should be false.
The ascii character ':' is not just after '9'
The C expression (uint_fast8_t)('9'-':')<10 is true. Should be false.
Conversion of NA_INT64 via double failed %<PRId64>!=%<PRId64>
NA_INT64_D (negative -0.0) is not == 0.0.
NA_INT64_D (negative -0.0) is not ==-0.0.
ISNAN(NA_INT64_D) is TRUE but should not be
isnan(NA_INT64_D) is TRUE but should not be
PRINTNAME(install(\"integer64\")) has returned %s not %s
verbose option must be length 1 non-NA logical or integer
.Last.value in namespace is not a length 1 integer
