Internal error: freadR input not a single character string: a filename or the data itself. Should have been caught at R level.
Input contains a \\n or is \")\". Taking this to be text input (not a filename)\n
Input contains no \\n. Taking this to be a filename to open\n
Internal error: freadR sep not a single character. R level catches this.
Internal error: freadR dec not a single character. R level catches this.
quote= must be a single character, blank \"\", or FALSE
Internal error: freadR nrows not a single real. R level catches this.
Internal error: skip not integer or string in freadR.c
Internal error: NAstringsArg is type '%s'. R level catches this
nThread(%d)<1
'integer64' must be a single character string
Invalid value integer64='%s'. Must be 'integer64', 'character', 'double' or 'numeric'
Use either select= or drop= but not both.
select= is type list for specifying types in select=, but colClasses= has been provided as well. Please remove colClasses=.
select= is type list but has no names; expecting list(type1=cols1, type2=cols2, ...)
select= is a named vector specifying the columns to select and their types, but colClasses= has been provided as well. Please remove colClasses=.
colClasses is type list but has no names
encoding='%s' invalid. Must be 'unknown', 'Latin-1' or 'UTF-8'
drop[%d]
colClasses[[%d]][%d]
Column name '%s' (%s) not found
%s is NA
%s is %d which is out of range [1,ncol=%d]
Internal error: typeSize[CT_BOOL8_N] != 1
Internal error: typeSize[CT_STRING] != 1
V%d
Column name '%s' not found in column name header (case sensitive), skipping.
Column number %d (select[%d]) is negative but should be in the range [1,ncol=%d]. Consider drop= for column exclusion.
select = 0 (select[%d]) has no meaning. All values of select should be in the range [1,ncol=%d].
Column number %d (select[%d]) is too large for this table, which only has %d columns.
Column number %d ('%s') has been selected twice by select=
colClasses= is an unnamed vector of types, length %d, but there are %d columns in the input. To specify types for a subset of columns, you can use a named vector, list format, or specify types using select= instead of colClasses=. Please see examples in ?fread.
Internal error: selectInts is NULL but selectColClasses is true
Internal error: length(selectSxp)!=length(colClassesSxp) but selectColClasses is true
colClasses is type '%s' but should be list or character
colClasses is type list but has no names
Column name '%s' (colClasses[[%d]][%d]) not found
colClasses[[%d]][%d] is NA
Column %d ('%s') appears more than once in colClasses. The second time is colClasses[[%d]][%d].
Column number %d (colClasses[[%d]][%d]) is out of range [1,ncol=%d]
Field size is 1 but the field is of type %d\n
Internal error: unexpected field of size %d\n
|--------------------------------------------------|\n|
%s
|\n
%s
