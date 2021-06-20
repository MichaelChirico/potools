Internal error: col passed to getMaxCategLen is missing levels
Internal error: getMaxListItemLen should have caught this up front.
Row %<PRId64> of list column is type '%s' - not yet implemented. fwrite() can write list columns containing items which are atomic vectors of type logical, integer, integer64, double, complex and character.
Internal error: row %<PRId64> of list column has no max length method implemented
fwrite must be passed an object of type list; e.g. data.frame, data.table
fwrite was passed an empty list of no columns. Nothing to write.
Column %d's length (%d) is not the same as column 1's length (%<PRId64>)
Column %d's type is '%s' - not yet implemented in fwrite.
No list columns are present. Setting sep2='' otherwise quote='auto' would quote fields containing sep2.\n
If quote='auto', fields will be quoted if the field contains either sep ('%c') or sep2 ('%c') because column %d is a list column.\n
sep ('%c'), sep2 ('%c') and dec ('%c') must all be different. Column %d is a list column.
