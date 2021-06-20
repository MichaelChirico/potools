x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.
key(x)<-value is deprecated and not supported. Please change to use setkey() with perhaps copy(). Has been warning since 2012 and will be an error in future.
x is not a data.table
cols is not a character vector. Please see further information in ?setkey.
Setting a physical key on .SD is reserved for possible future use; to modify the original data's order by group. Try setindex() instead. Or, set*(copy(.SD)) as a (slow) last resort.
cols is a character vector of zero length. Removed the key, but use NULL instead, or wrap with suppressWarnings() to avoid this warning.
cols is the empty string. Use NULL to remove the key.
cols contains some blanks.
some columns are not in the data.table:
,
x contains a column called '.xi'. Conflicts with internal use by data.table.
Column '
' is type '
' which is not supported as a key column type, currently.
Internal error. 'cols' should be character at this point in setkey; please report.
Internal error: index '
' exists but is invalid
x is vector but 'by' is supplied
x is a single vector, non-NULL 'by' doesn't make sense
Attempting to order a 0-column data.table or data.frame.
The first item passed to [f]order is a plain list but there are more items. It should be a data.table or data.frame.
Internal code should not be being called on type double
Input is not a vector of type double. New parallel sort has only been done for double vectors so far. Using one thread.
New parallel sort has not been implemented for decreasing=TRUE so far. Using one thread.
New parallel sort has not been implemented for vectors containing NA values so far. Using one thread.
x must be a data.frame or data.table
x must be a data.frame or data.table
na.last must be logical TRUE/FALSE
cols is not a character vector. Please see further information in ?setorder.
cols is a character vector of zero length. Use NULL instead, or wrap with suppressWarnings() to avoid this warning.
cols contains some blanks.
some columns are not in the data.table:
,
x contains a column called '.xi'. Conflicts with internal use by data.table.
Column '
' is type '
' which is not supported for ordering currently.
Internal error. 'cols' should be character at this point in setkey; please report.
'sorted' is TRUE but element
is non-atomic, which can't be sorted; try setting sorted = FALSE
Cross product of elements provided to CJ() would result in %.0f rows which exceeds .Machine$integer.max == %d
x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.
key(x)<-value is deprecated and not supported. Please change to use setkey() with perhaps copy(). Has been warning since 2012 and will be an error in future.
x is not a data.table
cols is not a character vector. Please see further information in ?setkey.
Setting a physical key on .SD is reserved for possible future use; to modify the original data's order by group. Try setindex() instead. Or, set*(copy(.SD)) as a (slow) last resort.
cols is a character vector of zero length. Removed the key, but use NULL instead, or wrap with suppressWarnings() to avoid this warning.
cols is the empty string. Use NULL to remove the key.
cols contains some blanks.
some columns are not in the data.table:
,
x contains a column called '.xi'. Conflicts with internal use by data.table.
Column '
' is type '
' which is not supported as a key column type, currently.
Internal error. 'cols' should be character at this point in setkey; please report.
Internal error: index '
' exists but is invalid
x is vector but 'by' is supplied
x is a single vector, non-NULL 'by' doesn't make sense
Attempting to order a 0-column data.table or data.frame.
The first item passed to [f]order is a plain list but there are more items. It should be a data.table or data.frame.
Internal code should not be being called on type double
Input is not a vector of type double. New parallel sort has only been done for double vectors so far. Using one thread.
New parallel sort has not been implemented for decreasing=TRUE so far. Using one thread.
New parallel sort has not been implemented for vectors containing NA values so far. Using one thread.
x must be a data.frame or data.table
x must be a data.frame or data.table
na.last must be logical TRUE/FALSE
cols is not a character vector. Please see further information in ?setorder.
cols is a character vector of zero length. Use NULL instead, or wrap with suppressWarnings() to avoid this warning.
cols contains some blanks.
some columns are not in the data.table:
,
x contains a column called '.xi'. Conflicts with internal use by data.table.
Column '
' is type '
' which is not supported for ordering currently.
Internal error. 'cols' should be character at this point in setkey; please report.
'sorted' is TRUE but element
is non-atomic, which can't be sorted; try setting sorted = FALSE
Cross product of elements provided to CJ() would result in %.0f rows which exceeds .Machine$integer.max == %d
