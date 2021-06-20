Argument 'x' must be a data.table object
Argument 'by' must be a character vector of column names used in grouping.
Argument 'id' must be a logical scalar.
Argument 'x' must be a data.table object
Argument 'by' must be a character vector of column names used in grouping.
Argument 'id' must be a logical scalar.
Argument 'j' is required
Argument 'x' must be a data.table object
Argument 'x' is a 0-column data.table; no measure to apply grouping over.
Input data.table must not contain duplicate column names.
Argument 'by' must be a character vector of column names used in grouping.
Argument 'by' must have unique column names for grouping.
Argument 'sets' must be a list of character vectors.
Argument 'id' must be a logical scalar.
All columns used in 'sets' argument must be in 'by' too. Columns used in 'sets' but not present in 'by':
When using `id=TRUE` the 'x' data.table must not have a column named 'grouping'.
Character vectors in 'sets' list must not have duplicated column names within a single grouping set.
'sets' contains a duplicate (i.e., equivalent up to sorting) element at index
; as such, there will be duplicate rows in the output -- note that grouping by A,B and B,A will produce the same aggregations. Use `sets=unique(lapply(sets, sort))` to eliminate duplicates.
Expression passed to grouping sets function must not update by reference. Use ':=' on results of your grouping function.
When using `id=TRUE` the 'j' expression must not evaluate to a column named 'grouping'.
There exists duplicated column names in the results, ensure the column passed/evaluated in `j` and those in `by` are not overlapping.
Using integer64 class columns require to have 'bit64' package installed.
Argument 'x' must be a data.table object
Argument 'by' must be a character vector of column names used in grouping.
Argument 'id' must be a logical scalar.
Argument 'x' must be a data.table object
Argument 'by' must be a character vector of column names used in grouping.
Argument 'id' must be a logical scalar.
Argument 'j' is required
Argument 'x' must be a data.table object
Argument 'x' is a 0-column data.table; no measure to apply grouping over.
Input data.table must not contain duplicate column names.
Argument 'by' must be a character vector of column names used in grouping.
Argument 'by' must have unique column names for grouping.
Argument 'sets' must be a list of character vectors.
Argument 'id' must be a logical scalar.
All columns used in 'sets' argument must be in 'by' too. Columns used in 'sets' but not present in 'by':
When using `id=TRUE` the 'x' data.table must not have a column named 'grouping'.
Character vectors in 'sets' list must not have duplicated column names within a single grouping set.
'sets' contains a duplicate (i.e., equivalent up to sorting) element at index
; as such, there will be duplicate rows in the output -- note that grouping by A,B and B,A will produce the same aggregations. Use `sets=unique(lapply(sets, sort))` to eliminate duplicates.
Expression passed to grouping sets function must not update by reference. Use ':=' on results of your grouping function.
When using `id=TRUE` the 'j' expression must not evaluate to a column named 'grouping'.
There exists duplicated column names in the results, ensure the column passed/evaluated in `j` and those in `by` are not overlapping.
Using integer64 class columns require to have 'bit64' package installed.
