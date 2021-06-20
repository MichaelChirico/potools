y and x must both be data.tables. Use `setDT()` to convert list/data.frames to data.tables by reference or as.data.table() to convert to data.tables by copying.
maxgap must be a non-negative integer value of length 1
minoverlap must be a positive integer value of length 1
which must be a logical vector of length 1. Either TRUE/FALSE
nomatch must either be NA or NULL
maxgap and minoverlap arguments are not yet implemented.
'y' must be keyed (i.e., sorted, and, marked as sorted). Call setkey(y, ...) first, see ?setkey. Also check the examples in ?foverlaps.
'by.x' and 'by.y' should contain at least two column names (or numbers) each - corresponding to 'start' and 'end' points of intervals. Please see ?foverlaps and examples for more info.
Invalid numeric value for 'by.x'; it should be a vector with values 1 <= by.x <= length(x)
Invalid numeric value for 'by.y'; it should be a vector with values 1 <= by.y <= length(y)
A non-empty vector of column names or numbers is required for by.x
A non-empty vector of column names or numbers is required for by.y
The first
columns of y's key must be identical to the columns specified in by.y.
Elements listed in 'by.x' must be valid names in data.table 'x'
Duplicate columns are not allowed in overlap joins. This may change in the future.
length(by.x) != length(by.y). Columns specified in by.x should correspond to columns specified in by.y and should be of same lengths.
x has some duplicated column name(s):
,
. Please remove or rename the duplicate(s) and try again.
y has some duplicated column name(s):
,
. Please remove or rename the duplicate(s) and try again.
The last two columns in by.x should correspond to the 'start' and 'end' intervals in data.table 'x' and must be integer/numeric type.
NA values in data.table 'x' start column: '
'. All rows with NA values in the range columns must be removed for foverlaps() to work.
NA values in data.table 'x' end column: '
'. All rows with NA values in the range columns must be removed for foverlaps() to work.
All entries in column
should be <= corresponding entries in column
in data.table 'x'.
The last two columns in by.y should correspond to the 'start' and 'end' intervals in data.table 'y' and must be integer/numeric type.
NA values in data.table 'y' start column: '
'. All rows with NA values in the range columns must be removed for foverlaps() to work.
NA values in data.table 'y' end column: '
'. All rows with NA values in the range columns must be removed for foverlaps() to work.
All entries in column
should be <= corresponding entries in column
in data.table 'y'.
Some interval cols are of type POSIXct while others are not. Please ensure all interval cols are (or are not) of POSIXct type
POSIXct interval cols have mixed timezones. Overlaps are performed on the internal numerical representation of POSIXct objects (always in UTC epoch time), therefore printed values may give the impression that values don't overlap but their internal representations do Please ensure that POSIXct type interval cols have identical 'tzone' attributes to avoid confusion.


Not yet implemented
Not yet implemented
maxgap > minoverlap. maxgap will have no effect here.
Not yet implemented
y and x must both be data.tables. Use `setDT()` to convert list/data.frames to data.tables by reference or as.data.table() to convert to data.tables by copying.
maxgap must be a non-negative integer value of length 1
minoverlap must be a positive integer value of length 1
which must be a logical vector of length 1. Either TRUE/FALSE
nomatch must either be NA or NULL
maxgap and minoverlap arguments are not yet implemented.
'y' must be keyed (i.e., sorted, and, marked as sorted). Call setkey(y, ...) first, see ?setkey. Also check the examples in ?foverlaps.
'by.x' and 'by.y' should contain at least two column names (or numbers) each - corresponding to 'start' and 'end' points of intervals. Please see ?foverlaps and examples for more info.
Invalid numeric value for 'by.x'; it should be a vector with values 1 <= by.x <= length(x)
Invalid numeric value for 'by.y'; it should be a vector with values 1 <= by.y <= length(y)
A non-empty vector of column names or numbers is required for by.x
A non-empty vector of column names or numbers is required for by.y
The first
columns of y's key must be identical to the columns specified in by.y.
Elements listed in 'by.x' must be valid names in data.table 'x'
Duplicate columns are not allowed in overlap joins. This may change in the future.
length(by.x) != length(by.y). Columns specified in by.x should correspond to columns specified in by.y and should be of same lengths.
x has some duplicated column name(s):
,
. Please remove or rename the duplicate(s) and try again.
y has some duplicated column name(s):
,
. Please remove or rename the duplicate(s) and try again.
The last two columns in by.x should correspond to the 'start' and 'end' intervals in data.table 'x' and must be integer/numeric type.
NA values in data.table 'x' start column: '
'. All rows with NA values in the range columns must be removed for foverlaps() to work.
NA values in data.table 'x' end column: '
'. All rows with NA values in the range columns must be removed for foverlaps() to work.
All entries in column
should be <= corresponding entries in column
in data.table 'x'.
The last two columns in by.y should correspond to the 'start' and 'end' intervals in data.table 'y' and must be integer/numeric type.
NA values in data.table 'y' start column: '
'. All rows with NA values in the range columns must be removed for foverlaps() to work.
NA values in data.table 'y' end column: '
'. All rows with NA values in the range columns must be removed for foverlaps() to work.
All entries in column
should be <= corresponding entries in column
in data.table 'y'.
Some interval cols are of type POSIXct while others are not. Please ensure all interval cols are (or are not) of POSIXct type
POSIXct interval cols have mixed timezones. Overlaps are performed on the internal numerical representation of POSIXct objects (always in UTC epoch time), therefore printed values may give the impression that values don't overlap but their internal representations do Please ensure that POSIXct type interval cols have identical 'tzone' attributes to avoid confusion.


Not yet implemented
Not yet implemented
maxgap > minoverlap. maxgap will have no effect here.
Not yet implemented
