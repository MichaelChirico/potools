Used more than one of the arguments input=, file=, text= and cmd=.
Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.
'text=' is type
but must be character.
input= must be a single character string containing a file name, a system command containing at least one space, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or, the input data itself containing at least one \\n or \\r
input= contains no \\n or \\r, but starts with a space. Please remove the leading space, or use text=, file= or cmd=
Input URL requires https:// connection for which fread() requires 'curl' package which cannot be found. Please install 'curl' using 'install.packages('curl')'.
Taking input= as a system command because it contains a space ('
'). If it's a filename please remove the space, or use file= explicitly. A variable is being passed to input= and when this is taken as a system command there is a security concern if you are creating an app, the app could have a malicious user, and the app is not running in a secure environment; e.g. the app is running as root. Please read item 5 in the NEWS file for v1.11.6 for more information and for the option to suppress this message.
File '
' does not exist or is non-readable. getwd()=='
'
File '
' is a directory. Not yet implemented.
File '
' has size 0. Returning a NULL
data.table
data.frame
.
To read gz and bz2 files directly, fread() requires 'R.utils' package which cannot be found. Please install 'R.utils' using 'install.packages('R.utils')'.
'autostart' is now deprecated and ignored. Consider skip='string' or skip=n
colClasses is type 'logical' which is ok if all NA but it has some TRUE or FALSE values in it which is not allowed. Please consider the drop= or select= argument instead. See ?fread.
colClasses is not type list or character vector
colClasses=\"NULL\" (quoted) is interpreted as colClasses=NULL (the default) as opposed to dropping every column.
strip.white==TRUE (default) and \"\" is present in na.strings, so any number of spaces in string columns will already be read as <NA>.
Since strip.white=TRUE (default), use na.strings=\"\" to specify that any number of spaces in a string column should be read as <NA>.
But strip.white=FALSE. Use strip.white=TRUE (default) together with na.strings=\"\" to turn any number of spaces in string columns into <NA>
'data.table' relies on the package 'yaml' to parse the file header; please add this to your library with install.packages('yaml') and try again.
Combining a search string as 'skip' and reading a YAML header may not work as expected -- currently,
reading will proceed to search for 'skip' from the beginning of the file, NOT from the end of
the metadata; please file an issue on GitHub if you'd like to see more intuitive behavior supported.
Encountered <
...
> at the first
unskipped line (
), which does not constitute the start to a valid YAML header
(expecting something matching regex \"
\"); please check your input and try again.
Reached the end of the file before finding a completion to the YAML header. A valid YAML header is bookended by lines matching
the regex \"
\". Please double check the input file is a valid csvy.
User-supplied 'header' will override that found in metadata.
User-supplied column names in 'col.names' will override those found in YAML metadata.
colClasses dictated by user input and those read from YAML header are in conflict (specifically, for column
s
[
,
]); the proceeding assumes the user input was
an intentional override and will ignore the types implied by the YAML header; please exclude
these columns
this column from colClasses if this was unintentional.
User-supplied 'sep' will override that found in metadata.
User-supplied 'quote' will override that found in metadata.
User-supplied 'dec' will override that found in metadata.
User-supplied 'na.strings' will override that found in metadata.
Column '
' was requested to be '
' but fread encountered the following
error
error
warning
:
so the column has been left as type '
'
key argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)
index argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)
Used more than one of the arguments input=, file=, text= and cmd=.
Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.
'text=' is type
but must be character.
input= must be a single character string containing a file name, a system command containing at least one space, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or, the input data itself containing at least one \\n or \\r
input= contains no \\n or \\r, but starts with a space. Please remove the leading space, or use text=, file= or cmd=
Input URL requires https:// connection for which fread() requires 'curl' package which cannot be found. Please install 'curl' using 'install.packages('curl')'.
Taking input= as a system command because it contains a space ('
'). If it's a filename please remove the space, or use file= explicitly. A variable is being passed to input= and when this is taken as a system command there is a security concern if you are creating an app, the app could have a malicious user, and the app is not running in a secure environment; e.g. the app is running as root. Please read item 5 in the NEWS file for v1.11.6 for more information and for the option to suppress this message.
File '
' does not exist or is non-readable. getwd()=='
'
File '
' is a directory. Not yet implemented.
File '
' has size 0. Returning a NULL
data.table
data.frame
.
To read gz and bz2 files directly, fread() requires 'R.utils' package which cannot be found. Please install 'R.utils' using 'install.packages('R.utils')'.
'autostart' is now deprecated and ignored. Consider skip='string' or skip=n
colClasses is type 'logical' which is ok if all NA but it has some TRUE or FALSE values in it which is not allowed. Please consider the drop= or select= argument instead. See ?fread.
colClasses is not type list or character vector
colClasses=\"NULL\" (quoted) is interpreted as colClasses=NULL (the default) as opposed to dropping every column.
strip.white==TRUE (default) and \"\" is present in na.strings, so any number of spaces in string columns will already be read as <NA>.
Since strip.white=TRUE (default), use na.strings=\"\" to specify that any number of spaces in a string column should be read as <NA>.
But strip.white=FALSE. Use strip.white=TRUE (default) together with na.strings=\"\" to turn any number of spaces in string columns into <NA>
'data.table' relies on the package 'yaml' to parse the file header; please add this to your library with install.packages('yaml') and try again.
Combining a search string as 'skip' and reading a YAML header may not work as expected -- currently,
reading will proceed to search for 'skip' from the beginning of the file, NOT from the end of
the metadata; please file an issue on GitHub if you'd like to see more intuitive behavior supported.
Encountered <
...
> at the first
unskipped line (
), which does not constitute the start to a valid YAML header
(expecting something matching regex \"
\"); please check your input and try again.
Reached the end of the file before finding a completion to the YAML header. A valid YAML header is bookended by lines matching
the regex \"
\". Please double check the input file is a valid csvy.
User-supplied 'header' will override that found in metadata.
User-supplied column names in 'col.names' will override those found in YAML metadata.
colClasses dictated by user input and those read from YAML header are in conflict (specifically, for column
s
[
,
]); the proceeding assumes the user input was
an intentional override and will ignore the types implied by the YAML header; please exclude
these columns
this column from colClasses if this was unintentional.
User-supplied 'sep' will override that found in metadata.
User-supplied 'quote' will override that found in metadata.
User-supplied 'dec' will override that found in metadata.
User-supplied 'na.strings' will override that found in metadata.
Column '
' was requested to be '
' but fread encountered the following
error
error
warning
:
so the column has been left as type '
'
key argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)
index argument of data.table() must be a character vector naming columns (NB: col.names are applied before this)
