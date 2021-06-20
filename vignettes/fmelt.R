The melt generic in data.table has been passed a
, but data.table::melt currently only has a method for data.tables. Please confirm your input is a data.table, with setDT(
) or as.data.table(
). If you intend to use a method from reshape2, try installing that package first, but do note that reshape2 is superseded and is no longer actively developed.
The melt generic in data.table has been passed a
and will attempt to redirect to the relevant reshape2 method; please note that reshape2 is superseded and is no longer actively developed, and this redirection is now deprecated. To continue using melt methods from reshape2 while both libraries are attached, e.g. melt.list, you can prepend the namespace like reshape2::melt(
). In the next version, this warning will become an error.
Input patterns must be of type character.
Pattern
s
not found: [
,
]
both sep and pattern arguments used in measure; must use either sep or pattern (not both)
multiple.keyword must be a character string with nchar>0
cols must be a character vector of column names
each ... argument to measure must be either a symbol without argument name, or a function with argument name, problems:
,
group names specified in ... conflict with measure argument names; please fix by changing group names:
,
names should be unique, problems:
,
number of ... arguments to measure =
must be same as
=
pattern must be character string
pattern did not match any cols, so nothing would be melted; fix by changing pattern
pattern must contain at least one capture group (parenthesized sub-pattern)
sep must be character string
each column name results in only one item after splitting using sep, which means that all columns would be melted; to fix please either specify melt on all columns directly without using measure, or use a different sep/pattern specification
number of unique column IDs =
is less than number of melted columns =
; fix by changing pattern/sep
each ... argument to measure must be a function with at least one argument, problem:
each ... argument to measure must be a function that returns an atomic vector with same length as its first argument, problem:
conversion function returned vector of all NA
number of unique groups after applying type conversion functions less than number of groups, change type conversion
column class=
after applying conversion function, but must be character
is the only group; fix by creating at least one more group
'data' must be a data.table
'value.name' provided in both 'measure.vars'
and 'value.name argument'; value provided in
'measure.vars' is given precedence.
Please provide a name to each element of 'measure.vars'.
The melt generic in data.table has been passed a
, but data.table::melt currently only has a method for data.tables. Please confirm your input is a data.table, with setDT(
) or as.data.table(
). If you intend to use a method from reshape2, try installing that package first, but do note that reshape2 is superseded and is no longer actively developed.
The melt generic in data.table has been passed a
and will attempt to redirect to the relevant reshape2 method; please note that reshape2 is superseded and is no longer actively developed, and this redirection is now deprecated. To continue using melt methods from reshape2 while both libraries are attached, e.g. melt.list, you can prepend the namespace like reshape2::melt(
). In the next version, this warning will become an error.
Input patterns must be of type character.
Pattern
s
not found: [
,
]
both sep and pattern arguments used in measure; must use either sep or pattern (not both)
multiple.keyword must be a character string with nchar>0
cols must be a character vector of column names
each ... argument to measure must be either a symbol without argument name, or a function with argument name, problems:
,
group names specified in ... conflict with measure argument names; please fix by changing group names:
,
names should be unique, problems:
,
number of ... arguments to measure =
must be same as
=
pattern must be character string
pattern did not match any cols, so nothing would be melted; fix by changing pattern
pattern must contain at least one capture group (parenthesized sub-pattern)
sep must be character string
each column name results in only one item after splitting using sep, which means that all columns would be melted; to fix please either specify melt on all columns directly without using measure, or use a different sep/pattern specification
number of unique column IDs =
is less than number of melted columns =
; fix by changing pattern/sep
each ... argument to measure must be a function with at least one argument, problem:
each ... argument to measure must be a function that returns an atomic vector with same length as its first argument, problem:
conversion function returned vector of all NA
number of unique groups after applying type conversion functions less than number of groups, change type conversion
column class=
after applying conversion function, but must be character
is the only group; fix by creating at least one more group
'data' must be a data.table
'value.name' provided in both 'measure.vars'
and 'value.name argument'; value provided in
'measure.vars' is given precedence.
Please provide a name to each element of 'measure.vars'.
