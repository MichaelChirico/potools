key argument of data.table() must be character
Object '
' not found. Perhaps you intended
Object '
' not found amongst
verbose must be logical or integer
verbose must be length 1 non-NA
Ignoring by/keyby because 'j' is not supplied
When by and keyby are both provided, keyby must be TRUE or FALSE
Argument 'by' after substitute:

When on= is provided but not i=, on= must be a named list or data.table|frame, and a natural join (i.e. join on common names) is invoked. Ignoring on= which is '
'.
i and j are both missing so ignoring the other arguments. This warning will be upgraded to error in future.
mult argument can only be 'first', 'last' or 'all'
roll must be a single TRUE, FALSE, positive/negative integer/double including +Inf and -Inf or 'nearest'
roll is '
' (type character). Only valid character value is 'nearest'.
rollends must be a logical vector
rollends must be length 1 or 2
nomatch= must be either NA or NULL (or 0 for backwards compatibility which is the same as NULL)
which= must be a logical vector length 1. Either FALSE, TRUE or NA.
which==
(meaning return row numbers) but j is also supplied. Either you need row numbers or the result of j, but only one type of result can be returned.
which=NA with nomatch=0 would always return an empty vector. Please change or remove either which or nomatch.
j must be provided when with=FALSE
Argument 'j'  after substitute:

The symbol .. is invalid. The .. prefix must be followed by at least one character.
Variable '
' is not found in calling scope. Looking in calling scope because you used the .. prefix.
Variable '..
' does exist in calling scope though, so please just removed the .. prefix from that variable name in calling scope.

Both '
' and '..
' exist in calling scope. Please remove the '..
' variable in calling scope for clarity.
Internal error:  DT[, ..var] should be dealt with by the branch above now.
Variable '
' is not found in calling scope. Looking in calling scope because you set with=FALSE. Also, please use .. symbol prefix and remove with=FALSE.
You have wrapped := with {} which is ok but then := must be the only thing inside {}. You have something else inside {} as well. Consider placing the {} on the RHS of := instead; e.g. DT[,someCol:={tmpVar1<-...;tmpVar2<-...;tmpVar1*tmpVar2}
:= with keyby is only possible when i is not supplied since you can't setkey on a subset of rows. Either change keyby to by or remove i
nomatch isn't relevant together with :=, ignoring nomatch
Argument 'i'  after substitute:

not-join '!' prefix is present on i but nomatch is provided. Please remove nomatch.
Operator := detected in i, the first argument inside DT[...], but is only valid in the second argument, j. Most often, this happens when forgetting the first comma (e.g. DT[newvar := 5] instead of DT[ , new_var := 5]). Please double-check the syntax. Run traceback(), and debugger() to get a line number.
is not found in calling scope
When the first argument inside DT[...] is a single symbol (e.g. DT[var]), data.table looks for var in calling scope.
i is invalid type (matrix). Perhaps in future a 2 column matrix could return a list of elements of DT (in the spirit of A[B] in FAQ 2.14). Please report to data.table issue tracker if you'd like this, or add your comments to FR #657.
When i is a data.table (or character vector), the columns to join by must be specified using 'on=' argument (see ?data.table), by keying x (i.e. sorted, and, marked as sorted, see ?setkey), or by sharing column names between x and i (i.e., a natural join). Keyed joins might have further speed benefits on very large data due to x being sorted in RAM.
Attempting to do natural join but no common columns in provided tables

Internal error. Cannot by=.EACHI when joining to an index, yet
Internal error. irows has length in by=.EACHI


logical error. i is not a data.table, but 'on' argument is provided.
i has evaluated to type
. Expecting logical, integer or double.
internal error: notjoin and which=NA (non-matches), huh? please provide reproducible example to issue tracker
i evaluates to a logical vector length
but there are
rows. Recycling of logical i is no longer allowed as it hides more bugs than is worth the rare convenience. Explicitly use rep(...,length=.N) if you really need to recycle.
Internal error: notjoin but byjoin or !integer or nomatch==NA

with=FALSE together with := was deprecated in v1.9.4 released Oct 2014. Please wrap the LHS of := with parentheses; e.g., DT[,(myVar):=sum(b),by=a] to assign to column name(s) held in variable myVar. See ?':=' for other examples. As warned in 2014, this is now a warning.
with=FALSE ignored, it isn't needed when using :=. See ?':=' for examples.
column(s) not removed because not found:
column(s) not found:
Item
of j is
which is outside the column number range [1,ncol=
]
j mixes positives and negatives
When with=FALSE, j-argument should be of type logical/character/integer indicating the columns to select.
by=c(...), key(...) or names(...) must evaluate to 'character'
'by' is a character vector length
but one or more items include a comma. Either pass a vector of column names (which can contain spaces, but no commas), or pass a vector length 1 containing comma separated column names. See ?data.table for other possibilities.
Internal error: irows isn't integer
'by' appears to evaluate to column names but isn't c() or key(). Use by=list(...) if you can. Otherwise, by=eval
should work. This is for efficiency so data.table can detect which columns are needed.
'by' or 'keyby' must evaluate to a vector or a list of vectors (where 'list' includes data.table and data.frame which are lists, too)
column or expression
of 'by' or 'keyby' is type
. Do not quote column names. Usage: DT[,sum(colC),by=list(colA,month(colB))]
The items in the 'by' or 'keyby' list are length(s) (%s). Each must be length %d; the same length as there are rows in x (after subsetting if i is provided).
Internal error: drop_dot passed
items
Item %d of the .() or list() passed to j is missing
j may not evaluate to the same number of columns for each group; if you're sure this warning is in error, please put the branching logic outside of [ for efficiency
Different branches of j expression produced different auto-named columns:
%s!=%s
; using the most \"last\" names
When .SDcols is a function, it is applied to each column; the output of this function must be a non-missing boolean scalar signalling inclusion/exclusion of the column. However, these conditions were not met for:
.SDcols missing at the following indices:
.SDcols is numeric but has both +ve and -ve indices
.SDcols is numeric but out of bounds [1,
] at:
.SDcols should be column numbers or names
Some items of .SDcols are not column names:
'(m)get' found in j. ansvars being set to all columns. Use .SDcols or a single j=eval(macro) instead. Both will detect the columns used which is important for efficiency.\nOld ansvars: %s
This j doesn't use .SD but .SDcols has been supplied. Ignoring .SDcols. See ?data.table.
.SD is locked. Using := in .SD's j is reserved for possible future use; a tortuously flexible way to modify by group. Use := in j directly to modify by group by reference.
In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.
In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.
LHS of := must be a symbol, or an atomic vector (column names or positions).
LHS of := appears to be column positions but are outside [1,ncol] range. New columns can only be added by name.
LHS of := isn't column names ('character') or positions ('integer' or 'numeric')
Invalid .internal.selfref detected and fixed by taking a (shallow) copy of the data.table so that := can add this new column by reference. At an earlier point, this data.table has been copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.
Cannot assign to an under-allocated recursively indexed list -- L[[i]][,:=] syntax is only valid when i is length 1, but its length is
Internal error -- item '
' not found in names of list
Internal error -- column(s) not found:
Internal error -- column(s) not found:
,
strptime() usage detected and wrapped with as.POSIXct(). This is to minimize the chance of assigning POSIXlt columns, which use 40+ bytes to store one date (versus 8 for POSIXct). Use as.POSIXct() (which will call strptime() as needed internally) to avoid this warning.
Variable '
' is not found in calling scope. Looking in calling scope because this symbol was prefixed with .. in the j= parameter.
Internal error: xcolAns does not pass checks:
,
Internal error: irows is NULL when making join result at R level. Should no longer happen now we use CsubsetDT earlier.
j (the 2nd argument inside [...]) is a single symbol but column name '
' is not found. Perhaps you intended DT[, ..
]. This difference to data.frame is deliberate and explained in FAQ 1.1.
Internal error: j has created a data.table result containing a NULL column
The column '.N' can't be grouped because it conflicts with the special .N variable. Try setnames(DT,'.N','N') first.
The column '.I' can't be grouped because it conflicts with the special .I variable. Try setnames(DT,'.I','I') first.
logical error. i is not data.table, but mult='all' and 'by'=.EACHI
Internal error: by= is missing



Internal error: byindex not the index name
Internal error: byindex not found


Unable to optimize call to mean() and could be very slow. You must name 'na.rm' like that otherwise if you do mean(x,TRUE) the TRUE is taken to mean 'trim' which is the 2nd argument of mean. 'trim' is not yet optimized.
Internal error: length(irows)!=length(o__)


The setkey() normally performed by keyby= has been skipped (as if by= was used) because := is being used together with keyby= but the keyby= contains some expressions. To avoid this warning, use by= instead, or provide existing column names to keyby=.
Internal error: jvnames is length
but ans is
and bynames is

rownames and rownames.value cannot both be used at the same time
length(rownames)==
but nrow(DT)==
. The rownames argument specifies a single column name or number. Consider rownames.value= instead.
length(rownames)==0 but should be a single column name or number, or NULL
rownames is TRUE but key has multiple columns
; taking first column x[,1] as rownames
'
' is not a column of x
as.integer(rownames)==
which is outside the column number range [1,ncol=
].
length(rownames.value)==
but should be nrow(x)==
Internal error: as.matrix.data.table length(X)==
but a dimension is zero
When i is a matrix in DT[i]<-value syntax, it doesn't make sense to provide j
j must be an atomic vector, see ?is.atomic
NA in j
j must be vector of column name or positions
Attempt to assign to column position greater than ncol(x). Create the column by name, instead. This logic intends to catch (most likely) user errors.
data.table inherits from data.frame (from v1.5), but this data.table does not. Has it been created manually (e.g. by using 'structure' rather than 'data.table') or saved to disk using a prior version of data.table?
attempting to assign invalid object to dimnames of a data.table
data.tables do not have rownames
Can't assign
colnames to a
-column data.table
'subset' must evaluate to logical
Argument 'invert' must be logical TRUE/FALSE
x argument must be a data.table
group length is 0 but data nrow > 0
passing 'f' argument together with 'by' is not allowed, use 'by' when split by column in data.table and 'f' when split by external factor
Either 'by' or 'f' argument must be supplied
Column '.ll.tech.split' is reserved for split.data.table processing
Column '.nm.tech.split' is reserved for split.data.table processing
Argument 'by' must refer to column names in x
Argument 'by' must refer only to atomic-type columns, but the following columns are non-atomic:
x is not a data.table. Shallow copy is a copy of the vector of column pointers (only), so is only meaningful for data.table
setalloccol attempting to modify `*tmp*`
Input is a length=1 logical that points to the same address as R's global value. Therefore the attribute has not been set by reference, rather on a copy. You will need to assign the result back to a variable. See issue #1281.
x is not a data.table or data.frame
x has
columns but its names are length
Passed a vector of type '
'. Needs to be type 'character'.
Can't assign
names to a
column data.table
'new' is not a character vector or a function
NA in 'new' at positions
Some duplicates exist in 'old':
'old' is type
but should be integer, double or character
'old' is length
but 'new' is length
NA (or out of bounds) in 'old' at positions
Item
of 'old' is '
' which appears several times in column names. Just the first will be changed. There are
other items in old that are also duplicated in column names.
Items of 'old' not found in column names:
. Consider skip_absent=TRUE.
Internal error: length(i)!=length(new)
x has some duplicated column name(s):
,
. Please remove or rename the duplicate(s) and try again.
Input is
but should be a plain list of items to be stacked
idcol must be a logical or character vector of length 1. If logical TRUE the id column will named '.id'.
use.names=NA invalid
use.names='check' cannot be used explicitly because the value 'check' is new in v1.12.2 and subject to change. It is just meant to convey default behavior. See ?rbindlist.
Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(\":=\").
setDF only accepts data.table, data.frame or list of equal length as input
rownames contains duplicates
rownames incorrect length; expected
names, got
rownames incorrect length; expected
names, got
All elements in argument 'x' to 'setDF' must be of same length
rownames incorrect length; expected
names, got
Cannot find symbol
Cannot convert '
' to data.table by reference because binding is locked. It is very likely that '
' resides within a package (or an environment) that is locked to prevent modifying its variable bindings. Try copying the object to your current environment, ex: var <- copy(var) and then using setDT again.
Some columns are a multi-column type (such as a matrix column):
. setDT will retain these columns as-is but subsequent operations like grouping and joining may fail. Please consider as.data.table() instead which will create a new column for each embedded column.
Column
is of POSIXlt type. Please convert it to POSIXct using as.POSIXct and run setDT again. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.
All elements in argument 'x' to 'setDT' must be of same length, but the profile of input lengths (length:frequency) is:
%s:%d
The first entry with fewer than
entries is
Argument 'x' to 'setDT' should be a 'list', 'data.frame' or 'data.table'
Item '
' not found in names of input list
'prefix' must be NULL or a character vector of length 1.
x is a single vector, non-NULL 'cols' doesn't make sense.
x is a list, 'cols' cannot be 0-length.
'prefix' must be NULL or a character vector of length 1.
x is a single vector, non-NULL 'cols' doesn't make sense.
x is a list, 'cols' cannot be 0-length.
RHS of %s is length %d which is not 1 or nrow (%d). For robustness, no recycling is allowed (other than of length 1 RHS). Consider %%in%% instead.
Internal error in .isFastSubsettable. Please report to data.table developers

'on' argument should be a named atomic vector of column names indicating which columns in 'i' should be joined with which columns in 'x'.
Found more than one operator in one 'on' statement:
. Please specify a single operator.
'on' contains no column name:
. Each 'on' clause must contain one or two column names.
'on' contains more than 2 column names:
. Each 'on' clause must contain one or two column names.
Invalid join operators %s. Only allowed operators are %s.
key argument of data.table() must be character
Object '
' not found. Perhaps you intended
Object '
' not found amongst
verbose must be logical or integer
verbose must be length 1 non-NA
Ignoring by/keyby because 'j' is not supplied
When by and keyby are both provided, keyby must be TRUE or FALSE
Argument 'by' after substitute:

When on= is provided but not i=, on= must be a named list or data.table|frame, and a natural join (i.e. join on common names) is invoked. Ignoring on= which is '
'.
i and j are both missing so ignoring the other arguments. This warning will be upgraded to error in future.
mult argument can only be 'first', 'last' or 'all'
roll must be a single TRUE, FALSE, positive/negative integer/double including +Inf and -Inf or 'nearest'
roll is '
' (type character). Only valid character value is 'nearest'.
rollends must be a logical vector
rollends must be length 1 or 2
nomatch= must be either NA or NULL (or 0 for backwards compatibility which is the same as NULL)
which= must be a logical vector length 1. Either FALSE, TRUE or NA.
which==
(meaning return row numbers) but j is also supplied. Either you need row numbers or the result of j, but only one type of result can be returned.
which=NA with nomatch=0 would always return an empty vector. Please change or remove either which or nomatch.
j must be provided when with=FALSE
Argument 'j'  after substitute:

The symbol .. is invalid. The .. prefix must be followed by at least one character.
Variable '
' is not found in calling scope. Looking in calling scope because you used the .. prefix.
Variable '..
' does exist in calling scope though, so please just removed the .. prefix from that variable name in calling scope.

Both '
' and '..
' exist in calling scope. Please remove the '..
' variable in calling scope for clarity.
Internal error:  DT[, ..var] should be dealt with by the branch above now.
Variable '
' is not found in calling scope. Looking in calling scope because you set with=FALSE. Also, please use .. symbol prefix and remove with=FALSE.
You have wrapped := with {} which is ok but then := must be the only thing inside {}. You have something else inside {} as well. Consider placing the {} on the RHS of := instead; e.g. DT[,someCol:={tmpVar1<-...;tmpVar2<-...;tmpVar1*tmpVar2}
:= with keyby is only possible when i is not supplied since you can't setkey on a subset of rows. Either change keyby to by or remove i
nomatch isn't relevant together with :=, ignoring nomatch
Argument 'i'  after substitute:

not-join '!' prefix is present on i but nomatch is provided. Please remove nomatch.
Operator := detected in i, the first argument inside DT[...], but is only valid in the second argument, j. Most often, this happens when forgetting the first comma (e.g. DT[newvar := 5] instead of DT[ , new_var := 5]). Please double-check the syntax. Run traceback(), and debugger() to get a line number.
is not found in calling scope
When the first argument inside DT[...] is a single symbol (e.g. DT[var]), data.table looks for var in calling scope.
i is invalid type (matrix). Perhaps in future a 2 column matrix could return a list of elements of DT (in the spirit of A[B] in FAQ 2.14). Please report to data.table issue tracker if you'd like this, or add your comments to FR #657.
When i is a data.table (or character vector), the columns to join by must be specified using 'on=' argument (see ?data.table), by keying x (i.e. sorted, and, marked as sorted, see ?setkey), or by sharing column names between x and i (i.e., a natural join). Keyed joins might have further speed benefits on very large data due to x being sorted in RAM.
Attempting to do natural join but no common columns in provided tables

Internal error. Cannot by=.EACHI when joining to an index, yet
Internal error. irows has length in by=.EACHI


logical error. i is not a data.table, but 'on' argument is provided.
i has evaluated to type
. Expecting logical, integer or double.
internal error: notjoin and which=NA (non-matches), huh? please provide reproducible example to issue tracker
i evaluates to a logical vector length
but there are
rows. Recycling of logical i is no longer allowed as it hides more bugs than is worth the rare convenience. Explicitly use rep(...,length=.N) if you really need to recycle.
Internal error: notjoin but byjoin or !integer or nomatch==NA

with=FALSE together with := was deprecated in v1.9.4 released Oct 2014. Please wrap the LHS of := with parentheses; e.g., DT[,(myVar):=sum(b),by=a] to assign to column name(s) held in variable myVar. See ?':=' for other examples. As warned in 2014, this is now a warning.
with=FALSE ignored, it isn't needed when using :=. See ?':=' for examples.
column(s) not removed because not found:
column(s) not found:
Item
of j is
which is outside the column number range [1,ncol=
]
j mixes positives and negatives
When with=FALSE, j-argument should be of type logical/character/integer indicating the columns to select.
by=c(...), key(...) or names(...) must evaluate to 'character'
'by' is a character vector length
but one or more items include a comma. Either pass a vector of column names (which can contain spaces, but no commas), or pass a vector length 1 containing comma separated column names. See ?data.table for other possibilities.
Internal error: irows isn't integer
'by' appears to evaluate to column names but isn't c() or key(). Use by=list(...) if you can. Otherwise, by=eval
should work. This is for efficiency so data.table can detect which columns are needed.
'by' or 'keyby' must evaluate to a vector or a list of vectors (where 'list' includes data.table and data.frame which are lists, too)
column or expression
of 'by' or 'keyby' is type
. Do not quote column names. Usage: DT[,sum(colC),by=list(colA,month(colB))]
The items in the 'by' or 'keyby' list are length(s) (%s). Each must be length %d; the same length as there are rows in x (after subsetting if i is provided).
Internal error: drop_dot passed
items
Item %d of the .() or list() passed to j is missing
j may not evaluate to the same number of columns for each group; if you're sure this warning is in error, please put the branching logic outside of [ for efficiency
Different branches of j expression produced different auto-named columns:
%s!=%s
; using the most \"last\" names
When .SDcols is a function, it is applied to each column; the output of this function must be a non-missing boolean scalar signalling inclusion/exclusion of the column. However, these conditions were not met for:
.SDcols missing at the following indices:
.SDcols is numeric but has both +ve and -ve indices
.SDcols is numeric but out of bounds [1,
] at:
.SDcols should be column numbers or names
Some items of .SDcols are not column names:
'(m)get' found in j. ansvars being set to all columns. Use .SDcols or a single j=eval(macro) instead. Both will detect the columns used which is important for efficiency.\nOld ansvars: %s
This j doesn't use .SD but .SDcols has been supplied. Ignoring .SDcols. See ?data.table.
.SD is locked. Using := in .SD's j is reserved for possible future use; a tortuously flexible way to modify by group. Use := in j directly to modify by group by reference.
In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.
In `:=`(col1=val1, col2=val2, ...) form, all arguments must be named.
LHS of := must be a symbol, or an atomic vector (column names or positions).
LHS of := appears to be column positions but are outside [1,ncol] range. New columns can only be added by name.
LHS of := isn't column names ('character') or positions ('integer' or 'numeric')
Invalid .internal.selfref detected and fixed by taking a (shallow) copy of the data.table so that := can add this new column by reference. At an earlier point, this data.table has been copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.
Cannot assign to an under-allocated recursively indexed list -- L[[i]][,:=] syntax is only valid when i is length 1, but its length is
Internal error -- item '
' not found in names of list
Internal error -- column(s) not found:
Internal error -- column(s) not found:
,
strptime() usage detected and wrapped with as.POSIXct(). This is to minimize the chance of assigning POSIXlt columns, which use 40+ bytes to store one date (versus 8 for POSIXct). Use as.POSIXct() (which will call strptime() as needed internally) to avoid this warning.
Variable '
' is not found in calling scope. Looking in calling scope because this symbol was prefixed with .. in the j= parameter.
Internal error: xcolAns does not pass checks:
,
Internal error: irows is NULL when making join result at R level. Should no longer happen now we use CsubsetDT earlier.
j (the 2nd argument inside [...]) is a single symbol but column name '
' is not found. Perhaps you intended DT[, ..
]. This difference to data.frame is deliberate and explained in FAQ 1.1.
Internal error: j has created a data.table result containing a NULL column
The column '.N' can't be grouped because it conflicts with the special .N variable. Try setnames(DT,'.N','N') first.
The column '.I' can't be grouped because it conflicts with the special .I variable. Try setnames(DT,'.I','I') first.
logical error. i is not data.table, but mult='all' and 'by'=.EACHI
Internal error: by= is missing



Internal error: byindex not the index name
Internal error: byindex not found


Unable to optimize call to mean() and could be very slow. You must name 'na.rm' like that otherwise if you do mean(x,TRUE) the TRUE is taken to mean 'trim' which is the 2nd argument of mean. 'trim' is not yet optimized.
Internal error: length(irows)!=length(o__)


The setkey() normally performed by keyby= has been skipped (as if by= was used) because := is being used together with keyby= but the keyby= contains some expressions. To avoid this warning, use by= instead, or provide existing column names to keyby=.
Internal error: jvnames is length
but ans is
and bynames is

rownames and rownames.value cannot both be used at the same time
length(rownames)==
but nrow(DT)==
. The rownames argument specifies a single column name or number. Consider rownames.value= instead.
length(rownames)==0 but should be a single column name or number, or NULL
rownames is TRUE but key has multiple columns
; taking first column x[,1] as rownames
'
' is not a column of x
as.integer(rownames)==
which is outside the column number range [1,ncol=
].
length(rownames.value)==
but should be nrow(x)==
Internal error: as.matrix.data.table length(X)==
but a dimension is zero
When i is a matrix in DT[i]<-value syntax, it doesn't make sense to provide j
j must be an atomic vector, see ?is.atomic
NA in j
j must be vector of column name or positions
Attempt to assign to column position greater than ncol(x). Create the column by name, instead. This logic intends to catch (most likely) user errors.
data.table inherits from data.frame (from v1.5), but this data.table does not. Has it been created manually (e.g. by using 'structure' rather than 'data.table') or saved to disk using a prior version of data.table?
attempting to assign invalid object to dimnames of a data.table
data.tables do not have rownames
Can't assign
colnames to a
-column data.table
'subset' must evaluate to logical
Argument 'invert' must be logical TRUE/FALSE
x argument must be a data.table
group length is 0 but data nrow > 0
passing 'f' argument together with 'by' is not allowed, use 'by' when split by column in data.table and 'f' when split by external factor
Either 'by' or 'f' argument must be supplied
Column '.ll.tech.split' is reserved for split.data.table processing
Column '.nm.tech.split' is reserved for split.data.table processing
Argument 'by' must refer to column names in x
Argument 'by' must refer only to atomic-type columns, but the following columns are non-atomic:
x is not a data.table. Shallow copy is a copy of the vector of column pointers (only), so is only meaningful for data.table
setalloccol attempting to modify `*tmp*`
Input is a length=1 logical that points to the same address as R's global value. Therefore the attribute has not been set by reference, rather on a copy. You will need to assign the result back to a variable. See issue #1281.
x is not a data.table or data.frame
x has
columns but its names are length
Passed a vector of type '
'. Needs to be type 'character'.
Can't assign
names to a
column data.table
'new' is not a character vector or a function
NA in 'new' at positions
Some duplicates exist in 'old':
'old' is type
but should be integer, double or character
'old' is length
but 'new' is length
NA (or out of bounds) in 'old' at positions
Item
of 'old' is '
' which appears several times in column names. Just the first will be changed. There are
other items in old that are also duplicated in column names.
Items of 'old' not found in column names:
. Consider skip_absent=TRUE.
Internal error: length(i)!=length(new)
x has some duplicated column name(s):
,
. Please remove or rename the duplicate(s) and try again.
Input is
but should be a plain list of items to be stacked
idcol must be a logical or character vector of length 1. If logical TRUE the id column will named '.id'.
use.names=NA invalid
use.names='check' cannot be used explicitly because the value 'check' is new in v1.12.2 and subject to change. It is just meant to convey default behavior. See ?rbindlist.
Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(\":=\").
setDF only accepts data.table, data.frame or list of equal length as input
rownames contains duplicates
rownames incorrect length; expected
names, got
rownames incorrect length; expected
names, got
All elements in argument 'x' to 'setDF' must be of same length
rownames incorrect length; expected
names, got
Cannot find symbol
Cannot convert '
' to data.table by reference because binding is locked. It is very likely that '
' resides within a package (or an environment) that is locked to prevent modifying its variable bindings. Try copying the object to your current environment, ex: var <- copy(var) and then using setDT again.
Some columns are a multi-column type (such as a matrix column):
. setDT will retain these columns as-is but subsequent operations like grouping and joining may fail. Please consider as.data.table() instead which will create a new column for each embedded column.
Column
is of POSIXlt type. Please convert it to POSIXct using as.POSIXct and run setDT again. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.
All elements in argument 'x' to 'setDT' must be of same length, but the profile of input lengths (length:frequency) is:
%s:%d
The first entry with fewer than
entries is
Argument 'x' to 'setDT' should be a 'list', 'data.frame' or 'data.table'
Item '
' not found in names of input list
'prefix' must be NULL or a character vector of length 1.
x is a single vector, non-NULL 'cols' doesn't make sense.
x is a list, 'cols' cannot be 0-length.
'prefix' must be NULL or a character vector of length 1.
x is a single vector, non-NULL 'cols' doesn't make sense.
x is a list, 'cols' cannot be 0-length.
RHS of %s is length %d which is not 1 or nrow (%d). For robustness, no recycling is allowed (other than of length 1 RHS). Consider %%in%% instead.
Internal error in .isFastSubsettable. Please report to data.table developers

'on' argument should be a named atomic vector of column names indicating which columns in 'i' should be joined with which columns in 'x'.
Found more than one operator in one 'on' statement:
. Please specify a single operator.
'on' contains no column name:
. Each 'on' clause must contain one or two column names.
'on' contains more than 2 column names:
. Each 'on' clause must contain one or two column names.
Invalid join operators %s. Only allowed operators are %s.
