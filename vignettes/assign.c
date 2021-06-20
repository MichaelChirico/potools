Internal error: finalizer hasn't received an ExternalPtr
Internal error: finalizer's ExternalPtr doesn't see names in tag
Internal error: finalizer sees l=%d, tl=%d
.internal.selfref ptr is NULL. This is expected and normal for a data.table loaded from disk. Please remember to always setDT() immediately after loading to prevent unexpected behavior. If this table was not loaded from disk or you've already run setDT(), please report to data.table issue tracker.\n
Internal error: .internal.selfref ptr is not NULL or R_NilValue
Internal error: .internal.selfref tag isn't NULL or a character vector
Internal error: length(names)>0 but <length(dt)
alloccol has been passed a NULL dt
dt passed to alloccol isn't type VECSXP
dt passed to alloccol has no class attribute. Please report result of traceback() to data.table issue tracker.
Internal error: length of names (%d) is not length of dt (%d)
Internal error, tl of class is marked but tl<0.
Internal error, please report (including result of sessionInfo()) to data.table issue tracker: tl (%d) < l (%d) but tl of class is marked.
tl (%d) is greater than 10,000 items over-allocated (l = %d). If you didn't set the datatable.alloccol option to be very large, please report to data.table issue tracker including the result of sessionInfo().
Attempt to reduce allocation from %d to %d ignored. Can only increase allocation via shallow copy. Please do not use DT[...]<- or DT$someCol<-. Use := inside DT[...] instead.
Has getOption('datatable.alloccol') somehow become unset? It should be a number, by default 1024.
getOption('datatable.alloccol') should be a number, by default 1024. But its type is '%s'.
getOption('datatable.alloc') is a numeric vector ok but its length is %d. Its length should be 1.
getOption('datatable.alloc')==%d.  It must be >=0 and not NA.
verbose must be TRUE or FALSE
assign has been passed a NULL dt
dt passed to assign isn't type VECSXP
.SD is locked. Updating .SD by reference using := or set are reserved for future use. Use := in j directly. Or use copy(.SD) as a (slow) last resort, until shallow() is exported.
Internal error: dt passed to Cassign is not a data.table or data.frame
dt passed to assign has no names
Internal error in assign: length of names (%d) is not length of dt (%d)
data.table is NULL; malformed. A null data.table should be an empty list. typeof() should always return 'list' for data.table.
Assigning to all %d rows\n
Coerced i from numeric to integer. Please pass integer for efficiency; e.g., 2L rather than 2
i is type '%s'. Must be integer, or numeric is coerced with warning. If i is a logical subset, simply wrap with which(), and take the which() outside the loop if possible for efficiency.
i[%d] is %d which is out of range [1,nrow=%d].
Assigning to %d row subset of %d rows\n
Added %d new column%s initialized with all-NA\n
length(LHS)==0; no columns to delete or assign RHS to.
set() on a data.frame is for changing existing columns, not adding new ones. Please use a data.table for that. data.table's are over-allocated and don't shallow copy.
Coerced j from numeric to integer. Please pass integer for efficiency; e.g., 2L rather than 2
j is type '%s'. Must be integer, character, or numeric is coerced with warning.
Can't assign to the same column twice in the same query (duplicates detected).
newcolnames is supplied but isn't a character vector
RHS_list_of_columns == %s\n
RHS_list_of_columns revised to true because RHS list has 1 item which is NULL, or whose length %d is either 1 or targetlen (%d). Please unwrap RHS.\n
Supplied %d columns to be assigned an empty list (which may be an empty data.table or data.frame since they are lists too). To delete multiple columns use NULL instead. To add multiple empty list columns, use list(list()).
Recycling single RHS list item across %d columns. Please unwrap RHS.\n
Supplied %d columns to be assigned %d items. Please see NEWS for v1.12.2.
Item %d of column numbers in j is %d which is outside range [1,ncol=%d]. set() on a data.frame is for changing existing columns, not adding new ones. Please use a data.table for that.
Item %d of column numbers in j is %d which is outside range [1,ncol=%d]. Use column names instead in j to add new columns.
When deleting columns, i should not be provided
RHS of assignment to existing column '%s' is zero length but not NULL. If you intend to delete the column use NULL. Otherwise, the RHS must have length > 0; e.g., NA_integer_. If you are trying to change the column type to be an empty list column then, as with all column type changes, provide a full length RHS vector such as vector('list',nrow(DT)); i.e., 'plonk' in the new column.
Internal error in assign.c: length(newcolnames)=%d, length(names)=%d, coln=%d
Column '%s' does not exist to remove
%d column matrix RHS of := will be treated as one vector
Can't assign to column '%s' (type 'factor') a value of type '%s' (not character, factor, integer or numeric)
Supplied %d items to be assigned to %d items of column '%s'. If you wish to 'recycle' the RHS please use rep() to make this intent clear to readers of your code.
This data.table has either been loaded from disk (e.g. using readRDS()/load()) or constructed manually (e.g. using structure()). Please run setDT() or setalloccol() on it first (to pre-allocate space for new columns) before assigning by reference to it.
Internal error: oldtncol(%d) < oldncol(%d). Please report to data.table issue tracker, including result of sessionInfo().
truelength (%d) is greater than 10,000 items over-allocated (length = %d). See ?truelength. If you didn't set the datatable.alloccol option very large, please report to data.table issue tracker including the result of sessionInfo().
Internal error: DT passed to assign has not been allocated enough column slots. l=%d, tl=%d, adding %d
It appears that at some earlier point, names of this data.table have been reassigned. Please ensure to use setnames() rather than names<- or colnames<-. Otherwise, please report to data.table issue tracker.
Internal error: selfrefnames is ok but tl names [%d] != tl [%d]
Internal error: earlier error 'When deleting columns, i should not be provided' did not happen.
RHS for item %d has been duplicated because NAMED==%d MAYBE_SHARED==%d, but then is being plonked. length(values)==%d; length(cols)==%d)\n
Direct plonk of unnamed RHS, no copy. NAMED==%d, MAYBE_SHARED==%d\n
Dropping index '%s' as it doesn't have '__' at the beginning of its name. It was very likely created by v1.9.4 of data.table.\n
Internal error: index name ends with trailing __
Internal error: Couldn't allocate memory for s4.
Internal error: Couldn't allocate memory for s5.
Dropping index '%s' due to an update on a key column\n
Shortening index '%s' to '%s' due to an update on a key column\n
Dropping index '%s' due to an update on a key column\n
Internal error: %d column numbers to delete not now in strictly increasing order. No-dups were checked earlier.
Internal error memrecycle: sourceStart=%d sourceLen=%d length(source)=%d
Internal error memrecycle: start=%d len=%d length(target)=%d
Internal error: recycle length error not caught earlier. slen=%d len=%d
Internal error: memrecycle has received NULL colname
target vector
column %d named '%s'
Cannot assign 'factor' to '%s'. Factors can only be assigned to factor, character or list columns.
Assigning factor numbers to %s. But %d is outside the level range [1,%d]
Assigning factor numbers to %s. But %f is outside the level range [1,%d], or is not a whole number.
Cannot assign '%s' to 'factor'. Factor columns can be assigned factor, character, NA in any type, or level numbers.
Internal error: levels of target are either not unique or have truelength<0
Unable to allocate working memory of %d bytes to combine factor levels
Internal error: extra level check sum failed
Coercing 'character' RHS to '%s' to match the type of %s.
Cannot coerce 'list' RHS to 'integer64' to match the type of %s.
Coercing 'list' RHS to '%s' to match the type of %s.
Zero-copy coerce when assigning '%s' to '%s' %s.\n
% (type '%s') at RHS position %d  when assigning to type '%s' (%s)
type '%s' cannot be coerced to '%s'
To assign integer64 to a target of type character, please use as.character() for clarity.
Unsupported column type in assign.c:memrecycle '%s'
Internal error: writeNA passed a vector of type '%s'
Internal error: savetl_init checks failed (%d %d %p %p). please report to data.table issue tracker.
Failed to allocate initial %d items in savetl_init
Internal error: reached maximum %d items for savetl. Please report to data.table issue tracker.
Failed to realloc saveds to %d items in savetl
Failed to realloc savedtl to %d items in savetl
x must be a character vector
'which' must be an integer vector
'new' must be a character vector
'new' is length %d. Should be the same as length of 'which' (%d)
Item %d of 'which' is %d which is outside range of the length %d character vector
