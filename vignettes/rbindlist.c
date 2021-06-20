fill= should be TRUE or FALSE
use.names= should be TRUE, FALSE, or not used (\"check\" by default)
Input to rbindlist must be a list. This list can contain data.tables, data.frames or plain lists.
use.names= cannot be FALSE when fill is TRUE. Setting use.names=TRUE.
Internal error: rbindlist.c idcol is not a single string
Item %d of input is not a data.frame, data.table or list
Item %d has %d columns, inconsistent with item %d which has %d columns. To fill missing columns use fill=TRUE.
Item %d has %d columns but %d column names. Invalid object.
Column %d of item %d is length %d inconsistent with column %d which is length %d. Only length-1 columns are recycled.
Column %d ['%s'] of item %d is length 0. This (and %d other%s like it) has been filled with NA (NULL for list columns) to make each item uniform.
Total rows in the list is %<PRId64> which is larger than the maximum number of rows, currently %d
use.names=TRUE but no item of input list has any names
Failed to allocate upper bound of %<PRId64> unique column names [sum(lapply(l,ncol))]
Failed to allocate nuniq=%d items working memory in rbindlist.c
Failed to allocate ncol=%d items working memory in rbindlist.c
Internal error: usenames==NA but fill=TRUE. usenames should have been set to TRUE earlier with warning.
 use.names='check' (default from v1.12.2) emits this message and proceeds as if use.names=FALSE for  backwards compatibility. See news item 5 in v1.12.2 for options to control this message.
Internal error: could not find the first column name not present in earlier item
Column %d ['%s'] of item %d is missing in item %d. Use fill=TRUE to fill with NA (NULL for list columns), or use.names=FALSE to ignore column names.%s
Internal error: usenames==NA but an out-of-order name has been found in an item with no names or the first item. [%d]
Column %d ['%s'] of item %d appears in position %d in item %d. Set use.names=TRUE to match by column name, or use.names=FALSE to ignore column names.%s
options()$datatable.rbindlist.check is set but is not a single string. See news item 5 in v1.12.2.
options()$datatable.rbindlist.check=='%s' which is not 'message'|'warning'|'error'|'none'. See news item 5 in v1.12.2.
Column %d of item %d has type 'factor' but has no levels; i.e. malformed.
Class attribute on column %d of item %d does not match with column %d of item %d.
V%d
Internal error: column %d of result is determined to be integer64 but maxType=='%s' != REALSXP
Failed to allocate working memory for %d ordered factor levels of result column %d
Column %d of item %d is an ordered factor but level %d ['%s'] is missing from the ordered levels from column %d of item %d. Each set of ordered factor levels should be an ordered subset of the first longest. A regular factor will be created for this column.
Column %d of item %d is an ordered factor with '%s'<'%s' in its levels. But '%s'<'%s' in the ordered levels from column %d of item %d. A regular factor will be created for this column due to this ambiguity.
Failed to allocate working memory for %d factor levels of result column %d when reading item %d of item %d
Column %d of item %d: %s
