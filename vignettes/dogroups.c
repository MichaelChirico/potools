Internal error: order not integer vector
Internal error: starts not integer
Internal error: lens not integer
Internal error: jiscols not NULL but o__ has length
Internal error: xjiscols not NULL but o__ has length
'env' should be an environment
Internal error: unsupported size-0 type '%s' in column %d of 'by' should have been caught earlier
!length(bynames)[%d]==length(groups)[%d]==length(grpcols)[%d]
row.names attribute of .SD not found
row.names of .SD isn't integer length 2 with NA as first item; i.e., .set_row_names(). [%s %d %d]
length(names)!=length(SD)
Internal error: size-0 type %d in .SD column %d should have been caught earlier
Internal error: SDall %d length = %d != %d
length(xknames)!=length(xSD)
Internal error: type %d in .xSD column %d should have been caught by now
length(iSD)[%d] != length(jiscols)[%d]
length(xSD)[%d] != length(xjiscols)[%d]
j evaluates to type '%s'. Must evaluate to atomic vector or list.
All items in j=list(...) should be atomic vectors or lists. If you are trying something like j=list(.SD,newcol=mean(colA)) then use := by group instead (much quicker), or cbind or merge afterwards.
RHS of := is NULL during grouped assignment, but it's not possible to delete parts of a column.
Supplied %d items to be assigned to group %d of size %d in column '%s'. The RHS length must either be 1 (single values are ok) or match the LHS length exactly. If you wish to 'recycle' the RHS please use rep() explicitly to make this intent clear to readers of your code.
Internal error: Trying to add new column by reference but tl is full; setalloccol should have run first at R level before getting to this point in dogroups
Group %d column '%s': %s
j doesn't evaluate to the same number of columns for each group
Column %d of j's result for the first group is NULL. We rely on the column types of the first result to decide the type expected for the remaining groups (and require consistency). NULL columns are acceptable for later groups (and those are replaced with NA of appropriate type and recycled) but not for the first. Please use a typed empty vector instead, such as integer() or numeric().
j appears to be a named vector. The same names will likely be created over and over again for each group and slow things down. Try and pass a named list (which data.table optimizes) or an unnamed list() instead.\n
Column %d of j is a named vector (each item down the rows is named, somehow). Please remove those names for efficiency (to save creating them over and over for each group). They are ignored anyway.\n
The result of j is a named list. It's very inefficient to create the same names over and over again for each group. When j=list(...), any names are detected, removed and put back after grouping has completed, for efficiency. Using j=transform(), for example, prevents that speedup (consider changing to :=). This message may be upgraded to warning in future.\n
dogroups: growing from %d to %d rows\n
dogroups: length(ans)[%d]!=ngrpcols[%d]+njval[%d]
Item %d of j's result for group %d is zero length. This will be filled with %d NAs to match the longest column in this result. Later groups may have a similar problem but only the first is reported to save filling the warning buffer.
Column %d of result for group %d is type '%s' but expecting type '%s'. Column types must be consistent for each group.
Supplied %d items for column %d of group %d which has %d rows. The RHS length must either be 1 (single values are ok) or match the LHS length exactly. If you wish to 'recycle' the RHS please use rep() explicitly to make this intent clear to readers of your code.
Wrote less rows (%d) than allocated (%d).\n
Internal error: block 0 [%d] and block 1 [%d] have both run
\n  %s took %.3fs for %d groups\n
  eval(j) took %.3fs for %d calls\n
growVector passed NULL
Internal error: growVector doesn't support type '%s'
