Using '
' as value column. Use 'value.var' to override
The dcast generic in data.table has been passed a
, but data.table::dcast currently only has a method for data.tables. Please confirm your input is a data.table, with setDT(
) or as.data.table(
). If you intend to use a reshape2::dcast, try installing that package first, but do note that reshape2 is superseded and is no longer actively developed.
The dcast generic in data.table has been passed a
and will attempt to redirect to the reshape2::dcast; please note that reshape2 is superseded and is no longer actively developed, and this redirection is now deprecated. Please do this redirection yourself like reshape2::dcast(
). In the next version, this warning will become an error.
Invalid formula. Cast formula should be of the form LHS ~ RHS, for e.g., a + b ~ c.
data.table to cast must have unique column names
value.var values
are not found in 'data'.
When 'fun.aggregate' and 'value.var' are both lists, 'value.var' must be either of length =1 or =length(fun.aggregate).
'data' must be a data.table.
'drop' must be logical TRUE/FALSE
Column [
] not found or of unknown type.
Columns specified in formula can not be of type list
Can not cast an empty data.table
Aggregate function missing, defaulting to 'length'
Internal error -- empty rhsnames in dcast; please report
Using '
' as value column. Use 'value.var' to override
The dcast generic in data.table has been passed a
, but data.table::dcast currently only has a method for data.tables. Please confirm your input is a data.table, with setDT(
) or as.data.table(
). If you intend to use a reshape2::dcast, try installing that package first, but do note that reshape2 is superseded and is no longer actively developed.
The dcast generic in data.table has been passed a
and will attempt to redirect to the reshape2::dcast; please note that reshape2 is superseded and is no longer actively developed, and this redirection is now deprecated. Please do this redirection yourself like reshape2::dcast(
). In the next version, this warning will become an error.
Invalid formula. Cast formula should be of the form LHS ~ RHS, for e.g., a + b ~ c.
data.table to cast must have unique column names
value.var values
are not found in 'data'.
When 'fun.aggregate' and 'value.var' are both lists, 'value.var' must be either of length =1 or =length(fun.aggregate).
'data' must be a data.table.
'drop' must be logical TRUE/FALSE
Column [
] not found or of unknown type.
Columns specified in formula can not be of type list
Can not cast an empty data.table
Aggregate function missing, defaulting to 'length'
Internal error -- empty rhsnames in dcast; please report
