'x' must be an integer
'n' must be a positive integer
Argument to 'which' must be logical
concat: 'vec' must be a character vector
concat: 'idx' must be an integer vector of length >= 0
Internal error in concat: 'idx' must take values between 1 and length(vec); 1 <= idx <= %d
Unknown 'measure.vars' type %s at index %d of list
id.vars and measure.vars are internally guessed when both are 'NULL'. All non-numeric/integer/logical type columns are considered id.vars, which in this case are columns [%s]. Consider providing at least one of 'id' or 'measure' vars in future.
Unknown 'id.vars' type %s, must be character or integer vector
One or more values in 'id.vars' is invalid.
'measure.vars' is missing. Assigning all columns other than 'id.vars' columns as 'measure.vars'.\n
Assigned 'measure.vars' are [%s].\n
Unknown 'measure.vars' type %s, must be character or integer vector/list
One or more values in 'measure.vars' is invalid.
'id.vars' is missing. Assigning all columns other than 'measure.vars' columns as 'id.vars'.\n
Assigned 'id.vars' are [%s].\n
Unknown 'id.vars' type %s, must be character or integer vector
One or more values in 'id.vars' is invalid.
Unknown 'measure.vars' type %s, must be character or integer vector
One or more values in 'measure.vars' is invalid.
When 'measure.vars' is a list, 'value.name' must be a character vector of length =1 or =length(measure.vars).
When 'measure.vars' is either not specified or a character/integer vector, 'value.name' must be a character vector of length =1.
'variable.name' must be a character/integer vector of length=1.
variable_table attribute of measure.vars should be a data table with at least one column
variable_table attribute of measure.vars should be a data table with same number of rows as max length of measure.vars vectors =%d
variable_table attribute of measure.vars should be either NULL or a data table
Internal error: combineFactorLevels in fmelt.c expects all-character input
Internal error: combineFactorLevels in fmelt.c expects a character target to factorize
'measure.vars' [%s] are not all of the same type. By order of hierarchy, the molten data value column will be of type '%s'. All measure variables not of type '%s' will be coerced too. Check DETAILS in ?melt.data.table for more on coercion.\n
Unknown column type '%s' for column '%s'.
Internal error: fmelt.c:getvarcols %d %d
%d
%d
variable_table does not support column type '%s' for column '%s'.
Unknown column type '%s' for column '%s' in 'data'
Input is not of type VECSXP, expected a data.table, data.frame or list
Argument 'value.factor' should be logical TRUE/FALSE
Argument 'variable.factor' should be logical TRUE/FALSE
Argument 'na.rm' should be logical TRUE/FALSE.
Argument 'variable.name' must be a character vector
Argument 'value.name' must be a character vector
Argument 'verbose' should be logical TRUE/FALSE
ncol(data) is 0. Nothing to melt. Returning original data.table.
names(data) is NULL. Please report to data.table-help
