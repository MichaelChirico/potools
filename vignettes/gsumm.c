env is not an environment
%s is not an integer vector
%s is not an integer vector
%s is not an integer vector
irowsArg is neither an integer vector nor NULL
length(f)=%d != length(l)=%d
o has length %d but sum(l)=%d
Internal error: o's maxgrpn attribute mismatches recalculated maxgrpn
Internal error: nrow=%d  ngrp=%d  nbit=%d  shift=%d  highSize=%d  nBatch=%d  batchSize=%d  lastBatchSize=%d\n
gforce initial population of grp took %.3f\n
Internal error: Failed to allocate counts or TMP when assigning g in gforce
gforce assign high and low took %.3f\n
gforce eval took %.3f\n
gather took ... 
gather implemented for INTSXP, REALSXP, and CPLXSXP but not '%s'
%.3fs\n
na.rm must be TRUE or FALSE
sum is not meaningful for factors.
This gsum took (narm=%s) ... 
nrow [%d] != length(x) [%d] in %s
The sum of an integer column for a group was more than type 'integer' can hold so the result has been coerced to 'numeric' automatically for convenience.
Type '%s' not supported by GForce sum (gsum). Either add the prefix base::sum(.) or turn off GForce optimization using options(datatable.optimize=1)
%.3fs\n
mean is not meaningful for factors.
na.rm must be TRUE or FALSE
This gmean took (narm=%s) ... 
nrow [%d] != length(x) [%d] in %s
Unable to allocate %d * %d bytes for non-NA counts in gmean na.rm=TRUE
Unable to allocate %d * %d bytes for non-NA counts in gmean na.rm=TRUE
Type '%s' not supported by GForce mean (gmean). Either add the prefix base::mean(.) or turn off GForce optimization using options(datatable.optimize=1)
%.3fs\n
na.rm must be TRUE or FALSE
GForce min can only be applied to columns, not .SD or similar. To find min of all items in a list such as .SD, either add the prefix base::min(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,min),by=,.SDcols=]'
min is not meaningful for factors.
nrow [%d] != length(x) [%d] in %s
No non-missing values found in at least one group. Coercing to numeric type and returning 'Inf' for such groups to be consistent with base
No non-missing values found in at least one group. Returning 'NA' for such groups to be consistent with base
No non-missing values found in at least one group. Returning 'Inf' for such groups to be consistent with base
Type 'complex' has no well-defined min
Type '%s' not supported by GForce min (gmin). Either add the prefix base::min(.) or turn off GForce optimization using options(datatable.optimize=1)
na.rm must be TRUE or FALSE
GForce max can only be applied to columns, not .SD or similar. To find max of all items in a list such as .SD, either add the prefix base::max(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,max),by=,.SDcols=]'
max is not meaningful for factors.
nrow [%d] != length(x) [%d] in %s
No non-missing values found in at least one group. Coercing to numeric type and returning 'Inf' for such groups to be consistent with base
No non-missing values found in at least one group. Returning 'NA' for such groups to be consistent with base
No non-missing values found in at least one group. Returning '-Inf' for such groups to be consistent with base
Type 'complex' has no well-defined max
Type '%s' not supported by GForce max (gmax). Either add the prefix base::max(.) or turn off GForce optimization using options(datatable.optimize=1)
na.rm must be TRUE or FALSE
GForce median can only be applied to columns, not .SD or similar. To find median of all items in a list such as .SD, either add the prefix stats::median(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,median),by=,.SDcols=]'
median is not meaningful for factors.
nrow [%d] != length(x) [%d] in %s
Type '%s' not supported by GForce median (gmedian). Either add the prefix stats::median(.) or turn off GForce optimization using options(datatable.optimize=1)
nrow [%d] != length(x) [%d] in %s
Type '%s' not supported by GForce tail (gtail). Either add the prefix utils::tail(.) or turn off GForce optimization using options(datatable.optimize=1)
nrow [%d] != length(x) [%d] in %s
Type '%s' not supported by GForce head (ghead). Either add the prefix utils::head(.) or turn off GForce optimization using options(datatable.optimize=1)
Internal error, gtail is only implemented for n=1. This should have been caught before. please report to data.table issue tracker.
Internal error, ghead is only implemented for n=1. This should have been caught before. please report to data.table issue tracker.
Internal error, `g[` (gnthvalue) is only implemented single value subsets with positive index, e.g., .SD[2]. This should have been caught before. please report to data.table issue tracker.
nrow [%d] != length(x) [%d] in %s
Type '%s' not supported by GForce subset `[` (gnthvalue). Either add the prefix utils::head(.) or turn off GForce optimization using options(datatable.optimize=1)
na.rm must be TRUE or FALSE
GForce var/sd can only be applied to columns, not .SD or similar. For the full covariance matrix of all items in a list such as .SD, either add the prefix stats::var(.SD) (or stats::sd(.SD)) or turn off GForce optimization using options(datatable.optimize=1). Alternatively, if you only need the diagonal elements, 'DT[,lapply(.SD,var),by=,.SDcols=]' is the optimized way to do this.
var/sd is not meaningful for factors.
nrow [%d] != length(x) [%d] in %s
Type '%s' not supported by GForce var (gvar). Either add the prefix stats::var(.) or turn off GForce optimization using options(datatable.optimize=1)
Type '%s' not supported by GForce sd (gsd). Either add the prefix stats::sd(.) or turn off GForce optimization using options(datatable.optimize=1)
na.rm must be TRUE or FALSE
GForce prod can only be applied to columns, not .SD or similar. To multiply all items in a list such as .SD, either add the prefix base::prod(.SD) or turn off GForce optimization using options(datatable.optimize=1). More likely, you may be looking for 'DT[,lapply(.SD,prod),by=,.SDcols=]'
prod is not meaningful for factors.
nrow [%d] != length(x) [%d] in %s
Unable to allocate %d * %d bytes for gprod
Type '%s' not supported by GForce prod (gprod). Either add the prefix base::prod(.) or turn off GForce optimization using options(datatable.optimize=1)
