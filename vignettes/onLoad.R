The option 'datatable.nomatch' is being used and is not set to the default NA. This option is still honored for now but will be deprecated in future. Please see NEWS for 1.12.4 for detailed information and motivation. To specify inner join, please specify `nomatch=NULL` explicitly in your calls rather than changing the default using this option.
The datatable.
version (
) does not match the package (
). Please close all R sessions to release the old
and reinstall data.table in a fresh R session. The root cause is that R's package installer can in some unconfirmed circumstances leave a package in a state that is apparently functional but where new R code is calling old C code silently: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17478. Once a package is in this mismatch state it may produce wrong results silently until you next upgrade the package. Please help by adding precise circumstances to 17478 to move the status to confirmed. This mismatch between R and C code can happen with any package not just data.table. It is just that data.table has added this check.
This is R
but data.table has been installed using R
. The major version must match. Please reinstall data.table.
Option 'datatable.old.bywithoutby' has been removed as warned for 2 years. It is now ignored. Please use by=.EACHI instead and stop using this option.
Option 'datatable.old.unique.by.key' has been removed as warned for 4 years. It is now ignored. Please use by=key(DT) instead and stop using this option.
Unexpected base R behaviour: list(x) has copied x
Unexpected base R behaviour: names<- has copied column contents
Unexpected base R behaviour: DF[2,2]<- did not copy column 2 which was assigned to
Unexpected base R behaviour: DF[2,2]<- copied the first column which was not assigned to, too
Unexpected base R behaviour: DF[2,2]<- has not copied address(DF)
Reminder to data.table developers: don't use getRversion() internally. Add a behaviour test to .onLoad instead.
The option 'datatable.nomatch' is being used and is not set to the default NA. This option is still honored for now but will be deprecated in future. Please see NEWS for 1.12.4 for detailed information and motivation. To specify inner join, please specify `nomatch=NULL` explicitly in your calls rather than changing the default using this option.
The datatable.
version (
) does not match the package (
). Please close all R sessions to release the old
and reinstall data.table in a fresh R session. The root cause is that R's package installer can in some unconfirmed circumstances leave a package in a state that is apparently functional but where new R code is calling old C code silently: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17478. Once a package is in this mismatch state it may produce wrong results silently until you next upgrade the package. Please help by adding precise circumstances to 17478 to move the status to confirmed. This mismatch between R and C code can happen with any package not just data.table. It is just that data.table has added this check.
This is R
but data.table has been installed using R
. The major version must match. Please reinstall data.table.
Option 'datatable.old.bywithoutby' has been removed as warned for 2 years. It is now ignored. Please use by=.EACHI instead and stop using this option.
Option 'datatable.old.unique.by.key' has been removed as warned for 4 years. It is now ignored. Please use by=key(DT) instead and stop using this option.
Unexpected base R behaviour: list(x) has copied x
Unexpected base R behaviour: names<- has copied column contents
Unexpected base R behaviour: DF[2,2]<- did not copy column 2 which was assigned to
Unexpected base R behaviour: DF[2,2]<- copied the first column which was not assigned to, too
Unexpected base R behaviour: DF[2,2]<- has not copied address(DF)
Reminder to data.table developers: don't use getRversion() internally. Add a behaviour test to .onLoad instead.
