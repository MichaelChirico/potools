library(testthat)
library(potools)

# Failed on Solaris because the command-line tools are missing there (which means tools doesn't work there), #186
if (isTRUE(check_potools_sys_reqs())) {
  test_check("potools")
}
