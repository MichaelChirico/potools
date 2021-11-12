library(withr)
library(testthat)
old = options(
  potools.use_colors = FALSE,
  width = 80L,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)
withr::local_envvar("_R_CHECK_LENGTH_1_CONDITION_" = "true")
library(potools)

# Failed on Solaris because the command-line tools are missing there (which means tools doesn't work there), #186

if (isTRUE(check_potools_sys_reqs())) {
  test_check("potools")
} else {
  writeLines("Skipping tests on system without gettext installed")
}
options(old)
