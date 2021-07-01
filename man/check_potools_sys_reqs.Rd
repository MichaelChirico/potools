\name{check_potools_sys_reqs}
\alias{check_potools_sys_reqs}
\title{
Check if the proper system utilities for running package translation are installed
}
\description{
potools uses the same gettext command line tools that R itself does to run translation. These are required for translation to work properly; this function is mainly for testing use & checks whether the current environment is equipped for translation.
}
\usage{
check_potools_sys_reqs()
}
\details{

Specifically, potools relies on these command-line utilities

\enumerate{
  \item \code{msgmerge}
  \item \code{msgfmt}
  \item \code{msginit}
  \item \code{msgconv}
}

}
\value{
\code{TRUE} if the system is ready for translation, otherwise a message suggesting how to proceed.
}
\seealso{
\code{\link[tools]{update_pkg_po}}
}
\author{Michael Chirico}
