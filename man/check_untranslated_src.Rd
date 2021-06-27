\name{check_untranslated_src}
\alias{check_untranslated_src}
\title{
Check for cracked messages in C/C++ sources
}
\description{
Diagnose the C/C++ messages in a package to discover untranslated messages
}
\usage{
check_untranslated_src(message_data)
}
\arguments{
  \item{message_data}{ A \code{data.table}, or object convertible to one. }
}
\details{

This diagnostic looks for literal \code{char} arrays passed to messaging functions (as identified by \code{\link{translate_package}}) which are not marked for translation (by tagging them for translation with \code{_} or \code{N_} macros). These strings cannot be translated until they are so marked.

}
\value{
A \code{data.table} with columns \code{call}, \code{file}, \code{line_number}, and \code{replacement} summarizing the results. \code{replacement} is \code{NA} at this time, i.e., no replacement is provided.
}
\seealso{
\code{\link{translate_package}}, \code{\link{update_pkg_po}}
}
\author{Michael Chirico}
