\name{check_untranslated_src}
\alias{check_untranslated_src}
\title{
Check for cracked messages in C/C++ sources
}
\description{
Diagnose the C/C++ messages in a package to discover untranslated messages
}
\usage{
check_cracked_messages(message_data)
}
\arguments{
  \item{message_data}{ A \code{data.table}, or object convertible to one. }
}
\details{

This diagnostic looks for literal \code{char} arrays passed to messaging functions (as identified by \code{\link{translate_package}}) which are not marked for translation (by tagging them with the correct \code{src_translation_macro}). These strings cannot be translated until they are so marked.

}
\value{
If no issues are detected, or there are no src messages (\code{message_source == "src"}) in the package, \code{'n'} is returned; otherwise, the user is prompted to respond yes or no for whether to interrupt execution of \code{translate_package} to address the issues.
}
\seealso{
\code{\link{translate_package}}, \code{\link{update_pkg_po}}
}
\author{Michael Chirico}
