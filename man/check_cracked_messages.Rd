\name{check_cracked_messages}
\alias{check_cracked_messages}
\title{
Check for cracked messages more suitable for templating
}
\description{
Diagnose the R messages in a package to discover the presence of "cracked" messages better served for translation by templating. See Details.
}
\usage{
check_cracked_messages(message_data)
}
\arguments{
  \item{message_data}{ A \code{data.table}, or object convertible to one. }
}
\details{

Error messages built like \code{stop("You gave ", n, " arguments, but ", m, " are needed.")} are in general hard for translators -- the correct translation may be in a totally different order (e.g., this is often the case for Japanese). It is preferable instead to use \code{\link[base]{gettextf}} to build a templated message like \code{stop(gettextf("You gave \%d arguments but \%d are needed.", n, m))}. Translators are then free to rearrange the template to put the numeric pattern where it fits most naturally in the target language.

}
\value{
If no issues are detected, or there are no R messages (\code{message_source == "R"}) in the package, \code{'n'} is returned; otherwise, the user is prompted to respond yes or no for whether to interrupt execution of \code{translate_package} to address the issues.
}
\seealso{
\code{\link{translate_package}}, \code{\link{update_pkg_po}}
}
\author{Michael Chirico}
