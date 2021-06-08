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
A \code{data.table} with columns \code{call}, \code{file}, \code{line_number}, and \code{replacement} summarizing the results.
}
\seealso{
\code{\link{translate_package}}, \code{\link{update_pkg_po}}
}
\author{Michael Chirico}
