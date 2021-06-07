\name{check_untranslated_cat}
\alias{check_untranslated_cat}
\title{
Check for untranslated messages emitted by cat
}
\description{
Diagnose the R messages in a package to discover the presence of messages emitted by \code{\link{cat}} which haven't been translated (i.e., passed through \code{\link{gettext}}, \code{\link{gettextf}}, or \code{\link{ngettext}}).
}
\usage{
check_cracked_messages(message_data)
}
\arguments{
  \item{message_data}{ A \code{data.table}, or object convertible to one. }
}
\details{

The function \code{cat} is commonly used to emit messages to users (e.g., for a \code{verbose} mode), but it is not equipped for translation. Instead, messages must first be translated and then emitted. Any character literals found in the package's R code used in \code{cat} but not translated will be flagged by this function.

For flagged calls, a potential replacement is offered, built using \code{gettext} or \code{gettextf} (depending on whether one or more \code{...} arguments are supplied to \code{cat}). For the \code{gettextf} case, the suggested template is always \code{\%s} (string) since this works for all inputs; the author should tighten this to the appropriate \code{\link{sprintf}} template marker as appropriate.

NB: not all \code{cat} calls are included -- in particular, no \code{cat} call specifying a non-default \code{file} are flagged, nor are any where the supplied \code{sep} is not a character literal (e.g., \code{sep=x} instead of \code{sep=""})

}
\value{
If no issues are detected, or there are no R messages (\code{message_source == "R"}) in the package, \code{'n'} is returned; otherwise, the user is prompted to respond yes or no for whether to interrupt execution of \code{translate_package} to address the issues.
}
\seealso{
\code{\link{translate_package}}, \code{\link{update_pkg_po}}
}
\author{Michael Chirico}
