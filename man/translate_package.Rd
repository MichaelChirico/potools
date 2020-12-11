\name{translate_package}
\alias{translate_package}
\title{
Interactively provide translations for a package's messages
}
\description{
This function handles the "grunt work" of build and updating translation libraries.
In addition to providing a friendly interface for supplying translations, some internal
logic is built to help make your package more translation-friendly. For example,
error messages built like \code{stop("You gave ", n, " arguments, but ", m, " are needed.")}
are in general hard for translators -- the correct translation may be in a totally different
order (e.g., this is often the case for Japanese). It is preferable instead to use
\code{\link{gettextf}} to build a templated message like
\code{stop(gettextf("You gave \%d arguments but \%d are needed.", n, m))}. Translators are then
free to rearrange the template to put the numeric pattern where it fits most naturally in
the target language.

To do so, it builds on low-level helpers from the \code{tools} package (which are in
turn built on lower-level \code{gettext} sytem utilities), namely \code{\link[tools]{xgettext}},
\code{\link[tools]{xngettext}}, and \code{\link[tools]{update_pkg_po}}.
}
\usage{
translate_package(
  dir='.', languages,
  copyright, bugs, verbose=FALSE
)
}
\arguments{
  \item{dir}{ Character, default the present directory; a directory in which an R package is stored. }
  \item{languages}{ Character vector; locale codes to which to translate. See Details. }
  \item{copyright}{ Character; passed on to \code{\link[tools]{update_pkg_po}}. }
  \item{bugs}{ Character; passed on to \code{\link[tools]{update_pkg_po}}. }
  \item{verbose}{ Logical, default \code{FALSE}. Should extra information about progress, etc. be reported? }
}
\details{
About locales...
}
\value{
Produces...
}
\references{
WRE, R-Admin, Translations sites...
}
\author{Michael Chirico}
