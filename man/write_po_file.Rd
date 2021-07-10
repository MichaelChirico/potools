\name{write_po_file}
\alias{write_po_file}
\alias{po_metadata}
\title{
Write a .po or .pot file corresponding to a message database
}
\description{
}
\usage{
write_po_file(
  message_data, po_file, metadata,
  width = 79L, wrap_at_newline = TRUE,
  use_base_rules = metadata$package \%chin\% .potools$base_package_names
)
po_metadata(
  package='', version='', language='',
  author='', email='',
  bugs='', copyright = NULL,
  ...
)
}
\arguments{
  \item{message_data}{ \code{data.table}, as returned from \code{\link{get_message_data}}. NB: R creates separate domains for R and C/C++ code; it is recommended you do the same by filtering the \code{get_message_data} output for \code{message_source == "R"} or \code{message_source == "src"}. Other approaches are untested. }
  \item{po_file}{ Character vector giving a destination path. Paths ending in \file{.pot} will be written with template files (e.g., \code{msgstr} entries will be blanked). }
  \item{metadata}{ A \code{po_metadata} object as returned by \code{po_metadata()}. }
  \item{width}{ Numeric governing the wrapping width of the output file. Default is \code{79L} to match the behavior of the \code{xgettext} utility. \code{Inf} turns off wrapping (except for file source markers comments). }
  \item{wrap_at_newline}{ Logical, default \code{TRUE} to match the \code{xgettext} utility's behavior. If \code{TRUE}, any \code{msgid} or \code{msgstr} will always be wrapped at an internal newline (i.e., literally matching \code{\n}). }
  \item{use_base_rules}{ Logical; Should internal behavior match base behavior as strictly as possible? \code{TRUE} if being run on a base package (i.e., \code{base} or one of the default packages like \code{utils}, \code{graphics}, etc.). See Details. }
  \item{package}{ Character; the name of the package being translated. }
  \item{version}{ Character; the version of the package being translated. }
  \item{language}{ Character; the language of the \code{msgstr}. See \code{\link{translate_package}} for details. }
  \item{author}{ Character; an author (combined with \code{email}) to whom to attribute the translations (as \code{Last-Translator}). }
  \item{email}{ Character; an e-mail address associated with \code{author}. }
  \item{bugs}{ Character; a URL where issues with the translations can be reported. }
  \item{copyright}{ An object used to construct the initial Copyright reference in the output. If \code{NULL}, no such comment is written. If a \code{list}, it should have components  }
}
\details{
details
}
\value{
value
}
\examples{
# examples
}
\references{
\url{https://www.gnu.org/software/gettext/manual/html_node/Header-Entry.html} \cr
}
\seealso{
\code{\link{translate_package}}, \code{\link[tools]{xgettext2pot}}, \code{\link[tools]{update_pkg_po}}
}
\author{Michael Chirico}
