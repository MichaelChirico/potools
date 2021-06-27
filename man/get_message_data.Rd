\name{get_message_data}
\alias{get_message_data}
\title{
Extract user-visible messages from a package
}
\description{
This function looks in the R and src directories of a package for user-visible messages and compiles them as a \code{\link[data.table]{data.table}} to facilitate analyzing this corpus as such.
}
\usage{
get_message_data(
  dir=".",
  custom_translation_functions = list(R = NULL, src = NULL),
  verbose=FALSE
)
}
\arguments{
  \item{dir}{ Character, default the present directory; a directory in which an R package is stored. }
  \item{custom_translation_functions}{ A \code{list} with either/both of two components, \code{R} and \code{src}, together governing how to extract any non-standard strings from the package. See Details in \code{\link{translate_package}}. }
  \item{verbose}{ Logical, default \code{FALSE}. Should extra information about progress, etc. be reported? }
}
\value{
A \code{data.table} with the following schema:

\enumerate{
  \item \code{message_source}, \code{character}, either \code{"R"} or \code{"src"}, saying whether the string was found in the R or the src folder of the package
  \item \code{type}, \code{character}, either \code{"singular"} or \code{"plural"}; \code{"plural"} means the string came from \code{\link{ngettext}} and can be pluralized
  \item \code{file}, \code{character}, the file where the string was found
  \item \code{msgid}, \code{character}, the string (character literal or \code{char} array as found in the source); missing for all \code{type == "plural"} strings
  \item \code{msgid_plural}, \code{list(character, character)}, the strings (character literals or \code{char} arrays as found in the source); the first applies in English for \code{n=1} (see \code{ngettext}), while the second applies for \code{n!=1}; missing for all \code{type == "singular"} strings
  \item \code{call}, \code{character}, the full call containing the string that was found
  \item \code{line_number}, \code{integer}, the line in \code{file} where the string was found
  \item \code{is_repeat}, \code{logical}, whether the \code{msgid} is a duplicate within this \code{message_source}
  \item \code{is_marked_for_translation}, \code{logical}, whether the string is marked for translation (e.g., in R, all character literals supplied to a \code{...} argument in \code{\link{stop}} are so marked)
  \item \code{is_templated}, \code{logical}, whether the string is templatable (e.g., uses \code{\%s} or other formatting markers)
}

}
\examples{
pkg <- file.path(system.file(package = 'potools'), 'pkg')
# copy to a temporary location to be able to read/write/update below
tmp_pkg <- file.path(tempdir(), "pkg")
dir.create(tmp_pkg)
file.copy(pkg, dirname(tmp_pkg), recursive = TRUE)

get_message_data(tmp_pkg)

# includes strings provided to the custom R wrapper function catf()
get_message_data(tmp_pkg, custom_translation_functions = list(R = "catf:fmt|1"))

# includes untranslated strings provided to the custom
#   C/C++ wrapper function ReverseTemplateMessage()
get_message_data(
  tmp_pkg,
  custom_translation_functions = list(src = "ReverseTemplateMessage:2")
)

# cleanup
unlink(tmp_pkg, recursive = TRUE)
rm(pkg, tmp_pkg)
}
\seealso{
\code{\link{translate_package}}
}
\author{Michael Chirico}
