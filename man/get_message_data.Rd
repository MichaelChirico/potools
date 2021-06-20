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
  dir=".", src_translation_macros=c("_", "N_"),
  use_base_rules=package \%chin\% .potools$base_package_names,
  verbose=FALSE
)
}
\arguments{
  \item{dir}{ Character, default the present directory; a directory in which an R package is stored. }
  \item{src_translation_macros}{ Character, the macro used to indicate which \code{char} arrays are to be marked for translation in C/C++ files. The default, \code{_} and \code{N_}, is shared by R itself & recommended in R-exts and R-ints (See references). }
  \item{use_base_rules}{ Logical; Should internal behavior match base behavior as strictly as possible? \code{TRUE} if being run on a base package (i.e., \code{base} or one of the default packages like \code{utils}, \code{graphics}, etc.). See Details. }
  \item{verbose}{ Logical, default \code{FALSE}. Should extra information about progress, etc. be reported? }
}
\details{
For this function, the role of \code{use_base_rules} is to dictate where to look for source files in the \file{src} directory. When \code{use_base_rules = TRUE}, only files in \file{src}, \file{src/unix}, and \file{src/windows} are included. Otherwise, \file{src/cairo} is included as well.
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
\seealso{
\code{\link{translate_package}}
}
\author{Michael Chirico}
