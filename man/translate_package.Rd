\name{translate_package}
\alias{translate_package}
\title{
Interactively provide translations for a package's messages
}
\description{
This function handles the "grunt work" of building and updating translation libraries.
In addition to providing a friendly interface for supplying translations, some internal
logic is built to help make your package more translation-friendly.

To do so, it builds on low-level helpers from the \code{tools} package (which are in
turn built on lower-level \code{gettext} sytem utilities), namely \code{\link[tools]{xgettext}},
\code{\link[tools]{xngettext}}, and \code{\link[tools]{update_pkg_po}}. See Details.
}
\usage{
translate_package(
  dir='.', languages,
  diagnostics = list(check_cracked_messages, check_untranslated_cat, check_untranslated_src),
  src_translation_macros = c("_", "N_"),
  use_base_rules = package \%chin\% .potools$base_package_names,
  copyright = NULL, bugs = NULL, verbose=FALSE
)
}
\arguments{
  \item{dir}{ Character, default the present directory; a directory in which an R package is stored. }
  \item{languages}{ Character vector; locale codes to which to translate. See Details. }
  \item{diagnostics}{ A \code{list} of diagnostic functions to be run on the package's message data. See Details.}
  \item{src_translation_macros}{ Character, the macro used to indicate which \code{char} arrays are to be marked for translation in C/C++ files. The default, \code{_} and \code{N_}, is shared by R itself & recommended in R-exts and R-ints (See references). }
  \item{use_base_rules}{ Logical; Should internal behavior match base behavior as strictly as possible? \code{TRUE} if being run on a base package (i.e., \code{base} or one of the default packages like \code{utils}, \code{graphics}, etc.). See Details. }
  \item{copyright}{ Character; passed on to \code{\link[tools]{update_pkg_po}}. }
  \item{bugs}{ Character; passed on to \code{\link[tools]{update_pkg_po}}. }
  \item{verbose}{ Logical, default \code{FALSE}. Should extra information about progress, etc. be reported? }
}
\details{
\code{translate_package} goes through roughly three "phases" of translation.

Phase one is setup -- \code{dir} is checked for existing translations (toggling between "update" and
"new" modes), and R files are parsed and combed for user-facing messages.

Phase two is for diagnostics; see the Diagnostics section below. Any diagnostic detecting "unhealthy" messages
will result in a yes/no prompt to exit translation to address the issues before continuing.

Phase three is translation. All of the messages found in phase one are iterated over -- the user
is shown a message in English and prompted for the translation in the target language. This process is repeated
for each domain in \code{languages}.

An attempt is made to provide hints for some translations that require special care (e.g. that have escape
sequences or use templates). For templated messages (e.g., that use \code{\%s}), the user-provided message
must match the templates of the English message. The templates \emph{don't} have to be in the same order --
R understands template reordering, e.g. \code{\%2$s} says "interpret the second input as a string". See
\code{\link{sprintf}} for more details.

After each language is completed, a corresponding \code{.po} file is written to the package's \code{po}
directory (which is created if it does not yet exist).

There are some discrepancies in the default behavior of \code{translate_package} and the translation workflow used to generate the \code{.po}/\code{.pot} files for R itself (mainly, the suite of functions from \code{tools}, \code{\link[tools]{update_pkg_po}}, \code{\link[tools]{xgettext2pot}}, \code{\link[tools]{xgettext}}, and \code{\link[tools]{xngettext}}). They should only be superficial (e.g., whitespace or comments), but nevertheless may represent a barrier to smoothly submitting patchings to R Core. To make the process of translating base R and the default packages (\code{tools}, \code{utils}, \code{stats}, etc.) as smooth as possible, set the \code{use_base_rules} argument to \code{TRUE} and your resulting \code{.po}/\code{.pot}/\code{.mo} file will match base's.

\bold{Diagnostics:}

A diagnostic is a function which takes as input a \code{data.table} summarizing the translatable strings in
a package, evaluates whether these messages are "healthy" in some sense, and produces a digest of "unhealthy"
strings and (optionally) suggested replacements.

The diagnostic function must have an attribute named \code{diagnostic_tag} that describes what the diagnostic does; it is reproduced in the format \code{Found {nrow(result)} {diagnostic_tag}:}. For example, \code{\link{check_untranslated_cat}} has \code{diagnostic_tag = "untranslated messaging calls passed through cat()"}.

The input message data table has the following schema:

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
}

The output diagnostic result has the following schema:

\enumerate{
  \item \code{call}, \code{character}, the call identified as problematic
  \item \code{file}, \code{character}, the file where \code{call} was found
  \item \code{line_number}, \code{integer}, the line in \code{file} where \code{call} was found
  \item \code{replacement}, \code{character}, \emph{optional}, a suggested fix to make the call "healthy"
}

See \code{\link{check_cracked_messages}}, \code{\link{check_untranslated_cat}}, and \code{\link{check_untranslated_src}} for examples of diagnostics.

\bold{Domains:}

The input to \code{languages} are the locale codes described in \code{\link[base]{Sys.getlocale}}, e.g.
\code{es} for Spanish, \code{es_AR} for Argentinian Spanish, \code{ro} for Romanian, etc. See that help
file for some helpful tips about how to tell which locales are currently available on your machine, and
see the References below for some web resources listing more locales.

Note also the advice given in the R Installation and Administration manual (also cited below) -- if you
are writing Spanish translations, a typical package should use \code{language = "es"} to generate
Spanish translations for \emph{all} Spanish domains. If you want to add more regional flair to your
messaging, you can do so through supplemental \code{.po} files. For example, you can add some
Argentinian messages to \code{es_AR}; users running R in the \code{es_AR} locale will see messages
specifically written for \code{es_AR} first; absent that, the \code{es} message will be shown; and
absent that, the default message (i.e., in the language written in the source code, usually English).

Chinese is a slightly different case -- typically, the \code{zh_CN} domain is used to write
with simplified characters while \code{zh_TW} is used for traditional characters. In principal you
could leverage \code{zh_TW} for Taiwanisms and \code{zh_HK} for Hongkieisms.

Currently, translation is limited to the same set of domains as is available for base R: Danish, German,
English, British English, Spanish, Farsi, French, Italian, Japanese, Korean, Dutch, Polish,
Brazilian Portugese, Russian, Turkish, Mainland Chinese, and Taiwanese Chinese.

This list can be expanded; please file an Issue request on GitHub.
}
\value{
This function returns nothing invisibly. As a side effect, a \code{.pot} file is written to the package's
\code{po} directory (updated if one does not yet exist, or created from scratch otherwise), and a
\code{.po} file is written in the same directory for each element of \code{languages}.
}
\references{
\url{https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Internationalization} \cr
\url{https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Internationalization} \cr
\url{https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Internationalization-in-the-R-sources} \cr
\url{https://developer.r-project.org/Translations30.html} \cr
\url{https://www.isi-web.org/publications/glossary-of-statistical-terms} \cr
\url{https://www.gnu.org/software/gettext/} \cr
\url{https://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip} \cr
\url{https://saimana.com/list-of-country-locale-code}
}
\seealso{
\code{\link[tools]{xgettext}}, \code{\link[tools]{update_pkg_po}}, \code{\link[tools]{checkPoFile}},
\code{\link[base]{gettext}}
}
\author{Michael Chirico}
