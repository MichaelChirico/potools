\name{translate_package}
\alias{translate_package}
\title{
Interactively provide translations for a package's messages
}
\description{
This function handles the "grunt work" of building and updating translation libraries.
In addition to providing a friendly interface for supplying translations, some internal
logic is built to help make your package more translation-friendly.

To do so, it builds on low-level command line tools from \code{gettext}. See Details.
}
\usage{
translate_package(
  dir='.', languages,
  diagnostics = list(
    check_cracked_messages,
    check_untranslated_cat,
    check_untranslated_src
  ),
  custom_translation_functions = list(R = NULL, src = NULL),
  use_base_rules = package \%chin\% .potools$base_package_names,
  copyright = NULL, bugs = NULL, verbose=FALSE
)
}
\arguments{
  \item{dir}{ Character, default the present directory; a directory in which an R package is stored. }
  \item{languages}{ Character vector; locale codes to which to translate. See Details. }
  \item{diagnostics}{ A \code{list} of diagnostic functions to be run on the package's message data. See Details. }
  \item{custom_translation_functions}{ A \code{list} with either/both of two components, \code{R} and \code{src}, together governing how to extract any non-standard strings from the package. See Details. }
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

After each language is completed, a corresponding \file{.po} file is written to the package's \file{po}
directory (which is created if it does not yet exist).

There are some discrepancies in the default behavior of \code{translate_package} and the translation workflow used to generate the \file{.po}/\file{.pot} files for R itself (mainly, the suite of functions from \code{tools}, \code{\link[tools]{update_pkg_po}}, \code{\link[tools]{xgettext2pot}}, \code{\link[tools]{xgettext}}, and \code{\link[tools]{xngettext}}). They should only be superficial (e.g., whitespace or comments), but nevertheless may represent a barrier to smoothly submitting patchings to R Core. To make the process of translating base R and the default packages (\code{tools}, \code{utils}, \code{stats}, etc.) as smooth as possible, set the \code{use_base_rules} argument to \code{TRUE} and your resulting \file{.po}/\file{.pot}/\file{.mo} file will match base's.

\bold{Custom translation functions:}

Some package developers may want to write their own messaging interface, or to use wrappers around the base interface (i.e., \code{stop}, \code{warning}, \code{message}, and a few others) which won't be detected by default (e.g. with \code{\link[tools]{update_pkg_po}}).

In such cases, use the \code{custom_translation_functions} argument, whose interface is inspired by the \code{--keyword} argument to the \code{xgettext} command-line tool. This argument consists of a list with two components, \code{R} and \code{src} (either can be excluded), owing to differences between R and C/C++. Both components, if present, should consist of a character vector.

For R, there are two types of input: one for named arguments, the other for unnamed arguments.

Entries for named arguments will look like \code{"fname:arg|num"} (singular string) or \code{"fname:arg1|num1,arg2|num2"} (plural string). \code{fname} gives the name of the function/call to be extracted from the R source, \code{arg}/\code{arg1}/\code{arg2} specify the name of the argument to \code{fname} from which strings should be extracted, and \code{num}/\code{num1}/\code{num2} specify the \emph{order} of the named argument within the signature of \code{fname}.

Entries for unnamed arguments will look like \code{"fname:...\\xarg1,...,xargn"}. All strings within calls to \code{fname} \emph{except} those supplied to the arguments named among \code{xarg1}, ..., \code{xargn} will be extracted.

To clarify, consider the how we would (redundantly) specify \code{custom_translation_functions} for some of the default messagers, \code{gettext}, \code{gettextf}, and \code{ngettext}: \code{custom_translation_functions = list(R = c("gettext:...\\domain", "gettextf:fmt|1", "ngettext:msg1|2,msg2|3"))}.

For src, there is only one type of input, which looks like \code{"fname:num"}, which says to look at the \code{num} argument of calls to \code{fname} for \code{char} arrays.

Note that there is a difference in how translation works for src vs. R -- in R, all strings passed to certain functions are considered marked for translations, but in src, all translatable strings must be explicitly marked as such. So for \code{src} translations, \code{custom_translation_functions} is not used to customize which strings are marked for translation, but rather, to expand the set of calls which are searched for potentially \emph{untranslated} arrays (i.e., arrays passed to the specified calls that are not explicitly marked for translation). These can then be reported in the \code{\link{check_untranslated_src}} diagnostic, for example.

\bold{Diagnostics:}

A diagnostic is a function which takes as input a \code{data.table} summarizing the translatable strings in
a package (e.g. as generated by \code{\link{get_message_data}}), evaluates whether these messages are "healthy" in some sense, and produces a digest of "unhealthy"
strings and (optionally) suggested replacements.

The diagnostic function must have an attribute named \code{diagnostic_tag} that describes what the diagnostic does; it is reproduced in the format \code{Found {nrow(result)} {diagnostic_tag}:}. For example, \code{\link{check_untranslated_cat}} has \code{diagnostic_tag = "untranslated messaging calls passed through cat()"}.

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
This function returns nothing invisibly. As a side effect, a \file{.pot} file is written to the package's
\file{po} directory (updated if one does not yet exist, or created from scratch otherwise), and a
\file{.po} file is written in the same directory for each element of \code{languages}.
}
\examples{
pkg <- file.path(system.file(package = 'potools'), 'pkg')
# copy to a temporary location to be able to read/write/update below
tmp_pkg <- file.path(tempdir(), "pkg")
dir.create(tmp_pkg)
file.copy(pkg, dirname(tmp_pkg), recursive = TRUE)

# run translate_package() without any languages
# this will generate a .pot template file and en@quot translations (in UTF-8 locales)
# we can also pass empty 'diagnostics' to skip the diagnostic step
translate_package(tmp_pkg, diagnostics = NULL)

\dontrun{
# launches the interactive translation dialog for translations into Estonian:
translate_package(tmp_pkg, "et_EE", diagnostics = NULL, verbose = TRUE)
}

# cleanup
unlink(tmp_pkg, recursive = TRUE)
rm(pkg, tmp_pkg)
}
\references{
\url{https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Internationalization} \cr
\url{https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Internationalization} \cr
\url{https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Internationalization-in-the-R-sources} \cr
\url{https://developer.r-project.org/Translations30.html} \cr
\url{https://www.isi-web.org/publications/glossary-of-statistical-terms} \cr
\url{https://www.gnu.org/software/gettext/} \cr
\url{https://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip} \cr
\url{https://saimana.com/list-of-country-locale-code/}
}
\seealso{
\code{\link[tools]{xgettext}}, \code{\link[tools]{update_pkg_po}}, \code{\link[tools]{checkPoFile}},
\code{\link[base]{gettext}}
}
\author{Michael Chirico}
