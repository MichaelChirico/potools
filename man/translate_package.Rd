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
\code{translate_package} goes through roughly three "phases" of translation.

Phase one is setup -- \code{dir} is checked for existing translations (toggling between "update" and
"new" modes), and R files are parsed and combed for user-facing messages.

Phase two is for diagnostics -- examining the usage of user-facing messages in your package and
suggesting more translation-friendly approaches as appropriate. Specifically, the following
diagnostics are performed:

\enumerate{
  \item Searching for "cracked messages". Error messages built like
    \code{stop("You gave ", n, " arguments, but ", m, " are needed.")} are in general
    hard for translators -- the correct translation may be in a totally different order
    (e.g., this is often the case for Japanese). It is preferable instead to use
    \code{\link[base]{gettextf}} to build a templated message like
    \code{stop(gettextf("You gave \%d arguments but \%d are needed.", n, m))}. Translators are then
    free to rearrange the template to put the numeric pattern where it fits most naturally in
    the target language.
  \item Searching for untranslated user-facing messages. Only strings passed through \code{base}
    functions with a \code{domain} argument is eligible for translation (see \code{\link[tools]{xgettext}}).
    However, it is not uncommon for some output may be shown to the end user via \code{\link[base]{cat}}.
    To translate such messages, it must first be passed to \code{\link[base]{gettext}} or
    \code{\link[base]{gettextf}}.
}

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
absent that, the default English message.

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
\url{https://cran.r-project.org/doc/manuals/R-admin.html#Internationalization} \cr
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
