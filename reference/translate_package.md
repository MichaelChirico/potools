# Interactively provide translations for a package's messages

This function handles the "grunt work" of building and updating
translation libraries. In addition to providing a friendly interface for
supplying translations, some internal logic is built to help make your
package more translation-friendly.

To get started, the package developer should run `translate_package()`
on your package's source to produce a template `.pot` file (or files, if
your package has both R and C/C++ messages to translated), e.g.

To add translations in your desired language, include the target
language: in the `translate_package(languages = "es")` call.

## Usage

``` r
translate_package(
  dir = ".",
  languages = NULL,
  diagnostics = list(check_cracked_messages, check_untranslated_cat,
    check_untranslated_src),
  custom_translation_functions = list(R = NULL, src = NULL),
  max_translations = Inf,
  use_base_rules = package %chin% .potools$base_package_names,
  copyright = NULL,
  bugs = "",
  verbose = !is_testing()
)
```

## Arguments

- dir:

  Character, default the present directory; a directory in which an R
  package is stored.

- languages:

  Character vector; locale codes to which to translate. Must be a valid
  language accepted by gettext. This almost always takes the form of (1)
  an ISO 639 2-letter language code; or (2) `ll_CC`, where `ll` is an
  ISO 639 2-letter language code and `CC` is an ISO 3166 2-letter
  country code e.g. `es` for Spanish, `es_AR` for Argentinian Spanish,
  `ro` for Romanian, etc. See
  [`base::Sys.getlocale()`](https://rdrr.io/r/base/locales.html) for
  some helpful tips about how to tell which locales are currently
  available on your machine, and see the References below for some web
  resources listing more locales.

- diagnostics:

  A `list` of diagnostic functions to be run on the package's message
  data. See Details.

- custom_translation_functions:

  A `list` with either/both of two components, `R` and `src`, together
  governing how to extract any non-standard strings from the package.
  See Details.

- max_translations:

  Numeric; used for setting a cap on the number of translations to be
  done for each language. Defaults to `Inf`, meaning all messages in the
  package.

- use_base_rules:

  Logical; Should internal behavior match base behavior as strictly as
  possible? `TRUE` if being run on a base package (i.e., `base` or one
  of the default packages like `utils`, `graphics`, etc.). See Details.

- copyright:

  Character; passed on to
  [`write_po_file()`](https://michaelchirico.github.io/potools/reference/write_po_file.md).

- bugs:

  Character; passed on to
  [`write_po_file()`](https://michaelchirico.github.io/potools/reference/write_po_file.md).

- verbose:

  Logical, default `TRUE` (except during testing). Should extra
  information about progress, etc. be reported?

## Value

This function returns nothing invisibly. As a side effect, a `.pot` file
is written to the package's `po` directory (updated if one does not yet
exist, or created from scratch otherwise), and a `.po` file is written
in the same directory for each element of `languages`.

## Phases

`translate_package()` goes through roughly three "phases" of
translation.

1.  Setup – `dir` is checked for existing translations (toggling between
    "update" and "new" modes), and R files are parsed and combed for
    user-facing messages.

2.  Diagnostics: see the Diagnostics section below. Any diagnostic
    detecting "unhealthy" messages will result in a yes/no prompt to
    exit translation to address the issues before continuing.

3.  Translation. All of the messages found in phase one are iterated
    over – the user is shown a message in English and prompted for the
    translation in the target language. This process is repeated for
    each domain in `languages`.

An attempt is made to provide hints for some translations that require
special care (e.g. that have escape sequences or use templates). For
templated messages (e.g., that use `%s`), the user-provided message must
match the templates of the English message. The templates *don't* have
to be in the same order – R understands template reordering, e.g. `%2$s`
says "interpret the second input as a string". See
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) for more details.

After each language is completed, a corresponding `.po` file is written
to the package's `po` directory (which is created if it does not yet
exist).

There are some discrepancies in the default behavior of
`translate_package` and the translation workflow used to generate the
`.po`/`.pot` files for R itself (mainly, the suite of functions from
`tools`,
[`tools::update_pkg_po()`](https://rdrr.io/r/tools/update_pkg_po.html),
[`tools::xgettext2pot()`](https://rdrr.io/r/tools/xgettext.html),
[`tools::xgettext()`](https://rdrr.io/r/tools/xgettext.html), and
[`tools::xngettext()`](https://rdrr.io/r/tools/xgettext.html)). They
should only be superficial (e.g., whitespace or comments), but
nevertheless may represent a barrier to smoothly submitting patchings to
R Core. To make the process of translating base R and the default
packages (`tools`, `utils`, `stats`, etc.) as smooth as possible, set
the `use_base_rules` argument to `TRUE` and your resulting
`.po`/`.pot`/`.mo` file will match base's.

## Custom translation functions

`base` R provides several functions for messaging that are natively
equipped for translation (they all have a `domain` argument):
[`stop()`](https://rdrr.io/r/base/stop.html),
[`warning()`](https://rdrr.io/r/base/warning.html),
[`message()`](https://rdrr.io/r/base/message.html),
[`gettext()`](https://rdrr.io/r/base/gettext.html),
[`gettextf()`](https://rdrr.io/r/base/sprintf.html),
[`ngettext()`](https://rdrr.io/r/base/gettext.html), and
[`packageStartupMessage()`](https://rdrr.io/r/base/message.html).

While handy, some developers may prefer to write their own functions, or
to write wrappers of the provided functions that provide some enhanced
functionality (e.g., templating or automatic wrapping). In this case,
the default R tooling for translation (`xgettext()`, `xngettext()`
`xgettext2pot()`, `update_pkg_po()` from `tools`) will not work, but
`translate_package()` and its workhorse
[`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md)
provide an interface to continue building translations for your
workflow.

Suppose you wrote a function `stopf()` that is a wrapper of
`stop(gettextf())` used to build templated error messages in R, which
makes translation easier for translators (see below), e.g.:

    stopf = function(fmt, ..., domain = NULL) {
      stop(gettextf(fmt, ...), domain = domain, call. = FALSE)
    }

Note that `potools` itself uses just such a wrapper internally to build
error messages! To extract strings from calls in your package to
`stopf()` and mark them for translation, use the argument
`custom_translation_functions`:

    get_message_data(
      '/path/to/my_package',
      custom_translation_functions = list(R = 'stopf:fmt|1')
    )

This invocation tells
[`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md)
to look for strings in the `fmt` argument in calls to `stopf()`. `1`
indicates that `fmt` is the first argument.

This interface is inspired by the `--keyword` argument to the `xgettext`
command-line tool. This argument consists of a list with two components,
`R` and `src` (either can be excluded), owing to differences between R
and C/C++. Both components, if present, should consist of a character
vector.

For R, there are two types of input: one for named arguments, the other
for unnamed arguments.

- Entries for **named** arguments will look like `"fname:arg|num"`
  (singular string) or `"fname:arg1|num1,arg2|num2"` (plural string).
  `fname` gives the name of the function/call to be extracted from the R
  source, `arg`/`arg1`/`arg2` specify the name of the argument to
  `fname` from which strings should be extracted, and
  `num`/`num1`/`num2` specify the *order* of the named argument within
  the signature of `fname`.

- Entries for **unnamed** arguments will look like
  `"fname:...\xarg1,...,xargn"`, i.e., `fname`, followed by `:`,
  followed by `...` (three dots), followed by a backslash (`\`),
  followed by a comma-separated list of argument names. All strings
  within calls to `fname` *except* those supplied to the arguments named
  among `xarg1`, ..., `xargn` will be extracted.

To clarify, consider the how we would (redundantly) specify
`custom_translation_functions` for some of the default messagers,
`gettext`, `gettextf`, and `ngettext`:
`custom_translation_functions = list(R = c("gettext:...\domain", "gettextf:fmt|1", "ngettext:msg1|2,msg2|3"))`.

For src, there is only one type of input, which looks like
`"fname:num"`, which says to look at the `num` argument of calls to
`fname` for `char` arrays.

Note that there is a difference in how translation works for src vs. R –
in R, all strings passed to certain functions are considered marked for
translations, but in src, all translatable strings must be explicitly
marked as such. So for `src` translations,
`custom_translation_functions` is not used to customize which strings
are marked for translation, but rather, to expand the set of calls which
are searched for potentially *untranslated* arrays (i.e., arrays passed
to the specified calls that are not explicitly marked for translation).
These can then be reported in the
[`check_untranslated_src()`](https://michaelchirico.github.io/potools/reference/check_untranslated_src.md)
diagnostic, for example.

## Diagnostics

### Cracked messages

A cracked message is one like:

    stop("There are ", n, " good things and ", m, " bad things.")

In its current state, translators will be asked to translate three
messages independently:

- "There are"

- "good things and"

- "bad things."

The message has been cracked; it might not be possible to translate a
string as generic as "There are" into many languages – context is key!

To keep the context, the error message should instead be build with
`gettextf` like so:

    stop(domain=NA, gettextf("There are %d good things and %d bad things."))

Now there is only one string to translate! Note that this also allows
the translator to change the word order as they see fit – for example,
in Japanese, the grammatical order usually puts the verb last (where in
English it usually comes right after the subject).

`translate_package` detects such cracked messages and suggests a
`gettextf`-based approach to fix them.

### Untranslated R messages produced by [`cat()`](https://rdrr.io/r/base/cat.html)

Only strings which are passed to certain `base` functions are eligible
for translation, namely `stop`, `warning`, `message`,
`packageStartupMessage`, `gettext`, `gettextf`, and `ngettext` (all of
which have a `domain` argument that is key for translation).

However, it is common to also produce some user-facing messages using
`cat` – if your package does so, it must first use `gettext` or
`gettextf` to translate the message before sending it to the user with
`cat`.

`translate_package` detects strings produced with `cat` and suggests a
`gettext`- or `gettextf`-based fix.

### Untranslated C/C++ messages

This diagnostic detects any literal `char` arrays provided to common
messaging functions in C/C++, namely
[`ngettext()`](https://rdrr.io/r/base/gettext.html), `Rprintf()`,
`REprintf()`, `Rvprintf()`, `REvprintf()`, `R_ShowMessage()`,
`R_Suicide()`, [`warning()`](https://rdrr.io/r/base/warning.html),
`Rf_warning()`, `error()`, `Rf_error()`, `dgettext()`, and `snprintf()`.
To actually translate these strings, pass them through the translation
macro `_`.

NB: Translation in C/C++ requires some additional `#include`s and
declarations, including defining the `_` macro. See the
Internationalization section of Writing R Extensions for details.

## Custom diagnostics

A diagnostic is a function which takes as input a `data.table`
summarizing the translatable strings in a package (e.g. as generated by
[`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md)),
evaluates whether these messages are "healthy" in some sense, and
produces a digest of "unhealthy" strings and (optionally) suggested
replacements.

The diagnostic function must have an attribute named `diagnostic_tag`
that describes what the diagnostic does; it is reproduced in the format
`Found {nrow(result)} {diagnostic_tag}:`. For example,
[`check_untranslated_cat()`](https://michaelchirico.github.io/potools/reference/check_untranslated_cat.md)
has
`diagnostic_tag = "untranslated messaging calls passed through cat()"`.

The output diagnostic result has the following schema:

- `call`: `character`, the call identified as problematic

- `file`: `character`, the file where `call` was found

- `line_number`: `integer`, the line in `file` where `call` was found

- `replacement`: `character`, *optional*, a suggested fix to make the
  call "healthy"

See
[`check_cracked_messages()`](https://michaelchirico.github.io/potools/reference/check_cracked_messages.md),
[`check_untranslated_cat()`](https://michaelchirico.github.io/potools/reference/check_untranslated_cat.md),
and
[`check_untranslated_src()`](https://michaelchirico.github.io/potools/reference/check_untranslated_src.md)
for examples of diagnostics.

## References

<https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Internationalization>  
<https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Internationalization>  
<https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Internationalization-in-the-R-sources>  
<https://developer.r-project.org/Translations30.html>  
<https://isi-web.org/glossary>  
<https://www.gnu.org/software/gettext/>  
<https://www.gnu.org/software/gettext/manual/html_node/Usual-Language-Codes.html#Usual-Language-Codes>  
<https://www.gnu.org/software/gettext/manual/html_node/Country-Codes.html#Country-Codes>  
<https://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip>  
<https://saimana.com/list-of-country-locale-code/>

## See also

[`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md),
[`write_po_file()`](https://michaelchirico.github.io/potools/reference/write_po_file.md),
[`tools::xgettext()`](https://rdrr.io/r/tools/xgettext.html),
[`tools::update_pkg_po()`](https://rdrr.io/r/tools/update_pkg_po.html),
[`tools::checkPoFile()`](https://rdrr.io/r/tools/checkPoFiles.html),
[`base::gettext()`](https://rdrr.io/r/base/gettext.html)

## Author

Michael Chirico

## Examples

``` r
pkg <- system.file('pkg', package = 'potools')
# copy to a temporary location to be able to read/write/update below
tmp_pkg <- file.path(tempdir(), "pkg")
dir.create(tmp_pkg)
file.copy(pkg, dirname(tmp_pkg), recursive = TRUE)
#> [1] TRUE

# run translate_package() without any languages
# this will generate a .pot template file and en@quot translations (in UTF-8 locales)
# we can also pass empty 'diagnostics' to skip the diagnostic step
# (skip if gettext isn't available to avoid an error)
if (isTRUE(check_potools_sys_reqs)) {
  translate_package(tmp_pkg, diagnostics = NULL)
}

if (FALSE) { # \dontrun{
# launches the interactive translation dialog for translations into Estonian:
translate_package(tmp_pkg, "et_EE", diagnostics = NULL, verbose = TRUE)
} # }

# cleanup
unlink(tmp_pkg, recursive = TRUE)
rm(pkg, tmp_pkg)
```
