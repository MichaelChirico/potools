#' Interactively provide translations for a package's messages
#'
#' @description
#' This function handles the "grunt work" of building and updating translation
#' libraries. In addition to providing a friendly interface for supplying
#' translations, some internal logic is built to help make your package more
#' translation-friendly.
#'
#' To get started, the package developer should run `translate_package()` on
#' your package's source to produce a template `.pot` file (or files, if your
#' package has both R and C/C++ messages to translated), e.g.
#'
#' To add translations in your desired language, include the target language:
#' in the `translate_package(languages = "es")` call.
#'
#' @section Phases:
#' `translate_package()` goes through roughly three "phases" of translation.
#'
#' 1. Setup -- `dir` is checked for existing translations
#'    (toggling between "update" and "new" modes), and R files are parsed and
#'    combed for user-facing messages.
#'
#' 2. Diagnostics:  see the Diagnostics section below. Any
#'    diagnostic detecting "unhealthy" messages will result in a yes/no prompt to
#'    exit translation to address the issues before continuing.
#'
#' 3. Translation. All of the messages found in phase one are iterated over --
#'    the user is shown a message in English and prompted for the translation
#'    in the target language. This process is repeated for each domain
#'    in `languages`.
#'
#' An attempt is made to provide hints for some translations that require
#' special care (e.g. that have escape sequences or use templates). For
#' templated messages (e.g., that use `%s`), the user-provided message
#' must match the templates of the English message. The templates *don't*
#' have to be in the same order -- R understands template reordering, e.g.
#' `%2$s` says "interpret the second input as a string". See
#' [sprintf()] for more details.
#'
#' After each language is completed, a corresponding \file{.po} file is written
#' to the package's \file{po} directory (which is created if it does not yet
#' exist).
#'
#' There are some discrepancies in the default behavior of
#' `translate_package` and the translation workflow used to generate the
#' \file{.po}/\file{.pot} files for R itself (mainly, the suite of functions
#' from `tools`, [tools::update_pkg_po()],
#' [tools::xgettext2pot()], [tools::xgettext()], and
#' [tools::xngettext()]). They should only be superficial (e.g.,
#' whitespace or comments), but nevertheless may represent a barrier to
#' smoothly submitting patchings to R Core. To make the process of translating
#' base R and the default packages (`tools`, `utils`, `stats`,
#' etc.) as smooth as possible, set the `use_base_rules` argument to
#' `TRUE` and your resulting \file{.po}/\file{.pot}/\file{.mo} file will
#' match base's.
#'
#' @section Custom translation functions:
#'
#' `base` R provides several functions for messaging that are natively equipped
#' for translation (they all have a `domain` argument): `stop()`, `warning()`,
#' `message()`, `gettext()`, `gettextf()`, `ngettext()`, and
#' `packageStartupMessage()`.
#'
#' While handy, some developers may prefer to write their own functions, or to
#' write wrappers of the provided functions that provide some enhanced
#' functionality (e.g., templating or automatic wrapping). In this case,
#' the default R tooling for translation (`xgettext()`, `xngettext()`
#' `xgettext2pot()`, `update_pkg_po()` from `tools`) will not work, but
#' `translate_package()` and its workhorse `get_message_data()` provide an
#' interface to continue building  translations for your workflow.
#'
#' Suppose you wrote a function `stopf()` that is a wrapper of
#' `stop(gettextf())` used to build templated error messages in R, which makes
#' translation easier for translators (see below), e.g.:
#'
#' ```R
#' stopf = function(fmt, ..., domain = NULL) {
#'   stop(gettextf(fmt, ...), domain = domain, call. = FALSE)
#' }
#' ```
#'
#' Note that `potools` itself uses just such a wrapper internally to build
#' error messages! To extract strings from calls in your package to `stopf()`
#' and mark them for translation, use the argument
#' `custom_translation_functions`:
#'
#' ```R
#' get_message_data(
#'   '/path/to/my_package',
#'   custom_translation_functions = list(R = 'stopf:fmt|1')
#' )
#' ```
#'
#' This invocation tells `get_message_data()` to look for strings in the
#' `fmt` argument in calls to `stopf()`. `1` indicates that `fmt` is the
#' first argument.
#'
#' This interface is inspired by the `--keyword` argument to the
#' `xgettext` command-line tool. This argument consists of a list with two
#' components, `R` and `src` (either can be excluded), owing to
#' differences between R and C/C++. Both components, if present, should consist
#' of a character vector.
#'
#' For R, there are two types of input: one for named arguments, the other for
#' unnamed arguments.
#'
#' * Entries for __named__ arguments will look like `"fname:arg|num"` (singular
#'   string) or `"fname:arg1|num1,arg2|num2"` (plural string). `fname`
#'   gives the name of the function/call to be extracted from the R source,
#'   `arg`/`arg1`/`arg2` specify the name of the argument to
#'   `fname` from which strings should be extracted, and
#'   `num`/`num1`/`num2` specify the *order* of the named
#'   argument within the signature of `fname`.
#'
#' * Entries for __unnamed__ arguments will look like
#'   `"fname:...\xarg1,...,xargn"`, i.e., `fname`, followed by
#'   `:`, followed by `...` (three dots), followed by a backslash
#'   (`\`), followed by a comma-separated list of argument names. All
#'   strings within calls to `fname` *except* those supplied to the
#'   arguments named among `xarg1`, ..., `xargn` will be extracted.
#'
#' To clarify, consider the how we would (redundantly) specify
#' `custom_translation_functions` for some of the default messagers,
#' `gettext`, `gettextf`, and `ngettext`:
#' `custom_translation_functions = list(R = c("gettext:...\domain",
#' "gettextf:fmt|1", "ngettext:msg1|2,msg2|3"))`.
#'
#' For src, there is only one type of input, which looks like
#' `"fname:num"`, which says to look at the `num` argument of calls
#' to `fname` for `char` arrays.
#'
#' Note that there is a difference in how translation works for src vs. R -- in
#' R, all strings passed to certain functions are considered marked for
#' translations, but in src, all translatable strings must be explicitly marked
#' as such. So for `src` translations, `custom_translation_functions`
#' is not used to customize which strings are marked for translation, but
#' rather, to expand the set of calls which are searched for potentially
#' *untranslated* arrays (i.e., arrays passed to the specified calls that
#' are not explicitly marked for translation). These can then be reported in
#' the [check_untranslated_src()] diagnostic, for example.
#'
#' @section Diagnostics:
#'
#' ### Cracked messages
#'
#' A cracked message is one like:
#'
#' ```r
#' stop("There are ", n, " good things and ", m, " bad things.")
#' ```
#'
#' In its current state, translators will be asked to translate three messages
#' independently:
#'
#' -   "There are"
#' -   "good things and"
#' -   "bad things."
#'
#' The message has been cracked; it might not be possible to translate a string
#' as generic as "There are" into many languages -- context is key!
#'
#' To keep the context, the error message should instead be build with
#' `gettextf` like so:
#'
#' ``` r
#' stop(domain=NA, gettextf("There are %d good things and %d bad things."))
#' ```
#'
#' Now there is only one string to translate! Note that this also allows the
#' translator to change the word order as they see fit -- for example, in
#' Japanese, the grammatical order usually puts the verb last (where in
#' English it usually comes right after the subject).
#'
#' `translate_package` detects such cracked messages and suggests a
#' `gettextf`-based approach to fix them.
#'
#' ### Untranslated R messages produced by `cat()`
#'
#' Only strings which are passed to certain `base` functions are eligible for
#' translation, namely `stop`, `warning`, `message`, `packageStartupMessage`,
#' `gettext`, `gettextf`, and `ngettext` (all of which have a `domain` argument
#' that is key for translation).
#'
#' However, it is common to also produce some user-facing messages using
#' `cat` -- if your package does so, it must first use `gettext` or `gettextf`
#' to translate the message before sending it to the user with `cat`.
#'
#' `translate_package` detects strings produced with `cat` and suggests a
#' `gettext`- or `gettextf`-based fix.
#'
#' ### Untranslated C/C++ messages
#'
#' This diagnostic detects any literal `char` arrays provided to common
#' messaging functions in C/C++, namely `ngettext()`, `Rprintf()`, `REprintf()`,
#' `Rvprintf()`, `REvprintf()`, `R_ShowMessage()`, `R_Suicide()`, `warning()`,
#' `Rf_warning()`, `error()`, `Rf_error()`, `dgettext()`, and `snprintf()`.
#' To actually translate these strings, pass them through the translation
#' macro `_`.
#'
#' NB: Translation in C/C++ requires some additional `#include`s and
#' declarations, including defining the `_` macro.
#' See the Internationalization section of Writing R Extensions for details.
#'
#' @section Custom diagnostics:
#'
#' A diagnostic is a function which takes as input a `data.table`
#' summarizing the translatable strings in a package (e.g. as generated by
#' [get_message_data()]), evaluates whether these messages are
#' "healthy" in some sense, and produces a digest of "unhealthy" strings and
#' (optionally) suggested replacements.
#'
#' The diagnostic function must have an attribute named `diagnostic_tag`
#' that describes what the diagnostic does; it is reproduced in the format
#' \code{Found {nrow(result)} {diagnostic_tag}:}. For example,
#' [check_untranslated_cat()] has `diagnostic_tag =
#' "untranslated messaging calls passed through cat()"`.
#'
#' The output diagnostic result has the following schema:
#'
#' * `call`: `character`, the call identified as problematic
#' * `file`: `character`, the file where `call` was found
#' * `line_number`: `integer`, the line in `file` where `call` was found
#' * `replacement`: `character`, *optional*, a suggested fix to make the call
#'   "healthy"
#'
#' See [check_cracked_messages()],
#' [check_untranslated_cat()], and
#' [check_untranslated_src()] for examples of diagnostics.
#'
#' @param dir Character, default the present directory; a directory in which an
#' R package is stored.
#' @param languages Character vector; locale codes to which to translate.
#'   Must be a valid language accepted by gettext. This almost always takes
#'   the form of (1) an ISO 639 2-letter language code; or (2) `ll_CC`, where
#'   `ll` is an ISO 639 2-letter language code and `CC` is an ISO 3166 2-letter
#'   country code e.g. `es` for Spanish, `es_AR` for Argentinian Spanish, `ro`
#'   for Romanian, etc. See [base::Sys.getlocale()] for some helpful tips
#'   about how to tell which locales are currently available on your machine, and
#'   see the References below for some web resources listing more locales.
#' @param diagnostics A `list` of diagnostic functions to be run on the
#' package's message data. See Details.
#' @param custom_translation_functions A `list` with either/both of two
#' components, `R` and `src`, together governing how to extract any
#' non-standard strings from the package. See Details.
#' @param max_translations Numeric; used for setting a cap on the number of
#' translations to be done for each language. Defaults to `Inf`, meaning
#' all messages in the package.
#' @param use_base_rules Logical; Should internal behavior match base behavior
#' as strictly as possible? `TRUE` if being run on a base package (i.e.,
#' `base` or one of the default packages like `utils`,
#' `graphics`, etc.). See Details.
#' @param copyright Character; passed on to [write_po_file()].
#' @param bugs Character; passed on to [write_po_file()].
#' @param verbose Logical, default `TRUE` (except during testing). Should
#' extra information about progress, etc. be reported?
#' @return This function returns nothing invisibly. As a side effect, a
#' \file{.pot} file is written to the package's \file{po} directory (updated if
#' one does not yet exist, or created from scratch otherwise), and a \file{.po}
#' file is written in the same directory for each element of `languages`.
#' @author Michael Chirico
#' @seealso [get_message_data()], [write_po_file()],
#' [tools::xgettext()], [tools::update_pkg_po()],
#' [tools::checkPoFile()], [base::gettext()]
#' @references
#' <https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Internationalization>
#' \cr
#' <https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Internationalization>
#' \cr
#' <https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Internationalization-in-the-R-sources>
#' \cr <https://developer.r-project.org/Translations30.html> \cr
#' <https://web.archive.org/web/20230108213934/https://www.isi-web.org/resources/glossary-of-statistical-terms> \cr
#' <https://www.gnu.org/software/gettext/> \cr
#' <https://www.gnu.org/software/gettext/manual/html_node/Usual-Language-Codes.html#Usual-Language-Codes>
#' \cr
#' <https://www.gnu.org/software/gettext/manual/html_node/Country-Codes.html#Country-Codes>
#' \cr <https://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip>
#' \cr <https://saimana.com/list-of-country-locale-code/>
#' @examples
#'
#' pkg <- system.file('pkg', package = 'potools')
#' # copy to a temporary location to be able to read/write/update below
#' tmp_pkg <- file.path(tempdir(), "pkg")
#' dir.create(tmp_pkg)
#' file.copy(pkg, dirname(tmp_pkg), recursive = TRUE)
#'
#' # run translate_package() without any languages
#' # this will generate a .pot template file and en@quot translations (in UTF-8 locales)
#' # we can also pass empty 'diagnostics' to skip the diagnostic step
#' # (skip if gettext isn't available to avoid an error)
#' if (isTRUE(check_potools_sys_reqs)) {
#'   translate_package(tmp_pkg, diagnostics = NULL)
#' }
#'
#' \dontrun{
#' # launches the interactive translation dialog for translations into Estonian:
#' translate_package(tmp_pkg, "et_EE", diagnostics = NULL, verbose = TRUE)
#' }
#'
#' # cleanup
#' unlink(tmp_pkg, recursive = TRUE)
#' rm(pkg, tmp_pkg)
#'
#' @export
translate_package = function(
  dir = '.',
  languages = NULL,
  diagnostics = list(check_cracked_messages, check_untranslated_cat, check_untranslated_src),
  custom_translation_functions = list(R = NULL, src = NULL),
  max_translations = Inf,
  use_base_rules = package %chin% .potools$base_package_names,
  copyright = NULL,
  bugs = '',
  verbose = !is_testing()
) {
  result <- check_potools_sys_reqs()
  if (!isTRUE(result)) stop(result) # nocov

  stopifnot(
    'Only one package at a time' = length(dir) == 1L,
    "'dir' must be a character" = is.character(dir),
    "'languages' must be a character vector" = is.null(languages) || is.character(languages),
    "'diagnostics' should be empty, a function, or list of functions" =
      is.null(diagnostics)
      || is.function(diagnostics)
      || (is.list(diagnostics) && all(vapply(diagnostics, is.function, logical(1L))))
  )

  # en-list singleton diagnostic for convenience
  if (is.function(diagnostics)) diagnostics = list(diagnostics)

  dir = get_directory(dir)

  desc_data = get_desc_data(dir)
  package <- desc_data['Package']
  is_base <- package == 'base'
  version <- desc_data['Version']

  po_dir <- file.path(dir, 'po')
  r_potfile <- file.path(po_dir, glue("R-{package}.pot"))
  src_potfile <- file.path(po_dir, sprintf("%s.pot", if (is_base) 'R' else package))
  update = file.exists(po_dir) && (file.exists(r_potfile) || file.exists(src_potfile))

  if (verbose) {
    if (update) {
      # is it worthwhile to try and distinguish the creation time of the
      #  R pot file and the src pot file? probably not...
      messagef(
        "Updating translation template for package '%s' (last updated %s)",
        package,
        format(file.info(r_potfile)$atime)
      )
    } else {
      messagef("Starting translations for package '%s'", package)
    }
  }
  if (!update) dir.create(po_dir, showWarnings = FALSE)

  message_data = get_message_data(dir, custom_translation_functions, verbose=verbose)

  if (!nrow(message_data)) {
    if (verbose) message('No messages to translate; finishing')
    return(invisible())
  }

  # for testing, we need a connection that stays open so that readLines(n=1L)
  #   reads successive lines. originally tried passing a test connection as
  #   an argument to prompt(), but that closed the connection each time -->
  #   only the first line is ever read.
  set_prompt_conn()
  on.exit(unset_prompt_conn())

  if (verbose) message('Running message diagnostics...')

  for (diagnostic in diagnostics) {
    diagnostic <- match.fun(diagnostic)
    result <- diagnostic(message_data)
    if (!nrow(result)) next
    show_diagnostic_results(result, diagnostic)
    responded_yes = tolower(prompt('Exit now to repair any of these? [y/N]')) %chin% c('y', 'yes')
    # length() required for use in batch mode
    if (length(responded_yes) && responded_yes) return(invisible())
  }

  po_params = list(package = package, version = version, copyright = copyright, bugs = bugs)
  write_po_files(message_data, po_dir, po_params, template = TRUE, use_base_rules = use_base_rules)

  if (l10n_info()[["UTF-8"]]) {
    # on UTF-8 machines we install the en@quot messages too
    # TODO: streamline this -- en_quote is definitely doing some redundant stuff
    message('Generating en@quot translations')
    update_en_quot_mo_files(dir, verbose)
  }


  if (is.null(languages)) {
    if (verbose) message('No languages provided; finishing')
    return(invisible())
  }

  for (language in languages) {
    metadata = get_lang_metadata(language)
    po_params$language = language

    # overwrite any existing translations written in previous translation.
    #   set blank initially (rather than deleting the column) to allow
    #   for interrupting the translation -- if unset, write_po_files will
    #   fail if both these columns are not yet present.
    message_data[type == 'singular', 'msgstr' := ""]
    message_data[type == 'plural', 'msgstr_plural' := .(list(rep("", metadata$nplurals)))]

    lang_file <- file.path(po_dir, glue("R-{language}.po"))
    if (update && file.exists(lang_file)) {
      if (verbose) {
        messagef(
          'Found existing R translations for %s (%s/%s) in %s. Running msgmerge...',
          language, metadata$full_name_eng, metadata$full_name_native, lang_file
        )
      }
      run_msgmerge(lang_file, r_potfile)

      find_fuzzy_messages(message_data, lang_file)
    } else {
      message_data[message_source == "R", 'fuzzy' := 0L]
    }

    lang_file <- file.path(po_dir, glue("{language}.po"))
    if (update && file.exists(lang_file)) {
      if (verbose) {
        messagef(
          'Found existing src translations for %s (%s/%s) in %s. Running msgmerge...',
          language, metadata$full_name_eng, metadata$full_name_native, lang_file
        )
      }
      run_msgmerge(lang_file, src_potfile)

      find_fuzzy_messages(message_data, lang_file)
    } else {
      message_data[message_source == "src", 'fuzzy' := 0L]
    }

    new_idx = message_data[
      is_marked_for_translation & (
        fuzzy == 1L
        | (type == 'singular' & !nzchar(msgstr) & nzchar(msgid, keepNA = TRUE))
        | (type == 'plural' & !vapply(msgstr_plural, function(x) all(nzchar(x)), logical(1L)))
      ),
      which = TRUE
    ]

    if (!length(new_idx)) {
      if (verbose) messagef('Translations for %s are up to date! Skipping.', language)
      next
    }
    if (verbose) {
      messagef(
        'Beginning new translations for %s (%s/%s); found %d untranslated messages',
        language, metadata$full_name_eng, metadata$full_name_native, length(new_idx)
      )
      message("(To quit translating, press 'Esc'; progress will be saved)")
    }

    po_params$author = prompt('Thanks! Who should be credited with these translations?')
    po_params$email = prompt('And what is their email?')

    # on.exit this to allow ESC to quit mid-translation. the intent is for the
    #   on.exit command to be overwritten on each iteration over languages --
    #   only one partially-finished language should be written at a time.
    INCOMPLETE = TRUE
    on.exit({
      if (INCOMPLETE) write_po_files(message_data, po_dir, po_params, use_base_rules = use_base_rules) # nocov
      # since add=FALSE, we overwrite the above call; duplicate it here
      unset_prompt_conn()
    })

    if (verbose) {
      # nolint start: line_length_linter.
      message(
        "***************************\n",
        "** BEGINNING TRANSLATION **\n",
        "***************************\n\n",
        "Some helpful reminders:\n",
        " * You can skip a translation by entering nothing (just press RETURN)\n",
        " * Special characters (like newlines, \\n, or tabs, \\t) should be written just like that (with an escape)\n",
        " * Be sure to match message templates. The count of templates (%s, %d, etc.) must match in all languages, as must initial and terminal newlines (\\n)\n",
        " * While the count of templates must match, the _order_ can be changed by using e.g. %2$s to mean 'use the second input as a string here'\n",
        " * Whenever templates or escaping is happening in a string, these will be 'highlighted' by carets (^) in the line below"
      )
      # nolint end: line_length_linter.
    }
    # NB: loop over rows to facilitate quitting without losing progress
    for (ii in head(new_idx, max_translations)) {
      if (message_data$type[ii] == 'plural') {
        translation = read_translation(
          message_data$msgid_plural[[ii]][1L],
          'plural',
          message_data$file[ii],
          message_data$call[ii],
          message_data$fuzzy[ii],
          message_data$msgstr_plural[[ii]],
          metadata
        )
        set(message_data, ii, 'msgstr_plural', list(translation))
      } else if (!message_data$is_repeat[ii]) {
        translation = read_translation(
          message_data$msgid[ii],
          'singular',
          message_data$file[ii],
          message_data$call[ii],
          message_data$fuzzy[ii],
          message_data$msgstr[ii],
          metadata
        )
        set(message_data, ii, 'msgstr', translation)
      }
    }

    # set INCOMPLETE after write_po_files for the event of a process interruption
    #   between the loop finishing and the write_po_files command executing
    write_po_files(message_data, po_dir, po_params, use_base_rules = use_base_rules)
    INCOMPLETE = FALSE
  }

  # TODO: reinstate source marker tags, at least for src .pot file & maybe for R .pot file too?
  po_compile(dir, package, verbose = verbose)
  return(invisible())
}

# take from those present in r-devel:
# ls -1 ~/svn/R-devel/src/library/*/po/*.po | \
#   awk -F"[./]" '{print $10}' | \
#   sed -r 's/^R(Gui)?-//g' | sort -u | \
#   awk '{print "  ", $1, " = ,"}'
# alternatively, a more complete list can be found on some websites:
#   https://saimana.com/list-of-country-locale-code/
# nplurals,plural info from https://l10n.gnome.org/teams/<language>
# NB: looks may be deceiving for right-to-left scripts (e.g. Farsi), where the
#   displayed below might not be in the order it is parsed.
# assign to .potools, not a package env, to keep more readily mutable inside update_metadata()
.potools$KNOWN_LANGUAGES = fread(system.file('extdata', 'language_metadata.csv', package='potools'), key='code')

# the 'plural' column above is designed for computers;
#   translate that to something human-legible here.
# NB: 'plural' is 0-based (like in the .po file), but
#   'plural_index' is 1-based (to match the above R-level code).
# assign to .potools, not a package env, to keep more readily mutable inside update_metadata()
.potools$PLURAL_RANGE_STRINGS = fread(
  system.file('extdata', 'plurals_metadata.csv', package='potools'),
  key = c('plural', 'plural_index')
)

# for testing; unexported
# nocov start
reset_language_metadata = function() {
  .potools$KNOWN_LANGUAGES = fread(
    system.file('extdata', 'language_metadata.csv', package='potools'),
    key='code'
  )
  .potools$PLURAL_RANGE_STRINGS = fread(
    system.file('extdata', 'plurals_metadata.csv', package='potools'),
    key = c('plural', 'plural_index')
  )
}
# nocov end
