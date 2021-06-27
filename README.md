# potools: Tools for Portability and Internationalization of R packages

## Overview

R users are a global community. From Xiamen to Santiago, Addis Ababa to Tbilisi, Ogallala to Adelaide, R users are legion and their native languages are as well.

If the target audience of your package extends beyond the English-speaking world, or if you want to make the user experience for the non-native English speakers using your tools, you can consider internationalizing your package by translating its user-facing communications (verbose messages, warnings, errors, etc.).

Unfortunately, to do so has some tedious aspects, namely, learning the gettext system of `.po` files, `.pot` templates, and `.mo` binaries -- another syntax rife with quirks and idiosyncrasies.

`potools` is designed to minimize the friction to translating your package by abstracting away as many details of the `.po` system of translations as possible.

The core function of `potools`, `translate_package`, is a one-stop-shop for interactively setting your package up for translation and providing those translations, all without ever having to touch a `.po` file yourself.

`potools` is a UTF-8 package -- all `.po` and `.pot` files it produces will be treated as UTF-8.

### Customized translators

`base` R provides several functions for messaging that are natively equipped for translation (they all have a `domain` argument): `stop()`, `warning()`, `message()`, `gettext()`, `gettextf()`, `ngettext()`, and `packageStartupMessage()`.

While handy, some developers may prefer to write their own functions, or to write wrappers of the provided functions that provide some enhanced functionality (e.g., templating or automatic wrapping). In this case, the default R tooling for translation (`xgettext()`, `xngettext()` `xgettext2pot()`, `update_pkg_po()` from `tools`) will not work, but `potools::translate_package()` and its workhorse `potools::get_message_data()` provide an interface to continue building translations for your workflow.

Suppose you wrote a function `stopf()` that is a wrapper of `stop(gettextf())` used to build templated error messages in R, which makes translation easier for translators (see below), e.g.:


```
stopf = function(fmt, ..., domain = NULL) {
  stop(gettextf(fmt, ...), domain = domain, call. = FALSE)
}
```

Note that `potools` itself uses just such a wrapper internally to build error messages! To extract strings from calls in your package to `stopf()` and mark them for translation, use the argument `custom_translation_functions`:

```
get_message_data(
  '/path/to/my_package',
  custom_translation_functions = list(R = 'stopf:fmt|1')
)
```

This invocation tells `get_message_data()` to look for strings in the `fmt` argument in calls to `stopf()`. `1` indicates that `fmt` is the first argument.

More specifications are possible, including marking custom calls in C/C++; see `?translate_package` for a full explanation.

Note that this is only good for _marking_ such strings for translation -- for them to actually be translated during code execution, your custom function will ultimately have to be pass the strings to `gettext()` or `gettextf()` (as done in the `stopf()` example). Without doing so, the string will always just appear in English.

### Diagnostics

`translate_package` also runs some diagnostics that can help make your package more translation-ready (see below).

#### Cracked messages

A cracked message is one like:

```r
stop("There are ", n, " good things and ", m, " bad things.")
```

In its current state, translators will be asked to translate three messages independently:

 - "There are"
 - "good things and"
 - "bad things."
 
The message has been cracked; it might not be possible to translate a string as generic as "There are" into many languages -- context is key!

To keep the context, the error message should instead be build with `gettextf` like so:

```r
stop(domain=NA, gettextf("There are %d good things and %d bad things."))
```

Now there is only one string to translate! Note that this also allows the translator to change the word
order as they see fit -- for example, in Japanese, the grammatical order usually puts the verb last (where
in English it usually comes right after the subject).

`translate_package` detects such cracked messages and suggests a `gettextf`-based approach to fix them.

#### Untranslated R messages produced by `cat()`

Only strings which are passed to certain `base` functions are eligible for translation, namely `stop`, `warning`, `message`, `packageStartupMessage`, `gettext`, `gettextf`, and `ngettext` (all of which have a `domain` argument that is key for translation).

However, it is common to also produce some user-facing messages using `cat` -- if your package does so, it must first
use `gettext` or `gettextf` to translate the message before sending it to the user with `cat`.

`translate_package` detects strings produced with `cat` and suggests a `gettext`- or `gettextf`-based fix.

#### Untranslated C/C++ messages

This diagnostic detects any literal `char` arrays provided to common messaging functions in C/C++, namely `ngettext()`, `Rprintf()`, `REprintf()`, `Rvprintf()`, `REvprintf()`, `R_ShowMessage()`, `R_Suicide()`, `warning()`, `Rf_warning()`, `error()`, `Rf_error()`, `dgettext()`, and `snprintf()`. To actually translate these strings, pass them through the translation macro `_`.

NB: Translation in C/C++ requires some additional `#include`s and declarations, including defining the `_` macro. See the Internationalization section of Writing R Extensions for details.

## Installation

`potools` is not yet available on CRAN. Your early feedback is welcome!

For now, please install from GitHub; the easiest way to do so:

```r
# install.packages("remotes")
remotes::install_github("MichaelChirico/potools")
```

## Tips & Tricks for Translation

### Searchable error messages

One observation about offering translated messages is that non-English messages are harder to google. A few suggestions:
 
     + You can give error messages a unique identifier (e.g. numbering). This may be harder to do for "established" packages since adding identifiers might be a breaking change.
     + End users can switch to an English locale mid-session by running `Sys.setenv(LANGUAGE = 'en')` -- error messages will be produced in English until they set `LANGUAGE` again.
     + You could write a custom error wrapper that produces the error both in English and as a translation.
     
### Translating technical terms

Technical terms are par for the course in R packages; showing users similar terms for the same concept might lead to needless confusion. R recommends using the [ISI Multilingual Glossary of Statistical Terms](https://www.isi-web.org/publications/glossary-of-statistical-terms) to help overcome this issue.

### Picking a domain for diasporic languages

What domain should you use when translating Spanish? There's `es_AR`, `es_BO`, `es_CL`, `es_DO`, `es_HN`, ... do I
really need to provide a separate file for my Nicaraguan (`es_NI`) users?

No, but you could. Typically, you are best off creating one set of translations under the language's general
domain (here, `es`). Once translations exist for `es`, users in all of the more specific locales will see the
messages for `es` whenever they exist. If you really do want to provide more regionally-specific error messages
(awesome!), you can either (1) create a whole new set of translations for each region or (2) write translations
_only for the region-specific messages_. The latter is how R handles messages that differ on British/American
spelling, for example.

Say a user is running in `es_GT` and triggers an error. R will first look for a translation into `es_GT`; if
none is defined, it will look for a translation into `es`. If none is defined again, it will finally fall
back to the package's default language (i.e., whatever language is written in the source code, usually English).
