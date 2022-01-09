# potools 0.2.3 (in development)

 * [Bugfix] `translate_package()` works in batch mode, [#224](https://github.com/MichaelChirico/potools/issues/224). Thanks @HenrikBengtsson for the report.
 * potools has a new hex logo! Thanks to @hadley for coordinating and @allisonhorst for the drawing!
 * [Bugfix] `check_cracked_messages()` recognizes named arguments (e.g. `call.` to `stop()` or `appendLF` to `message()`) and keeps them in the original call, [#227](https://github.com/MichaelChirico/potools/issues/227). Thanks @hadley for the report.
 * [Note] The test suite now relies on the 3rd edition of `testthat`, meaning a version requirement in the Suggested dependency.
 * [Note] The default value of `verbose` has been changed to `TRUE` in `translate_package()` and `get_message_data()`. Verbosity has also been increased to help detect the source of issues, [#288](https://github.com/MichaelChirico/potools/issues/288). Thanks for @LDalby for reporting an unhelpful error.
 * [Feature] `check_potools_sys_reqs()` (mostly intended for internal use, but exported for testing) gains a `which` argument to fine-tune which system requirements to check, [#275](https://github.com/MichaelChirico/potools/issues/275) and [#288](https://github.com/MichaelChirico/potools/issues/288). Thanks @hadley for the suggestion and @LDalby for early dev testing which emphasized the need for this.
 * [Note] potools gains a logo featuring a [potoo](https://en.wikipedia.org/wiki/Potoo) thanks to the artistic skills of @allisonhorst
 * [Feature] New function `po_explain_plurals()` to help de-mystify how to supply plurals for different languages. For example, `po_explain_plurals("pl", 3)` explains that "For Polish (Polski), plural index 2 applies when n = 0, 5-21, 25-31, 35-41, ...", [#278](https://github.com/MichaelChirico/potools/issues/278). Thanks @hadley for the suggestion to independently export this functionality which was already used as part of `translate_package()`.

## New languages/locales supported out of the box:

 * Swedish (svenska)

# potools 0.2.2 (July 2021)

 * Skip tests on machines where `gettext` is unavailable, #187; also alter the `msgfmt` command executed to create .mo files to skip options unavailable on Solaris, #218
 * Faster parsing of src messages (e.g. `get_message_data()` for the `base` package reduced from 14 to 7 seconds), #119
 * [New feature] New argument `max_translations` for `translate_package()` to limit the number of translations done, #188
 * When adding metadata for a new language, added tolerance for whitespace differences in specifying `plurals`, #183
 * [New feature] `get_message_data()` skips over messages on lines with comments `# notranslate`, and regions of lines between matched pairs of comments `# notranslate start` and `# notranslate end`, #10. Most useful for small fragmentary strings that are untranslateable/not worth translating, and for strings that are technically untranslateable (e.g., because they contain `\r`).
 * [New function] `write_po_file()` to convert a message database to a `.po` or `.pot` file manually (previously this was handled internally by `translate_package()`), #203. Also a constructor for the associated `po_metadata` class, `po_metadata()`. See `?po_metadata`.
 * [Bugfix] `get_message_data()` does a better job on files with unmatched parentheses inside preprocessor macros (`#define`s) in C/C++ files, #199

# potools 0.2.0 (June 2021)

 * Landed on CRAN!
