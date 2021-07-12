### v0.2.1 (in development)

 * Skip tests on machines where `gettext` is unavailable, #187; also alter the `msgfmt` command executed to create .mo files to skip options unavailable on Solaris, #218
 * Faster parsing of src messages (e.g. `get_message_data()` for the `base` package reduced from 14 to 7 seconds), #119
 * [New feature] New argument `max_translations` for `translate_package()` to limit the number of translations done, #188
 * When adding metadata for a new language, added tolerance for whitespace differences in specifying `plurals`, #183
 * [New feature] `get_message_data()` skips over messages on lines with comments `# notranslate`, and regions of lines between matched pairs of comments `# notranslate start` and `# notranslate end`, #10. Most useful for small fragmentary strings that are untranslateable/not worth translating, and for strings that are technically untranslateable (e.g., because they contain `\r`).
 * [New function] `write_po_file()` to convert a message database to a `.po` or `.pot` file manually (previously this was handled internally by `translate_package()`), #203. Also a constructor for the associated `po_metadata` class, `po_metadata()`. See `?po_metadata`.
 * [Bugfix] `get_message_data()` does a better job on files with unmatched parentheses inside preprocessor macros (`#define`s) in C/C++ files, #199

### v0.2.0 (June 2202)

 * Landed on CRAN!
