### v0.2.1 (in development)

 * Skip tests on machines where `gettext` is unavailable, #187
 * Faster parsing of src messages (e.g. `get_message_data()` for the `base` package reduced from 14 to 7 seconds), #119
 * [New feature] New argument `max_translations` for `translate_package()` to limit the number of translations done, #188
 * When adding metadata for a new language, added tolerance for whitespace differences in specifying `plurals`, #183
 * [New feature] `get_message_data()` skips over messages on lines with comments `# notranslate`, and regions of lines between matched pairs of comments `# notranslate start` and `# notranslate end`, #10

### v0.2.0 (June 2202)

 * Landed on CRAN!
