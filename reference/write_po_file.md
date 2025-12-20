# Write a .po or .pot file corresponding to a message database

Serialize a message database in the `.po` and `.pot` formats recognized
by the gettext ecosystem.

## Usage

``` r
write_po_file(
  message_data,
  po_file,
  metadata,
  width = 79L,
  wrap_at_newline = TRUE,
  use_base_rules = metadata$package %chin% .potools$base_package_names
)

po_metadata(
  package = "",
  version = "",
  language = "",
  author = "",
  email = "",
  bugs = "",
  copyright = NULL,
  ...
)

# S3 method for class 'po_metadata'
format(x, template = FALSE, use_plurals = FALSE, ...)

# S3 method for class 'po_metadata'
print(x, ...)
```

## Arguments

- message_data:

  `data.table`, as returned from
  [`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md).
  NB: R creates separate domains for R and C/C++ code; it is recommended
  you do the same by filtering the `get_message_data` output for
  `message_source == "R"` or `message_source == "src"`. Other approaches
  are untested.

- po_file:

  Character vector giving a destination path. Paths ending in `.pot`
  will be written with template files (e.g., `msgstr` entries will be
  blanked).

- metadata:

  A `po_metadata` object as returned by `po_metadata()`.

- width:

  Numeric governing the wrapping width of the output file. Default is
  `79L` to match the behavior of the `xgettext` utility. `Inf` turns off
  wrapping (except for file source markers comments).

- wrap_at_newline:

  Logical, default `TRUE` to match the `xgettext` utility's behavior. If
  `TRUE`, any `msgid` or `msgstr` will always be wrapped at an internal
  newline (i.e., literally matching `\n`).

- use_base_rules:

  Logical; Should internal behavior match base behavior as strictly as
  possible? `TRUE` if being run on a base package (i.e., `base` or one
  of the default packages like `utils`, `graphics`, etc.). See Details.

- package:

  Character; the name of the package being translated.

- version:

  Character; the version of the package being translated.

- language:

  Character; the language of the `msgstr`. See
  [`translate_package()`](https://michaelchirico.github.io/potools/reference/translate_package.md)
  for details.

- author:

  Character; an author (combined with `email`) to whom to attribute the
  translations (as `Last-Translator`).

- email:

  Character; an e-mail address associated with `author`.

- bugs:

  Character; a URL where issues with the translations can be reported.

- copyright:

  An object used to construct the initial Copyright reference in the
  output. If `NULL`, no such comment is written. If a `list`, it should
  the following structure:

  - `year`: Required, A year or hyphen-separated range of years

  - `holder`: Required, The name of the copyright holder

  - `title`: Optional, A title for the `.po`

  - `additional`: Optional, A character vector of additional lines for
    the copyright comment section

  If a `character` scalar, it is interpreted as the `holder` and the
  `year` is set as the `POT-Creation-Date`'s year.

- ...:

  Additional (named) components to add to the metadata. For
  `print.po_metadata`, passed on to `format.po_metadata`

- x:

  A `po_metadata` object.

- template:

  Logical; format the metadata as in a `.pot` template?

- use_plurals:

  Logical; should the `Plural-Forms` entry be included?

## Value

For `po_metadata`, an object of class `po_metadata` that has a `format`
method used to serialize the metadata.

## Details

Three components are set automatically if not provided:

- `pot_timestamp` - A `POSIXct` used to write the `POT-Creation-Date`
  entry. Defaults to the
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) at run time.

- `po_timestamp` - A `POSIXct` used to write the `PO-Revision-Date`
  entry. Defaults to be the same as `pot_timestamp`.

- `language_team` - A string used to write the `Language-Team` entry.
  Defaults to be the same as `language`; if provided manually, the
  format `LANGUAGE <LL@li.org>` is recommended.

The `charset` for output is always set to `"UTF-8"`; this is intentional
to make it more cumbersome to create non-UTF-8 files.

## References

<https://www.gnu.org/software/gettext/manual/html_node/Header-Entry.html>  

## See also

[`translate_package()`](https://michaelchirico.github.io/potools/reference/translate_package.md),
[`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md),
[`tools::xgettext2pot()`](https://rdrr.io/r/tools/xgettext.html),
[`tools::update_pkg_po()`](https://rdrr.io/r/tools/update_pkg_po.html)

## Author

Michael Chirico

## Examples

``` r
message_data <- get_message_data(system.file('pkg', package='potools'))
#> Getting R-level messages...
#> Getting src-level messages...
desc_data <- read.dcf(system.file('pkg', 'DESCRIPTION', package='potools'), c('Package', 'Version'))
metadata <- po_metadata(
  package = desc_data[, "Package"], version = desc_data[, "Version"],
  language = 'ar_SY', author = 'R User', email = 'ruser@gmail.com',
  bugs = 'https://github.com/ruser/potoolsExample/issues'
)

# add fake translations
message_data[type == "singular", msgstr := "<arabic translation>"]
#> Index: <type>
#>    message_source     type          file
#>            <char>   <char>        <char>
#> 1:              R singular         add.R
#> 2:              R singular         add.R
#> 3:              R singular      onLoad.R
#> 4:              R singular      onLoad.R
#> 5:              R singular      onLoad.R
#> 6:            src singular reverse_int.c
#>                                                            msgid msgid_plural
#>                                                           <char>       <list>
#> 1: add() only works on all-integer input, but found other types:       [NULL]
#> 2:                                                       integer       [NULL]
#> 3:                                                     Launching       [NULL]
#> 4:                                                             /       [NULL]
#> 5:                                                                     [NULL]
#> 6:        reverse_int() only works on integer input, received %s       [NULL]
#>                                                                                                                           call
#>                                                                                                                         <char>
#> 1: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#> 2: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#> 3:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 4:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 5:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 6:                                                                 _("reverse_int() only works on integer input, received %s")
#>    line_number is_repeat is_marked_for_translation is_templated
#>          <int>    <lgcl>                    <lgcl>       <lgcl>
#> 1:           7     FALSE                      TRUE        FALSE
#> 2:           8     FALSE                      TRUE        FALSE
#> 3:           2     FALSE                     FALSE        FALSE
#> 4:           2     FALSE                     FALSE        FALSE
#> 5:           2     FALSE                     FALSE        FALSE
#> 6:           9     FALSE                      TRUE         TRUE
#>                  msgstr
#>                  <char>
#> 1: <arabic translation>
#> 2: <arabic translation>
#> 3: <arabic translation>
#> 4: <arabic translation>
#> 5: <arabic translation>
#> 6: <arabic translation>
# Arabic has 6 plural forms
message_data[type == "plural", msgstr_plural := .(as.list(sprintf("<%d translation>", 0:5)))]
#> Index: <type>
#>    message_source     type          file
#>            <char>   <char>        <char>
#> 1:              R singular         add.R
#> 2:              R singular         add.R
#> 3:              R singular      onLoad.R
#> 4:              R singular      onLoad.R
#> 5:              R singular      onLoad.R
#> 6:            src singular reverse_int.c
#>                                                            msgid msgid_plural
#>                                                           <char>       <list>
#> 1: add() only works on all-integer input, but found other types:       [NULL]
#> 2:                                                       integer       [NULL]
#> 3:                                                     Launching       [NULL]
#> 4:                                                             /       [NULL]
#> 5:                                                                     [NULL]
#> 6:        reverse_int() only works on integer input, received %s       [NULL]
#>                                                                                                                           call
#>                                                                                                                         <char>
#> 1: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#> 2: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#> 3:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 4:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 5:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 6:                                                                 _("reverse_int() only works on integer input, received %s")
#>    line_number is_repeat is_marked_for_translation is_templated
#>          <int>    <lgcl>                    <lgcl>       <lgcl>
#> 1:           7     FALSE                      TRUE        FALSE
#> 2:           8     FALSE                      TRUE        FALSE
#> 3:           2     FALSE                     FALSE        FALSE
#> 4:           2     FALSE                     FALSE        FALSE
#> 5:           2     FALSE                     FALSE        FALSE
#> 6:           9     FALSE                      TRUE         TRUE
#>                  msgstr msgstr_plural
#>                  <char>        <list>
#> 1: <arabic translation>        [NULL]
#> 2: <arabic translation>        [NULL]
#> 3: <arabic translation>        [NULL]
#> 4: <arabic translation>        [NULL]
#> 5: <arabic translation>        [NULL]
#> 6: <arabic translation>        [NULL]

# Preview metadata
print(metadata)
#> msgid ""
#> msgstr ""
#> "Project-Id-Version: potoolsExample 0.0.1\n"
#> "Report-Msgid-Bugs-To: https://github.com/ruser/potoolsExample/issues\n"
#> "POT-Creation-Date: 2025-12-20 06:04+0000\n"
#> "PO-Revision-Date: 2025-12-20 06:04+0000\n"
#> "Last-Translator: R User <ruser@gmail.com>\n"
#> "Language-Team: ar_SY\n"
#> "Language: ar_SY\n"
#> "MIME-Version: 1.0\n"
#> "Content-Type: text/plain; charset=UTF-8\n"
#> "Content-Transfer-Encoding: 8bit\n"
# write .po file
write_po_file(
  message_data[message_source == "R"],
  tmp_po <- tempfile(fileext = '.po'),
  metadata
)
#> NULL
writeLines(readLines(tmp_po))
#> msgid ""
#> msgstr ""
#> "Project-Id-Version: potoolsExample 0.0.1\n"
#> "Report-Msgid-Bugs-To: https://github.com/ruser/potoolsExample/issues\n"
#> "POT-Creation-Date: 2025-12-20 06:04+0000\n"
#> "PO-Revision-Date: 2025-12-20 06:04+0000\n"
#> "Last-Translator: R User <ruser@gmail.com>\n"
#> "Language-Team: ar_SY\n"
#> "Language: ar_SY\n"
#> "MIME-Version: 1.0\n"
#> "Content-Type: text/plain; charset=UTF-8\n"
#> "Content-Transfer-Encoding: 8bit\n"
#> 
#> #: add.R:7
#> msgid "add() only works on all-integer input, but found other types:"
#> msgstr "<arabic translation>"
#> 
#> #: add.R:8
#> msgid "integer"
#> msgstr "<arabic translation>"

# write .pot template
write_po_file(
  message_data[message_source == "R"],
  tmp_pot <- tempfile(fileext = '.pot'),
  metadata
)
#> NULL
writeLines(readLines(tmp_pot))
#> msgid ""
#> msgstr ""
#> "Project-Id-Version: potoolsExample 0.0.1\n"
#> "Report-Msgid-Bugs-To: https://github.com/ruser/potoolsExample/issues\n"
#> "POT-Creation-Date: 2025-12-20 06:04+0000\n"
#> "PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n"
#> "Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
#> "Language-Team: LANGUAGE <LL@li.org>\n"
#> "Language: \n"
#> "MIME-Version: 1.0\n"
#> "Content-Type: text/plain; charset=UTF-8\n"
#> "Content-Transfer-Encoding: 8bit\n"
#> 
#> #: add.R:7
#> msgid "add() only works on all-integer input, but found other types:"
#> msgstr ""
#> 
#> #: add.R:8
#> msgid "integer"
#> msgstr ""

# cleanup
file.remove(tmp_po, tmp_pot)
#> [1] TRUE TRUE
rm(message_data, desc_data, metadata, tmp_po, tmp_pot)
```
