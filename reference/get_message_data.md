# Extract user-visible messages from a package

This function looks in the R and src directories of a package for
user-visible messages and compiles them as a
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
to facilitate analyzing this corpus as such.

## Usage

``` r
get_message_data(
  dir = ".",
  custom_translation_functions = list(R = NULL, src = NULL),
  style = NULL,
  verbose = !is_testing()
)
```

## Arguments

- dir:

  Character, default the present directory; a directory in which an R
  package is stored.

- custom_translation_functions:

  A `list` with either/both of two components, `R` and `src`, together
  governing how to extract any non-standard strings from the package.

  See Details in
  [`translate_package()`](https://michaelchirico.github.io/potools/reference/translate_package.md).

- style:

  Translation style, either `"base"` or `"explict"`. The default,
  `NULL`, reads from the `DESCRIPTION` field `Config/potools/style` so
  you can specify the style once for your package.

  Both styles extract strings explicitly flagged for translation with
  [`gettext()`](https://rdrr.io/r/base/gettext.html) or
  [`ngettext()`](https://rdrr.io/r/base/gettext.html). The base style
  additionally extracts strings in calls to
  [`stop()`](https://rdrr.io/r/base/stop.html),
  [`warning()`](https://rdrr.io/r/base/warning.html), and
  [`message()`](https://rdrr.io/r/base/message.html), and to `stopf()`,
  `warningf()`, and `messagef()` if you have added those helpers to your
  package. The explicit style also accepts `tr_()` as a short hand for
  [`gettext()`](https://rdrr.io/r/base/gettext.html). See
  `vignette("developer")` for more details.

- verbose:

  Logical, default `TRUE` (except during testing). Should extra
  information about progress, etc. be reported?

## Value

A `data.table` with the following schema:

- `message_source`: `character`, either `"R"` or `"src"`, saying whether
  the string was found in the R or the src folder of the package

- `type`: `character`, either `"singular"` or `"plural"`; `"plural"`
  means the string came from
  [`ngettext()`](https://rdrr.io/r/base/gettext.html) and can be
  pluralized

- `file`: `character`, the file where the string was found

- `msgid`: `character`, the string (character literal or `char` array as
  found in the source); missing for all `type == "plural"` strings

- `msgid_plural`: `list(character, character)`, the strings (character
  literals or `char` arrays as found in the source); the first applies
  in English for `n=1` (see `ngettext`), while the second applies for
  `n!=1`; missing for all `type == "singular"` strings

- `call`: `character`, the full call containing the string that was
  found

- `line_number`: `integer`, the line in `file` where the string was
  found

- `is_repeat`: `logical`, whether the `msgid` is a duplicate within this
  `message_source`

- `is_marked_for_translation`:`logical`, whether the string is marked
  for translation (e.g., in R, all character literals supplied to a
  `...` argument in [`stop()`](https://rdrr.io/r/base/stop.html) are so
  marked)

- `is_templated`, `logical`, whether the string is templatable (e.g.,
  uses `%s` or other formatting markers)

## Skipping translation

It is possible to skip translation for certain messages by adding a
comment with `# notranslate` on the same line as the message. For more
details, see the "Skipping translation" section in
[`vignette("developers", package = "potools")`](https://michaelchirico.github.io/potools/articles/developers.md).

## See also

[`translate_package()`](https://michaelchirico.github.io/potools/reference/translate_package.md),
[`write_po_file()`](https://michaelchirico.github.io/potools/reference/write_po_file.md)

## Author

Michael Chirico

## Examples

``` r
pkg <- system.file('pkg', package = 'potools')
get_message_data(pkg)
#> Getting R-level messages...
#> Getting src-level messages...
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

# includes strings provided to the custom R wrapper function catf()
get_message_data(pkg, custom_translation_functions = list(R = "catf:fmt|1"))
#> Getting R-level messages...
#> Getting src-level messages...
#>    message_source     type          file
#>            <char>   <char>        <char>
#> 1:              R singular         add.R
#> 2:              R singular         add.R
#> 3:              R singular         add.R
#> 4:              R singular      onLoad.R
#> 5:              R singular      onLoad.R
#> 6:              R singular      onLoad.R
#> 7:            src singular reverse_int.c
#>                                                            msgid msgid_plural
#>                                                           <char>       <list>
#> 1: add() only works on all-integer input, but found other types:       [NULL]
#> 2:                                                       integer       [NULL]
#> 3:                                      Adding %d integer inputs       [NULL]
#> 4:                                                     Launching       [NULL]
#> 5:                                                             /       [NULL]
#> 6:                                                                     [NULL]
#> 7:        reverse_int() only works on integer input, received %s       [NULL]
#>                                                                                                                           call
#>                                                                                                                         <char>
#> 1: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#> 2: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#> 3:                                                                           catf("Adding %d integer inputs\\n", length(dots))
#> 4:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 5:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 6:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 7:                                                                 _("reverse_int() only works on integer input, received %s")
#>    line_number is_repeat is_marked_for_translation is_templated
#>          <int>    <lgcl>                    <lgcl>       <lgcl>
#> 1:           7     FALSE                      TRUE        FALSE
#> 2:           8     FALSE                      TRUE        FALSE
#> 3:          12     FALSE                      TRUE        FALSE
#> 4:           2     FALSE                     FALSE        FALSE
#> 5:           2     FALSE                     FALSE        FALSE
#> 6:           2     FALSE                     FALSE        FALSE
#> 7:           9     FALSE                      TRUE         TRUE

# includes untranslated strings provided to the custom
#   C/C++ wrapper function ReverseTemplateMessage()
get_message_data(
  pkg,
  custom_translation_functions = list(src = "ReverseTemplateMessage:2")
)
#> Getting R-level messages...
#> Getting src-level messages...
#>    message_source     type          file
#>            <char>   <char>        <char>
#> 1:              R singular         add.R
#> 2:              R singular         add.R
#> 3:              R singular      onLoad.R
#> 4:              R singular      onLoad.R
#> 5:              R singular      onLoad.R
#> 6:            src singular reverse_int.c
#> 7:            src singular reverse_int.c
#>                                                            msgid msgid_plural
#>                                                           <char>       <list>
#> 1: add() only works on all-integer input, but found other types:       [NULL]
#> 2:                                                       integer       [NULL]
#> 3:                                                     Launching       [NULL]
#> 4:                                                             /       [NULL]
#> 5:                                                                     [NULL]
#> 6:        reverse_int() only works on integer input, received %s       [NULL]
#> 7:                        Reversing a vector with %d elements\\n       [NULL]
#>                                                                                                                           call
#>                                                                                                                         <char>
#> 1: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#> 2: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#> 3:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 4:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 5:                                                              cat("Launching", format(libname), "/", format(pkgname), "\\n")
#> 6:                                                                 _("reverse_int() only works on integer input, received %s")
#> 7:                                                         ReverseTemplateMessage(n, "Reversing a vector with %d elements\\n")
#>    line_number is_repeat is_marked_for_translation is_templated
#>          <int>    <lgcl>                    <lgcl>       <lgcl>
#> 1:           7     FALSE                      TRUE        FALSE
#> 2:           8     FALSE                      TRUE        FALSE
#> 3:           2     FALSE                     FALSE        FALSE
#> 4:           2     FALSE                     FALSE        FALSE
#> 5:           2     FALSE                     FALSE        FALSE
#> 6:           9     FALSE                      TRUE         TRUE
#> 7:          19     FALSE                     FALSE         TRUE

# cleanup
rm(pkg)
```
