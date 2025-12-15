# Check for cracked messages more suitable for templating

Diagnose the R messages in a package to discover the presence of
"cracked" messages better served for translation by templating. See
Details.

## Usage

``` r
check_cracked_messages(message_data)
```

## Arguments

- message_data:

  A `data.table`, or object convertible to one.

## Value

A `data.table` with columns `call`, `file`, `line_number`, and
`replacement` summarizing the results.

## Details

Error messages built like
`stop("You gave ", n, " arguments, but ", m, " are needed.")` are in
general hard for translators – the correct translation may be in a
totally different order (e.g., this is often the case for Japanese). It
is preferable instead to use
[`base::gettextf()`](https://rdrr.io/r/base/sprintf.html) to build a
templated message like
`stop(gettextf("You gave %d arguments but %d are needed.", n, m))`.
Translators are then free to rearrange the template to put the numeric
pattern where it fits most naturally in the target language.

## See also

[`translate_package()`](https://michaelchirico.github.io/potools/reference/translate_package.md),
`update_pkg_po()`

## Author

Michael Chirico

## Examples

``` r
pkg <- file.path(system.file(package = 'potools'), 'pkg')
# copy to a temporary location to be able to read/write/update below
tmp_pkg <- file.path(tempdir(), "pkg")
dir.create(tmp_pkg)
file.copy(pkg, dirname(tmp_pkg), recursive = TRUE)
#> [1] TRUE

# first, extract message data
message_data = get_message_data(tmp_pkg)
#> Getting R-level messages...
#> Getting src-level messages...

# now, diagnose the messages for any "cracked" ones
check_cracked_messages(message_data)
#>                                                                                                                           call
#>                                                                                                                         <char>
#> 1: stop( "add() only works on all-integer input, but found other types: ", toString(unique(setdiff(input_types, "integer"))) )
#>      file line_number
#>    <char>       <int>
#> 1:  add.R           7
#>                                                                                                                                         replacement
#>                                                                                                                                              <char>
#> 1: stop(domain=NA, gettextf("add() only works on all-integer input, but found other types: %s", toString(unique(setdiff(input_types, "integer")))))

# cleanup
unlink(tmp_pkg, recursive = TRUE)
rm(pkg, tmp_pkg, message_data)
```
