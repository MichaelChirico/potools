# Check for cracked messages in C/C++ sources

Diagnose the C/C++ messages in a package to discover untranslated
messages

## Usage

``` r
check_untranslated_src(message_data)
```

## Arguments

- message_data:

  A `data.table`, or object convertible to one.

## Value

A `data.table` with columns `call`, `file`, `line_number`, and
`replacement` summarizing the results. `replacement` is `NA` at this
time, i.e., no replacement is provided.

## Details

This diagnostic looks for literal `char` arrays passed to messaging
functions (as identified by
[`translate_package()`](https://michaelchirico.github.io/potools/reference/translate_package.md))
which are not marked for translation (by tagging them for translation
with `_` or `N_` macros). These strings cannot be translated until they
are so marked.

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
message_data = get_message_data(
  tmp_pkg,
  custom_translation_functions = list(src = "ReverseTemplateMessage:2")
)
#> Getting R-level messages...
#> Getting src-level messages...

# now, diagnose the messages for any untranslated messages in C/C++
check_untranslated_src(message_data)
#>                                                                   call
#>                                                                 <char>
#> 1: ReverseTemplateMessage(n, "Reversing a vector with %d elements\\n")
#>             file line_number replacement
#>           <char>       <int>      <char>
#> 1: reverse_int.c          19        <NA>

# cleanup
unlink(tmp_pkg, recursive = TRUE)
rm(pkg, tmp_pkg, message_data)
```
