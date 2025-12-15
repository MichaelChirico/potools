# Check for untranslated messages emitted by cat

Diagnose the R messages in a package to discover the presence of
messages emitted by [`cat()`](https://rdrr.io/r/base/cat.html) which
haven't been translated (i.e., passed through
[`gettext()`](https://rdrr.io/r/base/gettext.html),
[`gettextf()`](https://rdrr.io/r/base/sprintf.html), or
[`ngettext()`](https://rdrr.io/r/base/gettext.html)).

## Usage

``` r
check_untranslated_cat(message_data)
```

## Arguments

- message_data:

  A `data.table`, or object convertible to one.

## Value

A `data.table` with columns `call`, `file`, `line_number`, and
`replacement` summarizing the results.

## Details

The function `cat` is commonly used to emit messages to users (e.g., for
a `verbose` mode), but it is not equipped for translation. Instead,
messages must first be translated and then emitted. Any character
literals found in the package's R code used in `cat` but not translated
will be flagged by this function.

For flagged calls, a potential replacement is offered, built using
`gettext` or `gettextf` (depending on whether one or more `...`
arguments are supplied to `cat`). For the `gettextf` case, the suggested
template is always `%s` (string) since this works for all inputs; the
author should tighten this to the appropriate
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) template marker as
appropriate, for example if the author knows the input is an integer,
use `%d` or `%i` instead of `%s`.

NB: not all `cat` calls are included – in particular, no `cat` call
specifying a non-default `file` are flagged, nor are any where the
supplied `sep` is not a character literal (e.g., `sep=x` instead of
`sep=""`)

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

# now, diagnose the messages for any untranslated strings shown through cat()
check_untranslated_cat(message_data)
#>                                                              call     file
#>                                                            <char>   <char>
#> 1: cat("Launching", format(libname), "/", format(pkgname), "\\n") onLoad.R
#>    line_number
#>          <int>
#> 1:           2
#>                                                                 replacement
#>                                                                      <char>
#> 1: cat(gettextf("Launching %s / %s \\n", format(libname), format(pkgname)))

# cleanup
unlink(tmp_pkg, recursive = TRUE)
rm(pkg, tmp_pkg, message_data)
```
