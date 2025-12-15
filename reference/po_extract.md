# Extract messages for translation into a `.pot` file

`po_extract()` scans your package for strings to be translated and saves
them into a `.pot` template file (in the package's `po` directory). You
should never modify this file by hand; instead modify the underlying
source code and re-run `po_extract()`.

If you have existing translations, call
[`po_update()`](https://michaelchirico.github.io/potools/reference/po_update.md)
after `po_extract()` to update them with the changes.

## Usage

``` r
po_extract(
  dir = ".",
  custom_translation_functions = list(),
  verbose = !is_testing(),
  style = NULL
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

- verbose:

  Logical, default `TRUE` (except during testing). Should extra
  information about progress, etc. be reported?

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

## Value

The extracted messages as computed by
[`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md),
invisibly.
