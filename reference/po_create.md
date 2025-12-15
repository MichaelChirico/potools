# Create a new `.po` file

`po_create()` creates a new `po/{languages}.po` containing the messages
to be translated.

Generally, we expect you to use `po_create()` to create new `.po` files
but if you call it with an existing translation, it will update it with
any changes from the `.pot`. See
[`po_update()`](https://michaelchirico.github.io/potools/reference/po_update.md)
for details.

## Usage

``` r
po_create(languages, dir = ".", verbose = !is_testing())
```

## Arguments

- languages:

  Language identifiers. These are typically two letters (e.g. "en" =
  English, "fr" = French, "es" = Spanish, "zh" = Chinese), but can
  include an additional suffix for languages that have regional
  variations (e.g. "fr_CN" = French Canadian, "zh_CN" = simplified
  characters as used in mainland China, "zh_TW" = traditional characters
  as used in Taiwan.)

- dir:

  Character, default the present directory; a directory in which an R
  package is stored.

- verbose:

  Logical, default `TRUE` (except during testing). Should extra
  information about progress, etc. be reported?
