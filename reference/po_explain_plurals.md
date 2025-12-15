# Explain plural message criteria verbally

The `nplural` syntax in .po file metadata can be hard to grok, even for
native speakers. This function tries to de-mystify this by providing
verbal expressions of which numbers apply to which index in the `msgstr`
array.

## Usage

``` r
po_explain_plurals(language, index)
```

## Arguments

- language:

  A single locale code. See
  [`translate_package()`](https://michaelchirico.github.io/potools/reference/translate_package.md)
  for details.

- index:

  Optional. If supplied, a 0-based index to explain for a given
  language. If not supplied, all plurals for the supplied language are
  described.
