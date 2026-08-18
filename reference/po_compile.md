# Compile `.po` files to `.mo`

This function compiles the plain text `.po` files that translators work
with into the binary `.mo` files that are installed with packages and
used for live translations.

## Usage

``` r
po_compile(dir = ".", package = NULL, lazy = TRUE, verbose = !is_testing())
```

## Arguments

- dir:

  Path to package root directory.

- package:

  Name of package. If not supplied, read from `DESCRIPTION`.

- lazy:

  If `TRUE`, only `.mo` functions that are older than `.po` files be
  updated

- verbose:

  Logical, default `TRUE` (except during testing). Should extra
  information about progress, etc. be reported?
