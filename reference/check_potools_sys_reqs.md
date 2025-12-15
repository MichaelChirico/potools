# Check if the proper system utilities for running package translation are installed

potools uses the same gettext command line tools that R itself does to
run translation. These are required for translation to work properly;
this function is mainly for testing use & checks whether the current
environment is equipped for translation.

## Usage

``` r
check_potools_sys_reqs(which = SYSTEM_REQUIREMENTS)
```

## Arguments

- which:

  Which requirements to test for. Defaults to all of the command-line
  utilities on which potools relies, namely,

  - `msgmerge`

  - `msgfmt`

  - `msginit`

  - `msgconv`

## Value

`TRUE` if the system is ready for translation, otherwise a message
suggesting how to proceed.

## Details

Specifically, potools relies on these command-line utilities:

## See also

[`tools::update_pkg_po()`](https://rdrr.io/r/tools/update_pkg_po.html)

## Author

Michael Chirico
