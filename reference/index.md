# Package index

## Translate a package

- [`translate_package()`](https://michaelchirico.github.io/potools/reference/translate_package.md)
  : Interactively provide translations for a package's messages

- [`po_extract()`](https://michaelchirico.github.io/potools/reference/po_extract.md)
  :

  Extract messages for translation into a `.pot` file

- [`po_create()`](https://michaelchirico.github.io/potools/reference/po_create.md)
  :

  Create a new `.po` file

- [`po_compile()`](https://michaelchirico.github.io/potools/reference/po_compile.md)
  :

  Compile `.po` files to `.mo`

- [`po_update()`](https://michaelchirico.github.io/potools/reference/po_update.md)
  :

  Update all `.po` files with changes in `.pot`

- [`po_explain_plurals()`](https://michaelchirico.github.io/potools/reference/po_explain_plurals.md)
  : Explain plural message criteria verbally

## Check messages

- [`check_cracked_messages()`](https://michaelchirico.github.io/potools/reference/check_cracked_messages.md)
  : Check for cracked messages more suitable for templating
- [`check_potools_sys_reqs()`](https://michaelchirico.github.io/potools/reference/check_potools_sys_reqs.md)
  : Check if the proper system utilities for running package translation
  are installed
- [`check_untranslated_cat()`](https://michaelchirico.github.io/potools/reference/check_untranslated_cat.md)
  : Check for untranslated messages emitted by cat
- [`check_untranslated_src()`](https://michaelchirico.github.io/potools/reference/check_untranslated_src.md)
  : Check for cracked messages in C/C++ sources

## Internal helpers

- [`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md)
  : Extract user-visible messages from a package
- [`write_po_file()`](https://michaelchirico.github.io/potools/reference/write_po_file.md)
  [`po_metadata()`](https://michaelchirico.github.io/potools/reference/write_po_file.md)
  [`format(`*`<po_metadata>`*`)`](https://michaelchirico.github.io/potools/reference/write_po_file.md)
  [`print(`*`<po_metadata>`*`)`](https://michaelchirico.github.io/potools/reference/write_po_file.md)
  : Write a .po or .pot file corresponding to a message database
