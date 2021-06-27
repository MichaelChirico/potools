void foo(SEXP x) {
  error(_("a standard src message"));
}

void bar(SEXP y) {
  MySrcTranslator("An untranslated string");
  MyArg4Translator(1, 2, 3, "Another untranslated string");
}
