foo = function(x) {
  stop("A default message")
}

bar = function(x) {
  MyTranslator("you found me!")
  MyDotsTranslator(excluded = "untranslated argument", "a translated", "argument")
}

