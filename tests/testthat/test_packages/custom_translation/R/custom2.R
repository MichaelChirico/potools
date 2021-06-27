foo = function(x) {
  stop("A default message")
}

bar = function(x) {
  MyOtherTranslator(target_arg = "you found me too!")
  MyOtherTranslator(1, "found by position")
  MyPluralTranslator(singular = "singular", plural = "plural")
  MyPluralTranslator(1, 2, "another singular", "another plural")
}

