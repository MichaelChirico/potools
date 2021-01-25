# TODO:
#   - Logic for ngettext usage
#   - Logic to handle messaging functions where the translated string is not the
#     first argument
get_src_messages = function(file, translation_macro = "_") {
  contents = readChar(file, file.size(file))
  # as a vector of single characters
  contents_char = preprocess(strsplit(contents, NULL)[[1L]])
  # as a single string
  contents = paste(contents_char, collapse = "")

  newlines_loc = as.integer(gregexpr("\n", contents, fixed = TRUE)[[1L]])

  # regex breakdown:
  #   (?:^|(?<=[^a-zA-Z_.])) : going for \\b but that doesn't work exactly;
  #     we want to be sure the matched function isn't part of a larger identifier
  #     (e.g. matching xRprintf or terror instead of Rprintf or error, resp.)
  #   (?:%s)\\s*\\(\\s* : %s is a |-separated list of matching calls, e.g.
  #     Rprintf|error|warning; the identifier can be followed by whitespace, but
  #     we make sure to include the "(" which assures we're at a call to the function.
  #     Terminal \\s* assures that the match "lands" just when the char array should
  #     appear, if it's there.
  #   (%s\\s*\\()?\\s* : %s is the translation_macro, _ by recommendation in WRE;
  #     this assures we capture arrays which are already marked for translation
  #     correctly. Presence/absence of this is crucial for informing the user about
  #     potentially-untranslated char arrays, so we capture this group.
  msg_match = gregexpr(
    sprintf(
      "(?:^|(?<=[^a-zA-Z_.]))(?:%s)\\s*\\(\\s*(%s\\s*\\()?\\s*",
      paste(MESSAGE_CALLS, collapse = "|"),
      translation_macro
    ),
    contents,
    perl = TRUE
  )[[1L]]

  # inherits msg_match, contents, contents_char, nn
  nn = length(contents_char)
  get_message = function(ii) {
    string = character(1L)
    if (contents_char[ii] == '"') {
      jj = ii + 1L
      # each iteration of this repeat adds on another char array. recall that in C
      #   "a string " "another string"
      #   automatically concatenates to "a string another string" (as a way to facilitate
      #   char arrays spanning several lines). keep accumulating these until the end of
      #   the function argument, i.e., hitting a ',' (multi-argument call)
      #   or a ')' (single-argument call)
      repeat {
        # jj starts on the character after the initial "; iterate along until we find the
        #   next _unescaped_ " (which is where jj is when we terminate)
        while (jj <= nn && contents_char[jj] != '"' && contents_char[jj - 1L] != '\\') { jj = jj + 1L }
        if (jj > nn) {
          stop("File terminated before char array completed")
        }
        # add this completed array to our string
        string = paste0(string, substr(contents, ii+1L, jj-1L))
        # jump past "
        jj = jj + 1L
        # now jump past any whitespace
        while (jj <= nn && contents_char[jj] %chin% c(" ", "\n", "\t")) { jj = jj + 1L }
        if (jj > nn) {
          stop("File terminated before message call completed")
        }
        if (contents_char[jj] %chin% c(")", ",")) {
          return(string)
        } else if (contents_char[jj] != '"') {
          stop('Unexpected sequence -- a char array not followed by whitespace then any of [,)"]')
        }
      }
    } else {
      return(string)
    }
  }

  src_messages = vapply(as.integer(msg_match) + attr(msg_match, "match.length"), get_message, character(1L))

}

# strip comments (specifically, overwrite them to " " so all
#   other character positions are preserved).
# in principle this could try macro expansion too but that's not trivial.
preprocess = function(contents) {
  ii = 1L
  nn = length(contents)
  while (ii < nn - 1L) {
    if (contents[ii] == "/") {
      jj = 0L
      if (contents[ii + 1L] == "/") {
        jj = ii + 2L
        while (jj <= nn && contents[jj] != "\n") { jj = jj + 1L }
        contents[ii:(jj - 1L)] = " "
        ii = jj
      } else if (contents[ii + 1L] == "*") {
        jj = ii + 2L
        while (jj <= nn - 1L && (contents[jj] != "*" || contents[jj + 1L] != "/")) { jj = jj + 1L }
        idx = ii:(jj + 1L)
        contents[idx] = fifelse(contents[idx] == "\n", "\n", " ")
        ii = jj + 1L
      }
    }
    ii = ii + 1L
  }
  return(contents)
}

# gleaned from iterating among WRE, src/include/Rinternals.h, src/include/R_ext/{Error.h,Print.h}
MESSAGE_CALLS = c(
  "Rprintf", "REprintf", "Rvprintf", "REvprintf",
  "R_ShowMessage", "R_Suicide",
  "warning", "Rf_warning", "error", "Rf_error"
)

