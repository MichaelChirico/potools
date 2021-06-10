# TODO:
#   - Logic for ngettext usage (FWIW, no results on CRAN using this)
#   - Logic to handle messaging functions where the translated string is not the
#     first argument
#   - mark c-format by looking for % ?, though note that this is note advised:
#     https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html#PO-Files
#     will the wrap-up call to update_pkg_po() handle this for us?
#   - check that we're parsing \-continued char arrays correctly:
#     https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html#String-Constants
get_src_messages = function(dir = ".", translation_macro = "_") {
  src_files = package_src_files(dir)
  names(src_files) = src_files

  if (!length(src_files)) {
    return(data.table(
      file = character(),
      call = character(),
      msgid = character(),
      line_number = integer(),
      is_marked_for_translation = logical()
    ))
  }

  msg = rbindlist(lapply(src_files, get_file_src_messages, translation_macro), idcol = "file")
  # TODO(#40): plural messages in src
  msg[ , "type" := "singular"]

  # TODO: R side also uses column_number to sort, but it's ~basically~ not relevant for C... yet
  setorderv(msg, c("type", "file", "line_number"), c(-1L, 1L, 1L))

  msg[type == "singular", "is_repeat" := duplicated(msgid)]
  msg[]
}

get_file_src_messages = function(file, translation_macro = "_") {
  macro_width = nchar(translation_macro)

  contents = readChar(file, file.size(file))
  # as a vector of single characters
  contents_char = preprocess(strsplit(contents, NULL)[[1L]])
  # as a single string
  contents = paste(contents_char, collapse = "")

  newlines_loc = c(0L, as.integer(gregexpr("\n", contents, fixed = TRUE)[[1L]]))

  browser()
  # need to strip out arrays from matching translation arrays, e.g. if we have
  #   Rprintf(_("a really annoying _(msg) nested _(\"array\")"));
  # since we otherwise would try and find _(msg). Note that the latter array would
  #   be easier to skip because the " would have to be escaped, but we may as well skip
  #   this anyway.
  quote_idx <- gregexpr('"', contents, fixed = TRUE)[[1L]]
  # messy (impossible?) to write a regex for an unescaped quote directly --
  #   it should be " preceded by an even (0, 2, 4, ...) number of backslashes
  #   (for an odd number, the pairs become escaped backslashes, the leftover escapes the ").
  #   instead, find all quotes, then mod out the escaped ones with this _relatively_ tame regex:
  escape_quote_idx <- gregexpr('[^\\](?:[\\][\\]){0,}[\\]"', contents)[[1L]]
  quote_idx <- setdiff(quote_idx, escape_quote_idx + attr(escape_quote_idx, "match.length") - 1L)
  if (length(quote_idx) %% 2L != 0L) {
    stop(domain=NA, gettextf(
      'Found an odd number (%d) of unscaped double quotes (") in %s; parsing error.'
    ))
  }
  arrays <- as.data.table(matrix(quote_idx, ncol = 2L, byrow = TRUE))
  setnames(arrays, c('start', 'end'))
  setkeyv(arrays, names(arrays))

  # first find all calls.
  call_idx = gregexpr(sprintf("%s\\s*\\(", C_IDENTIFIER_REGEX), contents)[[1L]]
  calls = data.table(
    call_start = as.integer(call_idx),
    start = as.integer(call_idx) + attr(call_idx, "match.length") - 1L,
    fname = trimws(substring(contents, call_idx, call_idx + attr(call_idx, "match.length") - 2L), "right")
  )
  # remove common false positives for efficiency
  calls = calls[!fname %chin% c(
    "if", "while", "for", "switch", "return",
    "PROTECT", "UNPROTECT", "free", "malloc", "Calloc",
    "TYPEOF", "sizeof", "INHERITS", "type2char", "copyMostAttrib", "LENGTH", "length", "strlen",
    "SET_STRING_ELT", "STRING_ELT", "CHAR", "SET_VECTOR_ELT", "VECTOR_ELT", "SEXPPTR_RO", "STRING_PTR",
    "LOGICAL", "INTEGER", "REAL", "COMPLEX", "CAR", "CDR", "CADR", "checkArity"
  )]
  calls[ , "end" := start]
  calls[ , "spurious" := foverlaps(calls, arrays, which = TRUE)$yid]
  # mod out any that happen to be inside a char array
  calls = calls[is.na(spurious)]
  calls[ , "spurious" := NULL]

  # slightly wasteful (a 10-times nested call will be skipped over many times), but oh well...
  #   the largest file in R-devel (src/library/grDevices/src/devPS.c) has O(4K) calls in it. not terrible.
  #   this step runs in about .75 seconds on that file which is :\
  # more efficient ideas:
  #   (1) iterate over calls. whenever stack_size increases, start recording that call too, then update
  #       all of the calls at once in this table
  #   (2) restrict focus to known calls; problem is that this approach will inevitably miss translated arrays
  #   (3) gregexpr("[()]", contents) to pre-fetch all the parens rather than checking characters individually
  #   (4) implement a C parser/AST builder :|
  #   (5) write this (or the whole function?) in C
  calls[ , "end" := -1L + vapply(
    start,
    skip_parens,
    integer(1L),
    contents_char,
    arrays
  )]
  setkeyv(calls, c('start', 'end'))

  # a bit hard to keep track of what names mean what here.
  #   start,end --> calls$start,calls$end
  #   i.start,i.end --> arrays$start, arrays$end
  call_arrays = foverlaps(arrays, calls)

  translation_idx = calls[ , fname == translation_macro]

  translations = calls[(translation_idx)]
  # includes all arrays, even e.g. '#include "grid.h"', so not necessarily all relevant
  untranslated_idx = translation_arrays[ , is.na(start)]
  untranslated_arrays = translation_arrays[untranslated_idx]
  untranslated_arrays[ , c("start", "end") := NULL]
  setnames(untranslated_arrays, c("start", "end"))

  # simple case: translation is just _("..."), done.
  translation_arrays = translation_arrays[!untranslated_idx]
  singular_array_idx = translation_arrays[ , start + 1 + macro_width == i.start & i.end + 1 == end]
  translations[
    translation_arrays[singular_array_idx],
    on = c('start', 'end'),
    msgid := substring(contents, i.start + 1L, i.end - 1L)
  ]

  translations[
    translation_arrays[
      !(singular_array_idx),
      .(msgid = build_msgid(.BY$start + 1 + macro_width, .BY$end - 1L, i.start, i.end, contents)),
      by= .(start, end)
    ],
    on = c('start', 'end'),
    msgid := i.msgid
  ]

  call_idx = gregexpr(sprintf("%s\\(", C_IDENTIFIER_REGEX), contents)
  calls = data.table(
    start = as.integer()
  )
  msg_match = gregexpr(
    sprintf(
      "(?:^|(?<=[^a-zA-Z_.]))(?<call>%s)\\s*\\(\\s*(?<trans>%s\\s*\\()?\\s*",
      paste(MESSAGE_CALLS, collapse = "|"),
      translation_macro
    ),
    contents,
    perl = TRUE
  )[[1L]]

  # no matches in the file; return empty
  if (length(msg_match) == 1L && msg_match[1L] == -1L) {
    return(data.table(
      call = character(),
      msgid = character(),
      line_number = integer(),
      is_marked_for_translation = logical()
    ))
  }

  msg_start = as.integer(msg_match) + attr(msg_match, "match.length")
  # more verbosely, "is marked for translation"
  is_translated = attr(msg_match, "capture.length")[ , "trans"] > 0L
  # find which line each message starts on
  msg_line = findInterval(msg_match, newlines_loc)

  # inherits msg_match, msg_start, is_translated, contents, contents_char, nn
  nn = length(contents_char)
  get_call_message = function(msg_i) {
    ii = msg_start[msg_i]

    string = ""
    # regex landed us after ( in untranslated calls and after (_( in translated ones
    stack_size = if (is_translated[msg_i]) 2L else 1L
    if (contents_char[ii] == '"') {
      # each iteration of this repeat adds on another char array. recall that in C
      #   "a string " "another " "string"
      #   automatically concatenates to "a string another string" (as a way to facilitate
      #   char arrays spanning several lines). keep accumulating these until the end of
      #   the function argument, i.e., hitting a ',' (multi-argument call)
      #   or a ')' (single-argument call)
      repeat {
        jj = ii + 1L
        # jj starts on the character after the initial "; iterate along until we find the
        #   next _unescaped_ " (which is where jj is when we terminate)
        while (jj <= nn) {
          switch(
            contents_char[jj],
            '"' = break,
            '\\' = { jj = jj + 2L },
            { jj = jj + 1L }
          )
        }
        if (jj > nn) {
          stop("File terminated before char array completed")
        }
        # add this completed array to our string
        string = paste0(string, substr(contents, ii+1L, jj-1L))
        # jump past " and any subsequent whitespace
        jj = skip_white(jj + 1L, contents_char)
        if (jj > nn) {
          stop("File terminated before translation array completed")
        }
        if (contents_char[jj] == ")") {
          stack_size = stack_size - 1L
          jj = jj + 1L
          break
        } else if (contents_char[jj] == ",") {
          jj = jj + 1L
          break
          # could be macro-designated format string like "Item %d of lower (%"PRId64") is greater..."
          #   which needs to come out like "Item %d of lower (%<PRId64>) is greater..." in the .pot
        } else if (grepl(C_IDENTIFIER_1, contents_char[jj])) {
          kk = jj + 1L
          while (grepl(C_IDENTIFIER_REST, contents_char[kk])) { kk = kk + 1L }
          string = paste0(string, "<", substr(contents, jj, kk-1L), ">")
          ii = skip_white(kk, contents_char)
          # e.g. error(_("... %"PRId64"!=%"PRId64), ...);
          if (contents_char[ii] == ")") {
            stack_size = stack_size - 1L
            jj = ii + 1L
            break
          }
        } else if (contents_char[jj] == '"') {
          ii = jj
        } else if (contents_char[jj] == "\\" && jj < nn && contents_char[jj+1L] %chin% c("\n", "\r")) {
          # line continuation, e.g. as seen in src/library/stats/src/optimize.c:686 as of r80365.
          ii = skip_white(jj + 2L, contents_char)
        } else {
          stop('Unexpected sequence -- a char array not followed by whitespace then any of [,)"] or a macro')
        }
      }
    } else {
      jj = ii
    }
    while (jj <= nn && stack_size > 0L) {
      if (contents_char[jj] == "(") {
        stack_size = stack_size + 1L
      } else if (contents_char[jj] == ")") {
        stack_size = stack_size - 1L
      }
      jj = jj + 1L
    }
    if (jj > nn) {
      stop("File terminated before message call completed")
    }

    return(list(call = substr(contents, msg_match[msg_i], jj), msgid = string))
  }

  src_messages = rbindlist(lapply(seq_along(msg_match), get_call_message))
  src_messages[ , "line_number" := msg_line]
  src_messages[ , "is_marked_for_translation" := is_translated]
  src_messages[]
}

# strip comments (specifically, overwrite them to " " so all
#   other character positions are preserved).
# in principle this could try macro expansion too but that's not trivial.
preprocess = function(contents) {
  ii = 1L
  nn = length(contents)
  while (ii < nn - 1L) {
    # skip quotes to avoid skipping "comments" inside char arrays, e.g. for URLs http://...
    if (contents[ii] == '"') {
      ii = ii + 1L
      while (ii < nn - 1L) {
        switch(
          contents[ii],
          '"' = break,
          "\\" = { ii = ii + 2L },
          { ii = ii + 1L }
        )
      }
    } else if (contents[ii] == "/") {
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

skip_white = function(jj, chars) {
  nn = length(chars)
  while (jj <= nn && chars[jj] %chin% c(" ", "\n", "\r", "\t")) { jj = jj + 1L }
  if (jj > nn) stop("Reached end of file while trying to skip whitespace")
  return(jj)
}

skip_parens = function(jj, chars, array_boundaries) {
  if (chars[jj] != "(")
    stop(domain=NA, gettextf("Expected to start from '(', but chars[%d]='%s'", jj, chars[jj]))

  nn = length(chars)
  stack_size = 1L
  jj = jj + 1L
  while (jj <= nn && stack_size > 0L) {
    switch(
      chars[jj],
      ')' = { stack_size = stack_size - 1L; jj = jj + 1L },
      '"' = { jj = array_boundaries[.(jj), end] + 1L },
      { jj = jj + 1L }
    )
  }
  jj
}

build_msgid = function(left, right, starts, ends, contents) {
  # we have one or several macros between `_(` and `)`, between which is "grout".
  #   NB: we could have _("array ending with formatter: %"PRId64). I'm not sure it's possible
  #   for an array to start with a macro, but leave the logic to check here anyway
  grout_left = c(left, ends + 1L)
  grout_right = c(starts - 1L, right)

  # drop the first & last if there's no whitespace between `_(` and `"` or `"` and `)`
  valid_idx = grout_left < grout_right
  grout = character(length(starts) + 1L)
  grout[valid_idx] = substring(contents, grout_left[valid_idx], grout_right[valid_idx])

  # drop spurious whitespace
  grout = gsub("[ \n\r\t]", "", grout)

  # pad macros (e.g. "a formatter with %"PRId64" becomes" --> "a formatter with %<PRId64> becomes")
  macro_idx = nzchar(grout)
  grout[macro_idx] = paste0("<", grout[macro_idx], ">")

  msgid = character(2L * length(starts) + 1L)
  msgid[1L + 2L * seq_along(grout)] = grout
  msgid[2L + 2L * seq_along(starts)] = substring(contents, starts+1L, ends-1L)

  # combine
  paste(msgid, collapse = "")
}

# gleaned from iterating among WRE, src/include/Rinternals.h, src/include/R_ext/{Error.h,Print.h}
MESSAGE_CALLS = c(
  "Rprintf", "REprintf", "Rvprintf", "REvprintf",
  "R_ShowMessage", "R_Suicide",
  "warning", "Rf_warning", "error", "Rf_error"
)

# https://docs.microsoft.com/en-us/cpp/c-language/c-identifiers?view=msvc-160 suggests this
#   ASCII identifier regex, though some other sources suggest a much broader range
C_IDENTIFIER_REGEX = "[_a-zA-Z][_a-zA-Z0-9]{0,30}"
C_IDENTIFIER_1 = "[_a-zA-Z]"
C_IDENTIFIER_REST = "[_a-zA-Z0-9]"
