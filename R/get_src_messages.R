get_src_messages = function(dir = ".", translation_macros = c("_", "N_"), use_base_rules = FALSE, is_base = FALSE) {
  if (is_base) {
    potfiles_loc <- file.path(dir, "../../../po/POTFILES")
    if (!file.exists(potfiles_loc)) {
      stop("Translation of the 'base' package can only be done on a local mirror of r-devel. Such a copy has a file po/POTFILES at the top level that is required to proceed.")
    }
    # NB: also skips blank lines
    src_files <- grep("^[^#]", readLines(potfiles_loc), value = TRUE)
    # in R.pot, file paths are given relative to the top level (and so include src/, where other packages drop src/)
    dir = dirname(dirname(potfiles_loc))
  } else {
    # alternative would be to handle the if (!use_base_rules) branch in write_po_file(),
    #   which would require keeping track of column numbers
    #   until that point so as to keep the file order correct in both use_base_rules cases
    src_files = list_package_files(dir, 'src', c(if (!use_base_rules) 'cairo', 'windows'), "(?i)\\.(c|cc|cpp|m|mm)$")
    dir = file.path(dir, 'src')
  }

  if (!length(src_files)) return(src_msg_schema())

  msg = rbindlist(
    lapply(normalizePath(file.path(dir, src_files)), get_file_src_messages, translation_macros),
    idcol = "file"
  )
  msg[ , "file" := src_files[file]]
  # TODO(#40): plural messages in src
  msg[ , "type" := "singular"]

  # line continuation mid-array is treated as blank, #91. we might be able to handle this in pre-process but
  #   that risks throwing off line numbers later on... \r? for windows of course
  msg[ , "msgid" := gsub("[\\]\r?\n", "", msgid)]
  # "\'" is treated the same as '\'', per
  #   https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html#String-Constants
  msg[ , "msgid" := gsub("\\'", "'", msgid, fixed = TRUE)]

  # TODO: write this
  # internal line breaks & spacing minimized
  # msg[ , call := cleanup_call(call)]

  # TODO: R side also uses column_number to sort, but it's ~basically~ not relevant for C... yet
  # in_subdir done for #104; see also the comment in get_r_messages.R. For base, POTFILES gives the right order.
  if (is_base) {
    msg[ , "in_subdir" := TRUE]
  } else {
    msg[ , "in_subdir" := grepl("/", file, fixed = TRUE)]
  }
  setorderv(msg, c("type", "in_subdir", "file", "line_number"), c(-1L, 1L, 1L, 1L))
  msg[ , "in_subdir" := NULL]

  msg[type == "singular", "is_repeat" := duplicated(msgid)]
  msg[]
}

get_file_src_messages = function(file, translation_macros = c("_", "N_")) {
  contents = readChar(file, file.size(file))
  # as a vector of single characters
  contents_char = preprocess(strsplit(contents, NULL)[[1L]])
  # as a single string
  contents = paste(contents_char, collapse = "")

  newlines_loc = c(0L, as.integer(gregexpr("\n", contents, fixed = TRUE)[[1L]]))

  # need to strip out arrays from matching translation arrays, e.g. if we have
  #   Rprintf(_("a really annoying _(msg) nested _(\"array\")"));
  # since we otherwise would try and find _(msg). Note that the latter array would
  #   be easier to skip because the " would have to be escaped, but we may as well skip
  #   this anyway.
  quote_idx <- gregexpr('"', contents, fixed = TRUE)[[1L]]
  # no arrays --> nothing to do
  if (quote_idx[1L] < 0L) return(file_msg_schema())

  # messy (impossible?) to write a regex for an unescaped quote directly --
  #   it should be " preceded by an even (0, 2, 4, ...) number of backslashes
  #   (for an odd number, the pairs become escaped backslashes, the leftover escapes the ").
  #   instead, find all quotes, then mod out the escaped ones with this much simpler regex:
  escape_quote_idx <- gregexpr('[\\]+"', contents)[[1L]]
  valid_escape_quote_idx <- attr(escape_quote_idx, "match.length") %% 2L == 0L
  # also remove literal char elements '"'
  quote_char_idx <- gregexpr("'\"'", contents, fixed = TRUE)[[1L]]
  quote_idx <- setdiff(
    quote_idx,
    c(
      escape_quote_idx[valid_escape_quote_idx] + attr(escape_quote_idx, "match.length")[valid_escape_quote_idx] - 1L,
      quote_char_idx + attr(quote_char_idx, "match.length") - 2L
    )
  )
  if (length(quote_idx) %% 2L != 0L) {
    stop(domain=NA, call. = FALSE, gettextf(
      'Parsing error: found an odd number (%d) of unscaped double quotes (") in %s.',
      length(quote_idx), file
    ))
  }
  arrays <- as.data.table(matrix(quote_idx, ncol = 2L, byrow = TRUE))
  setnames(arrays, c('array_start', 'array_end'))
  setkeyv(arrays, names(arrays))

  # first find all calls.
  call_idx = gregexpr(sprintf("%s\\s*\\(", C_IDENTIFIER_REGEX), contents)[[1L]]
  calls = data.table(
    call_start = as.integer(call_idx),
    paren_start = as.integer(call_idx) + attr(call_idx, "match.length") - 1L,
    fname = trimws(safe_substring(contents, call_idx, call_idx + attr(call_idx, "match.length") - 2L), "right")
  )
  # remove common false positives for efficiency
  calls = calls[!fname %chin% c(
    "if", "while", "for", "switch", "return",
    "PROTECT", "UNPROTECT", "free", "malloc", "Calloc", "memcpy",
    "TYPEOF", "sizeof", "INHERITS", "type2char", "LENGTH", "length", "XLENGTH", "xlength", "strlen",
    "copyMostAttrib", "getAttrib", "setAttrib", "R_compute_identical",
    "SET_STRING_ELT", "STRING_ELT", "CHAR", "ENC2UTF8", "SET_VECTOR_ELT", "VECTOR_ELT", "SEXPPTR_RO", "STRING_PTR",
    "PRIMVAL", "RAW", "LOGICAL", "INTEGER", "INTEGER_RO", "REAL", "REAL_RO", "COMPLEX",
    "CAR", "CDR", "CADR", "checkArity",
    "isFactor", "isLogical", "isInteger", "isNumeric", "isReal", "isComplex", "isString",
    "isS4", "isNull", "ISNA", "ISNAN", "R_FINITE"
  )]
  calls[ , "paren_end" := paren_start]

  calls[ , "non_spurious" := is.na(foverlaps(calls, arrays, by.x = c('paren_start', 'paren_end'), which = TRUE)$yid)]
  # mod out any that happen to be inside a char array
  calls = calls[(non_spurious)]
  calls[ , "non_spurious" := NULL]

  # slightly wasteful (a 10-times nested call will be skipped over many times), but oh well...
  #   the largest file in R-devel (src/library/grDevices/src/devPS.c) has O(4K) calls in it. not terrible.
  #   this step runs in about .75 seconds on that file which is :\
  # more efficient ideas:
  #   (1) iterate over calls. whenever stack_size increases, start recording that call too, then update
  #       all of the calls at once in this table
  #   (2) restrict focus to known calls; problem is that this approach will inevitably miss translated arrays
  #   (3) gregexpr("[()]", contents) to pre-fetch all the parens rather than checking characters individually
  #       * could be used to make a rudimentary AST? associate each ( / ) pair with ascendants/descendants?
  #   (4) implement a C parser/AST builder :|
  #   (5) write this (or the whole function?) in C
  calls[ , "paren_end" := -1L + vapply(
    paren_start,
    skip_parens,
    integer(1L),
    contents_char,
    arrays,
    file,
    newlines_loc
  )]
  setkeyv(calls, c('paren_start', 'paren_end'))

  translation_idx = calls[ , fname %chin% translation_macros]
  translations = calls[(translation_idx), .(fname, paren_start, paren_end)]
  # run here in case nrow(translations)=0
  translations[ , "is_marked_for_translation" := TRUE]

  # nomatch=NULL: drop arrays appearing outside _any_ call (for now?)
  call_arrays = foverlaps(arrays, calls, by.x = c('array_start', 'array_end'), nomatch=NULL)

  calls = calls[(!translation_idx)]

  # translations like _("abc"), i.e., just one array and no gaps at all
  singular_array_idx = call_arrays[ , paren_start == array_start - 1L & paren_end == array_end + 1]
  translation_array_idx = call_arrays[ , fname %chin% translation_macros]
  translations[
    call_arrays[translation_array_idx & singular_array_idx],
    on = c('paren_start', 'paren_end'),
    c("msgid", "array_start") := .(safe_substring(contents, i.array_start + 1L, i.array_end - 1L), i.array_start)
  ]

  translations[
    call_arrays[
      translation_array_idx & !singular_array_idx,
      # record the location of the first array to tag the source line correctly, see #148
      .(array_start = array_start[1L],
        msgid = build_msgid(.BY$paren_start, .BY$paren_end, array_start, array_end, contents)),
      by= .(paren_start, paren_end)
    ],
    on = c('paren_start', 'paren_end'),
    c("msgid", "array_start") := .(i.msgid, i.array_start)
  ]

  call_arrays = call_arrays[(!translation_array_idx)]
  translations[
    call_arrays,
    on = .(paren_start > paren_start, paren_end < paren_end),
    c('call', 'call_start') := .(safe_substring(contents, i.call_start, i.paren_end), i.call_start)
  ]

  # find cases like error(_(msg)); (i.e., arrays passed as variables instead of literals)
  # assign paren_start of _(msg) (though it shouldn't matter since these shouldn't end up in the .pot file)
  translations[
    is.na(msgid),
    c('call', 'call_start', 'array_start') := calls[
      .SD,
      on = .(paren_start < paren_start, paren_end > paren_end),
      .(safe_substring(contents, x.call_start, x.paren_end), x.call_start, i.paren_start)
    ]
  ]

  # find cases like #define MSG _("msg") where the array doesn't show up in any "outer" call
  translations[
    !is.na(msgid) & is.na(call),
    c("call_start", "call") := {
      call_start = paren_start - nchar(fname)
      .(call_start, substring(contents, call_start, paren_end))
    }
  ]


  # drop calls associated with a translation, and anything but "known" messaging functions
  call_arrays = call_arrays[!translations, on = 'call_start']
  call_arrays = call_arrays[fname %chin% MESSAGE_CALLS]

  # TODO: handle calls with multiple distinct arrays, e.g. foo("abc", "def")
  singular_array_idx = call_arrays[ , paren_start == array_start - 1L & paren_end == array_end + 1]
  call_arrays = rbind(
    call_arrays[
      (singular_array_idx),
      .(
        call_start, paren_start, paren_end,
        array_start,
        msgid = safe_substring(contents, array_start + 1L, array_end - 1L),
        call = safe_substring(contents, call_start, paren_end)
      )
    ],
    call_arrays[
      (!singular_array_idx),
      .(
        array_start = array_start[1L],
        msgid = build_msgid(.BY$paren_start, .BY$paren_end, array_start, array_end, contents),
        call = safe_substring(contents, .BY$call_start, .BY$paren_end)
      ),
      by = .(call_start, paren_start, paren_end)
    ]
  )
  call_arrays[ , "is_marked_for_translation" := FALSE]

  # use paren_start for translated arrays to get the line number right when the call & array lines differ
  src_messages = rbind(
    translations[ , .(msgid, call, array_start, is_marked_for_translation)],
    call_arrays[ , .(msgid, call, array_start, is_marked_for_translation)]
  )

  src_messages[ , "line_number" := findInterval(array_start, newlines_loc)]
  src_messages[ , "array_start" := NULL]
  setcolorder(src_messages, c("msgid", "line_number", "call", "is_marked_for_translation"))
  src_messages[]
}

# strip comments (specifically, overwrite them to " " so all
#   other character positions are preserved).
# in principle this could try macro expansion too but that's not trivial.
preprocess = function(contents) {
  ii = 1L
  nn = length(contents)
  while (ii < nn - 1L) {
    switch(
      contents[ii],
      # skip quotes to avoid skipping "comments" inside char arrays, e.g. for URLs http://...
      '"' = {
        ii = ii + 1L
        while (ii < nn - 1L) {
          switch(
            contents[ii],
            '"' = break,
            "\\" = { ii = ii + 2L },
            { ii = ii + 1L }
          )
        }
      },
      # " as a char ('"') also presents an issue for char array detection
      "'" = { ii = ii + 2L + (contents[ii+1L] == '\\')},
      "/" = {
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
      },
      # handling platform-robust template macros. See #154 for some discussion/exploration.
      #   we ignore some possibilities involving LEAST/FAST/MAX/PTR and only allow 32/64 widths, for simplicity.
      "P" = {
        if (
          ii < nn-6L
          && (ii == 1L || !contents[ii-1L] %chin% C_IDENTIFIER_CHARS)
          && contents[ii+1L] == "R" && contents[ii+2L] == "I"
          && contents[ii+3L] %chin% c("d", "i", "o", "u", "x", "X")
          && (
            (contents[ii+4L] == "3" && contents[ii+5L] == "2")
            || (contents[ii+4L] == "6" && contents[ii+5L] == "4")
          # ensure the identifier ends here, and it's a symbol, not a call
          && (ii == nn-7L || !contents[ii+6L] %chin% C_IDENTIFIER_CHARS)
          )
        ) {
          contents = c(head(contents, ii-1L), '"', '<', contents[ii:(ii+5L)], '>', '"', tail(contents, -ii-5L))
          ii = ii + 9L
        }
      }
    )
    ii = ii + 1L
  }
  return(contents)
}

skip_parens = function(ii, chars, array_boundaries, file, newlines_loc) {
  nn = length(chars)
  stack_size = 1L
  jj = ii + 1L
  while (jj <= nn && stack_size > 0L) {
    switch(
      chars[jj],
      '(' = { stack_size = stack_size + 1L; jj = jj + 1L },
      ')' = { stack_size = stack_size - 1L; jj = jj + 1L },
      '"' = { jj = array_boundaries[.(jj), array_end] + 1L },
      # don't get mixed up by '(' or ')', #112
      # jump one further for e.g. '\0', #135
      "'" = { jj = jj + 3L + (chars[jj+1L] == "\\") },
      { jj = jj + 1L }
    )
  }
  # file & newlines_loc both only needed for this error region which is a bit awkward
  if (jj > nn) stop(domain = NA, call. = FALSE, gettextf(
    "Parsing error: unmatched parentheses in %s starting from line %d",
    file, findInterval(ii, newlines_loc)
  ))

  jj
}

build_msgid = function(left, right, starts, ends, contents) {
  # we have one or several "bricks" (char arrays) between `_(` and `)`, between which is "grout".
  grout_left = c(left + 1L, ends + 1L)
  grout_right = c(starts - 1L, right - 1L)

  # drop the first & last if there's no whitespace between any "brick"s (e.g. `_("` or `"a""b"` or `")`)
  valid_idx = grout_left < grout_right
  grout = character(length(starts) + 1L)
  grout[valid_idx] = safe_substring(contents, grout_left[valid_idx], grout_right[valid_idx])

  # drop spurious whitespace. first drop line continuations (\), then also strip arguments.
  #   I think this assumes translated arrays only occur at one argument in the call...
  grout = gsub("\\", "", grout, fixed = TRUE)
  grout = gsub("^(?:.*,)?[ \n\r\t]*|[ \n\r\t]*(?:,.*)?$", "", grout)

  # Only the first array is extracted from ternary operator usage inside _(), #154
  # IINM, ternary operator usage has to come first, i.e., "abc" (test ? "def" : "ghi") won't parse
  if (endsWith(grout[1L], "?")) {
    return(safe_substring(contents, starts[1L]+1L, ends[1L]-1L))
  }

  # any unknown macros are not expanded and the recorded array is cut off on encountering one,
  #   unless that macro comes between `_(` and the first literal array
  #   (c.f. xgettext output on _(xxx"abc""def") vs _("abc"xxx"def") vs _("abc""def"xxx))
  if (length(keep_idx <- which(nzchar(grout[-1L])))) {
    starts = head(starts, keep_idx)
    ends = head(ends, keep_idx)
  }

  paste(safe_substring(contents, starts+1L, ends-1L), collapse = "")
}

# gleaned from iterating among WRE, src/include/Rinternals.h, src/include/R_ext/{Error.h,Print.h}
MESSAGE_CALLS = c(
  "Rprintf", "REprintf", "Rvprintf", "REvprintf",
  "R_ShowMessage", "R_Suicide",
  "warning", "Rf_warning", "error", "Rf_error",
  "snprintf"
)

# https://docs.microsoft.com/en-us/cpp/c-language/c-identifiers?view=msvc-160 suggests this
#   ASCII identifier regex, though some other sources suggest a much broader range
C_IDENTIFIER_REGEX = "[_a-zA-Z][_a-zA-Z0-9]{0,30}"
C_IDENTIFIER_1 = "[_a-zA-Z]"
C_IDENTIFIER_REST = "[_a-zA-Z0-9]"
# ASCII-sorted
C_IDENTIFIER_CHARS = c('(', as.character(0:9), LETTERS, '_', letters)

src_msg_schema = function() data.table(
  type = character(),
  file = character(),
  msgid = character(),
  line_number = integer(),
  call = character(),
  is_repeat = logical(),
  is_marked_for_translation = logical()
)

file_msg_schema = function() data.table(
  msgid = character(),
  line_number = integer(),
  call = character(),
  is_marked_for_translation = logical()
)
