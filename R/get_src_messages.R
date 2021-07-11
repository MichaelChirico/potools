get_src_messages = function(
  dir = ".",
  custom_translation_functions = NULL,
  is_base = FALSE
) {
  if (is_base) {
    potfiles_loc <- file.path(dir, "../../../po/POTFILES")
    if (!file.exists(potfiles_loc)) {
      # templated to share with R-side message
      stopf(
        "Translation of the 'base' package can only be done on a local mirror of r-devel. Such a copy has a file %s at the top level that is required to proceed.",
        "po/POTFILES"
      )
    }
    # NB: also skips blank lines
    src_files <- grep("^[^#]", readLines(potfiles_loc), value = TRUE)
    # in R.pot, file paths are given relative to the top level (and so include src/, where other packages drop src/)
    dir = dirname(dirname(potfiles_loc))
  } else {
    src_files = list_package_files(dir, 'src', c('cairo', 'windows'), "(?i)\\.(c|cc|cpp|m|mm)$")
    dir = file.path(dir, 'src')
  }

  if (!length(src_files)) return(src_msg_schema())

  custom_params = parse_src_keywords(custom_translation_functions)

  msg = rbindlist(
    lapply(normalizePath(file.path(dir, src_files)), get_file_src_messages, custom_params),
    idcol = "file"
  )

  msg[ , "type" := fifelse(fname == 'ngettext', "plural", "singular")]

  # TODO: R side also uses column_number to sort, but it's ~basically~ not relevant for C... yet
  # in_subdir done for #104; see also the comment in get_r_messages.R.
  if (is_base) {
    # For base, POTFILES gives the right order, so use the file number to sort before assigning file name
    # NB: in R, singular comes before plural; not so here. strings are simply extracted in order.
    setorderv(msg, c("file", "line_number"), c(1L, 1L))
    msg[ , "file" := src_files[file]]
  } else {
    msg[ , "file" := src_files[file]]
    msg[ , "in_subdir" := grepl("/", file, fixed = TRUE)]
    setorderv(msg, c("in_subdir", "file", "line_number"), c(1L, 1L, 1L))
    msg[ , "in_subdir" := NULL]
  }

  # line continuation mid-array is treated as blank, #91. we might be able to handle this in pre-process but
  #   that risks throwing off line numbers later on... \r? is for windows of course
  msg[ , "msgid" := gsub("[\\]\r?\n", "", msgid)]
  # "\'" is treated the same as '\'', per
  #   https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html#String-Constants
  msg[ , "msgid" := gsub("\\'", "'", msgid, fixed = TRUE)]

  # all calls to certain functions are marked as templated, regardless of template markers, #137
  msg[ , "is_templated" :=
         fname %chin% TEMPLATE_CALLS
         | grepl(SPRINTF_TEMPLATE_REGEX, msgid, perl=TRUE)
         | vapply(msgid_plural, function(str) any(grepl(SPRINTF_TEMPLATE_REGEX, str, perl=TRUE)), logical(1L))
      ]
  msg[ , "fname" := NULL]

  # TODO: write this
  # internal line breaks & spacing minimized
  # msg[ , call := cleanup_call(call)]

  msg[type == "singular", "is_repeat" := duplicated(msgid)]
  msg[]
}

get_file_src_messages = function(file, custom_params = NULL) {
  contents = readChar(file, file.size(file))
  exclusion_pos = gregexpr("# notranslate( (start|end))?", contents, perl=TRUE)[[1L]]
  # as a vector of single characters
  contents_char = preprocess(strsplit(contents, NULL)[[1L]])
  # as a single string
  contents = paste(contents_char, collapse = "")

  # NB: should still be fine to look only for \n on windows
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
    stopf(
      'Parsing error: found an odd number (%d) of unscaped double quotes (") in %s.',
      length(quote_idx), file
    )
  }
  arrays <- as.data.table(matrix(quote_idx, ncol = 2L, byrow = TRUE))
  setnames(arrays, c('array_start', 'array_end'))
  setkeyv(arrays, names(arrays))

  # first find all calls.
  # TODO: maybe leverage the match_parens() call which runs a nearly identical gregexpr?
  call_idx = gregexpr(sprintf("%s\\s*\\(", C_IDENTIFIER_REGEX), contents)[[1L]]
  calls = data.table(
    call_start = as.integer(call_idx),
    paren_start = as.integer(call_idx) + attr(call_idx, "match.length") - 1L,
    fname = trimws(safe_substring(contents, call_idx, call_idx + attr(call_idx, "match.length") - 2L), "right")
  )
  # remove common false positives for efficiency
  calls = calls[!fname %chin% COMMON_NON_MESSAGE_CALLS]

  # mod out any that happen to be inside a char array
  calls = calls[is_outside_char_array(paren_start, arrays)]

  if (!nrow(calls)) return(file_msg_schema())

  calls[match_parens(file, contents, arrays), on = 'paren_start', "paren_end" := i.paren_end]

  setkeyv(calls, c('paren_start', 'paren_end'))

  translation_idx = calls[ , fname %chin% DEFAULT_MACROS]
  translations = calls[(translation_idx), .(fname, paren_start, paren_end)]
  # run here in case nrow(translations)=0
  translations[ , "is_marked_for_translation" := TRUE]

  # nomatch=NULL: drop arrays appearing outside _any_ call (for now?)
  call_arrays = foverlaps(arrays, calls, by.x = c('array_start', 'array_end'), nomatch=NULL)

  calls = calls[(!translation_idx)]

  # Vectorize over calls with just one array for efficiency
  # NB: down the route using `if (.N == 1) .I` lies anguish & suffering from edge cases.
  singular_array_idx = call_arrays[ , if (.N == 1L) TRUE else rep(FALSE, .N), by = .(paren_start, paren_end)]$V1
  translation_array_idx = call_arrays[ , fname %chin% DEFAULT_MACROS]
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
    c('call', 'call_start', 'fname') := .(safe_substring(contents, i.call_start, i.paren_end), i.call_start, i.fname)
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
  if (length(custom_params)) {
    # NB: could potentially save & do this just once (vs now: once per file), but it should be very cheap to do
    message_calls = rbind(
      DEFAULT_MESSAGE_CALLS,
      custom_params
    )
    setkeyv(message_calls, "fname")
  } else {
    message_calls = DEFAULT_MESSAGE_CALLS
  }
  call_arrays = call_arrays[fname %chin% message_calls$fname]

  # skip checking for singular arrays, since build_msgid_plural is doing a check
  #   on if that singular array should be translated to begin with (by vetting against message_calls)
  # NB: it's a bit sleight-of-hand-y to call it build_msgid_plural() when it's more like "extract_messages"
  untranslated = call_arrays[
    ,
    .(
      array_start = array_start[1L],
      msgid = NA_character_,
      msgid_plural = build_msgid_plural(
        .BY$fname, .BY$paren_start, .BY$paren_end,
        array_start, array_end,
        contents, message_calls
      ),
      call = safe_substring(contents, .BY$call_start, .BY$paren_end)
    ),
    by = .(fname, call_start, paren_start, paren_end)
  ]

  untranslated[ , "is_marked_for_translation" := fname %chin% c("dgettext", "ngettext")]
  untranslated[
    lengths(msgid_plural) == 1L,
    c('msgid', 'msgid_plural') := .(vapply(msgid_plural, `[`, character(1L), 1L), list(NULL))
  ]

  # awkward workaround to error(ngettext("a", "b")) returning error:"a"
  # TODO: revisit build_msgid_plural not to need this step
  # msgid != "NA" for #184... could be revisited. I'm too motivated to fix it quickly.
  untranslated = untranslated[!msgid %chin% unlist(msgid_plural) & (is.na(msgid) | msgid != "NA")]

  # use paren_start for translated arrays to get the line number right when the call & array lines differ
  src_messages = rbind(
    translations[ , .(msgid, msgid_plural = list(), fname, call, array_start, is_marked_for_translation)],
    untranslated[ , .(msgid, msgid_plural, fname, call, array_start, is_marked_for_translation)]
  )

  src_messages[ , "line_number" := findInterval(array_start, newlines_loc)]
  if (length(exclusion_pos)) {
    # build_exclusion_ranges designed for R where file & line1 are inherited from getParseData, so conform to that
    exclusions = data.table(
      file = file,
      line1 = findInterval(as.integer(exclusion_pos), newlines_loc),
      capture_lengths = attr(exclusion_pos, "capture.length")[ , 1L]
    )
    src_messages = drop_excluded(src_messages, exclusions[is_outside_char_array(exclusion_pos, arrays)])
  }
  src_messages[ , "array_start" := NULL]
  setcolorder(src_messages, c("msgid", "msgid_plural", "line_number", "fname", "call", "is_marked_for_translation"))
  src_messages[]
}

# TODO: support custom plural messaging functions in src? only really did so on the base side since it's
#   no added complexity to do so, whereas it would be here.
# TODO: allow a default of 1 (like xgettext) instead of needing to supply :1 every time?
parse_src_keywords = function(spec) {
  if (!length(spec)) return(data.table(NULL))

  if (!all(idx <- grepl("[a-zA-Z0-9_]+:[0-9]+", spec))) {
    stopf(
      "Invalid custom translator specification(s): %s.\nAll inputs for src must be key-value pairs like fn:arg1. Custom plural messagers are not yet supported.",
      toString(spec[!idx])
    )
  }

  keyval = setDT(tstrsplit(spec, ":", fixed = TRUE))
  setnames(keyval, c("fname", "str_arg"))
  set(keyval, NULL, "str_arg", as.integer(keyval$str_arg))
  keyval
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
          # blank the comment, not the newline; also don't overwrite \r on Windows
          contents[ii:(jj - 1L - (contents[jj-1L] == "\r"))] = " "
          ii = jj
        } else if (contents[ii + 1L] == "*") {
          jj = ii + 2L
          while (jj <= nn - 1L && (contents[jj] != "*" || contents[jj + 1L] != "/")) { jj = jj + 1L }
          idx = ii:(jj + 1L)
          # <3 windows
          contents[idx] = fifelse(contents[idx] %chin% c("\n", "\r"),  contents[idx], " ")
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

is_outside_char_array = function(char_pos, arrays) {
  if (char_pos[1L] < 0L) return(logical())

  charDT = data.table(start = char_pos, end = char_pos)
  is.na(foverlaps(charDT, arrays, by.x = c('start', 'end'), which = TRUE)$yid)
}

drop_excluded = function(msg_data, exclusions) {
  if (any(inline_idx <- exclusions$capture_lengths == 0L)) {
    msg_data = msg_data[!line_number %in% exclusions[(inline_idx), line1]]
    exclusions = exclusions[(!inline_idx)]
  }
  if (nrow(exclusions)) {
    # somewhat hacky (and not at all robust to being flexible about the range marker), but 6 = nchar(" start")
    start_idx = exclusions$capture_lengths == 6L
    ranges = build_exclusion_ranges(exclusions[(start_idx)], exclusions[(!start_idx)])
    msg_data = msg_data[!ranges, on = .(line_number > start, line_number < end)]
  }
  msg_data
}

# this approach is ~8x faster than the earlier approach of iterating over calls' lparens & finding its rparen.
# other ideas for potential ways to find paren_end:
#   (1) iterate over calls. whenever stack_size increases, start recording that call too, then update
#       all of the calls at once in this table
#   (2) restrict focus to known calls; problem is that this approach will inevitably miss translated arrays
#   (3) implement a C parser/AST builder :|
#   (4) write this (or the whole function?) in C
match_parens = function(file, contents, arrays) {
  # also exclude char arrays like '(' or ')'; note the difficulty of including/excluding correctly in
  #   foo('(') with regex alone -- a naive approach might exclude any of the three parens.
  paren_locs = setdiff(
    gregexpr("[()]", contents, perl = TRUE)[[1L]],
    gregexpr("(?<=')[()](?=')", contents, perl = TRUE)[[1L]]
  )
  all_parens = data.table(paren_start = paren_locs, paren_end = paren_locs, key = c("paren_start", "paren_end"))
  # exclude parens inside arrays, which needn't be balanced
  in_array = foverlaps(arrays, all_parens, nomatch = NULL, which = TRUE)$yid
  if (length(in_array)) {
    all_parens = all_parens[-in_array]
  }

  # goal: associate lparens with their corresponding rparen. rparens assigned end=-1 for the %in% step below to work
  all_parens[ , "lparen" := substring(contents, paren_start, paren_start) == "("]
  # TODO: this still fails for a file like )))((( -- we'd have to stop() if there's no update within the while loop...
  all_parens[ , "paren_end" := fifelse(lparen, NA_integer_, -1L)]

  # idea: match all lparens immediately followed by rparen. next, ignore matched parens & repeat for unmatched parens.
  while (any(idx <- is.na(all_parens$paren_end))) {
    # an ugly hack to fix #199
    # TODO(#209): a less hacky version of this, but requires serious refactor I'm afraid.
    if (sum(idx) == 1L) {
      all_parens[is.na(paren_end), "paren_end" := paren_start + 1L]
      break
    }
    all_parens[idx | (!lparen & !paren_start %in% paren_end), `:=`(
      next_paren = shift(lparen, type="lead"),
      next_start = shift(paren_start, type="lead")
    )]
    all_parens[idx & lparen, "paren_end" := fcase(!next_paren, next_start)]
    all_parens[ , c("next_paren", "next_start") := NULL]
  }
  return(all_parens[(lparen)])
}

build_msgid = function(left, right, starts, ends, contents) {
  grout = get_grout(left, right, starts, ends, contents)
  grout = gsub("[\n\r\t ]", "", grout)

  # Only the first array is extracted from ternary operator usage inside _(), #154
  # IINM, ternary operator usage has to come first, i.e., "abc" (test ? "def" : "ghi") won't parse
  if (endsWith(grout[1L], "?")) {
    return(safe_substring(contents, starts[1L]+1L, ends[1L]-1L))
  }

  # any unknown macros are not expanded and the recorded array is cut off on encountering one,
  #   unless that macro comes between `_(` and the first literal array
  #   (c.f. xgettext output on _(xxx"abc""def") vs _("abc"xxx"def") vs _("abc""def"xxx))
  if (length(keep_idx <- which(nzchar(grout[-1L])))) {
    starts = head(starts, keep_idx[1L])
    ends = head(ends, keep_idx[1L])
  }

  paste(safe_substring(contents, starts+1L, ends-1L), collapse = "")
}

# this could probably go for more stress testing. it didn't _crash_ on base, but
#   I haven't vetted whether it gets the results right always (just for the one dgettext() usage in base)
build_msgid_plural = function(fun, left, right, starts, ends, contents, message_calls) {
  if (!length(starts)) return(list())

  grout = get_grout(left, right, starts, ends, contents)
  target_arg = message_calls[.(fun), str_arg]

  # this is not really the same as the implicit xgettext behavior of --keyword=dngettext:2,3
  if (target_arg == 0L) {
    msgid_plural = character()
    msgid = ''
    n_grout = length(grout)
    for (ii in 2:length(grout)) {
      msgid = paste0(msgid, safe_substring(contents, starts[ii-1L]+1L, ends[ii-1L]-1L))
      if (ii == n_grout || grepl(',', grout[ii], fixed = TRUE)) {
        msgid_plural = c(msgid_plural, msgid)
        msgid = ''
      }
    }
  } else {
    arg_i = 1L
    ii = 1L
    while (arg_i != target_arg) {
      # nocov start
      if (arg_i > target_arg) stopf(
        "Logic for detecting argument calls broken at: %s%s; please report.",
        fun, substring(contents, left, right)
      )
      # nocov end
      # bold assumption: no nested commas. let's see if it works out...
      commas = gregexpr(",", grout[ii], fixed=TRUE)[[1L]]
      if (commas[1L] > 0L) arg_i = arg_i + length(commas)
      ii = ii + 1L
    }
    jj = ii
    while (jj < length(grout) && !grepl(",", grout[jj], fixed = TRUE)) { jj = jj + 1L }
    msgid = paste(safe_substring(contents, starts[ii:jj - 1L]+1L, ends[ii:jj - 1L]-1L), collapse = '')
    msgid_plural = msgid
  }
  return(list(msgid_plural))
}

get_grout = function(left, right, starts, ends, contents) {
  # we have one or several "bricks" (char arrays) between `(` and `)`, between which is "grout".
  grout_left = c(left + 1L, ends + 1L)
  grout_right = c(starts - 1L, right - 1L)

  # drop the first & last if there's no whitespace between any "brick"s (e.g. `_("` or `"a""b"` or `")`)
  valid_idx = grout_left < grout_right
  grout = character(length(starts) + 1L)
  grout[valid_idx] = safe_substring(contents, grout_left[valid_idx], grout_right[valid_idx])

  # drop spurious whitespace. first drop line continuations (\), then also strip arguments.
  #   I think this assumes translated arrays only occur at one argument in the call...
  grout = gsub("\\", "", grout, fixed = TRUE)
  grout
}

DEFAULT_MACROS = c("_", "N_")

# gleaned from iterating among WRE, src/include/Rinternals.h, src/include/R_ext/{Error.h,Print.h}
DEFAULT_MESSAGE_CALLS = data.table(
  fname = c(
    "ngettext",
    "Rprintf", "REprintf", "Rvprintf", "REvprintf",
    "R_ShowMessage", "R_Suicide",
    "warning", "Rf_warning", "error", "Rf_error",
    "dgettext", # NB: xgettext ignores the domain (first arg) when extracting templates, so we don't bother checking either
    "snprintf"
  ),
  str_arg = c(0L, rep(1L, 10L), 2L, 3L),
  key = 'fname'
)

TEMPLATE_CALLS = "snprintf"

COMMON_NON_MESSAGE_CALLS = sort(c(
  "if", "while", "for", "switch", "return",
  "PROTECT", "UNPROTECT", "free", "malloc", "Calloc", "memcpy",
  "TYPEOF", "sizeof", "INHERITS", "type2char", "LENGTH", "length", "XLENGTH", "xlength", "strlen",
  "copyMostAttrib", "getAttrib", "setAttrib", "R_compute_identical",
  "SET_STRING_ELT", "STRING_ELT", "CHAR", "ENC2UTF8", "SET_VECTOR_ELT", "VECTOR_ELT", "SEXPPTR_RO", "STRING_PTR",
  "PRIMVAL", "RAW", "LOGICAL", "INTEGER", "INTEGER_RO", "REAL", "REAL_RO", "COMPLEX",
  "CAR", "CDR", "CADR", "checkArity",
  "isFactor", "isLogical", "isInteger", "isNumeric", "isReal", "isComplex", "isString",
  "isS4", "isNull", "ISNA", "ISNAN", "R_FINITE"
))

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
  msgid_plural = list(),
  line_number = integer(),
  call = character(),
  is_repeat = logical(),
  is_marked_for_translation = logical(),
  is_templated = logical()
)

file_msg_schema = function() data.table(
  msgid = character(),
  msgid_plural = list(),
  line_number = integer(),
  fname = character(),
  call = character(),
  is_marked_for_translation = logical()
)
