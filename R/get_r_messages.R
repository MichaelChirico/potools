# Spiritual cousin version of tools::{x,xn}gettext. Instead of iterating the AST
#   as R objects, do so from the parse data given by utils::getParseData().
get_r_messages <- function(dir, custom_translation_functions = NULL, is_base = FALSE, style = c("base", "explicit")) {
  style <- match.arg(style)

  expr_data <- rbindlist(lapply(parse_r_files(dir, is_base), getParseData), idcol = 'file')
  # R-free package (e.g. a data package) fails, #56
  if (!nrow(expr_data)) return(r_message_schema())

  setkeyv(expr_data, "file")
  # strip quotation marks now rather than deal with that at write time.
  expr_data[token == 'STR_CONST', text := clean_text(text)]

  setindexv(expr_data, c("file", "line1", "col1", "line2", "col2"))
  setindexv(expr_data, c("file", "id"))
  setindexv(expr_data, c("file", "parent"))
  setindexv(expr_data, c("token", "text"))

  # skip # notranslate lines / blocks
  # comments assigned here & re-used below
  # NB: at the R level, each COMMENT token is restricted to a single line
  comments = expr_data[token == "COMMENT"]
  setkeyv(comments, c("file", "line1"))
  expr_data = exclude_untranslated(expr_data, comments)

  # on the XML tree, messaging calls look like
  # <expr>    <- parent of 'msg_call_neighbors'
  #   <expr>  <- 'msg_call_exprs'; might also be a more complicated expression here e.g. for base::stop()
  #     <SYMBOL_FUNCTION_CALL>stop</SYMBOL_FUNCTION_CALL>
  #   </expr>
  #   <OP-LEFT-PAREN>(</OP-LEFT-PAREN>
  #   <!-- here is an unnamed argument -->
  #   <expr> ... </expr>
  #   <OP-COMMA>,</OP-COMMA>
  #   <!-- the following three are a named argument -->
  #   <SYMBOL_SUB>domain</SYMBOL_SUB>
  #   <EQ_SUB>=</SYMBOL_SUB>
  #   <expr> ... </expr>
  #   <!-- mix and match those two types indefinitely -->
  #   <OP-RIGHT-PAREN>)</OP-RIGHT-PAREN>
  # </expr>
  dots_funs <- domain_dots_funs(use_conditions = style == "base")
  fmt_funs <- domain_fmt_funs(use_conditions = style == "base")

  singular_strings = rbind(
    get_dots_strings(expr_data, dots_funs, NON_DOTS_ARGS),
    # treat gettextf separately since it takes a named argument, and we ignore ...
    get_named_arg_strings(expr_data, fmt_funs, c(fmt = 1L), recursive = TRUE),
    # TODO: drop recursive=FALSE option now that exclude= is available? main purpose of recursive=
    #   was to block cat(gettextf(...)) usage right?
    get_dots_strings(expr_data, 'cat', c("file", "sep", "fill", "labels", "append"), recursive = FALSE)
  )
  plural_strings = get_named_arg_strings(expr_data, 'ngettext', c(msg1 = 2L, msg2 = 3L), plural = TRUE)

  if (style == "explicit") {
    tr_ <- get_dots_strings(expr_data, 'tr_', character(), recursive = TRUE)
    tr_n <- get_named_arg_strings(expr_data, 'tr_n', c(singular = 2L, plural = 3L), plural = TRUE)

    singular_strings <- rbind(singular_strings, tr_)
    plural_strings <- rbind(plural_strings, tr_n)
  }

  # for plural strings, the ordering within lines doesn't really matter since there's only one .pot entry,
  #   so just use the parent's location to get the line number
  plural_strings[ , id := parent]

  # TODO: how hard would it be to run it all at once? i.e. create a fun-arg lookup table and "vectorize" over that
  #   all at once and for all functions? it would speed things up a bit & also get around the hand-waving now where
  #   we just specify NON_DOTS_ARGS the same for all the translators (even though they have different signatures)
  if (length(custom_translation_functions)) {
    custom_params = parse_r_keywords(custom_translation_functions)

    singular_strings = rbind(
      singular_strings,
      rbindlist(lapply(
        custom_params$singular$dots,
        function(params) get_dots_strings(expr_data, params$fname, params$excluded_args)
      )),
      rbindlist(lapply(
        custom_params$singular$named,
        function(params) get_named_arg_strings(expr_data, params$fname, params$args)
      ))
    )

    plural_strings = rbind(
      plural_strings,
      rbindlist(lapply(
        custom_params$plural,
        function(params) get_named_arg_strings(expr_data, params$fname, params$args, plural = TRUE)
      ))
    )
  } else {
    custom_params = list()
  }

  msg = rbind(
    singular = singular_strings,
    plural = plural_strings,
    idcol = 'type'
  )

  if (!nrow(msg)) return(r_message_schema())

  msg_files = unique(msg$file)
  if (is_base) {
    paths <- file.path(dir, 'R', msg_files)
    share_idx <- startsWith(msg_files, 'share/R')
    paths[share_idx] <- file.path(dir, '../../..', msg_files[share_idx])
  } else {
    paths <- file.path(dir, 'R', msg_files)
  }
  file_lines = lapply(normalizePath(paths), readLines, warn = FALSE)
  names(file_lines) = msg_files


  msg[
    expr_data, on = c('file', parent = 'id'),
    `:=`(line1 = i.line1, col1 = i.col1, line2 = i.line2, col2 = i.col2)
  ]
  # need to strip comments for build_call, see #59.
  msg[ , by = c('file', 'line1', 'col1', 'line2', 'col2'),
    call := build_call(
      file_lines[[.BY$file]],
      # match any comments between line1 & line2
      comments[.(.BY$file, .BY$line1:.BY$line2), .SD, nomatch=NULL],
      params = .BY
    )
  ]

  # these are the parent's stats
  msg[ , c('parent', 'line1', 'line2', 'col1', 'col2') := NULL]

  # now add the child's stats to order within the file
  msg[
    expr_data, on = c('file', 'id'),
    `:=`(line_number = i.line1, column_number = i.col1)
  ]

  # descending 'type' so that "singular" comes before "plural".
  # NB: forder uses C order for file, which happens to match the base behavior to set LC_COLLATE=C
  # in_subdir is done to (hackily) match the within-directory ordering done by tools::update_pkg_po(); see #104
  msg[ , "in_subdir" := grepl("/", file, fixed = TRUE)]
  if (is_base) {
    # share/R messages come after the other base/R source files
    msg[ , "in_share" := grepl("share/R", file, fixed = TRUE)]
    setorderv(msg, c("type", "in_subdir", "in_share", "file", "line_number", "column_number"), c(-1L, rep(1L, 5L)))
  } else {
    setorderv(msg, c("type", "in_subdir", "file", "line_number", "column_number"), c(-1L, 1L, 1L, 1L, 1L))
  }
  # kept id, column_number to get order within lines; can drop now
  msg[ , c('id', 'column_number', 'in_subdir') := NULL]

  msg[type == 'singular', 'msgid' := escape_string(trimws(msgid))]
  msg[type == 'plural', 'msgid_plural' := lapply(msgid_plural, escape_string)]

  # keep duplicates & define this field, in case duplicates are also part of diagnostic checks
  msg[ , 'is_repeat' := FALSE]
  # TODO: skip empty strings, check why this isn't counted as duplicated:
  #   You are trying to join data.tables where %s has 0 columns.
  msg[type == 'singular', 'is_repeat' := duplicated(msgid)]

  known_translators = c(dots_funs, 'ngettext', fmt_funs, get_fnames(custom_params))
  if (style == "explicit") {
    known_translators <- c(known_translators, "tr", "tr_")
  }
  msg[ , 'is_marked_for_translation' := fname %chin% known_translators]

  # TODO: assume custom translators are translated? or maybe just check the regex?
  msg[ , "is_templated" := fname %chin% fmt_funs]
  msg[ , "fname" := NULL]

  msg[]
}

# parse the R files in a directory.
parse_r_files = function(dir, is_base) {
  # somehow on windows I was seeing absolute paths with \ but paths
  #   from list.files as / -- normalizePath makes it consistent
  r_files = list_package_files(dir, 'R', subsubdirs = c('unix', 'windows'), pattern = "(?i)\\.r$")
  out = lapply(normalizePath(file.path(dir, 'R', r_files)), parse, keep.source=TRUE)
  if (is_base) {
    r_share_dir = file.path(dir, "../../../share")
    if (!dir.exists(file.path(r_share_dir, 'R'))) {
      # templated to share with src-side message
      stopf(
        # nolint next: line_length_linter.
        "Translation of the 'base' package can only be done on a local mirror of r-devel. Such a copy has a file %s at the top level that is required to proceed.",
        "share/R/REMOVE.R"
      )
    }
    share_files = list_package_files(r_share_dir, 'R', pattern = "(?i)\\.r$")
    out = c(
      out,
      lapply(normalizePath(file.path(r_share_dir, 'R', share_files)), parse, keep.source = TRUE)
    )
    names(out) = c(r_files, file.path('share', 'R', share_files))
    return(out)
  }
  names(out) = r_files
  return(out)
}

# inspired by the --keyword argument in gettext, but customized to make sense for R.
# specifically there are two ways to specify a function for translation:
#   (1) f:arg1|n1[,arg2|n2] - named arguments & positions, e.g. gettextf:fmt|1 and ngettext:msg1|2,msg2|3
#   (2) f:...\arg1,...,argn - varargs & excluded arguments, e.g. stop:...\call.,domain or message:...\domain,appendLF
parse_r_keywords = function(spec) {
  keyval = setDT(tstrsplit(spec, ":", fixed = TRUE))
  if (ncol(keyval) != 2L) {
    idx <- if (ncol(keyval) == 1L) seq_along(spec) else which(is.na(keyval$V2))
    stopf(
      # nolint next: line_length_linter.
      "Invalid custom translator specification(s): %s.\nAll inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.",
      toString(spec[idx])
    )
  }

  # not a proper test of R identifiers, but that should be OK, they just won't be found in the result -- no error
  named_idx = grepl("^[a-zA-Z0-9._]+\\|[0-9]+$", keyval$V2)
  plural_idx = grepl("^[a-zA-Z0-9._]+\\|[0-9]+,[a-zA-Z0-9._]+\\|[0-9]+$", keyval$V2)
  dots_idx = grepl("^[.]{3}[\\](?:[a-zA-Z0-9._]+,)*[a-zA-Z0-9._]+$", keyval$V2)
  if (any(idx <- !named_idx & !dots_idx & !plural_idx)) {
    stopf(
      # nolint next: line_length_linter.
      "Invalid custom translator specification(s): %s.\nAll inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.",
      toString(spec[idx])
    )
  }

  list(
    singular = list(
      dots = lapply(
        which(dots_idx),
        function(ii) list( # nolint: brace_linter.
          fname = keyval$V1[ii],
          excluded_args = strsplit(gsub("^[.]{3}[\\]", "", keyval$V2[ii]), ",", fixed = TRUE)[[1L]]
        )
      ),
      named = lapply(
        which(named_idx),
        function(ii) {
          arg_keyval = strsplit(keyval$V2[ii], "|", fixed = TRUE)[[1L]]
          # regex above ensures as.integer() will succeed here
          list(fname = keyval$V1[ii], args = setNames(as.integer(arg_keyval[2L]), arg_keyval[1L]))
        }
      )
    ),
    plural = lapply(
      which(plural_idx),
      function(ii) {
        arg_keyval = tstrsplit(strsplit(keyval$V2[ii], ",", fixed = TRUE)[[1L]], "|", fixed = TRUE)
        # regex above ensures as.integer() will succeed here
        list(fname = keyval$V1[ii], args = setNames(as.integer(arg_keyval[[2L]]), arg_keyval[[1L]]))
      }
    )
  )
}

# wrapper for extracting the fnames from the parse_r_keywords object
get_fnames = function(params) {
  unlist(c(
    lapply(params$singular$dots, `[[`, "fname"),
    lapply(params$singular$named, `[[`, "fname"),
    lapply(params$plural, `[[`, "fname")
  ))
}

exclude_untranslated = function(expr_data, comments) {
  # single-line exclusions
  inline_idx <- grepl("# notranslate", comments$text, fixed = TRUE)
  if (any(inline_idx)) {
    expr_data = expr_data[
      !comments[(inline_idx)],
      on = c("file", "line1")
    ]
  }

  starts = comments[grepl("# notranslate start", text, fixed = TRUE)]
  ends = comments[grepl("# notranslate end", text, fixed = TRUE)]

  ranges = build_exclusion_ranges(starts, ends)
  if (nrow(ranges)) {
    expr_data = expr_data[!ranges, on = .(file == file, line1 > start, line1 < end)]
  }

  expr_data
}

# these functions all have a domain= argument. taken from the xgettext source, but could be
#   refreshed with the following (skipping bindtextdomain and .makeMessage):
# for (obj in ls(BASE <- asNamespace('base'))) {
#     if (!is.function(f <- get(obj, envir = BASE))) next
#     if (is.null(f_args <- args(f))) next
#     if (any(names(formals(f_args)) == 'domain')) cat(obj, '\n')
# }
domain_dots_funs <- function(use_conditions = TRUE) {
  c(
    "gettext",
    if (use_conditions) c("stop", "warning", "message", "packageStartupMessage")
  )
}

domain_fmt_funs <- function(use_conditions = TRUE) {
  paste0(domain_dots_funs(use_conditions), "f")
}

#
NON_DOTS_ARGS = c("domain", "call.", "appendLF", "immediate.", "noBreaks.")

# for functions (e.g. domain_dots_funs) where we extract strings from ... arguments
get_dots_strings = function(expr_data, funs, arg_names,
                            exclude = c('gettext', 'gettextf', 'ngettext'),
                            recursive = TRUE) {
  call_neighbors = get_call_args(expr_data, funs)
  call_neighbors = drop_suppressed_and_named(call_neighbors, expr_data, arg_names)

  # as we search the AST "below" call_neighbors, drop whichever of the excluded expr parents we find.
  #   practically speaking, this is how we disassociate "hi" from stop() in stop(gettext("hi"))
  exclude_parents = expr_data[
    expr_data[token == 'SYMBOL_FUNCTION_CALL' & text %chin% exclude, .(file, parent)],
    on = c('file', id = 'parent'),
    .(file, id = x.parent)
  ]
  # lop off these expr so they can't be found later
  expr_data = expr_data[!exclude_parents, on = c('file', 'id')]

  # drop '(', ')', ',', and now-orphaned SYMBOL_SUB/EQ_SUB
  call_neighbors = call_neighbors[token == 'expr'][!exclude_parents, on = c('file', 'id')]
  setnames(call_neighbors, 'parent', 'ancestor')

  get_strings_from_expr(call_neighbors, expr_data, recursive = recursive)
}

# for functions (e.g. ngettext, gettextf) where we extract strings from named arguments
# arg_names should be a key-value vector, the names give the arguments' names, the values
#   give their position in the signature (e.g. gettextf's target argument is 'fmt', hence
#   we pass `c(fmt = 1L)`; for ngettext, we'd use `c(msg1 = 2L, msg2 = 3L)`).
# NB: this is a poor man's version of match.call(), whose actual dynamics are much harder
#   to imitate. I think it's reasonable to work in some simple cases & expect end users to
#   conform to that if they want this to work correctly (famous last words...)
get_named_arg_strings = function(expr_data, fun, args, recursive = FALSE, plural = FALSE) {
  call_neighbors = get_call_args(expr_data, fun)
  call_neighbors = drop_suppressed_and_named(call_neighbors, expr_data, "domain")
  setnames(call_neighbors, 'parent', 'ancestor')

  string_expr = call_neighbors[
    , by = c('file', 'fname', 'ancestor'),
    {
      idx = shift(token, fill = '') == 'SYMBOL_SUB' & shift(text, fill = '') %chin% names(args)
      if (any(idx) & !all(matched <- names(args) %chin% text[token == 'SYMBOL_SUB'])) {
        stopf(
          # nolint next: line_length_linter.
          "In line %s of %s, found a call to %s that names only some of its messaging arguments explicitly. Expected all of [%s] to be named. Please name all or none of these arguments.",
          expr_data[.BY, on = c(id = 'ancestor'), line1[1L]], .BY$file, .BY$fname, toString(names(args)[!matched])
        )
      }
      .(id = id[idx])
    }
  ]

  call_neighbors = call_neighbors[!string_expr, on = c('file', 'ancestor')]
  if (nrow(call_neighbors)) {
    string_expr = rbind(
      string_expr,
      call_neighbors[token == 'expr', by = c('file', 'fname', 'ancestor'), .(id = id[args + 1L])]
    )
  }

  strings = get_strings_from_expr(string_expr, expr_data, recursive = recursive)
  # TODO: do this directly in get_strings_from_expr()? be more careful that messages are in the right order?
  if (plural && nrow(strings)) {
    strings = strings[
      , by = c('file', 'parent', 'fname'),
      .(id = id[1L], msgid = NA_character_, msgid_plural = list(msgid))
    ]
  }
  strings
}

get_strings_from_expr = function(target_expr, expr_data, recursive = FALSE) {
  strings = string_schema()
  while (nrow(target_expr) > 0L) {
    target_expr = expr_data[
      target_expr, on = c('file', parent = 'id'),
      .(file, ancestor = i.ancestor, fname = i.fname, id = x.id, token = x.token, text = x.text)
    ]
    strings = rbind(
      strings,
      target_expr[
        token == 'STR_CONST',
        .(file, parent = ancestor, id, fname, msgid = text)
      ],
      fill = TRUE
    )
    # much cleaner to do this tiny check a small number (e.g. nesting level of 10-15) times
    #   repetitively rather than make a whole separate branch for the once-and-done case
    if (!recursive) break
    target_expr = target_expr[token == "expr"]
  }
  strings
}

get_call_args = function(expr_data, calls) {
  msg_call_exprs = expr_data[
    expr_data[token == "SYMBOL_FUNCTION_CALL" & text %chin% calls],
    on = c('file', id = 'parent'),
    .(file, call_id = i.id, call_expr_id = x.id, call_parent_id = x.parent, fname = i.text)
  ]
  # if not, just skip to a join to get the right schema & return
  if (nrow(msg_call_exprs)) {
    msg_call_expr_children = expr_data[
      msg_call_exprs,
      on = c('file', parent = 'call_expr_id'),
      .(file, parent = x.parent, token = x.token)
    ]
    # filter out calls like l$stop("x"), keep calls like base::stop("x")
    msg_call_expr_children = msg_call_expr_children[
      , by = .(file, parent),
      # filter .SD here to ensure one row per file/parent, otherwise we get duplicates below
      if (.N == 1L || 'NS_GET' %chin% token) .SD[token == 'SYMBOL_FUNCTION_CALL']
    ]
    msg_call_exprs = msg_call_exprs[
      msg_call_expr_children,
      on = c('file', call_expr_id = 'parent'),
      .(file, call_id, call_expr_id, call_parent_id, fname)
    ]
  }
  msg_call_neighbors = expr_data[
    msg_call_exprs, on = c('file', parent = 'call_parent_id'),
    .(file, id = x.id, parent = x.parent, token = x.token, text = x.text, fname)
  ]
  msg_call_neighbors[token %chin% c('expr', 'SYMBOL_SUB')]
}

get_named_args = function(calls_data, expr_data, target_args) {
  # NB: use this instead of flipping the join order since that will find
  #   a SYMBOL_SUB for every expr rather than an expr for every SYMBOL_SUB. The
  #   former might return multiple rows if domain= is followed by more named args.
  #   important in the current logic because we do drop_suppressed before
  #   running this again, at which point there will be orphaned SYMBOL_SUB
  # summary: rolling backwards from the expr id to the corresponding SYMBOL_SUB id
  named_args = calls_data[token == "expr"][
    calls_data[token == "SYMBOL_SUB" & text %chin% target_args],
    on = c('file', 'parent', 'id'), roll = -Inf,
    .(file, parent, id = x.id, fname = x.fname, arg_name = i.text)
  ]
  named_args[expr_data, on = c('file', id = 'parent'), arg_value := i.text][]
}

drop_suppressed_and_named = function(calls_data, expr_data, target_args) {
  named_args = get_named_args(calls_data, expr_data, target_args)
  # strip away calls where domain=NA by dropping the common parent's immediate children;
  #   nested expressions without domain=NA will still be there
  calls_data = calls_data[
    # text == "NA" implicitly filtering non-literal arg values since those <expr> nodes will have empty text
    !named_args[arg_name == "domain" & arg_value == "NA"],
    on = c('file', 'parent')
  ]
  # strip away any other expr associated with named args (note join to id, not parent)
  calls_data[!named_args, on = c('file', 'id')]
}

build_call = function(lines, comments, params) {
  if (params$line1 == params$line2) {
    return(substr(adjust_tabs(lines[params$line1]), params$col1, params$col2))
  } else {
    lines = lines[params$line1:params$line2]

    # substring not substr here so we can eschew providing
    #   last=nchar(lines[1L]) because we'd need to recalculate it after adjust_tabs()
    lines[1L] = substring(adjust_tabs(lines[1L]), params$col1)
    lines[length(lines)] = substring(adjust_tabs(lines[length(lines)]), 1L, params$col2)

    # strip comments, _after_ getting the tab-adjusted column #s
    for (ii in seq_len(nrow(comments))) {
      # we've already subset lines, so line numbers have to be re-mapped
      adj_line_idx = comments$line1[ii] - params$line1 + 1L
      # column number has to be re-mapped relative to col1 as well, but only on line1 itself
      col_adj = if (adj_line_idx == 1L) params$col1 else 1L
      lines[adj_line_idx] = substr(
        lines[adj_line_idx],
        1L,
        comments$col1[ii] - col_adj
      )
    }

    # strip internal whitespace across lines in the call
    #   NB: eventually, this will need to be smarter about multi-line STR_CONST...
    #       for now, just wave hands around those....
    return(paste(trimws(lines), collapse = " "))
  }
}

adjust_tabs = function(l) {
  while ((idx <- regexpr("\t", l, fixed = TRUE)) > 0L) {
    l = sub("\t", strrep(" ", 9L - (idx %% 8L)), l, fixed = TRUE)
  }
  l
}

# the text column in getParseData() needs some tidying:
#  1. the actual quotes are kept, e.g. text='"a string"'
#  2. "unescape" strings (e.g. "\\n" --> \n) so that trimws() works. note that
#     later we "re-apply" encodeString() so this feels redundant, but necessary for trimws()
clean_text = function(x) {
  # See ?Quotes for the rules governing string constants. this regex has two parts:
  #   the first for raw strings, the second for "normal" strings. regex considers there
  #   to be two capture groups, hence the need for \\1\\2. the two parts are mutually
  #   exclusive, so only one group is ever matched, the other is always empty.
  # Handle raw strings separately, lest we catch a string like "abc)--" that _looks_
  #   like a raw string on the RHS but actually is not one. also consider a real
  #   pain like r'("abc")' and 'r"(abc)"' -- whatever we do, if we strip away one first,
  #   then the other, we'll end up with the wrong strings.
  # h/t https://stackoverflow.com/a/8303552/3576984 for (?s) for . to match \n in part 2
  x = gsub(
    '^[rR]["\'][-]*[\\[({](.*)[\\])}][-]*["\']$|^(?s)["\'](.*)["\']$',
    '\\1\\2', x, perl = TRUE
  )
  # there may be others, these are the main ones... lookback since actual escaped \\n shouldn't be replaced.
  #   an non-perl approach with capture groups like (^|[^\\])[\\]n fails on consecutive \\n\\n due to greediness
  x = gsub("(?:^|(?<![\\\\]))[\\\\]n", "\n", x, perl = TRUE)
  x = gsub("(?:^|(?<![\\\\]))[\\\\]t", "\t", x, perl = TRUE)
  # maybe stop() instead? \r is blocked by gettext...
  x = gsub("(?:^|(?<![\\\\]))[\\\\]r", "\r", x, perl = TRUE)
  # quotes that are escaped _in the text_ are not escaped _in R_ (i.e., after parsing),
  #   e.g. in 'a string with an \"escaped\" quote', the escapes for " disappear after parsing. See #128
  x = gsub("(?:^|(?<![\\\\]))[\\\\](['\"])", "\\1", x, perl = TRUE)
  x = gsub('\\\\', '\\', x, fixed = TRUE)
  return(x)
}

string_schema = function() {
  data.table(
    file = character(),
    # needed to build the call
    parent = integer(),
    # needed to order the strings correctly within the call
    id = integer(),
    fname = character(),
    msgid = character(),
    msgid_plural = list()
  )
}

# the schema for empty edge cases
r_message_schema = function() {
  data.table(
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
}
