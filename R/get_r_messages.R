# Spiritual cousin version of tools::{x,xn}gettext. Instead of iterating the AST
#   as R objects, do so from the parse data given by utils::getParseData().
get_r_messages <- function (dir) {
  expr_data <- rbindlist(lapply(parse_r_files(dir), getParseData), idcol = 'file')
  # R-free package (e.g. a data package) fails, #56
  if (!nrow(expr_data)) return(r_message_schema())

  setkeyv(expr_data, "file")
  # strip quotation marks now rather than deal with that at write time.
  expr_data[token == 'STR_CONST', text := clean_text(text)]

  setindexv(expr_data, c("file", "line1", "col1", "line2", "col2"))
  setindexv(expr_data, c("file", "id"))
  setindexv(expr_data, c("file", "parent"))
  setindexv(expr_data, c("token", "text"))

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
  singular_strings = rbind(
    get_dots_strings(expr_data, DOMAIN_DOTS_FUNS, NON_DOTS_ARGS),
    # treat gettextf separately since it takes a named argument, and we ignore ...
    get_named_arg_strings(expr_data, 'gettextf', 'fmt'),
    # TODO: drop recursive=FALSE option now that exclude= is available? main purpose of recursive=
    #   was to block cat(gettextf(...)) usage right?
    get_dots_strings(expr_data, 'cat', c("file", "sep", "fill", "labels", "append"), recursive = FALSE)
  )

  plural_strings = get_named_arg_strings(expr_data, 'ngettext', c('msg1', 'msg2'), plural = TRUE)
  # for plural strings, the ordering within lines doesn't really matter since there's only one .pot entry,
  #   so just use the parent's location to get the line number
  plural_strings[ , id := parent]

  msg = rbind(
    singular = singular_strings,
    plural = plural_strings,
    idcol = 'type'
  )

  # drop empty strings. we could do this earlier but it's messier.
  #   TODO: can we just strip these STR_CONST from expr_data with no
  #         other repercussions?
  msg = msg[type == 'plural' | nzchar(msgid)]

  if (!nrow(msg)) return(r_message_schema())

  msg_files = unique(msg$file)
  file_lines = lapply(msg_files, readLines, warn = FALSE)
  names(file_lines) = msg_files

  # need to strip comments for build_call, see #59.
  # NB: at the R level, each COMMENT token is restricted to a single line
  comments = expr_data[token == "COMMENT"]
  setkeyv(comments, c("file", "line1"))

  msg[
    expr_data, on = c('file', parent = 'id'),
    `:=`(line1 = i.line1, col1 = i.col1, line2 = i.line2, col2 = i.col2)
  ]
  msg[ , by = c('file', 'line1', 'col1', 'line2', 'col2'),
    call := build_call(
      file_lines[[.BY$file]],
      # match any comments between line1 & line2
      comments[.(.BY$file, .BY$line1:.BY$line2), .SD, nomatch=NULL],
      params = .BY
    )
  ]

  # Remove duplicates introduced by unsuppressed warning() (etc) calls like
  #   warning(sprintf(ngettext(n, "a", "b")))
  # TODO: hacky, but gets the job done. improve by suppressing earlier. the complication is,
  #   we need get_dots_strings to do drop_suppress_and_named() for ngettext()
  #   _at any level of nesting_ which means a recursive search. ugh. maybe this is best after all.
  # NB: an anti-join approach like singular_strings[!plural_strings] would also potentially strip
  #   any duplicates like ngettext(n, "a", "b") and also warning("a", "b"). which I think
  #   is an xgettext error anyway...
  msg = msg[
    type == 'plural'
    | !grepl("ngettext", call, fixed = TRUE)
    | !msgid %chin% unlist(plural_strings$msgid_plural)
  ]

  # these are the parent's stats
  msg[ , c('parent', 'line1', 'line2', 'col1', 'col2') := NULL]

  # now add the child's stats to order within the file
  msg[
    expr_data, on = c('file', 'id'),
    `:=`(line_number = i.line1, column_number = i.col1)
  ]

  # descending 'type' so that "singular" comes before "plural"
  setorderv(msg, c("type", "file", "line_number", "column_number"), c(-1L, 1L, 1L, 1L))
  # kept id, column_number to get order within lines; can drop now
  msg[ , c('id', 'column_number') := NULL]

  msg[type == 'singular', 'msgid' := escape_string(msgid)]
  msg[type == 'plural', 'msgid_plural' := lapply(msgid_plural, escape_string)]

  # keep duplicates & define this field, in case duplicates are also part of diagnostic checks
  msg[ , 'is_repeat' := FALSE]
  # TODO: skip empty strings, check why this isn't counted as duplicated:
  #   You are trying to join data.tables where %s has 0 columns.
  msg[type == 'singular', 'is_repeat' := duplicated(msgid)]

  msg[type == 'plural', 'is_marked_for_translation' := TRUE]
  msg[type == 'singular', 'is_marked_for_translation' := fname %chin% c(DOMAIN_DOTS_FUNS, 'gettextf')]
  msg[ , 'fname' := NULL]

  msg[]
}

# these functions all have a domain= argument. taken from the xgettext source, but could be
#   refreshed with the following (skipping bindtextdomain and .makeMessage):
# for (obj in ls(BASE <- asNamespace('base'))) {
#     if (!is.function(f <- get(obj, envir = BASE))) next
#     if (is.null(f_args <- args(f))) next
#     if (any(names(formals(f_args)) == 'domain')) cat(obj, '\n')
# }
DOMAIN_DOTS_FUNS = c("warning", "stop", "message", "packageStartupMessage", "gettext")
NON_DOTS_ARGS = c("domain", "call.", "appendLF", "immediate.", "noBreaks.")

# for functions (e.g. DOMAIN_DOTS_FUNS) where we extract strings from ... arguments
get_dots_strings = function(expr_data, funs, arg_names, exclude = c('gettext', 'gettextf'), recursive = TRUE) {
  call_neighbors = get_call_args(expr_data, funs)
  named_args = get_named_args(call_neighbors, expr_data, arg_names)
  call_neighbors = drop_suppressed_and_named(call_neighbors, named_args)

  # as we search the AST "below" call_neighbors, drop whichever of the excluded expr parents we find.
  #   practically speaking, this is how we disassociate "hi" from stop() in stop(gettext("hi"))
  exclude_parents = expr_data[
    expr_data[token == 'SYMBOL_FUNCTION_CALL' & text %chin% exclude, .(file, parent)],
    on = c('file', id = 'parent'),
    .(file, id = x.parent)
  ]

  # drop '(', ')', ',', and now-orphaned SYMBOL_SUB/EQ_SUB
  call_neighbors = call_neighbors[token == 'expr'][!exclude_parents, on = c('file', 'id')]
  setnames(call_neighbors, 'parent', 'ancestor')

  strings = string_schema()
  while (nrow(call_neighbors) > 0L) {
    call_neighbors = expr_data[
      call_neighbors, on = c('file', parent = 'id'),
      .(file, ancestor = i.ancestor, fname = i.fname, id = x.id, token = x.token, text = x.text)
    ]
    strings = rbind(
      strings,
      call_neighbors[
        token == 'STR_CONST',
        .(file, parent = ancestor, id, fname, msgid = text)
      ],
      fill = TRUE
    )
    # much cleaner to do this tiny check a small number (e.g. nesting level of 10-15) times
    #   repetitively rather than make a whole separate branch for the once-and-done case
    if (!recursive) break
    call_neighbors = call_neighbors[token == "expr"][!exclude_parents, on = c('file', 'id')]
  }
  return(strings)
}

# for functions (e.g. ngettext, gettextf) where we extract strings from named arguments
get_named_arg_strings = function(expr_data, funs, arg_names, plural = FALSE) {
  call_neighbors = get_call_args(expr_data, funs)
  named_args = get_named_args(call_neighbors, expr_data, "domain")
  call_neighbors = drop_suppressed_and_named(call_neighbors, named_args)

  explicit_args = get_named_args(call_neighbors, expr_data, arg_names)

  strings = string_schema()
  if (nrow(explicit_args)) {
    if (plural) {
      new_strings = explicit_args[
        order(file, parent),
        by = .(file, parent, fname),
        {
          if (.N == length(arg_names)) {
            .(msgid_plural = list(arg_value))
          } else {
            stop(domain = NA, call. = FALSE, gettextf(
              "In line %s of %s, found a call to %s that names only some of its messaging arguments explicitly. Expected all of [%s] to be named. Please name all or none of these arguments.",
              # funs[1L] is very lazy. should name the function actually responsible...
              expr_data[.BY, on = c(id = 'parent'), line1[1L]], .BY$file, .BY$fname, toString(arg_names)
            ))
          }
        }
      ]
    } else {
      new_strings = explicit_args[
        order(file, parent),
        by = .(file, parent, fname),
        {
          if (.N == length(arg_names)) {
            .(msgid = arg_value)
          } else {
            # TODO: this is currently uncoverable, since only gettextf uses this branch
            #   and that only uses one named argument. Revisit...
            stop(domain = NA, call. = FALSE, gettextf(
              "In line %s of %s, found a call to %s that names only some of its messaging arguments explicitly. Expected all of [%s] to be named. Please name all or none of these arguments.",
              expr_data[.BY, on = c(id = 'parent'), line1[1L]], .BY$file, .BY$fname, toString(arg_names)
            ))
          }
        }
      ]
    }
    strings = rbind(strings, new_strings, fill = TRUE)
    # now that the arguments have been extracted here, drop these expressions
    call_neighbors = call_neighbors[!explicit_args, on = c('file', 'parent')]
  }

  # now pull out only the arguments that are defined implicitly
  new_strings = expr_data[
    call_neighbors[token == 'expr'],
    on = c('file', parent = 'id'),
    .(file, id = x.id, parent = i.parent, fname = i.fname, token = x.token, text = x.text)
  ][
    token == 'STR_CONST'
  ]
  if (plural) {
    new_strings = new_strings[
      order(id),
      by = .(file, parent, fname),
      # some calls like gettextf("hey '%s'", "you") to get templating even though
      #   the second argument is literal, used e.g. when "hey '%s'" will be repeated
      #   with different '%s' values, hence get the first length(arg_names) args
      .(msgid_plural = list(text[seq_along(arg_names)]))
    ]
  } else {
    new_strings = new_strings[
      order(id),
      by = .(file, parent, fname),
      .(id = id[seq_along(arg_names)], msgid = text[seq_along(arg_names)])
    ]
  }
  strings = rbind(strings, new_strings, fill = TRUE)
  return(strings)
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
  msg_call_neighbors
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

drop_suppressed_and_named = function(calls_data, named_args) {
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
  while((idx <- regexpr("\t", l, fixed = TRUE)) > 0L) {
    l = sub("\t", strrep(" ", 9L-(idx %% 8L)), l, fixed = TRUE)
  }
  l
}

# the text column in getParseData() needs some tidying:
#  1. the actual quotes are kept, e.g. text='"a string"'
#  2. "unescape" strings (e.g. "\\n" --> \n) so that trimws() works. note that
#     later we "re-apply" encodeString() so this feels redundant; it's really for 3.
#  3. trimws()
clean_text = function(x) {
  # See ?Quotes for the rules governing string constants. this regex has two parts:
  #   the first for raw strings, the second for "normal" strings. regex considers there
  #   to be two capture groups, hence the need for \\1\\2. the two parts are mutually
  #   exclusive, so only one group is ever matched, the other is always empty.
  # Handle raw strings separately, lest we catch a string like "abc)--" that _looks_
  #   like a raw string on the RHS but actually is not one. also consider a real
  #   pain like r'("abc")' and 'r"(abc)"' -- whatever we do, if we strip away one first,
  #   then the other, we'll end up with the wrong strings.
  #   TODO: add tests for these edge cases
  x = gsub(
    '^[rR]["\'][-]*[\\[({](.*)[\\])}][-]*["\']$|^["\'](.*)["\']$',
    '\\1\\2', x, perl = TRUE
  )
  # there may be others, these are the main ones...
  #   lookback since actual escaped \\n shoudln't be replaced. perl escaping sure is ugly.
  x = gsub("(?<![\\\\])[\\\\]n", "\n", x, perl = TRUE)
  x = gsub("(?<![\\\\])[\\\\]t", "\t", x, perl = TRUE)
  x = gsub("(?<![\\\\])[\\\\]r", "\r", x, perl = TRUE)
  x = gsub('\\"', '"', x, fixed = TRUE)
  x = gsub('\\\\', '\\', x, fixed = TRUE)
  return(trimws(x))
}

string_schema = function() data.table(
  file = character(),
  # needed to build the call
  parent = integer(),
  # needed to order the strings correctly within the call
  id = integer(),
  fname = character(),
  msgid = character(),
  msgid_plural = list()
)

# the schema for empty edge cases
r_message_schema = function() data.table(
  type = character(),
  file = character(),
  msgid = character(),
  msgid_plural = list(),
  line_number = integer(),
  call = character(),
  is_repeat = logical(),
  is_marked_for_translation = logical()
)
