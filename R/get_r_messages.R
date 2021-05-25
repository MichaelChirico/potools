# Extended/adapted/combined version of tools::{x,xn}gettext. Mainly:
#   (1) We want the results of both asCall=TRUE and asCall=FALSE together
#   (2) We want to keep the caller (e.g. stop(), message(), etc.) as well
#   (3) Take the parse trees as input, rather than calculating locally,
#       since this is re-used elsewhere in translate_package
# To pair the results, instead of the asCall=TRUE approach, use deparse()
#   of any messaging input (to be shown to user later as an "in-context"
#   version of the strings to translate). Things can be a little wonky,
#   however, e.g. for how asCall=TRUE/FALSE handles domain=NA and
#   nested strings
get_r_messages <- function (x) {
  if (is.list(x) && all(vapply(x, typeof, character(1L)) == "expression")) {
    exprs <- x
  } else {
    # mostly used for convenient debugging right now. assumption
    #   is that x is a directory so that we can use get_r_messages directly on a folder
    exprs <- parse_r_files(x) # nocov
  }

  expr_data <- rbindlist(lapply(exprs, getParseData), idcol = 'file')
  # R-free package (e.g. a data package) fails, #56
  if (!nrow(expr_data)) return(r_message_schema())

  setkeyv(expr_data, "file")
  setindexv(expr_data, c("file", "line1", "col1", "line2", "col2"))
  setindexv(expr_data, c("file", "id"))
  setindexv(expr_data, c("file", "parent"))
  setindexv(expr_data, c("token", "text"))

  # on the XML tree, messaging calls look like
  # <expr>    <- parent of 'msg_call_neighbors'
  #   <expr>  <- 'msg_call_exprs'
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
  msg_call_neighbors = get_call_args(expr_data, MSG_FUNS)
  named_args = get_named_args(msg_call_neighbors, expr_data, NON_STRING_ARGS)
  msg_call_neighbors = drop_suppressed_and_named(msg_call_neighbors, named_args)

  # drop '(', ')', ',', and now-orphaned SYMBOL_SUB/EQ_SUB
  msg_call_neighbors = msg_call_neighbors[token == 'expr']
  setnames(msg_call_neighbors, 'parent', 'ancestor')

  singular_strings = data.table(file = character(), parent = integer(), msgid = character())
  while (nrow(msg_call_neighbors) > 0L) {
    msg_call_neighbors = expr_data[
      msg_call_neighbors, on = c('file', parent = 'id'),
      .(file, ancestor = i.ancestor, id = x.id, token = x.token, text = x.text)
    ]
    singular_strings = rbind(
      singular_strings,
      msg_call_neighbors[token == 'STR_CONST', .(file = file, parent = ancestor, msgid = text)]
    )
    msg_call_neighbors = msg_call_neighbors[token == "expr"]
  }

  # treat gettextf separately since it takes a named argument, and we ignore ...
  gettextf_call_neighbors = get_call_args(expr_data, "gettextf")
  explicit_args = get_named_args(gettextf_call_neighbors, expr_data, "fmt")
  if (nrow(explicit_args)) {
    singular_strings = rbind(
      singular_strings,
      explicit_args[ , .(file, parent, msgid = arg_value)]
    )
    # now that the arguments have been extracted here, drop these expressions
    gettextf_call_neighbors = gettextf_call_neighbors[!explicit_args, on = c('file', 'parent')]
  }

  # now pull out only the first argument in the implicit args case
  singular_strings = rbind(
    singular_strings,
    expr_data[
      gettextf_call_neighbors[token == 'expr'],
      on = c('file', parent = 'id'),
      .(file, id = x.id, parent = i.parent, token = x.token, text = x.text)
    ][
      order(id)
    ][
      token == 'STR_CONST',
      # some calls like gettextf("hey '%s'", "you") to get templating even though
      #   the second argument is literal, used e.g. when "hey '%s'" will be repeated
      #   with different '%s' values, hence text[1L] to get the first string.
      .(msgid = text[1L]),
      by = .(file, parent)
    ]
  )

  plural_call_neighbors = get_call_args(expr_data, "ngettext")
  named_args = get_named_args(plural_call_neighbors, expr_data, "domain")
  plural_call_neighbors = drop_suppressed_and_named(plural_call_neighbors, named_args)

  plural_strings = data.table(file = character(), parent = integer(), plural_msgid = list())
  explicit_args = get_named_args(plural_call_neighbors, expr_data, c("msg1", "msg2"))
  if (nrow(explicit_args)) {
    plural_strings = rbind(
      plural_strings,
      explicit_args[
        order(file, parent, arg_name),
        if (.N == 2L) .(plural_msgid = list(arg_value)) else stop(domain = NA, gettextf(
          "In line %d of %s, found an ngettext() call that explicitly names only one of the msg1/msg2 arguments. Please name both if naming either.",
          .BY$file, expr_data[.BY, on = c(id = 'parent'), line1[1L]]
        )),
        by = .(file, parent)
      ]
    )
    # now that the arguments have been extracted here, drop these expressions
    plural_call_neighbors = plural_call_neighbors[!explicit_args, on = c('file', 'parent')]
  }

  # no nesting to deal with for ngettext; just extract the two STR_CONST
  plural_strings = rbind(
    plural_strings,
    expr_data[
      plural_call_neighbors[token == 'expr'],
      on = c('file', parent = 'id'),
      .(file, id = x.id, parent = i.parent, token = x.token, text = x.text)
    ][
      order(id)
    ][
      token == 'STR_CONST',
      .(plural_msgid = list(text)),
      by = .(file, parent)
    ]
  )

  msg = rbind(
    singular = singular_strings,
    plural = plural_strings,
    idcol = 'type', fill = TRUE, use.names = TRUE
  )

  if (!nrow(msg)) return(r_message_schema())

  msg_files = unique(msg$file)
  file_lines = lapply(msg_files, readLines, warn = FALSE)
  names(file_lines) = msg_files

  msg[
    expr_data, on = c('file', parent = 'id'),
    `:=`(line1 = i.line1, col1 = i.col1, line2 = i.line2, col2 = i.col2)
  ]
  msg[ , by = c('file', 'line1', 'col1', 'line2', 'col2'),
    call := build_call(file_lines[[.BY$file]], .BY)
  ]

  setnames(msg, 'line1', 'line_number')
  msg[ , c('parent', 'col1', 'line2', 'col2') := NULL]
  # descending type so that "singular" comes before "plural"
  setorderv(msg, c("type", "file", "line_number"), c(-1L, 1L, 1L))

  msg[type == 'singular', 'msgid' := escape_string(msgid)]
  msg[type == 'plural', 'plural_msgid' := lapply(plural_msgid, escape_string)]
  msg[ , 'is_repeat' := FALSE]
  msg[type == 'singular', 'is_repeat' := duplicated(msgid)]
  msg[ , 'is_marked_for_translation' := TRUE]

  msg[]
}

# these functions all have a domain= argument. taken from the xgettext source, but could be
#   refreshed with the following (skipping bindtextdomain and .makeMessage):
# for (obj in ls(BASE <- asNamespace('base'))) {
#     if (!is.function(f <- get(obj, envir = BASE))) next
#     if (is.null(f_args <- args(f))) next
#     if (any(names(formals(f_args)) == 'domain')) cat(obj, '\n')
# }
MSG_FUNS = c("warning", "stop", "message", "packageStartupMessage", "gettext")
NON_STRING_ARGS = c("domain", "call.", "appendLF", "immediate.", "noBreaks.")

get_call_args = function(expr_data, calls) {
  msg_call_exprs = expr_data[
    expr_data[token == "SYMBOL_FUNCTION_CALL" & text %chin% calls],
    on = c('file', id = 'parent'),
    .(file, call_id = i.id, call_expr_id = x.id, call_parent_id = x.parent)
  ]
  msg_call_neighbors = expr_data[
    msg_call_exprs, on = c('file', parent = 'call_parent_id'),
    .(file, id = x.id, parent = x.parent, token = x.token, text = x.text)
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
    .(file, parent, id = x.id, arg_name = i.text)
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

build_call = function(lines, params) {
  if (params$line1 == params$line2) {
    return(substring(adjust_tabs(lines[params$line1]), params$col1, params$col2))
  } else {
    lines = lines[params$line1:params$line2]
    # eschew providing last=nchar(lines[1L]) because we'd need to recalculate it after adjust_tabs()
    lines[1L] = substring(adjust_tabs(lines[1L]), params$col1)
    lines[length(lines)] = substring(adjust_tabs(lines[length(lines)]), 1L, params$col2)
    # strip internal whitespace across lines in the call
    return(gsub("\\s+", " ", paste(lines, collapse = " ")))
  }
}

adjust_tabs = function(l) {
  while((idx <- regexpr("\t", l, fixed = TRUE)) > 0L) {
    l = sub("\t", strrep(" ", 9L-(idx %% 8L)), l, fixed = TRUE)
  }
  l
}

# the schema for empty edge cases
r_message_schema = function() data.table(
  type = character(),
  file = character(),
  msgid = character(),
  plural_msgid = list(),
  line_number = integer(),
  call = character(),
  is_repeat = logical(),
  is_marked_for_translation = logical()
)
