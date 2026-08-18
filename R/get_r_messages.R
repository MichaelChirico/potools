# Spiritual cousin version of tools::{x,xn}gettext. Instead of iterating the AST
#   as R objects or flat table rows, do so using xmlparsedata and xml2 with XPath.
get_r_messages <- function(dir, custom_translation_functions = NULL, is_base = FALSE, style = c("base", "explicit")) {
  style <- match.arg(style)

  parsed_files <- parse_r_files(dir, is_base)
  if (!length(parsed_files)) return(r_message_schema())

  dots_funs <- domain_dots_funs(use_conditions = style == "base")
  fmt_funs <- domain_fmt_funs(use_conditions = style == "base")
  if (length(custom_translation_functions)) {
    custom_params <- parse_r_keywords(custom_translation_functions)
  } else {
    custom_params <- list()
  }

  msg_list <- list()

  for (f in names(parsed_files)) {
    p <- parsed_files[[f]]
    xml_str <- xml_parse_data(p)
    if (!length(xml_str) || !nzchar(xml_str)) next

    doc <- read_xml(xml_str)

    # 1. skip # notranslate lines / blocks
    comments <- xml_find_all(doc, "//COMMENT")
    if (length(comments)) {
      comm_texts <- xml_text(comments)
      comm_lines <- as.integer(xml_attr(comments, "line1"))

      inline_mask <- grepl("# notranslate", comm_texts, fixed = TRUE)
      inline_lines <- unique(comm_lines[inline_mask])

      start_mask <- grepl("# notranslate start", comm_texts, fixed = TRUE)
      end_mask <- grepl("# notranslate end", comm_texts, fixed = TRUE)
      starts <- data.table(file = f, line1 = comm_lines[start_mask])
      ends <- data.table(file = f, line1 = comm_lines[end_mask])

      ranges <- build_exclusion_ranges(starts, ends)

      excl_lines <- inline_lines
      if (nrow(ranges)) {
        for (ri in seq_len(nrow(ranges))) {
          s <- ranges$start[ri]
          e <- ranges$end[ri]
          if (s < e - 1L) {
            excl_lines <- c(excl_lines, (s + 1L):(e - 1L))
          }
        }
      }
      excl_lines <- unique(excl_lines)

      if (length(excl_lines)) {
        excl_str <- paste(sprintf("@line1='%d'", excl_lines), collapse = " or ")
        del_nodes <- xml_find_all(doc, sprintf("//expr[%s]", excl_str))
        xml_remove(del_nodes)
      }
    }

    # 2. Extract messaging calls
    dots_res <- extract_dots_from_doc(doc, f, dots_funs, NON_DOTS_ARGS, recursive = TRUE)
    fmt_res  <- extract_named_from_doc(doc, f, fmt_funs, c(fmt = 1L), recursive = TRUE, plural = FALSE)
    cat_res  <- extract_dots_from_doc(doc, f, "cat", c("file", "sep", "fill", "labels", "append"), recursive = FALSE)
    nget_res <- extract_named_from_doc(doc, f, "ngettext", c(msg1 = 2L, msg2 = 3L), recursive = FALSE, plural = TRUE)

    sing_list <- list()
    if (!is.null(dots_res) && nrow(dots_res)) sing_list[[length(sing_list) + 1L]] <- dots_res
    if (!is.null(fmt_res) && nrow(fmt_res)) sing_list[[length(sing_list) + 1L]] <- fmt_res
    if (!is.null(cat_res) && nrow(cat_res)) sing_list[[length(sing_list) + 1L]] <- cat_res

    plur_list <- list()
    if (!is.null(nget_res) && nrow(nget_res)) plur_list[[length(plur_list) + 1L]] <- nget_res

    if (style == "explicit") {
      tr_res  <- extract_dots_from_doc(doc, f, "tr_", character(), recursive = TRUE)
      trn_res <- extract_named_from_doc(doc, f, "tr_n", c(singular = 2L, plural = 3L), recursive = FALSE, plural = TRUE)
      if (!is.null(tr_res) && nrow(tr_res)) sing_list[[length(sing_list) + 1L]] <- tr_res
      if (!is.null(trn_res) && nrow(trn_res)) plur_list[[length(plur_list) + 1L]] <- trn_res
    }

    if (length(custom_params)) {
      for (cp in custom_params$singular$dots) {
        c_res <- extract_dots_from_doc(doc, f, cp$fname, cp$excluded_args)
        if (!is.null(c_res) && nrow(c_res)) sing_list[[length(sing_list) + 1L]] <- c_res
      }
      for (cp in custom_params$singular$named) {
        c_res <- extract_named_from_doc(doc, f, cp$fname, cp$args, recursive = FALSE)
        if (!is.null(c_res) && nrow(c_res)) sing_list[[length(sing_list) + 1L]] <- c_res
      }
      for (cp in custom_params$plural) {
        c_res <- extract_named_from_doc(doc, f, cp$fname, cp$args, recursive = FALSE, plural = TRUE)
        if (!is.null(c_res) && nrow(c_res)) plur_list[[length(plur_list) + 1L]] <- c_res
      }
    }

    dt_list <- list()
    if (length(sing_list)) dt_list$singular <- rbindlist(sing_list, fill = TRUE)
    if (length(plur_list)) dt_list$plural <- rbindlist(plur_list, fill = TRUE)

    if (length(dt_list)) {
      file_msg <- rbindlist(dt_list, idcol = "type", fill = TRUE)
      msg_list[[f]] <- file_msg
    }
  }

  if (!length(msg_list)) return(r_message_schema())
  msg <- rbindlist(msg_list, fill = TRUE)
  if (!nrow(msg)) return(r_message_schema())

  if (!"msgid_plural" %in% names(msg)) msg[ , msgid_plural := .(vector("list", .N))]

  msg_files <- unique(msg$file)
  if (is_base) {
    paths <- file.path(dir, 'R', msg_files)
    share_idx <- startsWith(msg_files, 'share/R')
    paths[share_idx] <- file.path(dir, '../../..', msg_files[share_idx])
  } else {
    paths <- file.path(dir, 'R', msg_files)
  }
  file_lines <- lapply(normalizePath(paths), readLines, warn = FALSE)
  names(file_lines) <- msg_files

  all_comments <- list()
  for (f in msg_files) {
    p <- parsed_files[[f]]
    pd <- getParseData(p)
    if (!is.null(pd) && nrow(pd)) {
      cm <- pd[pd$token == "COMMENT", c("line1", "line2", "col1")]
      if (nrow(cm)) {
        cm$file <- f
        all_comments[[f]] <- setDT(cm)
      }
    }
  }
  comments <- if (length(all_comments)) rbindlist(all_comments) else data.table(file = character(), line1 = integer(), line2 = integer(), col1 = integer())
  setkeyv(comments, c("file", "line1", "line2"))

  u_calls <- unique(msg[, .(file, line1 = parent_line1, col1 = parent_col1, line2 = parent_line2, col2 = parent_col2)])
  u_calls[ , call := character(.N)]
  is_single <- u_calls$line1 == u_calls$line2

  if (any(is_single)) {
    u_calls[is_single, call := {
      lines_sub <- file_lines[[.BY$file]][line1]
      if (any(has_tabs <- grepl("\t", lines_sub, fixed = TRUE))) {
        lines_sub[has_tabs] <- vapply(lines_sub[has_tabs], adjust_tabs, character(1L), USE.NAMES = FALSE)
      }
      substr(lines_sub, col1, col2)
    }, by = file]
  }

  multi_idx <- which(!is_single)
  if (length(multi_idx)) {
    multi <- u_calls[multi_idx]
    ov <- foverlaps(multi, comments, which = TRUE, nomatch = NULL)
    comm_by_call <- split(ov$yid, ov$xid)
    calls_res <- character(length(multi_idx))
    for (j in seq_along(multi_idx)) {
      f <- multi$file[j]
      flines <- file_lines[[f]]
      match_rows <- comm_by_call[[as.character(j)]]
      cm <- if (length(match_rows)) comments[match_rows] else comments[0L]
      calls_res[j] <- build_call(flines, cm, multi[j])
    }
    u_calls[multi_idx, call := calls_res]
  }

  msg[u_calls, on = .(file, parent_line1 = line1, parent_col1 = col1, parent_line2 = line2, parent_col2 = col2), call := i.call]

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

  msg[ , c("parent_line1", "parent_col1", "parent_line2", "parent_col2", "column_number", "in_subdir") := NULL]

  msg[type == 'singular', 'msgid' := escape_string(trimws(msgid))]
  msg[type == 'plural', 'msgid_plural' := lapply(msgid_plural, escape_string)]

  # keep duplicates & define this field, in case duplicates are also part of diagnostic checks
  msg[ , 'is_repeat' := FALSE]
  # TODO: skip empty strings, check why this isn't counted as duplicated:
  #   You are trying to join data.tables where %s has 0 columns.
  msg[type == 'singular', 'is_repeat' := duplicated(msgid)]

  known_translators <- c(dots_funs, 'ngettext', fmt_funs, get_fnames(custom_params))
  if (style == "explicit") {
    known_translators <- c(known_translators, "tr", "tr_")
  }
  msg[ , 'is_marked_for_translation' := fname %chin% known_translators]

  # TODO: assume custom translators are translated? or maybe just check the regex?
  msg[ , "is_templated" := fname %chin% fmt_funs]
  msg[ , "fname" := NULL]

  col_order <- c("type", "file", "msgid", "msgid_plural", "call", "line_number", if (is_base) "in_share", "is_repeat", "is_marked_for_translation", "is_templated")
  setcolorder(msg, col_order)

  msg[]
}

# Helper to extract line and column coordinates of an XML node
get_node_coords <- function(node) {
  list(
    line1 = as.integer(xml_attr(node, "line1")),
    col1 = as.integer(xml_attr(node, "col1")),
    line2 = as.integer(xml_attr(node, "line2")),
    col2 = as.integer(xml_attr(node, "col2"))
  )
}

# Helper to extract STR_CONST nodes under an expression, respecting non-recursion and excluded calls
extract_strings_from_node <- function(expr_node, recursive = TRUE, exclude_fns = c('gettext', 'gettextf', 'ngettext')) {
  if (!recursive) {
    str_nodes <- xml_find_all(expr_node, "./STR_CONST")
    return(str_nodes)
  }

  fn_check <- paste(sprintf("text() = '%s'", exclude_fns), collapse = " or ")
  if (length(exclude_fns)) {
    is_self_excl <- length(xml_find_all(expr_node, sprintf("self::expr[expr[1][SYMBOL_FUNCTION_CALL[%s] and (count(*) = 1 or NS_GET or NS_GET_INT)] and OP-LEFT-PAREN]", fn_check))) > 0
    if (is_self_excl) return(xml_find_all(expr_node, "./doesnotexist"))
  }

  all_strs <- xml_find_all(expr_node, ".//STR_CONST")
  if (!length(all_strs) || !length(exclude_fns)) return(all_strs)

  xpath_excl_calls <- sprintf(".//expr[expr[1][SYMBOL_FUNCTION_CALL[%s] and (count(*) = 1 or NS_GET or NS_GET_INT)] and OP-LEFT-PAREN]", fn_check)
  excl_calls <- xml_find_all(expr_node, xpath_excl_calls)

  if (!length(excl_calls)) return(all_strs)

  keep <- rep(TRUE, length(all_strs))
  for (ec in excl_calls) {
    ec_strs <- xml_find_all(ec, ".//STR_CONST")
    if (length(ec_strs)) {
      ec_coords <- paste(xml_attr(ec_strs, "line1"), xml_attr(ec_strs, "col1"),
                         xml_attr(ec_strs, "line2"), xml_attr(ec_strs, "col2"))
      all_coords <- paste(xml_attr(all_strs, "line1"), xml_attr(all_strs, "col1"),
                          xml_attr(all_strs, "line2"), xml_attr(all_strs, "col2"))
      keep[all_coords %in% ec_coords] <- FALSE
    }
  }
  all_strs[keep]
}

# Extract strings from ... arguments for functions like stop, warning, gettext, cat
extract_dots_from_doc <- function(doc, f, fnames, non_dots_args = NON_DOTS_ARGS,
                                  recursive = TRUE, exclude_fns = c('gettext', 'gettextf', 'ngettext')) {
  fn_filter <- paste(sprintf("text() = '%s'", fnames), collapse = " or ")
  xpath_calls <- sprintf("//expr[
    expr[1][SYMBOL_FUNCTION_CALL[%s] and (count(*) = 1 or NS_GET or NS_GET_INT)]
    and OP-LEFT-PAREN
    and not(SYMBOL_SUB[text() = 'domain']/following-sibling::expr[1][. = 'NA'])
  ]", fn_filter)

  call_nodes <- xml_find_all(doc, xpath_calls)
  if (!length(call_nodes)) return(NULL)

  res_list <- list()
  for (cn in call_nodes) {
    fn_name <- xml_text(xml_find_first(cn, "expr[1]//SYMBOL_FUNCTION_CALL"))
    call_coords <- get_node_coords(cn)

    all_arg_exprs <- xml_find_all(cn, "./expr[position() > 1]")
    if (!length(all_arg_exprs)) next

    if (length(non_dots_args)) {
      excl_sub_filter <- paste(sprintf("text() = '%s'", non_dots_args), collapse = " or ")
      excl_args <- xml_find_all(cn, sprintf("./SYMBOL_SUB[%s]/following-sibling::expr[1]", excl_sub_filter))
      if (length(excl_args)) {
        excl_coords <- paste(xml_attr(excl_args, "line1"), xml_attr(excl_args, "col1"))
        arg_coords <- paste(xml_attr(all_arg_exprs, "line1"), xml_attr(all_arg_exprs, "col1"))
        all_arg_exprs <- all_arg_exprs[!arg_coords %in% excl_coords]
      }
    }

    if (!length(all_arg_exprs)) next

    for (arg in all_arg_exprs) {
      str_nodes <- extract_strings_from_node(arg, recursive = recursive, exclude_fns = exclude_fns)
      if (length(str_nodes)) {
        for (sn in str_nodes) {
          txt <- clean_text(xml_text(sn))
          l_num <- as.integer(xml_attr(sn, "line1"))
          c_num <- as.integer(xml_attr(sn, "col1"))
          res_list[[length(res_list) + 1L]] <- list(
            file = f,
            parent_line1 = call_coords$line1, parent_col1 = call_coords$col1,
            parent_line2 = call_coords$line2, parent_col2 = call_coords$col2,
            line_number = l_num, column_number = c_num,
            fname = fn_name,
            msgid = txt
          )
        }
      }
    }
  }
  if (!length(res_list)) return(NULL)
  rbindlist(res_list)
}

# Extract strings from named arguments for functions like gettextf, ngettext, tr_n
extract_named_from_doc <- function(doc, f, fnames, target_args, recursive = FALSE, plural = FALSE) {
  fn_filter <- paste(sprintf("text() = '%s'", fnames), collapse = " or ")
  xpath_calls <- sprintf("//expr[
    expr[1][SYMBOL_FUNCTION_CALL[%s] and (count(*) = 1 or NS_GET or NS_GET_INT)]
    and OP-LEFT-PAREN
    and not(SYMBOL_SUB[text() = 'domain']/following-sibling::expr[1][. = 'NA'])
  ]", fn_filter)

  call_nodes <- xml_find_all(doc, xpath_calls)
  if (!length(call_nodes)) return(NULL)

  res_list <- list()
  for (cn in call_nodes) {
    fn_name <- xml_text(xml_find_first(cn, "expr[1]//SYMBOL_FUNCTION_CALL"))
    call_coords <- get_node_coords(cn)

    sub_nodes <- xml_find_all(cn, "./SYMBOL_SUB")
    sub_names <- xml_text(sub_nodes)

    target_names <- names(target_args)
    has_named <- any(target_names %in% sub_names)

    target_expr_nodes <- list()

    if (has_named) {
      if (!all(target_names %in% sub_names)) {
        missing_args <- target_names[!target_names %in% sub_names]
        stopf(
          "In line %s of %s, found a call to %s that names only some of its messaging arguments explicitly. Expected all of [%s] to be named. Please name all or none of these arguments.",
          call_coords$line1, f, fn_name, toString(missing_args)
        )
      }
      for (arg_nm in target_names) {
        target_expr_nodes[[arg_nm]] <- xml_find_first(cn, sprintf("./SYMBOL_SUB[text()='%s']/following-sibling::expr[1]", arg_nm))
      }
    } else {
      all_child_exprs <- xml_find_all(cn, "./expr")
      for (arg_nm in target_names) {
        pos <- target_args[[arg_nm]]
        idx <- pos + 1L
        if (idx <= length(all_child_exprs)) {
          target_expr_nodes[[arg_nm]] <- all_child_exprs[[idx]]
        }
      }
    }

    if (!length(target_expr_nodes)) next

    if (plural) {
      str_parts <- list()
      for (arg_nm in target_names) {
        en <- target_expr_nodes[[arg_nm]]
        if (is.null(en) || inherits(en, "xml_missing")) next
        str_nodes <- extract_strings_from_node(en, recursive = recursive)
        if (length(str_nodes)) {
          str_parts[[arg_nm]] <- clean_text(xml_text(str_nodes[[1L]]))
        }
      }
      if (length(str_parts) == length(target_names)) {
        res_list[[length(res_list) + 1L]] <- list(
          file = f,
          parent_line1 = call_coords$line1, parent_col1 = call_coords$col1,
          parent_line2 = call_coords$line2, parent_col2 = call_coords$col2,
          line_number = call_coords$line1, column_number = call_coords$col1,
          fname = fn_name,
          msgid = NA_character_,
          msgid_plural = list(unlist(str_parts, use.names = FALSE))
        )
      }
    } else {
      for (arg_nm in target_names) {
        en <- target_expr_nodes[[arg_nm]]
        if (is.null(en) || inherits(en, "xml_missing")) next
        str_nodes <- extract_strings_from_node(en, recursive = recursive)
        if (length(str_nodes)) {
          for (sn in str_nodes) {
            txt <- clean_text(xml_text(sn))
            l_num <- as.integer(xml_attr(sn, "line1"))
            c_num <- as.integer(xml_attr(sn, "col1"))
            res_list[[length(res_list) + 1L]] <- list(
              file = f,
              parent_line1 = call_coords$line1, parent_col1 = call_coords$col1,
              parent_line2 = call_coords$line2, parent_col2 = call_coords$col2,
              line_number = l_num, column_number = c_num,
              fname = fn_name,
              msgid = txt
            )
          }
        }
      }
    }
  }
  if (!length(res_list)) return(NULL)
  rbindlist(res_list)
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
  out
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
  if (any(bad_idx <- !named_idx & !dots_idx & !plural_idx)) {
    stopf(
      # nolint next: line_length_linter.
      "Invalid custom translator specification(s): %s.\nAll inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.",
      toString(spec[bad_idx])
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

domain_dots_funs <- function(use_conditions = TRUE) {
  c(
    "gettext",
    if (use_conditions) c("stop", "warning", "message", "packageStartupMessage")
  )
}

domain_fmt_funs <- function(use_conditions = TRUE) {
  paste0(domain_dots_funs(use_conditions), "f")
}

NON_DOTS_ARGS = c("domain", "call.", "appendLF", "immediate.", "noBreaks.")

build_call = function(lines, comments, params) {
  if (params$line1 == params$line2) {
    return(substr(adjust_tabs(lines[params$line1]), params$col1, params$col2))
  }

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
  paste(trimws(lines), collapse = " ")
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
  has_backslash = grepl('\\', x, fixed = TRUE)
  if (!any(has_backslash)) return(x)

  xb = x[has_backslash]
  # there may be others, these are the main ones... lookback since actual escaped \\n shouldn't be replaced.
  #   an non-perl approach with capture groups like (^|[^\\])[\\]n fails on consecutive \\n\\n due to greediness
  xb = gsub("(?:^|(?<![\\\\]))[\\\\]n", "\n", xb, perl = TRUE)
  xb = gsub("(?:^|(?<![\\\\]))[\\\\]t", "\t", xb, perl = TRUE)
  # maybe stop() instead? \r is blocked by gettext...
  xb = gsub("(?:^|(?<![\\\\]))[\\\\]r", "\r", xb, perl = TRUE)
  # quotes that are escaped _in the text_ are not escaped _in R_ (i.e., after parsing),
  #   e.g. in 'a string with an \"escaped\" quote', the escapes for " disappear after parsing. See #128
  xb = gsub("(?:^|(?<![\\\\]))[\\\\](['\"])", "\\1", xb, perl = TRUE)
  xb = gsub('\\\\', '\\', xb, fixed = TRUE)
  x[has_backslash] = xb
  x
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
