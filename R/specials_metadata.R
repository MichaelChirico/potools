# given a string like
#   Found %d arguments in %s. Average %02.3f%%
# get data necessary to build a second line
# highlighting the string format templates:
#   Found %d arguments in %s. \n Average %02.3f%%\n
#         ^^              ^^             ^----^^^^^
# TODO: maybe erroring here is too harsh? maybe let these cases pass &
#   (1) error if it's being run on msgid
#   (2) throw the warning in all.equal() if it's an issue with msgstr so user can retry?
get_specials_metadata = function(x) {
  # tested with xgettext: warning against using \a | \b | \f | \v in internationalized messages;
  #   and msgfmt doesn't warn about whether \t is matched, so no need to bother matching that either
  special_matches = gregexpr(sprintf("%s|^[\\\\]n|[\\\\]n$", SPRINTF_TEMPLATE_REGEX), x, perl = TRUE)[[1L]]
  group_starts = attr(special_matches, "capture.start")
  group_lengths = attr(special_matches, "capture.length")

  if (special_matches[1L] < 0L) {
    meta = data.table(special=character(), id = character(), sequence = integer(), start=integer(), stop=integer())
    setattr(meta, "class", c("specials_metadata", class(meta)))
    return(meta)
  }

  # strip attributes with as.integer()
  meta = data.table(
    start = as.integer(special_matches),
    end = as.integer(special_matches + attr(special_matches, 'match.length') - 1L),
    redirect_start = group_starts[ , "redirect"],
    redirect_length = group_lengths[ , "redirect"],
    width_precision_start = group_starts[ , "width_precision"],
    width_precision_length = group_lengths[ , "width_precision"],
    id_start = group_starts[ , "id"],
    id_length = group_lengths[ , "id"]
  )
  meta[ , "special" := substring(x, start, end)]

  # % escapes don't matter for validation
  meta = meta[special != "%%"]

  template_idx = meta$special != "\\n"
  redirect_idx = meta$redirect_start > 0L
  if (any(template_idx & redirect_idx)) {
    if (any(template_idx & !redirect_idx)) stopf(
      # nolint next: line_length_linter.
      "Invalid templated message. If any %%N$ redirects are used, all templates must be redirected.\n\tRedirected tempates: %s\n\t Un-redirected templates: %s",
      meta$special[template_idx & redirect_idx], meta$special[template_idx & !redirect_idx]
    )

    # make sure newlines are retained in the right order, if present
    meta[ , "redirect_id" := fcase(
      template_idx,
      as.integer(substring(x, redirect_start, redirect_start + redirect_length - 2L)),
      start == 1L, 0L,
      default = .N + 1L
    )]

    setorderv(meta, "redirect_id")
  }

  meta[(template_idx), "id" := safe_substring(x, id_start, id_start + id_length - 1L)]
  # check if variable-width/precision formatting is used (e.g. %.*f or %*.0f)
  meta[
    template_idx & width_precision_start > 0L,
    "width_precision" := safe_substring(x, width_precision_start, width_precision_start + width_precision_length - 1L)
  ]
  meta[template_idx & grepl("*", width_precision, fixed = TRUE), "id" := paste0("*", id)]
  # either it's an initial newline or it's a terminal newline; tag so as to distinguish
  meta[(!template_idx), "id" := fifelse(start == 1L, "^\\n", "\\n$")]

  if (any(template_idx & redirect_idx)) {
    if (nrow(fail <- meta[ , .N, by = c("redirect_id", "id")][ , .N, by = "redirect_id"][N > 1L])) {
      stopf(
        # nolint next: line_length_linter.
        "Invalid templated message string with redirects -- all messages pointing to the same input must have identical formats, but received %s",
        meta[
          fail,
          on = "redirect_id"
        ][ , .(by_id = sprintf("[%s]", toString(special))), by = "redirect_id"][ , paste(by_id, collapse = " / ")]
      )
    }
    meta = unique(meta, by = "redirect_id")
  }

  # need 'sequence' to ensure repeated %d are uniquely identified
  meta = meta[ , .(special, id, sequence = seq_len(.N), start, end)]
  setattr(meta, "class", c("specials_metadata", class(meta)))
  meta[]
}

# convert to a tag as described above
#' @export
format.specials_metadata = function(x, ...) {
  if (!nrow(x)) return('')

  out = rep(" ", max(x$end))
  out[c(x$start, x$end)] = "^"
  # formats wider than %s get padded out with '-' between the "^" anchors
  out[x[start < end - 1L, unlist(lapply(seq_len(.N), function(ii) (start[ii] + 1L):(end[ii] - 1L)))]] = "-"
  paste(out, collapse="")
}

# target: msgid [template translation]
# current: msgstr [received translation]
#' @export
all.equal.specials_metadata = function(target, current, ...) {
  if (nrow(target) != nrow(current)) return(gettextf(
    "received %d unique templated arguments + bordering newlines but there are %d in the original",
    nrow(current), nrow(target)
  ))

  matched = merge(target, current, by = c('id', 'sequence'), all = TRUE)
  # found in y, not x (i.e., in msgstr, not msgid)
  if (anyNA(matched$start.x)) {
    if (isTRUE(all.equal(table(target$id), table(current$id)))) {
      return(gettextf(
        # nolint next: line_length_linter.
        "received the same set of templates + bordering newlines, but in incorrect order (%s vs %s). Recall that you can use %%$N to do redirect, e.g. to swap the order of '%%d %%s' to be translated more naturally, your translation can use '%%1$s %%2$d'",
        sprintf("[%s]", toString(target$special)), sprintf("[%s]", toString(current$special))
      ))
    }
    return(gettextf(
      "received templates + bordering newlines not present in the original: %s",
      toString(matched[is.na(start.x)]$special.y)
    ))
  }
  # NB: since nrow(target) == nrow(current) and !anyNA(matched$start.x), we are done --
  #   anyNA(matched$start.y is already impossible)
  # found in x, not y (i.e., in msgid, not msgstr)

  # else target/current have the same number of rows
  return(TRUE)
}
