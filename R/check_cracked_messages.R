# check for calls like stop("a", i, "b", j) that are better suited for
#   translation as calls like gettextf("a%db%d", i, j)


#' Check for cracked messages more suitable for templating
#'
#' Diagnose the R messages in a package to discover the presence of "cracked"
#' messages better served for translation by templating. See Details.
#'
#'
#' Error messages built like
#' `stop("You gave ", n, " arguments, but ", m, " are needed.")` are
#' in general hard for translators -- the correct
#' translation may be in a totally different order (e.g., this is often the
#' case for Japanese). It is preferable instead to use
#' [base::gettextf()] to build a templated message like
#' `stop(gettextf("You gave %d arguments but %d are needed.", n, m))`.
#' Translators are then free to rearrange the template to put the numeric
#' pattern where it fits most naturally in the target language.
#'
#' @param message_data A `data.table`, or object convertible to one.
#' @return A `data.table` with columns `call`, `file`,
#' `line_number`, and `replacement` summarizing the results.
#' @author Michael Chirico
#' @seealso [translate_package()], [update_pkg_po()]
#' @examples
#'
#' pkg <- file.path(system.file(package = 'potools'), 'pkg')
#' # copy to a temporary location to be able to read/write/update below
#' tmp_pkg <- file.path(tempdir(), "pkg")
#' dir.create(tmp_pkg)
#' file.copy(pkg, dirname(tmp_pkg), recursive = TRUE)
#'
#' # first, extract message data
#' message_data = get_message_data(tmp_pkg)
#'
#' # now, diagnose the messages for any "cracked" ones
#' check_cracked_messages(message_data)
#'
#' # cleanup
#' unlink(tmp_pkg, recursive = TRUE)
#' rm(pkg, tmp_pkg, message_data)
#' @export
check_cracked_messages = function(message_data) {
  if (!is.data.table(message_data)) message_data = as.data.table(message_data)

  # check for , as a crude filter to avoid parsing too many calls
  dup_messages = message_data[
    !is_repeat
    & is_marked_for_translation
    & message_source == "R"
    & type == 'singular'
    & grepl(",", call, fixed = TRUE),
    # line_number is sorted; use head() for 0-length edge case
    .(line_number = head(line_number, 1L)),
    by=c('file', 'call')
  ]
  if (!nrow(dup_messages)) return(diagnostic_schema())

  dup_messages[ , 'call_expr' := lapply(call, str2lang)]
  dup_messages = dup_messages[count_dots(call_expr) > 1L]
  if (!nrow(dup_messages)) return(diagnostic_schema())

  for (ii in seq_len(nrow(dup_messages))) {
    set(dup_messages, ii, "replacement", build_gettextf_call(dup_messages$call_expr[[ii]]))
  }

  return(dup_messages[ , .(call, file, line_number, replacement)])
}
attr(check_cracked_messages, "diagnostic_tag") =
  "R messaging calls that might be better suited for gettextf for ease of translation"

# take a call like stop("a", i, "b", j) and suggest
#   stop(domain=NA, gettextf("a%sb%s", i, j))
build_gettextf_call = function(e) {
  call <- as.character(e[[1L]])
  rest <- e[-1L]
  arg_names <- names(rest)
  if (is.null(arg_names) || all(keep_args <- arg_names %chin% c("", "domain"))) {
    as.character(glue('{call}(domain=NA, {gettextify(rest)})'))
  } else {
    # if other arguments, e.g. call., appendLF, immediate. are present, keep them with the right call (#227)
    kwargs = toString(paste(names(rest[!keep_args]), "=", vapply(rest[!keep_args], deparse1, character(1L))))
    as.character(glue('{call}(domain=NA, {gettextify(rest[keep_args])}, {kwargs})'))
  }
}

# count the number of ... arguments by excluding named arguments; uses a match.call()
#   and a get() to the function definition, which is relatively expensive, so done carefully/sparingly.
count_dots = function(call) {
  fname = vapply(
    call,
    # get_r_messages should ensure that one of these two conditions will return a scalar
    function(x) if (is.name(x[[1L]])) as.character(x[[1L]]) else as.character(x[[1L]][[3L]]),
    character(1L)
  )
  DT = data.table(call, fname)
  DT[ , 'n_args' := 0L]
  DT[
    fname %chin% c("stop", "warning", "message", "packageStartupMessage") & lengths(call) > 2L,
    by = fname,
    'n_args' := {
      definition = get(.BY$fname)
      vapply(
        call,
        function(x) {
          e = match.call(definition, x)
          if (is.null(names(e))) return(length(e) - 1L)
          return(length(e) - sum(names(e) %chin% c("call.", "immediate.", "noBreaks.", "domain", "appendLF")) - 1L)
        },
        integer(1L)
      )
    }
  ]
  DT$n_args
}
