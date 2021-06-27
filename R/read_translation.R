# readline on steroids.
#  - all the points in utils.R about prompt() apply
#  - we want to unescape strings. the encodeString version
#    should be treated as an "implementation detail" -- we
#    show the user the most recognizable version of it.
#    it might make sense to just store two columns --
#    e.g. msgid and msgid_plain, but this doubles the number
#    of columns. the number of messages is not large enough
#    in any package I've seen (base and data.table are
#    both < 2000), so the advantage of vectorizing is not
#    expected to be noticeable
#  - we want to help users get templates right (which can be
#    tough).
read_translation = function(msgid, type, file, call, fuzzy, msgstr, metadata) {
  # drop escaping for quotes/backslashes so they appear unadorned to the user,
  #   who can then enter then without escaping (and we handle the escaping for them).
  msgid = unescape_string_out(msgid)
  # NB: it's tempting to vectorize running get_specials to e.g.
  #   add a new column msgid_fmt to message_data. But
  #   unescape_string_out here dashes hopes that could work.
  #   Moreover we can't simply get the template _just prior_
  #   to running escape_string in get_r_messages, because
  #   escape_string and unescape_string_out are (intentionally)
  #   not inverses. Bummer. So run it here, to make sure
  #   the match positions are relative to the string that
  #   will be shown to the user.
  msgid_metadata = get_specials_metadata(msgid)
  if (type == 'plural') {
    translation <- character(metadata$nplurals)
    for (jj in seq_len(metadata$nplurals)) {
      if (fuzzy == 1L) {
        fuzzy_tag = gettextf(
          "\n **Note: a similar message was previously translated as: **\n%s",
          msgstr[[jj]]
        )
      } else {
        fuzzy_tag = ""
      }
      translation[jj] = prompt_with_templates(msgid_metadata, gettextf(
        '\nFile: %s\nCall: %s\nPlural message: %s%s\nHow would you translate this message into %s %s?%s',
        file_color(file),
        call_color(call),
        msgid_color(msgid),
        # add enough blanks for Plural message:
        paste0("\n                ", format(msgid_metadata)),
        language_color(metadata$full_name_eng),
        plural_range_color(PLURAL_RANGE_STRINGS[.(metadata$plural, jj-1L), range]),
        fuzzy_tag,
        domain = "R-potools"
      ))
    }
  } else {
    if (fuzzy == 1L) {
      fuzzy_tag = gettextf(
        "\n **Note: a similar message was previously translated as: **\n%s",
        msgstr
      )
    } else {
      fuzzy_tag = ""
    }
    translation = prompt_with_templates(msgid_metadata, gettextf(
      '\nFile: %s\nCall: %s\nMessage: %s%s\nHow would you translate this message into %s?%s',
      file_color(file),
      call_color(call),
      msgid_color(msgid),
      # add enough blanks for Message:
      paste0("\n         ", format(msgid_metadata)),
      language_color(metadata$full_name_eng),
      fuzzy_tag,
      domain = "R-potools"
    ))
  }
  translation
}

set_prompt_conn <- function() {
  conn <- getOption('__potools_testing_prompt_connection__', stdin())
  if (is.character(conn)) {
    conn <- file(conn, "r")
  }
  assign("prompt_conn", conn, envir=.potools)
  return(invisible())
}

unset_prompt_conn <- function() {
  if (!exists("prompt_conn", envir=.potools) || inherits(.potools$prompt_conn, "terminal")) return(invisible())
  close(.potools$prompt_conn)
  return(invisible())
}

# would be great to use readline() but it has several fatal flaws:
#   (1) the prompt argument is a buffer capped at 256 chars, which is far too few
#   (2) readline is _strictly_ interactive -- it can't be tested.
# See this post for testing:
#   https://debruine.github.io/posts/interactive-test/
prompt = function(..., conn = .potools$prompt_conn, require_type) {
  cat(...)
  cat('\n')
  if (inherits(conn, "terminal")) {
    out = enc2utf8(readLines(conn, n=1L)) # nocov
  } else {
    out = readLines(conn, n=1L, encoding="UTF-8")
    # issue often encountered in dev when adjusting the test packages & things get bumped around...
    # nocov start
    if (is.na(out)) {
      stopf("Connection empty when trying to read prompt %s", do.call(paste, list(...)))
    }
    # nocov end
  }
  # See #105 / #95... confusing stuff
  if (missing(require_type)) return(out)
  out = type.convert(out, as.is = TRUE)
  if (typeof(out) == require_type) return(out)

  messagef("Input must be of type '%s', but received '%s'. Trying again.", require_type, typeof(out))
  return(prompt(..., conn=conn, require_type=require_type))
}

prompt_with_templates = function(msgid_metadata, prompt_msg) {
  # NB: this allows msgstr to have templates missing from msgid, but then again, so does msgfmt
  if (!nrow(msgid_metadata)) return(escape_string_in(prompt(prompt_msg)))
  repeat {
    translation = prompt(prompt_msg)
    if (
      !nzchar(translation)
      || isTRUE(diagnosis <- all.equal(msgid_metadata, get_specials_metadata(translation)))
    ) break
    cat(gettextf("\n\n** Oops! Invalid translation -- %s. Retrying... **\n", diagnosis))
  }
  escape_string_in(translation)
}

# hide escaping that's only needed for the .po file
# TODO: still need to think if this is the best way to handle it. A universal
#   approach to escaping in the package would do wonders.
#   NB: we leave the control characters as is for visibility. it's very
#   common for messages for translation to end with \n -- if we unescape
#   this, a newline will be printed, and it will take a trained eye or
#   some extra text decoration to draw attention to this. Moreover, while
#   I suspect there could be hope for \n, i think all is lost for \r and even \t.
unescape_string_out = function(x) {
  gsub('\\"', '"', x, fixed = TRUE)
}

# reverse of unescape_string_out -- restore escaped quotes & backslashes
escape_string_in = function(x) {
  x = gsub('"', '\\"', x, fixed = TRUE)
  x
}
