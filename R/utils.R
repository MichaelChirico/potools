# normalizePath with some sanity checks added
get_directory = function(dir) {
  dir = normalizePath(dir)

  if (!file.exists(dir)) {
    stop(domain=NA, gettextf('%s does not exist', dir, domain='R-potools'))
  }
  if (!file.info(dir)$isdir) {
    stop(domain=NA, gettextf('%s is not a directory', dir, domain='R-potools'))
  }
  return(dir)
}

# check dir is a package & return its name & version
get_desc_data = function(dir) {
  desc_file <- file.path(dir, 'DESCRIPTION')
  if (!file.exists(desc_file)) {
    stop(domain=NA, gettextf('%s is not a package (missing DESCRIPTION', dir, domain='R-potools'))
  }
  desc_data <- read.dcf(desc_file, c('Package', 'Version'))[1L, ]
  if (anyNA(desc_data)) {
    stop(domain=NA, gettextf(
      '%s is not a package (missing Package or Version field in DESCRIPTION', dir, domain='R-potools'
    ))
  }
  return(desc_data)
}

# see ?tools::update_pkg_po
SYSTEM_REQUIREMENTS = c('xgettext', 'msgmerge', 'msgfmt', 'msginit', 'msgconv')
RTOOLS_URL = 'https://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip'

check_sys_reqs = function() {
  if (any(is_missing <- !nzchar(Sys.which(SYSTEM_REQUIREMENTS)))) {
    if (.Platform$OS.type == 'windows') {
      platform_msg = gettextf(
        'These tools are available as an Rtools goodie, check %s',
        RTOOLS_URL, domain='R-potools'
      )
    } else {
      if (Sys.info()['sysname'] == 'Darwin') {
        platform_msg = gettext(
          'These GNU tools are commonly available, try installing from brew or apt-get', domain='R-potools'
        )
      } else {
        platform_msg = gettext(
          'These GNU tools are commonly available from the Linux package manager for your system', domain='R-potools'
        )
      }
    }
    stop(domain = NA, gettextf(
      'Missing (or not on PATH) system requirements %s.\n%s',
      toString(SYSTEM_REQUIREMENTS[is_missing]), platform_msg, domain='R-potools'
    ))
  }
}

list_r_files = function(dir) list.files(dir, full.names = TRUE, pattern = "(?i)\\.r")

# second condition is for calls like (function(x) x+1)(2)
is_name_call = function(e) is.call(e) && is.name(e[[1L]])
do_suppress = function(e) {
  domain = e[['domain']]
  !is.null(domain) && !is.name(domain) && is.na(domain)
}

# ensure length-1 output of deparse
agg_deparse = function(x) paste(deparse(x), collapse = ' ')

# shQuote(type='cmd') + encodeString, but don't wrap in the outer ""
escape_string = function(x) gsub('"', '\\"', encodeString(x), fixed = TRUE)

# attempt to _partially_ invert escape_string. namely, unescape quotes
#   and backslashes that were added by escape_string.
#   NB: we leave the control characters as is for visibility. it's very
#   common for messages for translation to end with \n -- if we unescape
#   this, a newline will be printed, and it will take a trained eye or
#   some extra text decoration to draw attention to this. moreover, while
#   I suspect there could be hope for \n, i think all is lost for \r and even \t.
unescape_string = function(x) {
  gsub('[\\]([\\"])', '\\1', x)
}

# two types of specials to highlight:
#   (1: see ?sprintf) templates in the fmt argument to sprintf
#   (2: see ?encodeString) escaped control characters: \\, \n, \t, \r, \v, \a, \b, \f
# NB: also tried combining these two regexes, but they're not used the same.
#   For sprintf, we try to enforce consistency between the translated & original message
#   (e.g. %d is used in both), but such consistency is not required for encoded strings.
# regex for sprintf templates is taken from a thorough reading of ?sprintf. it will
#   generate some false positives, though it is a bit hard to cook up such examples
#   (e.g., sprintf has no problem with sprintf("%#0###     ++++0f", pi) )
SPRINTF_TEMPLATE_REGEX = paste0(
  "[%]",
  "(?:",
    "[%]|", # % separately to reduce false positives -- it can't be used with other specials
    "(?:[1-9][0-9]?[$])?", # "redirection" markers -- %2$s says "use the second element of ... here"
    "(?:[0-9*]+[.]?|[.]?[0-9*]+|[0-9]+[.][0-9]+|[ -+#])*",
    "[aAdifeEgGosxX]", # there are more available at the C level...
  ")"
)
ENCODED_STRING_REGEX = "[\\][\\ntrvabf]"

# basically return the gregexpr output on SPRINTF_TEMPLATE_REGEX|ENCODED_STRING_REGEX in a nicer format
get_specials = function(x) {
  special_starts = gregexpr(sprintf("(?:%s|%s)", SPRINTF_TEMPLATE_REGEX, ENCODED_STRING_REGEX), x)
  # NULL when no match is easier to work with (can use lengths, e.g.)
  lapply(special_starts, function(l) {
    if (length(l) == 1L && l < 0L) return(NULL)
    list(idx = as.integer(l), length = attr(l, 'match.length'))
  })
}

# streamlined get_specials for counting the matches
count_formats = function(x) vapply(
  gregexpr(SPRINTF_TEMPLATE_REGEX, x),
  function(x) if (length(x) == 1L && x == -1L) 0L else length(x),
  integer(1L)
)

# given a string like
#   Found %d arguments in %s. Average %02.3f%%
# get a second line highlighting the string format templates:
#   Found %d arguments in %s. \n Average %02.3f%%
#         ^^              ^^  ^^         ^----^^^
get_special_tags = function(s, specials) {
  out = rep(" ", nchar(s))
  for (ii in seq_along(specials$idx)) {
    start = specials$idx[ii]
    end = start + specials$length[ii] - 1L
    out[c(start, end)] = '^'
    if (end - start > 1L) {
      out[seq(start + 1L, end - 1L)] = '-'
    }
  }
  paste(out, collapse="")
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
prompt = function(..., encode = TRUE, conn = .potools$prompt_conn) {
  cat(...)
  cat('\n')
  txt = readLines(conn, n=1L, encoding = if (inherits(conn, "terminal")) "unknown" else "UTF-8")
  cat(sprintf("bin rep of txt before enc2utf8: %s\n", paste(charToRaw(txt), sep=".")))
  txt = enc2utf8(txt)
  cat(sprintf("bin rep of txt after enc2utf8: %s\n", paste(charToRaw(txt), sep=".")))
  txt = if (encode) encodeString(txt) else txt
  cat(sprintf("bin rep of txt after encodeString: %s\n", paste(charToRaw(txt), sep=".")))
  return()
}

prompt_with_templates = function(n_target, prompt_msg) {
  if (n_target == 0) return(prompt(prompt_msg))
  while (TRUE) {
    translation = prompt(prompt_msg)
    if (!nzchar(translation) || (n_fmt <- count_formats(translation)) == n_target) break
    cat(gettextf(
      "\n\n** Oops! You supplied %d templates; but the target message has %d. Retrying... **\n",
      n_fmt, n_target, domain='R-potools'
    ))
  }
  translation
}

if (requireNamespace('crayon', quietly = TRUE)) {
  call_color = getOption('potools.call_color', crayon::green)
  file_color = getOption('potools.file_color', crayon::white)
  msgid_color = getOption('potools.msgid_color', crayon::red)
  language_color = getOption('potools.language_color', crayon::cyan)
  build_gettextf_color = getOption('potools.build_gettextf_color', crayon::blue)
  plural_range_color = getOption('potools.plural_range_color', crayon::yellow)
} else {
  call_color = file_color = msgid_color = language_color = build_gettextf_color = plural_range_color = identity
}
