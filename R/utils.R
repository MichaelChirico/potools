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

# would be great to use readline() but it has several fatal flaws:
#   (1) the prompt argument is a buffer capped at 256 chars, which is far too few
#   (2) readline is _strictly_ interactive -- it can't be tested.
# See this post for testing:
#   https://debruine.github.io/posts/interactive-test/
prompt = function(..., encode = TRUE, conn = getOption('__potools_testing_prompt_connection__', stdin())) {
  cat(...)
  cat('\n')
  txt = readLines(conn, n=1L)
  return(if (encode) encodeString(txt) else txt)
}

if (requireNamespace('crayon', quietly = TRUE)) {
  red = crayon::red
  white = crayon::white
  blue = crayon::blue
  green = crayon::green
  yellow = crayon::yellow
} else {
  red = white = blue = green = yellow = identity
}
