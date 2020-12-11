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

# newlines in an R string, e.g. "\n" get printed literally in the .po file (c.f.
#   difference b/w print("\n") and cat("\n")), so right now, "\n" is "over-escaped" --
#   this step aims to "un-escape" to match the original R message. I'm not 100% sure this
#   is always accurate yet. But I did check base for similar escapes:
#   grep -Er '(^|[^\\])\\[^nt"\\]' ~/svn/R-devel/src/library/*/po/*.pot
# NB This regex is confusing because of multiple layers of string escaping :)
unescape_str = function(x) {
  # order matters: consider "\\n"
  x = gsub('\\\\', '\\', x, fixed = TRUE)
  # now that backslashes are unescaped, if we see \\n, it's not a newline
  x = gsub("(?:^|[^\\])[\\]n", "\n", x, fixed = TRUE)
  x = gsub("(?:^|[^\\])[\\]t", "\t", x, fixed = TRUE)
  x = gsub('(?:^|[^\\])[\\]"', '"', x, fixed = TRUE)
  x
}
