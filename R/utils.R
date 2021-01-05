# normalizePath with some sanity checks added
get_directory = function(dir) {
  dir = normalizePath(dir, mustWork=FALSE)

  if (!file.exists(dir)) {
    stop(domain=NA, gettextf('%s does not exist', dir, domain='R-potools'))
  }
  if (!file.info(dir)$isdir) {
    stop(domain=NA, gettextf('%s is not a directory', dir, domain='R-potools'))
  }
  return(normalizePath(dir))
}

# check dir is a package & return its name & version
get_desc_data = function(dir) {
  desc_file <- file.path(dir, 'DESCRIPTION')
  if (!file.exists(desc_file)) {
    stop(domain=NA, gettextf('%s is not a package (missing DESCRIPTION)', dir, domain='R-potools'))
  }
  desc_data <- read.dcf(desc_file, c('Package', 'Version'))
  if (nrow(desc_data) != 1L || anyNA(desc_data)) {
    stop(domain=NA, gettextf(
      '%s is not a package (missing Package and/or Version field in DESCRIPTION)', dir, domain='R-potools'
    ))
  }
  return(desc_data[1L, ])
}

# see ?tools::update_pkg_po
SYSTEM_REQUIREMENTS = c('xgettext', 'msgmerge', 'msgfmt', 'msginit', 'msgconv')
RTOOLS_URL = 'https://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip'

# nocov start. in principal, could do another GH action on an ill-equipped machine?
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
# nocov end

# parse the R files in a directory. do this once & reuse the results.
parse_r_files = function(dir) {
  r_files = package_r_files(dir)
  out = lapply(r_files, parse, keep.source=TRUE)
  names(out) = r_files
  return(out)
}

# get R files in a directory
list_r_files = function(dir) list.files(dir, full.names = TRUE, pattern = "(?i)\\.r")
# get R files in a package
package_r_files = function(dir) {
  dir = file.path(dir, 'R')
  r_files = list_r_files(dir)
  for (os in c("unix", "windows")) {
    os_dir = file.path(dir, os)
    if (dir.exists(os_dir)) r_files = c(r_files, list_r_files(os_dir))
  }
  # somehow on windows I was seeing absolute paths with \ but paths
  #   from list.files as / -- normalizePath makes it consistent
  return(normalizePath(r_files))
}

# patch analogous fix for Bugzilla#18025 here
`%is_name%` = function(e, f) is.name(e) && e %chin% f
`%is_base_call%` = function(e, f) {
  if (e %is_name% f) return(TRUE)
  if (!is.call(e) || length(e) != 3L) return(FALSE)
  return(e[[1L]] %is_name% "::" && e[[2L]] %is_name% "base" && e[[3L]] %is_name% f)
}
do_suppress = function(e) {
  domain = e[['domain']]
  !is.null(domain) && !is.name(domain) && is.na(domain)
}

gettextify = function(e, sep = '', package) {
  str_idx = vapply(e, is.character, logical(1L))

  if (all(str_idx)) {
    call_nm = "gettext"
    dots = ""
  } else {
    call_nm = "gettextf"
    dots = paste(",", toString(vapply(e[!str_idx], deparse1, character(1L))))
  }

  fmt = character(length(e))
  fmt[str_idx] = as.character(e[str_idx])
  fmt[!str_idx] = '%s'
  fmt = paste0(paste0(fmt[-length(fmt)], sep, collapse = ''), fmt[length(fmt)])

  sprintf(
    '%s("%s"%s, domain="R-%s")',
    call_nm, fmt, dots, package
  )
}

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
