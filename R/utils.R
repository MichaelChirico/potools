# normalizePath with some sanity checks added
get_directory = function(dir) {
  dir = normalizePath(dir, mustWork=FALSE)

  if (!file.exists(dir)) {
    stopf('%s does not exist', dir)
  }
  if (!file.info(dir)$isdir) {
    stopf('%s is not a directory', dir)
  }
  return(normalizePath(dir))
}

# check dir is a package & return its name & version
get_desc_data = function(dir) {
  desc_file <- file.path(dir, 'DESCRIPTION')
  if (!file.exists(desc_file)) {
    stopf('%s is not a package (missing DESCRIPTION)', normalizePath(dir))
  }
  desc_data <- read.dcf(desc_file, c('Package', 'Version'))
  if (nrow(desc_data) != 1L || anyNA(desc_data)) {
    stopf('%s is not a package (missing Package and/or Version field in DESCRIPTION)', normalizePath(dir))
  }
  return(desc_data[1L, ])
}

# msgmerge | msgmerge.R | run_msgmerge()
# msgfmt   | msgmerge.R | run_msgfmt()
# msginit  | msgmerge.R | tools:::en_quote()
# msgconv  | msgmerge.R | tools:::en_quote()
SYSTEM_REQUIREMENTS = c('msgmerge', 'msgfmt', 'msginit', 'msgconv')
RTOOLS_URL = 'https://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip'

# nocov start. in principal, could do another GH action on an ill-equipped machine?
check_potools_sys_reqs = function() {
  if (any(is_missing <- !nzchar(Sys.which(SYSTEM_REQUIREMENTS)))) {
    if (.Platform$OS.type == 'windows') {
      platform_msg = gettextf(
        'These tools are available as an Rtools goodie, check %s',
        RTOOLS_URL
      )
    } else if (Sys.info()['sysname'] == 'Darwin') {
        platform_msg = gettext('These GNU tools are commonly available, try installing from brew or apt-get')
    } else {
      platform_msg = gettext(
        'These GNU tools are commonly available from the Linux package manager for your system'
      )
    }
    return(gettextf(
      'Missing (or not on PATH) system requirements %s.\n%s',
      toString(SYSTEM_REQUIREMENTS[is_missing]), platform_msg
    ))
  }
  return(TRUE)
}
# nocov end

list_package_files = function(dir, subdir, subsubdirs = character(), pattern) {
  subdir = file.path(dir, subdir)
  files = list.files(subdir, pattern = pattern)
  for (subsubdir in subsubdirs) {
    subsubdir_ = file.path(subdir, subsubdir)
    if (dir.exists(subsubdir_)) files = c(files, file.path(subsubdir, list.files(subsubdir_, pattern = pattern)))
  }
  return(files)
}

# patch analogous fix for Bugzilla#18025 here
`%is_name%` = function(e, f) is.name(e) && e %chin% f
`%is_base_call%` = function(e, f) {
  if (e %is_name% f) return(TRUE)
  if (!is.call(e) || length(e) != 3L) return(FALSE)
  return(e[[1L]] %is_name% "::" && e[[2L]] %is_name% "base" && e[[3L]] %is_name% f)
}

gettextify = function(e, sep = '') {
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
  if (length(fmt) > 1L) {
    fmt = paste0(paste0(fmt[-length(fmt)], sep, collapse = ''), fmt[length(fmt)])
  }

  sprintf(
    '%s("%s"%s)',
    call_nm, fmt, dots
  )
}

# regex for sprintf templates is taken from a thorough reading of ?sprintf,
#   https://en.wikipedia.org/wiki/Printf_format_string, http://manpages.org/sprintf,
#   lots of iteration on the set of msgid in .pot files in r-devel, and some ad-hoc testing
#   of msgfmt in sample .po files. See #7.
SPRINTF_TEMPLATE_REGEX = paste0(
  "[%]",
  "(?<all_template>",
    "[%]|", # % separately to reduce false positives -- it can't be used with other specials
    "(?<template>",
      "(?<redirect>[1-9][0-9]?[$])?",
      "(?<width_precision>[0-9]+[.]?|[0-9]*[.][0-9]+|[*]|[*][.][0-9]+|[0-9]+[.][*]|[- +#])?",
      "(?<id>[aAcdeEfgGiopsuxX]|ll?[diux]|I(?:32|64)[diux]|l[cs]|<PRI[diux](?:32|64)>)",
    ")",
  ")"
)

# shQuote(type='cmd') + encodeString, but don't wrap in the outer ""
escape_string = function(x) gsub('"', '\\"', encodeString(x), fixed = TRUE)

# substring often gets run on 0-row j, which errors substring; just exit in that case
safe_substring = function(text, first, last) {
  if (!length(first)) return(character())
  substring(text, first, last)
}

build_exclusion_ranges = function(starts, ends) {
  all_counts = merge(
    starts[ , .N, by = 'file'],
    ends[ , .N, by = 'file'],
    by = 'file', all = TRUE
  )
  setnafill(all_counts, fill=0L, cols = 2:3)
  if (nrow(all_counts[N.x != N.y])) {
    stopf(
      "Invalid # notranslate start/end range(s):\n%s",
      all_counts[N.x != N.y, toString(gettextf("File %s: %d start(s) / %d end(s)", file, N.x, N.y))]
    )
  }

  ranges = starts[ends, on = c('file', 'line1'), roll = Inf, .(file, start = x.line1, end = i.line1)]
  if (anyNA(ranges$start)) {
    stopf(
      "Invalid # notranslate start/end range(s):\n%s",
      ranges[is.na(start), gettextf("Unmatched start/end pairs in files: %s", toString(unique(file)))]
    )
  }

  ranges
}

get_lang_metadata = function(language) {
  if (language %chin% .potools$KNOWN_LANGUAGES$code) {
    return(.potools$KNOWN_LANGUAGES[.(language)])
  }
  update_metadata(language)
}
