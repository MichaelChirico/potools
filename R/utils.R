# normalizePath with some sanity checks added
get_directory = function(dir) {
  dir = normalizePath(dir, mustWork=FALSE)

  if (!file.exists(dir)) {
    stop(domain=NA, gettextf('%s does not exist', dir))
  }
  if (!file.info(dir)$isdir) {
    stop(domain=NA, gettextf('%s is not a directory', dir))
  }
  return(normalizePath(dir))
}

# check dir is a package & return its name & version
get_desc_data = function(dir) {
  desc_file <- file.path(dir, 'DESCRIPTION')
  if (!file.exists(desc_file)) {
    stop(domain=NA, gettextf('%s is not a package (missing DESCRIPTION)', dir))
  }
  desc_data <- read.dcf(desc_file, c('Package', 'Version'))
  if (nrow(desc_data) != 1L || anyNA(desc_data)) {
    stop(domain=NA, gettextf(
      '%s is not a package (missing Package and/or Version field in DESCRIPTION)',
      dir
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
        RTOOLS_URL
      )
    } else {
      if (Sys.info()['sysname'] == 'Darwin') {
        platform_msg = gettext('These GNU tools are commonly available, try installing from brew or apt-get')
      } else {
        platform_msg = gettext(
          'These GNU tools are commonly available from the Linux package manager for your system'
        )
      }
    }
    stop(domain = NA, gettextf(
      'Missing (or not on PATH) system requirements %s.\n%s',
      toString(SYSTEM_REQUIREMENTS[is_missing]), platform_msg
    ))
  }
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
  "(?<all_template>",
    "[%]|",
    "(?<template>", # % separately to reduce false positives -- it can't be used with other specials
      "(?<redirect>[1-9][0-9]?[$])?", # "redirection" markers -- %2$s says "use the second element of ... here"
      "(?<width_precision>[0-9*]+[.]?|[.]?[0-9*]+|[0-9]+[.][0-9]+|[- +#])*",
      # taken from https://en.wikipedia.org/wiki/Printf_format_string and http://manpages.org/sprintf
      "(?<id>[aAcdeEfgGiopsuxX]|(?:ll?|I(?:32|64))[diux]|l[cs]|<PRI[diux](?:32|64)>)",
    ")",
  ")"
)

# given a string like
#   Found %d arguments in %s. Average %02.3f%%
# get data necessary to build a second line
# highlighting the string format templates:
#   Found %d arguments in %s. \n Average %02.3f%%\n
#         ^^              ^^             ^----^^^^^
get_specials_metadata = function(x) {
  # tested with xgettext: warning against using \a | \b | \f | \v in internationalized messages;
  #   and msgfmt doesn't warn about whether \t is matched, so no need to bother matching that either
  special_matches = gregexpr(sprintf("%s|^[\\]n|[\\]n$", SPRINTF_TEMPLATE_REGEX), x)[[1L]]

  if (special_matches[1L] < 0L) {
    return(data.table(special=character(), id = character(), start=integer(), stop=integer()))
  }

  meta = data.table(
    start = special_matches,
    end = special_matches + attr(special_matches, 'match.length') - 1L
  )
  meta[ , "special" := substring(x, start, end)]

  # % escapes don't matter for validation
  meta = meta[special != "%%"]

  template_idx = meta$special != "\\n"
  redirect_idx = grepl("%[1-9][0-9]?[$]", meta$special)
  if (any(template_idx & redirect_idx)) {
    if (any(template_idx & !redirect_idx)) stop(domain=NA, gettextf(
      "Invalid templated message. If any %N$ redirects are used, all templates must be redirected.\n\tRedirected tempates: %s\n\t Un-redirected templates: %s",
      meta$special[template_idx & redirect_idx], meta$special[template_idx & !redirect_idx]
    ))

    meta[ , "redirect_id" := fifelse(
      template_idx,
      as.integer(sub("%([1-9][0-9]?)[$].*", "\\1", special)),
      0L
    )]
    meta[(template_idx), "special" := gsub("%[1-9][0-9]?[$](.*)", "%\\1", special)]

    meta[(template_idx), "special_id" := gsub("%(?:[^*]*)([a-zA-Z].*)")]
  }
  redirect_id = rep(0L, length(special))
  redirect_id[template_idx]

  # strip away extraneous parts of the format to which
  id =
  list(idx = as.integer(special_starts), length = attr(special_starts, 'match.length'))
}

# streamlined get_specials for counting the matches
count_formats = function(x) vapply(
  gregexpr(SPRINTF_TEMPLATE_REGEX, x),
  function(x) if (x[1L] < 0L) 0L else length(x),
  integer(1L)
)

get_specials_metadata = function(s) {
  specials = get_specials(s)
  if (!length(specials)) return('')

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

# shQuote(type='cmd') + encodeString, but don't wrap in the outer ""
escape_string = function(x) gsub('"', '\\"', encodeString(x), fixed = TRUE)

# substring often gets run on 0-row j, which errors substring; just exit in that case
safe_substring = function(text, first, last) {
  if (!length(first)) return(character())
  substring(text, first, last)
}
