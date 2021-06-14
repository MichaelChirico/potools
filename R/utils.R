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

# parse the R files in a directory. do this once & reuse the results.
parse_r_files = function(dir) {
  r_files = package_r_files(dir)
  out = lapply(r_files, parse, keep.source=TRUE)
  names(out) = r_files
  return(out)
}

# get R files in a directory
list_r_files = function(dir) list.files(dir, full.names = TRUE, pattern = "(?i)\\.r$")
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

# get src files in a directory. exclude .h files
list_src_files = function(dir, use_base_rules = FALSE) {
  # recursive to include subdirectories, e.g. as found in R-devel/src/library/{grDevices,utils}
  # NB: tools::update_pkg_po() is only looking for files in
  #   src/*.{c,cc,cpp,m,mm} and src/windows/*.{c,cc,cpp,m,mm}
  if (use_base_rules) {
    src_files = c(list.files(dir, full.names = TRUE), list.files(file.path(dir, 'windows'), full.names = TRUE))
  } else {
    src_files = list.files(dir, full.names = TRUE, recursive = TRUE)
  }
  # tried to be inclusive at first, but stringi broke my resolve -- src/ includes
  #   .zip, .txt, .html, .pl, ... broke my conviction we have any hope of making anything
  #   but an allow list succeed here.
  # thus we match the update_pkg_po() behavior to only include *.{c,cc,cpp,m,mm} files
  grep("\\.(c|cc|cpp|m|mm)$", src_files, value = TRUE, ignore.case = TRUE)
}
# get src files in a package
package_src_files = function(dir, use_base_rules = FALSE) {
  dir = file.path(dir, 'src')
  src_files = list_src_files(dir, use_base_rules)
  # somehow on windows I was seeing absolute paths with \ but paths
  #   from list.files as / -- normalizePath makes it consistent
  return(normalizePath(src_files))
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
  "(?:",
    "[%]|", # % separately to reduce false positives -- it can't be used with other specials
    "(?:[1-9][0-9]?[$])?", # "redirection" markers -- %2$s says "use the second element of ... here"
    "(?:[0-9*]+[.]?|[.]?[0-9*]+|[0-9]+[.][0-9]+|[ -+#])*",
    "[aAcdeEfgGiopsuxX]|", # taken from https://en.wikipedia.org/wiki/Printf_format_string#Type_field
    "<[a-zA-Z0-9_]+>", # macro-based formatters in C, e.g. %<PRId64>
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

# substring often gets run on 0-row j, which errors substring; just exit in that case
safe_substring = function(text, first, last) {
  if (!length(first)) return(character())
  substring(text, first, last)
}
