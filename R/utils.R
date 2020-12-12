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

# would be great to use readline() but it has several fatal flaws:
#   (1) the prompt argument is a buffer capped at 256 chars, which is far too few
#   (2) readline is _strictly_ interactive -- it can't be tested.
# See this post for testing:
#   https://debruine.github.io/posts/interactive-test/
prompt = function(..., conn = getOption('__potools_testing_prompt_connection__', stdin())) {
  cat(...)
  cat('\n')
  return(readLines(conn, n=1L))
}

#' small wrapper to list the (code) contents of the `src` directory
#' @param dir a file path to a package directory
list_src_files = function(dir) list.files(file.path(dir, 'src'), pattern='\\.(?:c|cpp|h)')

#' load the src code with `readLines`. only one of `dir` or `src_files` should be used
#' @param dir a file path to a package directory
#' @param src_files the files in `src` in a package
load_src_contents = function(dir, src_files) {
  if (missing(src_files)) src_files = list_src_files(dir)
 sapply(
    src_files, simplify=FALSE,
    function(f) readLines(file.path(dir, 'src', f), warn=FALSE)
  )
}

#' simple function for creating a po.h header that enables
#'   _(...) to be used as a simple wrapper macro for
#'   identifying translatable strings for gettext, as is
#'   done in base R and recommended by R-exts:
#'     https://cran.r-project.org/doc/manuals/R-exts.html#Internationalization
#' @param src_dir the package/src directory path
#' @param pkg the package name
po_header_contents_fmt = '
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("%s", String)
#else
#define _(String) (String)
#endif
'
write_po_header = function(src_dir, pkg) {
  writeLines(
    trimws(sprintf(po_header_contents_fmt, pkg)),
    file.path(src_dir, 'po.h')
  )
}

#' Try to determine if this package has already been set up for use with
#'   src translations through two potential pathways:
#'   (1) The po.h header file
#'   (2) A macro definition of _ as a dgettext wrapper
#' @param src_contents the contents of src files, as returned by `readLines`
uses_src_po = function(src_contents) {
  if ('po.h' %in% names(src_contents)) return(TRUE)
  for (flines in src_contents) {
    if (any(grepl('#\\s*define _.*\\bdgettext\\(', flines, ignore.case=TRUE))) return(TRUE)
  }
  return(FALSE)
}

#' Check if any messages in src use a translation already. If not,
#'   `tools::update_pkg_po` errors
uses_src_translation = function(src_contents) {
  any(sapply(src_contents, function(flines) {
    any(grepl('\\b_\\("', flines))
  }))
}
