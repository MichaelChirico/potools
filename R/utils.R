#' get the name of the package as found in the Package
#'   field of the file at dir/DESCRIPTION (or return NA if none)
get_package_name = function(dir) {
  if (length(dir) > 1L) stop("Please provide a single directory path")
  desc_path = file.path(dir, 'DESCRIPTION')
  desc_info = na.omit(file.info(desc_path))
  if (nrow(desc_info) != 1L || desc_info$isdir || !desc_info$size) return(NA_character_)
  read.dcf(desc_path, 'Package')[1L]
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
