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
    trimws(sprintf(po_header_contents_fmt, pkg), 'left'),
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

#' Workhorse function for setting up translation infrastructure for a package.
#' @param dir a directory containing a package
#' @param copyright see `tools::update_pkg_po`
#' @param bugs see `tools::update_pkg_po`
initialize_translations = function(dir, copyright, bugs) {
  if (!nzchar(Sys.which('xgettext'))) warning(
    "gettext wasn't found on this system, or at least it's not on the PATH for this session. ",
    "Please ensure this is rectified before testing your translations."
  )

  pkg = get_package_name(dir)
  if (is.na(pkg)) stop(domain = NA, gettextf(
    "Translations are added within a package context, but %s doesn't appear to be a package directory (at a minimum, there should be a DESCRIPTION file in DCF format with a Package field)",
    dir, domain = "R-potools"
  ))

  src_files = list.files(file.path(dir, 'src'), pattern='\\.(?:c|cpp|h)')
  src_contents = sapply(
    src_files, simplify=FALSE,
    function(f) readLines(file.path(dir, 'src', f), warn=FALSE)
  )

  if (!length(src_files) || uses_src_po(src_contents))
    return(tools::update_pkg_po(dir, copyright=copyright, bugs=bugs))

  # could be improved, somewhat sloppy for now. a more robust version would treat the
  #   src files as a DAG, and look for where to "inject" a po.h header so as to "infect"
  #   all the source files with as few possible #include "po.h" as possible.
  # Instead, here I look at all c/cpp files and see which #include "header.h" header
  #   is most common, then inject #include "po.h" into that header, as well as into
  #   any "orphan" src files which don't include that header. In principle it could be
  #   added to one of the other "local" headers included in those orphans (this is done
  #   in data.table) but I eschew that for now.
  if (any(is_src <- grepl('\\.c(?:pp)?', src_files))) {
    write_po_header(file.path(dir, 'src'), pkg)

    src_direct_includes = sapply(src_contents[is_src], simplify=FALSE, function(flines) {
      direct_includes = grep('#\\s*include "', flines, value=TRUE, ignore.case=TRUE)
      gsub('.*#\\s*include "([^"]+)".*', '\\1', direct_includes)
    })

    top_header = names(sort(-table(unlist(src_direct_includes)))[1L])
    orphans = names(Filter(function(x) !top_header %in% x, src_direct_includes))

    # insert the include for "po.h" after the last include currently in the file;
    #   if there are no includes yet, put "po.h" at the top of the file
    for (needs_po in c(top_header, orphans)) {
      flines = src_contents[[needs_po]]
      last_include_idx = tail(grep('#\\s*include', flines, ignore.case=TRUE), 1L)
      if (length(last_include_idx)) {
        outlines = c(
          head(flines, last_include_idx),
          '#include "po.h"',
          tail(flines, -last_include_idx)
        )
      } else {
        outlines = c('#include "po.h"', flines)
      }
      writeLines(outlines, file.path(dir, 'src', needs_po))
    }
  }

  tools::update_pkg_po(dir, copyright=copyright, bugs=bugs)
}
