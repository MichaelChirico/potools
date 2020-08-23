#' Determine whether a path is a package by
#'   looking for the Package: field in a dcf file
#' @param dir a (one) path
is_package_tld = function(dir) {

}

#' Workhorse function for setting up translation infrastructure for a package.
#' @param dir a directory containing a package
#' @param copyright see `tools::update_pkg_po`
#' @param bugs see `tools::update_pkg_po`
initialize_translations = function(dir, copyright, bugs) {
  if (!is_package_tld(dir)) stop(domain = NA, gettextf(
    "Translations are added within a package context, but %s doesn't appear to be a package directory (at a minimum, there should be a DESCRIPTION file in DCF format with a Package field)",
    dir, domain = "R-potools"
  ))

  src_files = list.files(file.path(dir, 'src'))
  # could be improved, somewhat sloppy for now. a more robust version would treat the
  #   src files as a DAG, and look for where to "inject" a po.h header so as to "infect"
  #   all the source files with as few possible #include "po.h" as possible.
  # Instead, here I look at all c/cpp files and see which #include "header.h" header
  #   is most common, then inject #include "po.h" into that header, as well as into
  #   any "orphan" src files which don't include that header. In principle it could be
  #   added to one of the other "local" headers included in those orphans (this is done
  #   in data.table) but I eschew that for now.
  if (any(is_src <- grepl('\\.c(?:pp)?', src_files))) {
    src_direct_includes = sapply(src_files[is_src], simplify=FALSE, function(f) {
      flines = readLines(file.path(dir, 'src', f), warn=FALSE)
      direct_includes = grep('#\\s*include "', flines, value=TRUE, ignore.case=TRUE)
      gsub('.*#\\s*include "([^"]+)".*', '\\1', direct_includes)
    })

    top_header = names(sort(-table(unlist(src_direct_includes)))[1L])

    orphans = names(Filter(function(x) !top_header %in% x, src_direct_includes))
  }

  tools::update_pkg_po(dir, copyright=copyright, bugs=bugs)
}
