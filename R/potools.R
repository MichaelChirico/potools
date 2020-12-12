# load_msg = function(f) {
#   function(f, strictPlural = FALSE)
# {
#     getfmts <- function(s) .Call(C_getfmts, s)
#
#     lines <- readLines(f, encoding = "bytes")
#     i <- 0L
#     noCformat <- FALSE
#     f1_plural <- NULL
#     ref <- NA
#     fuzzy <- FALSE
#
#     while (i < length(lines)) {
#       i <- i + 1L
#
#       if (startsWith(lines[i], "#,")) { # useBytes=TRUE (speedup ?)
#         noCformat <- noCformat || grepl("no-c-format", lines[i], useBytes = TRUE)
#         fuzzy <- fuzzy || grepl("fuzzy", lines[i], useBytes = TRUE)
#       } else if (startsWith(lines[i], "#:")) {
#         if (!is.na(ref))
#           ref <- paste(ref, "etc.")
#         else
#           ref <- sub("^#:[[:blank:]]*", "", lines[i])
#       } else if (startsWith(lines[i], "msgid ")) {
#         s1 <- sub('^msgid[[:blank:]]+["](.*)["][[:blank:]]*$', "\\1", lines[i])
#         while (startsWith(lines[i+1L], '"')) {
#           i <- i + 1L
#           s1 <- paste0(s1, sub('^["](.*)["][[:blank:]]*$', "\\1", lines[i]))
#         }
#         f1 <- tryCatch(getfmts(s1), error = identity)
#         j <- i + 1L
#
#         if (noCformat || inherits(f1, "error")) {
#           noCformat <- FALSE
#           next
#         }
#
#         while (j <= length(lines)) {
#           if (grepl("^msgid_plural[[:blank:]]", lines[j], useBytes = TRUE))
#             statement <- "msgid_plural"
#           else if (grepl("^msgstr[[:blank:]]", lines[j], useBytes = TRUE))
#             statement <- "msgstr"
#           else if (grepl("^msgstr\\[[[:digit:]]+\\][[:blank:]]", lines[j], useBytes = TRUE))
#             statement <- sub("^(msgstr)\\[([[:digit:]]+)\\].*$", "\\1\\\\[\\2\\\\]", lines[j])
#           else
#             break
#
#           s2 <- sub( paste0("^", statement, '[[:blank:]]+["](.*)["][[:blank:]]*$'),
#                      "\\1", lines[j])
#           while (!is.na(lines[j+1L]) && startsWith(lines[j+1L], '"')) {
#             j <- j+1L
#             s2 <- paste0(s2, sub('^["](.*)["][[:blank:]]*$', "\\1", lines[j]))
#           }
#
#           if (s1 == "") { # The header
#             encoding <- sub(".*Content-Type:[^\\]*charset=([^\\[:space:]]*)[[:space:]]*\\\\n.*", "\\1", s2)
#             lines <- iconv(lines, encoding, "UTF-8")
#             break
#           }
#
#           f2 <- tryCatch(getfmts(s2), error = identity)
#
#           if (statement == "msgid_plural") {
#             if (!strictPlural) {
#               f1_plural <- f2
#               j <- j+1L
#               next
#             }
#           }
#
#           if (nzchar(s2) &&
#               !(identical(f1, f2) || identical(f1_plural, f2))) {
#             location <- paste0(f, ":", j)
#             if (inherits(f2, "error"))
#               diff <- conditionMessage(f2)
#             else {
#               if (length(f1) < length(f2)) {
#                 diff <- "too many entries"
#                 length(f2) <- length(f1)
#               } else if (length(f1) > length(f2)) {
#                 diff <- "too few entries"
#                 length(f1) <- length(f2)
#               } else
#                 diff <- ""
#               diffs <- which(f1 != f2)
#               if (length(diffs)) {
#                 if (nzchar(diff))
#                   diff <- paste0(diff, ", ")
#                 if (length(diffs) > 1)
#                   diff <- paste(paste0(diff, "differences in entries"),
#                                 paste(diffs, collapse = ","))
#                 else
#                   diff <- paste(paste0(diff, "difference in entry"),
#                                 diffs)
#               }
#               if (grepl("\u066A", s2, fixed=TRUE))
#                 diff <- paste0(diff, ", translation contains arabic percent sign U+066A")
#               if (grepl("\uFE6A", s2, fixed=TRUE))
#                 diff <- paste0(diff, ", translation contains small percent sign U+FE6A")
#               if (grepl("\uFF05", s2, fixed=TRUE))
#                 diff <- paste0(diff, ", translation contains wide percent sign U+FF05")
#             }
#             if (!fuzzy)
#               result <- rbind(result, c(location, ref, diff, s1, s2))
#           }
#           j <- j+1L
#         }
#         i <- j-1L
#         noCformat <- FALSE
#         f1_plural <- NULL
#         ref <- NA
#         fuzzy <- FALSE
#       }
#     }
#     structure(result, class = "check_po_files")
#   }
# }

load_msg = function(f) {
  function(f, strictPlural = FALSE)
{
    getfmts <- function(s) .Call(C_getfmts, s)

    lines <- readLines(f, encoding = "bytes")
    i <- 0L
    noCformat <- FALSE
    f1_plural <- NULL
    ref <- NA
    fuzzy <- FALSE

    while (i < length(lines)) {
      i <- i + 1L

      if (startsWith(lines[i], "#,")) { # useBytes=TRUE (speedup ?)
        noCformat <- noCformat || grepl("no-c-format", lines[i], useBytes = TRUE)
        fuzzy <- fuzzy || grepl("fuzzy", lines[i], useBytes = TRUE)
      } else if (startsWith(lines[i], "#:")) {
        if (!is.na(ref))
          ref <- paste(ref, "etc.")
        else
          ref <- sub("^#:[[:blank:]]*", "", lines[i])
      } else if (startsWith(lines[i], "msgid ")) {
        s1 <- sub('^msgid[[:blank:]]+["](.*)["][[:blank:]]*$', "\\1", lines[i])
        while (startsWith(lines[i+1L], '"')) {
          i <- i + 1L
          s1 <- paste0(s1, sub('^["](.*)["][[:blank:]]*$', "\\1", lines[i]))
        }
        f1 <- tryCatch(getfmts(s1), error = identity)
        j <- i + 1L

        if (noCformat || inherits(f1, "error")) {
          noCformat <- FALSE
          next
        }

        while (j <= length(lines)) {
          if (grepl("^msgid_plural[[:blank:]]", lines[j], useBytes = TRUE))
            statement <- "msgid_plural"
          else if (grepl("^msgstr[[:blank:]]", lines[j], useBytes = TRUE))
            statement <- "msgstr"
          else if (grepl("^msgstr\\[[[:digit:]]+\\][[:blank:]]", lines[j], useBytes = TRUE))
            statement <- sub("^(msgstr)\\[([[:digit:]]+)\\].*$", "\\1\\\\[\\2\\\\]", lines[j])
          else
            break

          s2 <- sub( paste0("^", statement, '[[:blank:]]+["](.*)["][[:blank:]]*$'),
                     "\\1", lines[j])
          while (!is.na(lines[j+1L]) && startsWith(lines[j+1L], '"')) {
            j <- j+1L
            s2 <- paste0(s2, sub('^["](.*)["][[:blank:]]*$', "\\1", lines[j]))
          }

          if (s1 == "") { # The header
            encoding <- sub(".*Content-Type:[^\\]*charset=([^\\[:space:]]*)[[:space:]]*\\\\n.*", "\\1", s2)
            lines <- iconv(lines, encoding, "UTF-8")
            break
          }

          f2 <- tryCatch(getfmts(s2), error = identity)

          if (statement == "msgid_plural") {
            if (!strictPlural) {
              f1_plural <- f2
              j <- j+1L
              next
            }
          }

          if (nzchar(s2) &&
              !(identical(f1, f2) || identical(f1_plural, f2))) {
            location <- paste0(f, ":", j)
            if (inherits(f2, "error"))
              diff <- conditionMessage(f2)
            else {
              if (length(f1) < length(f2)) {
                diff <- "too many entries"
                length(f2) <- length(f1)
              } else if (length(f1) > length(f2)) {
                diff <- "too few entries"
                length(f1) <- length(f2)
              } else
                diff <- ""
              diffs <- which(f1 != f2)
              if (length(diffs)) {
                if (nzchar(diff))
                  diff <- paste0(diff, ", ")
                if (length(diffs) > 1)
                  diff <- paste(paste0(diff, "differences in entries"),
                                paste(diffs, collapse = ","))
                else
                  diff <- paste(paste0(diff, "difference in entry"),
                                diffs)
              }
              if (grepl("\u066A", s2, fixed=TRUE))
                diff <- paste0(diff, ", translation contains arabic percent sign U+066A")
              if (grepl("\uFE6A", s2, fixed=TRUE))
                diff <- paste0(diff, ", translation contains small percent sign U+FE6A")
              if (grepl("\uFF05", s2, fixed=TRUE))
                diff <- paste0(diff, ", translation contains wide percent sign U+FF05")
            }
            if (!fuzzy)
              result <- rbind(result, c(location, ref, diff, s1, s2))
          }
          j <- j+1L
        }
        i <- j-1L
        noCformat <- FALSE
        f1_plural <- NULL
        ref <- NA
        fuzzy <- FALSE
      }
    }
    structure(result, class = "check_po_files")
  }
}

load_po = function(pkg = '.') {
# output data.table:
#  $ type [src/R]
#  $ domain [zh_CN/zh_TW/...]
#  $ msgid
#  $ source
#  $ n (for msgstr[0] etc.)
#  $ msgstr

}

find_new_src_messages = function(pkg = '.', macro = '_') {
# look for char arrays that aren't _()-processed
}

#' Workhorse function for setting up translation infrastructure for a package.
#' @param dir a directory containing a package
#' @param copyright see `tools::update_pkg_po`
#' @param bugs see `tools::update_pkg_po`
initialize_translations = function(dir = '.', copyright, bugs, verbose=TRUE) {
  if (!nzchar(Sys.which('xgettext'))) warning(
    "gettext wasn't found on this system, or at least it's not on the PATH for this session. ",
    "Please ensure this is rectified before testing your translations."
  )

  pkg = get_package_name(dir)

  src_files = list_src_files(dir)
  src_contents = load_src_contents(src_files = src_files)

  if (!length(src_files)) {
    if (verbose) message("Didn't find any src messages to translate; simply running tools::update_pkg_po, which will work for this package.")
    return(tools::update_pkg_po(dir, copyright=copyright, bugs=bugs))
  }
  if (uses_src_po(src_contents)) {
    if (verbose) message("Detected a pre-existing setup for translating src messages (e.g. a po.h header)")
    if (!file.exists(src_po <- file.path(dir, 'po', sprintf('%s.pot', pkg)))) {
      if (verbose) message(domain=NA, gettextf(
        "Creating the file po/%s.pot for src message templates", pkg, domain="R-potools"
      ))
      dir.create(file.path(dir, 'po'), showWarnings=FALSE)
      file.create(src_po)
    }
    if (uses_src_translation(src_contents)) {
      if (verbose) message("Running tools::update_pkg_po")
      return(tools::update_pkg_po(dir, copyright=copyright, bugs=bugs))
    } else {
      if (verbose) message('No messages in src are translated yet. Wrap string literals with _() to mark them for translation, e.g. "Error! Try again!" becomes _("Error! Try again!")')
      return(NULL)
    }
  }

  # could be improved, somewhat sloppy for now. a more robust version would treat the
  #   src files as a DAG, and look for where to "inject" a po.h header so as to "infect"
  #   all the source files with as few possible #include "po.h" as possible.
  # Instead, here I look at all c/cpp files and see which #include "header.h" header
  #   is most common, then inject #include "po.h" into that header, as well as into
  #   any "orphan" src files which don't include that header. In principle it could be
  #   added to one of the other "local" headers included in those orphans (this is done
  #   in data.table) but I eschew that for now.
  # Below could also condition on whether a src file does/doesn't have any string literals...
  if (verbose) message("No existing setup for translating src messages detected; creating src/po.h and adding an #include as needed...")
  if (any(is_src <- grepl('\\.c(?:pp)?', src_files))) {
    write_po_header(file.path(dir, 'src'), pkg)

    # need to create this file for update_pkg_po to work completely
    dir.create(file.path(dir, 'po'), showWarnings=FALSE)
    file.create(file.path(dir, 'po', sprintf('%s.pot', pkg)))

    src_direct_includes = sapply(src_contents[is_src], simplify=FALSE, function(flines) {
      direct_includes = grep('#\\s*include "', flines, value=TRUE, ignore.case=TRUE)
      gsub('.*#\\s*include "([^"]+)".*', '\\1', direct_includes)
    })

    top_header = names(sort(-table(unlist(src_direct_includes)))[1L])
    orphans = names(Filter(function(x) !top_header %in% x, src_direct_includes))

    # insert the include for "po.h" after the last include currently in the file;
    #   if there are no includes yet, put "po.h" at the top of the file
    for (needs_po in c(top_header, orphans)) {
      if (verbose) message(domain=NA, gettextf(
        "Adding #include of po.h to src/%s", needs_po, domain="R-potools"
      ))
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

  if (verbose) message('Translation is now prepared -- please translate strings in the src directory by wrapping them with _(), e.g. "Error! Try again!" becomes _("Error! Try again!")')
  return(NULL)
}

#' Browse through the src directory and identify message strings (literal character arrays)
#'   which are not yet translated. This is a bit fuzzy because we don't want to
#'   translate _every_ character array (e.g. `"a"` is not to be translated), but
#'   we don't (basically can't) know the full set of calls that could result in
#'   something send to stdout. So we build a standard/default set of `calls` to look
#'   for, while also allowing for customization
find_untranslated_src_messages = function(
  dir = '.', calls = c('error', 'warning', 'Rprintf'),
  ask_to_translate = interactive()
) {
  #anyone please has an AST builder for C in R???
  calls_re = sprintf('\\b(?:%s)\\(', paste(calls, collapse='|'))
  src_contents = load_src_contents(dir = dir)
  for (flines in src_contents) {
    nlines = length(flines)
    linei = 1L
    while (linei <= nlines) {
      if (grepl(calls_re, flines[linei])) {
      }
      linei = linei + 1L
    }
  }
}
