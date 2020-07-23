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

