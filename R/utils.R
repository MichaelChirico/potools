list_r_files = function(dir) list.files(dir, full.names = TRUE, pattern = "(?i)\\.r")

# second condition is for calls like (function(x) x+1)(2)
is_name_call = function(e) is.call(e) && is.name(e[[1L]])
do_suppress = function(e) {
  domain = e[['domain']]
  !is.null(domain) && !is.name(domain) && is.na(domain)
}

# newlines in an R string, e.g. "\n" get printed literally in the .po file (c.f.
#   difference b/w print("\n") and cat("\n")), so right now, "\n" is "over-escaped" --
#   this step aims to "un-escape" to match the original R message. I'm not 100% sure this
#   is always accurate yet. But I did check base for similar escapes:
#   grep -Er '(^|[^\\])\\[^nt"\\]' ~/svn/R-devel/src/library/*/po/*.pot
# NB This regex is confusing because of multiple layers of string escaping :)
unescape_str = function(x) {
  # order matters: consider "\\n"
  x = gsub('\\\\', '\\', x, fixed = TRUE)
  # now that backslashes are unescaped, if we see \\n, it's not a newline
  x = gsub("(?:^|[^\\])[\\]n", "\n", x, fixed = TRUE)
  x = gsub("(?:^|[^\\])[\\]t", "\t", x, fixed = TRUE)
  x = gsub('(?:^|[^\\])[\\]"', '"', x, fixed = TRUE)
  x
}
