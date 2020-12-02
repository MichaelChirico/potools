list_r_files <- function(dir) list.files(dir, full.names = TRUE, pattern = "(?i)\\.r")

# second condition is for calls like (function(x) x+1)(2)
is_name_call <- function(e) is.call(e) && is.name(e[[1L]])
do_suppress <- function(e) {
  domain = e[['domain']]
  !is.null(domain) && !is.name(domain) && is.na(domain)
}
