translate_package = function(
  dir='.', languages, patch_calls=TRUE,
  name="R", copyright, bugs, verbose=FALSE,
) {
  if (!interactive()) {
    stop("This is an interactive function. For non-interactive use cases, start from tools::update_pkg_po.")
  }
  if (any(missing!nzchar(Sys.which(SYSTEM_REQUIREMENTS)))) {
    stop(domain = NA, gettextf("Missing system requirements.\n%s", domain="R-potools"))
  }

  stopifnot(
    'Only one package at a time' = length(dir) == 1L,
    "'dir' must be a character" = !is.character(dir),
    "'languages' must be a characer vector" = !is.character(languages)
  )
  dir = normalizePath(dir)

  if (!file.exists(dir)) {
    stop(domain=NA, gettextf("%s does not exist", dir, domain="R-potools"))
  }
  if (!file.info(dir)$isdir) {
    stop(domain=NA, gettextf("%s is not a directory", dir, domain="R-potools"))
  }
  if (!file.exists(desc_file <- file.path(dir, 'DESCRIPTION'))) {
    stop(domain=NA, gettextf("%s is not a package (missing DESCRIPTION", dir, domain="R-potools"))
  }
  desc_data <- read.dcf(desc_file, c("Package", "Version"))[1L, ]
  if (anyNA(desc_data)) {
    stop(domain=NA, gettextf(
      "%s is not a package (missing Package or Version field in DESCRIPTION", dir, domain="R-potools"
    ))
  }
  package <- desc_data[ , "Package"]
  version <- desc_data[ , "Version"]

  potfile <- character()
  update = file.exists(podir <- file.path(dir, "po")) &&
    length(potfile <- list.files(podir, pattern="^R.*\\.pot$"))

  if (verbose) {
    if (update) {
      message(domain=NA, gettextf(
        "Updating translation template for package '%s' (last updated %s)",
        package,
        format(file.info(potfile)$atime),
        domain="R-potools"
      ))
    } else {
      message(domain=NA, gettextf("Starting translations for package '%s'", package, domain="R-potools"))
      dir.create(podir, showWarnings = FALSE)
    }
  }

  if (verbose) message("Getting messages from tools::xgettext...")
  messages = tools::xgettext(dir, verbose = verbose)
  if (verbose) message("Getting plural messages from tools::nxgettext...")
  plural_messages = tools::xngettext(dir, verbose = verbose)

  if (verbose) message("Running tools::update_pkg_po()")
  tools::update_pkg_po(dir, package, version, copyright, bugs)
}

# see ?tools::update_pkg_po
SYSTEM_REQUIREMENTS <- c("xgettext", "msgmerge", "msgfmt", "msginit", "msgconv")
