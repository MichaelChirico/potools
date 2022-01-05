test_that("translate_package arg checking errors work", {
  expect_error(translate_package(c("dplyr", "data.table")), "Only one package at a time", fixed=TRUE)
  expect_error(translate_package(1), "'dir' must be a character", fixed=TRUE)
  expect_error(translate_package(languages = 1L), "'languages' must be a character vector", fixed=TRUE)
  expect_error(translate_package(file.path(tempdir(), "abcdefghijklmnopqrstuvwxyz")), "does not exist", fixed=TRUE)

  file.create(tmp <- tempfile())
  on.exit(unlink(tmp))
  expect_error(translate_package(tmp), "not a directory", fixed=TRUE)

  expect_error(translate_package(tempdir()), "not a package (missing DESCRIPTION)", fixed=TRUE)

  file.create(tmp_desc <- file.path(tempdir(), "DESCRIPTION"))
  on.exit(unlink(tmp_desc), add=TRUE)
  expect_error(translate_package(tempdir()), "not a package (missing Package and/or Version", fixed=TRUE)

  # diagnostic argument
  expect_error(translate_package(tempdir(), diagnostics = 1L), "'diagnostics' should be", fixed=TRUE)
  expect_error(translate_package(tempdir(), diagnostics = list(1L)), "'diagnostics' should be", fixed=TRUE)
})

with_restoration_test_that("translate_package handles empty packages", "no_msg", {
  expect_normalized_snapshot(translate_package())
  expect_normalized_snapshot(translate_package(verbose=TRUE))
})

with_restoration_test_that("translate_package handles a data package (no R dir)", "r_data_pkg", {
  expect_normalized_snapshot(translate_package(verbose=TRUE))
})

with_restoration_test_that("translate_package works on a simple package w/o translating", "r_msg", {
  mockery::stub(translate_package, "get_atime", "0000-01-01 00:00:00")
  expect_normalized_snapshot(translate_package(verbose = TRUE))

  pkg_files <- list.files(recursive=TRUE)

  pot_file <- "po/R-rMsg.pot"
  expect_true(pot_file %in% pkg_files)
  # testing gettextf's ... arguments are skipped
  expect_all_match(readLines(file.path(pot_file)), "don't translate me", invert=TRUE, fixed=TRUE)

  # Non-UTF-8 machines don't run en@quot translations by default.
  #   Mostly applies to Windows, but can also apply to Unix
  #   (e.g. r-devel-linux-x86_64-debian-clang on CRAN), #191
  if (l10n_info()[["UTF-8"]]) {
    expect_match(pkg_files, "inst/po/en@quot/LC_MESSAGES/R-rMsg.mo", all = FALSE)
  }
})

with_restoration_test_that(
  "translate_package works on a simple package w/ translating",
  pkg = "r_msg",
  conn = "test-translate-package-r_msg-1.input",
  {
    mockery::stub(translate_package, "get_atime", "0000-01-01 00:00:00")
    expect_normalized_snapshot(translate_package(languages="zh_CN", verbose=TRUE))

    pkg_files <- list.files(recursive = TRUE)

    expect_true("po/R-zh_CN.po" %in% pkg_files)
    expect_match(pkg_files, "inst/po/zh_CN/LC_MESSAGES/R-rMsg.mo", all = FALSE, fixed = TRUE)

    zh_translations <- readLines(file.path("po/R-zh_CN.po"), encoding='UTF-8')

    expect_match(zh_translations, "Last-Translator.*test-user.*test-user@github.com", all = FALSE)
    expect_match(zh_translations, "早上好", all = FALSE)
    # plural message
    expect_match(zh_translations, "该起床了", all = FALSE)
  }
)

with_restoration_test_that(
  "translate package works for up-to-date translations",
  pkg = "r_msg",
  code = {
    mockery::stub(translate_package, "get_atime", "0000-01-01 00:00:00")
    expect_normalized_snapshot(translate_package(languages="fa", verbose=TRUE))
  }
)

with_restoration_test_that(
  "translate_package works on package with outdated (fuzzy) translations",
  pkg = "r_fuzzy",
  conn = "test-translate-package-r_fuzzy-1.input",
  code = {
    mockery::stub(translate_package, "get_atime", "0000-01-01 00:00:00")
    expect_normalized_snapshot(translate_package(languages="zh_CN", verbose=TRUE))
  }
)

# NB: keep this test here (not in test-diagnostics) to keep coverage of the diagnostic flow in translate_package()
with_restoration_test_that(
  "translate_package identifies potential translations in cat() calls",
  pkg = "r_cat_msg",
  conn = "test-translate-package-r_cat_message-1.input",
  code = expect_normalized_snapshot(translate_package(languages = "zh_CN"))
)

# NB: this test will fail if test_that is re-run on the same R session since potools'
#   internal state is altered for the remainder of the session... not sure it's worth changing...
with_restoration_test_that(
  'Unknown language flow works correctly',
  pkg = "r_msg",
  conn = 'test-translate-package-r_msg-2.input',
  # earlier, did Arabic, but now that's an included language. switched two Welsh on the
  #   (perhaps naive) judgment that it's unlikely to enter our scope anytime soon
  #   and because there are still several (4) plural forms
  code = expect_normalized_snapshot(translate_package(languages = 'cy'))
)

# #183
with_restoration_test_that(
  "whitespace matching for plural is lenient",
  pkg = "r_msg",
  conn = "test-translate-package-r_msg-5.input",
  # Catalan -- romance language with >1 plural
  code = expect_normalized_snapshot(translate_package(languages='ca', diagnostics=NULL))
)

with_restoration_test_that(
  'Erroneous messages stop get_specials_metadata (mixed use of template redirects)',
  pkg = "r_msg",
  conn = 'test-translate-package-r_msg-3.input',
  code = expect_normalized_snapshot(translate_package(languages='zh_CN', diagnostics=NULL), error=TRUE)
)

with_restoration_test_that(
  'Erroneous messages stop get_specials_metadata (duplicate redirects with different formatters)',
  pkg = "r_msg",
  conn = 'test-translate-package-r_msg-4.input',
  code = expect_normalized_snapshot(translate_package(languages='zh_CN', diagnostics=NULL), error=TRUE)
)

with_restoration_test_that(
  "Packages with src code work correctly",
  pkg = "r_src_c",
  conn = 'test-translate-package-r_src_c-1.input',
  {
    expect_normalized_snapshot(translate_package(languages="zh_CN", diagnostics = check_untranslated_src))

    pkg_files <- list.files(recursive = TRUE)
    expect_true("po/R-zh_CN.po" %in% pkg_files)
    expect_true("po/zh_CN.po" %in% pkg_files)
    expect_true("po/rSrcMsg.pot" %in% pkg_files)
    # extended tracing here for hard-to-reproduce issue.
    # TODO: refactor this into a custom expectation helper so as not to take up so much space here.
    expect(
      any(grepl("inst/po/zh_CN/LC_MESSAGES/rSrcMsg.mo", pkg_files, fixed = TRUE)),
      sprintf(
        "Didn't find rSrcMsg.mo; found %s.\n**Sysreq paths: %s.\n**po/zh_CN contents:\n%s\n**Direct msgfmt output:\n%s**Session info:\n%s",
        toString(pkg_files), toString(Sys.which(potools:::SYSTEM_REQUIREMENTS)),
        paste(readLines(file.path('po/zh_CN.po')), collapse='\n'),
        {
          out <- tempfile()
          system2(
            "msgfmt",
            c("-o", tempfile(fileext = '.mo'), file.path("po/zh_CN.po")),
            stdout = out, stdin = out, stderr = out
          )
          paste(readLines(out), collapse='\n')
        },
        paste(capture.output(print(sessionInfo())), collapse = '\n')
      )
    )

    # TODO: is there any snapshotting equivalent for .pot/.po files? is writeLines() enough to overcome Windows issues?
    # NB: paste(readLines(), collapse="\n") instead of readChar() for platform robustness
    pot_lines <- paste(readLines(file.path('po', 'rSrcMsg.pot')), collapse = "\n")
    # (1) test N_-marked messages are included for translation
    # (2) test untemplated snprintf() calls get c-format tagged (#137)
    # (3)-(4) ngettext() arrays are extracted
    expect_all_match(
      pot_lines,
      c(
        '"Don\'t translate me now."',
        '#, c-format\nmsgid "a simple message"',
        'msgid "singular"\nmsgid_plural "plural"',
        'msgid "singular %d"\nmsgid_plural "plural %d"'
      )
    )
  }
)

with_restoration_test_that(
  "Packages with src code & fuzzy messages work",
  pkg = "r_src_fuzzy",
  conn = 'test-translate-package-r_src_fuzzy-1.input',
  code = {
    mockery::stub(translate_package, "get_atime", "0000-01-01 00:00:00")
    expect_normalized_snapshot(translate_package(languages="zh_CN", verbose=TRUE))
  }
)

# TODO: separate get_message_data() tests from write_po_files() tests here
with_restoration_test_that(
  "Various edge cases in retrieving/outputting messages in R files are handled",
  pkg = "unusual_msg",
  {
    translate_package(diagnostics = NULL)

    r_pot_file <- readLines(file.path("po", "R-rMsgUnusual.pot"))
    src_pot_file <- readLines(file.path("po", "rMsgUnusual.pot"))

    # (1)-(4) raw strings edge cases
    # (5)-(6) whitespace trimming behavior (trim for R singular, don't for R plural)
    # (7) repeated escapables (#130)
    # (8)-(9) gettextf(paste()) gets nested strings (#163)
    expect_all_match(
      r_pot_file,
      c(
        'msgid "\'abc\'"', 'msgid "\\"def\\""', 'msgid "R(\'abc\')"', 'msgid "r(\\"def\\")"',
        'msgid "ghi"', 'good %s',
        'msgid "singular "', '"I warned you!"',
        'msgid "part 1 %s"', 'msgid "part 2"'
      ),
      fixed = TRUE
    )

    # skip empty strings
    expect_all_match(paste(r_pot_file, collapse = "\n"), 'msgid ""\nmsgstr ""\n\n', fixed = TRUE, invert = TRUE)

    # don't collapse strings in similar node positions across files
    expect_all_match(r_pot_file, c('msgid "copy one"', 'msgid "copy two"'))

    # ordering within the file
    # "\\"first\\"" also tests escaping of " on msgid border, #128
    expect_true(which(r_pot_file == 'msgid "\\"first\\""') < which(r_pot_file == 'msgid "second"'))
    expect_true(which(r_pot_file == 'msgid "second"') < which(r_pot_file == 'msgid "third"'))
    expect_true(which(r_pot_file == 'msgid "third"') < which(r_pot_file == 'msgid "fourth"'))

    # escaping/unescaping
    expect_all_match(
      r_pot_file,
      c('"\\\\n vs \\n',
        'msgid "\\\\t vs \\t is OK"',
        'msgid "strings with \\"quotes\\" are OK"',
        'msgid "strings with escaped \\"quotes\\" are OK"'),
      fixed = TRUE
    )

    # (1)-(2) whitespace trimming in C
    # (3) always split at newlines
    # (4) exotic formatters like %lld
    # (5) ordering of files within the .pot (#104), and line # when call & array lines differ (#148)
    # (6) correct message after removing line continuation (#91)
    # (7) a message outside a call (e.g. in a macro) gets a source marker (#133)
    # (8) ternary operators return first array; only arrays through first interrupting macro are included (#154)
    # (9) initial macro is ignored; arrays through first interrupting macro are included; dgettext() included (#153)
    # (10) when a msgid is repeated and is_templated differs, c-format is assumed
    expect_all_match(
      paste(src_pot_file, collapse = "\n"), # NB: this is a get-out-of-\r\n-jail-free card on Windows, too
      c(
        'looks like [*]/ "', 'looks like %s "',
        '"This message[\\]n"',
        '#, c-format\nmsgid "Exotic formatters',
        '#: msg[.]c.*#: cairo/bedfellows[.]c:13',
        '"any old message"',
        '#: msg[.]c:[0-9]+\n#, c-format\nmsgid "a message in a macro %s"',
        '#: msg[.]c:[0-9]+ msg[.]c:[0-9]+\nmsgid "abc"',
        '#:( msg[.]c:[0-9]+){3}\nmsgid "abcdef"',
        '#: msg[.]c:[0-9]+ msg[.]c:[0-9]+\n#, c-format\nmsgid "This one does not[\\]n"'
      ),
    )
  }
)

with_restoration_test_that(
  "use_base_rules=FALSE produces our preferred behavior",
  pkg = "unusual_msg",
  conn = "test-translate-package-unusual_msg-1.input",
  {
    expect_normalized_snapshot(translate_package(languages="es", copyright="Mata Hari", diagnostics=NULL))

    r_pot_lines <- readLines(file.path("po", "R-rMsgUnusual.pot"))
    src_pot_lines <- readLines(file.path("po", "rMsgUnusual.pot"))

    # (1) default copyright comment in metadata
    # (2) default blank Language field in metadata
    # (3) testing plural string padding
    # (4) source tagging
    # (5) splitting at newlines
    # (6) msgid quote escaping
    # (7) copyright
    expect_all_match(
      r_pot_lines,
      c(
        "SOME DESCRIPTIVE TITLE", "Language: \\n", "nplurals=INTEGER",
        'msgid "singular "', '#: foo.R', '"\\\\n vs \\n"',
        '"strings with escaped \\"quotes\\"',
        'Copyright (C) YEAR Mata Hari'
      ),
      fixed = TRUE
    )

    # (1) lack of strwrap despite >79 width
    # (2) inclusion of file in "unrecognized" cairo folder
    expect_all_match(
      src_pot_lines,
      c(
        "#: [A-Z]{26}[.]c:[0-9] [a-z0-5]{32}[.]c:[0-9] msg[.]c:[0-9]{2} msg[.]c:[0-9]{2}",
        '^#: cairo/bedfellows\\.c:'
      )
    )
  }
)

with_restoration_test_that(
  "use_base_rules=TRUE produces base-aligned behavior",
  pkg = "unusual_msg",
  conn = "test-translate-package-unusual_msg-1.input",
  {
    expect_normalized_snapshot(translate_package(languages = "es", use_base_rules = TRUE, diagnostics = NULL))
    r_pot_lines <- readLines(file.path("po", "R-rMsgUnusual.pot"))
    src_pot_lines <- readLines(file.path("po", "rMsgUnusual.pot"))

    # (1)-(5) invert the corresponding number in the previous test_that
    expect_all_match(
      r_pot_lines,
      c("SOME DESCRIPTIVE TITLE", "Language: [\\]n", "nplurals=INTEGER", '#: ', '"\\\\n vs \\n is OK"'),
      fixed = TRUE, invert = TRUE
    )
    expect_all_match(r_pot_lines, 'msgid        "small fail "', fixed = TRUE)

    # (1) MSG.c comes before msg.c (sort/collate order)
    # (2) c-format tags are produced
    # (3) msgid with many duplicates wraps the source markers _exactly_ at width=79
    # (4)-(11) template-adjacent wrapping/non-wrapping (#90, #150)
    expect_all_match(
      paste(src_pot_lines, collapse='\n'),
      c(
        'MSGs\\.c.*msg\\.c', '#, c-format',
        '#: [A-Z]{26}[.]c:[0-9] [a-z0-5]{32}[.]c:[0-9] msg[.]c:[0-9]{2}\n#: msg[.]c',
        ' [.]"\n"%s[.]"', ' [?]"\n"%s[?]"', ' ;"\n"%s;"', ' /"\n"%s/"',
        '"\'%s\'"', '"[[]%s[]]"', '"[|]%s[|]"', '"-%s-"'
      )
    )

    # (1) only src/*.c and src/windows/*.c are included (no other subdirectories), #114
    # (2)-(8) wrapping surrounding \" matches xgettex, #91
    expect_all_match(
      src_pot_lines,
      c(
        '#: bedfellows.c:',
        '56\\"890"', '5(\\"890"', '5\'\\"890"',
        '345a"', '345A"', '345#"', '345@"'
      ),
      fixed = TRUE, invert = TRUE
    )
  }
)

# TODO: extend with_restoration_test_that to handle
#   copying a top directory (here, r-devel) but setting to a sub-directory (here, r-devel/src/library/grDevices)
with_restoration_test_that(
  "use_base_rules is auto-detected",
  pkg = "r-devel",
  {
    withr::local_dir("src/library/grDevices")
    translate_package(diagnostics = NULL)

    r_pot_lines <- readLines(file.path('po', 'R-grDevices.pot'))
    src_pot_lines <- readLines(file.path('po', 'grDevices.pot'))

    expect_all_match(
      r_pot_lines,
      c('"Project-Id-Version: R', '"Report-Msgid-Bugs-To: bugs.r-project.org\\n"'),
      fixed = TRUE
    )

    # copyright isn't written in the R file, but is in src? A bit strange but done for consistency w base
    expect_all_match(
      r_pot_lines,
      '# Copyright (C) YEAR The R Core Team',
      fixed = TRUE, invert = TRUE
    )

    expect_all_match(
      src_pot_lines,
      '# Copyright (C) YEAR The R Core Team',
      fixed = TRUE
    )

    # first argument to dgettext() should be ignored, #184
    expect_all_match(
      src_pot_lines,
      'msgid "grDevices"',
      fixed = TRUE, invert = TRUE
    )
  }
)

# NB: this is _mostly_ about get_message_data(), but we also test the correct R.pot file is created
with_restoration_test_that(
  "translation of 'base' works correctly",
  pkg = "r-devel",
  {
    withr::local_dir("src/library/base")

    # NB: it seems file.rename doesn't work for directories on Windows, so we have the
    #   more cumbersome file.copy() approach here
    correct_share <- file.path('../../../share')
    tmp_share <- file.path(tempdir(), 'share')
    dir.create(tmp_share)
    on.exit(unlink(tmp_share, recursive=TRUE))
    file.copy(dirname(correct_share), tmp_share, recursive = TRUE)
    unlink(correct_share, recursive = TRUE)
    expect_error(translate_package(diagnostics = NULL), "Translation of the 'base' package", fixed = TRUE)
    dir.create(correct_share)
    file.copy(tmp_share, dirname(correct_share), recursive = TRUE)

    correct_potfiles <- normalizePath(file.path('../../../po/POTFILES'))
    # tried file.rename, but it fails on some systems (e.g. Debian) as an "Invalid cross-device link"
    expect_true(file.copy(correct_potfiles, tmp_potfiles <- tempfile()))
    unlink(correct_potfiles)
    expect_error(translate_package(diagnostics = NULL), "Translation of the 'base' package", fixed = TRUE)
    file.copy(tmp_potfiles, correct_potfiles)
    on.exit(unlink(tmp_potfiles), add = TRUE)

    translate_package(diagnostics = NULL)

    expect_true(file.exists(file.path('po', 'R-base.pot')))
    expect_true(file.exists(file.path('po', 'R.pot')))

    r_pot_lines <- readLines(file.path('po', 'R-base.pot'))
    src_pot_lines <- readLines(file.path('po', 'R.pot'))

    # confirm share/R messages are included
    expect_all_match(
      r_pot_lines,
      'msgid "Go clean your room!"',
      fixed = TRUE
    )

    # check relative path is recorded correctly
    expect_all_match(
      src_pot_lines,
      "#: src/main/msg.c:",
      fixed = TRUE
    )
  }
)

with_restoration_test_that(
  "max_translations works as expected",
  pkg = "r_msg",
  conn = 'test-translate-package-r_msg-1.input',
  code = expect_normalized_snapshot(translate_package(languages='es', max_translations = 1L, diagnostics = NULL))
)
