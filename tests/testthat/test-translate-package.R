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

test_that("translate_package handles empty packages", {
  restore_package(
    pkg <- test_package("no_msg"),
    {
      expect_invisible(translate_package(pkg))

      expect_message(translate_package(pkg, verbose=TRUE), "No messages to translate", fixed=TRUE)
    }
  )

  # a package with no R directory (e.g. a data package)
  restore_package(
    pkg <- test_package("r_data_pkg"),
    expect_message(translate_package(pkg, verbose=TRUE), "No messages to translate", fixed=TRUE)
  )
})

test_that("translate_package works on a simple package", {
  # simple run-through without doing translations
  restore_package(
    pkg <- test_package("r_msg"),
    {
      expect_messages(
        translate_package(pkg, verbose=TRUE),
        c("Generating .pot files", "No languages provided"),
        fixed = TRUE
      )

      pkg_files <- list.files(pkg, recursive=TRUE)

      pot_file <- "po/R-rMsg.pot"
      expect_true(pot_file %in% pkg_files)
      # testing gettextf's ... arguments are skipped
      expect_all_match(readLines(file.path(pkg, pot_file)), "don't translate me", invert=TRUE, fixed=TRUE)

      # Windows doesn't produce the en@quot translations at all
      if (.Platform$OS.type != "windows") {
        expect_match(pkg_files, "inst/po/en@quot/LC_MESSAGES/R-rMsg.mo", all = FALSE)
      }
    }
  )
  # do translations with mocked input
  prompts <- restore_package(
    pkg,
    tmp_conn = mock_translation("test-translate-package-r_msg-1.input"),
    {
      expect_messages(
        translate_package(pkg, "zh_CN", verbose=TRUE),
        c("Beginning new translations", "BEGINNING TRANSLATION", '"Installing" translations with msgfmt'),
        fixed = TRUE
      )

      pkg_files <- list.files(pkg, recursive = TRUE)

      expect_true("po/R-zh_CN.po" %in% pkg_files)
      # . here is LC_MESSAGES; not sure how platform-robust that is
      expect_match(pkg_files, "inst/po/zh_CN/.*/R-rMsg.mo", all = FALSE)

      zh_translations <- readLines(file.path(pkg, "po/R-zh_CN.po"), encoding='UTF-8')

      expect_match(zh_translations, "Last-Translator.*test-user.*test-user@github.com", all = FALSE)
      expect_match(zh_translations, "早上好", all = FALSE)
      # plural message
      expect_match(zh_translations, "该起床了", all = FALSE)
    }
  )
  expect_outputs(prompts, c("^---^", "^^"), fixed=TRUE)
})

test_that("translate_package works on package with 'cracked' messages needing templates", {
  # simple run-through without doing translations
  restore_package(
    pkg <- test_package("r_non_template"),
    tmp_conn = mock_translation("test-translate-package-r_non_template-1.input"),
    {
      expect_messages(
        translate_package(pkg, "zh_CN", diagnostics = list(check_cracked_messages)),
        "Found 2 R messaging calls that might be better suited for gettextf",
        fixed = TRUE
      )
    }
  )
})

test_that("translate_package works on package with outdated (fuzzy) translations", {
  # simple run-through without doing translations
  prompts = restore_package(
    pkg <- test_package("r_fuzzy"),
    tmp_conn = mock_translation("test-translate-package-r_fuzzy-1.input"),
    {
      expect_messages(
        translate_package(pkg, "zh_CN", verbose=TRUE),
        c("translations marked as deprecated", "SINGULAR MESSAGES", "PLURAL MESSAGES"),
        fixed = TRUE
      )
    }
  )
  expect_match(prompts, "a similar message was previously translated as", all=FALSE)
})

test_that("translate_package identifies potential translations in cat() calls", {
  prompts = restore_package(
    pkg <- test_package("r_cat_msg"),
    tmp_conn = mock_translation("test-translate-package-r_cat_message-1.input"),
    {
      expect_messages(
        translate_package(pkg, "zh_CN"),
        "Found 4 untranslated messaging calls passed through cat()",
        fixed = TRUE
      )
    }
  )
  expect_outputs(
    prompts,
    c(
      'cat(gettext("I warned you!"), fill=TRUE)',
      'cat(gettext("Oh no you\\ndon\'t!"))',
      "Hixxboss"
    ),
    fixed=TRUE
  )
  expect_outputs(
    prompts,
    c("shouldn't be translated", "Miss me"),
    fixed=TRUE, invert=TRUE
  )
})

test_that('Unknown language flow works correctly', {
  prompts = restore_package(
    pkg <- test_package('r_msg'),
    tmp_conn = mock_translation('test-translate-package-r_msg-2.input'),
    {
      expect_messages(
        translate_package(pkg, 'ar_SY'),
        # TODO: why isn't "Did not match any known 'plural's" matching?
        c('not a known language', 'Please file an issue'),
        fixed=TRUE
      )
    }
  )
  expect_outputs(
    prompts,
    c('How would you refer to this language in English?'),
    fixed=TRUE
  )
})

test_that("Packages with src code work correctly", {
  prompts = restore_package(
    pkg <- test_package('r_src_c'),
    tmp_conn = mock_translation('test-translate-package-r_src_c-1.input'),
    {
      translate_package(pkg, "zh_CN")

      pkg_files <- list.files(pkg, recursive = TRUE)
      expect_true("po/R-zh_CN.po" %in% pkg_files)
      expect_true("po/zh_CN.po" %in% pkg_files)
      expect_true("po/rSrcMsg.pot" %in% pkg_files)
      expect(
        any(grepl("inst/po/.*/rSrcMsg.mo", pkg_files)),
        "Didn't find rSrcMsg.mo; found %s", toString(pkg_files)
      )
    }
  )

  expect_outputs(
    prompts,
    c("Rprintf(_(", "warning(_("),
    fixed = TRUE
  )
})

test_that("Packages with src code & C syntax errors fail gracefully", {
  restore_package(
    pkg <- test_package("r_src_err_1"),
    {
      expect_error(translate_package(pkg, "zh_CN"), "Parsing error: found an odd number (3)", fixed = TRUE)
    }
  )

  restore_package(
    pkg <- test_package("r_src_err_2"),
    {
      expect_error(translate_package(pkg, "zh_CN"), "Parsing error: unmatched parentheses", fixed = TRUE)
    }
  )

  restore_package(
    pkg <- test_package("r_src_err_3"),
    {
      expect_error(translate_package(pkg, "zh_CN"), "Parsing error: unmatched parentheses", fixed = TRUE)
    }
  )
})

test_that("Packages with src code & fuzzy messages work", {
  prompts = restore_package(
    pkg <- test_package("r_src_fuzzy"),
    tmp_conn = mock_translation('test-translate-package-r_src_fuzzy-1.input'),
    {
      translate_package(pkg, "zh_CN")
    }
  )
  expect_outputs(
    prompts,
    "Note: a similar message was previously translated as",
    fixed = TRUE
  )
})

test_that("Diagnostic for unmarked src translations works", {
  prompts = restore_package(
    pkg <- test_package("r_src_untranslated"),
    tmp_conn = mock_translation("test-translate-package-r_src_untranslated-1.input"),
    {
      expect_messages(
        translate_package(pkg, "zh_CN", diagnostics = check_untranslated_src),
        "Found 2 src messaging calls that were not properly marked for translation",
        fixed = TRUE
      )
    }
  )
  expect_outputs(
    prompts,
    c(
      'an untranslated string',
      'an untranslated error'
    ),
    fixed=TRUE
  )
})

test_that("Partially named messaging arguments are an error", {
  restore_package(
    pkg <- test_package("plural_semi_named"),
    expect_error(
      translate_package(pkg),
      "found a call to ngettext that names only some of its messaging arguments",
      fixed = TRUE
    )
  )
})

test_that("Various edge cases in retrieving/outputting messages in R files are handled", {
  restore_package(
    pkg <- test_package("unusual_msg"),
    {
      translate_package(pkg, diagnostics = NULL)

      r_pot_file <- readLines(file.path(pkg, "po", "R-rMsgUnusual.pot"))
      src_pot_file <- readLines(file.path(pkg, "po", "rMsgUnusual.pot"))

      # (1) raw strings edge cases
      # (2) whitespace trimming behavior (trim for R singular, don't for R plural)
      # (3) repeated escapables (#130)
      expect_all_match(
        r_pot_file,
        c('msgid "\'abc\'"', 'msgid "\\"def\\""', 'msgid "R(\'abc\')"', 'msgid "r(\\"def\\")"', 'msgid "ghi"',
          'good %s', 'msgid "singular "', '"I warned you!"'),
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
        c('"\\\\n vs \\n', 'msgid "\\\\t vs \\t is OK"',
          'msgid "strings with \\"quotes\\" are OK"', 'msgid "strings with escaped \\"quotes\\" are OK"'),
        fixed = TRUE
      )

      # (1) whitespace trimming in C
      # (2) always split at newlines
      # (3) exotic formatters like %lld
      # (4) ordering of files within the .pot (#104)
      # (5) correct message after removing line continuation (#91)
      # (6) a message outside a call (e.g. in a macro) gets a source marker (#133)
      expect_all_match(
        paste(src_pot_file, collapse = "\n"), # NB: this is a get-out-of-\r\n-jail-free card on Windows, too
        c('looks like [*]/ "', 'looks like %s "', '"This message[\\]n"',
          '#, c-format\nmsgid "Exotic formatters', '#: msg[.]c.*#: cairo/bedfellows[.]c',
          '"any old message"', '#: msg[.]c:[0-9]+\nmsgid "a message in a macro"'),
      )
    }
  )
})

test_that("use_base_rules=FALSE produces our preferred behavior", {
  restore_package(
    pkg <- test_package("unusual_msg"),
    tmp_conn = mock_translation("test-translate-package-unusual_msg-1.input"),
    {
      translate_package(pkg, "es", diagnostics = NULL)
      r_pot_lines <- readLines(file.path(pkg, "po", "R-rMsgUnusual.pot"))
      src_pot_lines <- readLines(file.path(pkg, "po", "rMsgUnusual.pot"))

      # (1) default copyright comment in metadata
      # (2) default blank Language field in metadata
      # (3) testing plural string padding
      # (4) source tagging
      # (5) splitting at newlines
      # (6) msgid quote escaping
      expect_all_match(
        r_pot_lines,
        c("SOME DESCRIPTIVE TITLE", "Language: \\n", "nplurals=INTEGER",
          'msgid "singular "', '#: foo.R', '"\\\\n vs \\n"',
          '"strings with escaped \\"quotes\\"'),
        fixed = TRUE
      )
      expect_all_match(
        src_pot_lines,
        # testing no strwrap for many duplicate locations
        c("(msg\\.c:[0-9]+ ){10}", '^#: cairo/bedfellows\\.c:')
      )
    }
  )
})

test_that("use_base_rules=TRUE produces base-aligned behavior", {
  restore_package(
    pkg <- test_package("unusual_msg"),
    tmp_conn = mock_translation("test-translate-package-unusual_msg-2.input"),
    {
      translate_package(pkg, "es", use_base_rules = TRUE, diagnostics = NULL)
      r_pot_lines <- readLines(file.path(pkg, "po", "R-rMsgUnusual.pot"))
      src_pot_lines <- readLines(file.path(pkg, "po", "rMsgUnusual.pot"))

      # (1)-(5) invert the corresponding number in the previous test_that
      expect_all_match(
        r_pot_lines,
        c("SOME DESCRIPTIVE TITLE", "Language: [\\]n", "nplurals=INTEGER", '#: ', '"\\\\n vs \\n is OK"'),
        fixed = TRUE, invert = TRUE
      )
      expect_all_match(r_pot_lines, 'msgid        "small fail "', fixed = TRUE)

      # (1) MSG.c comes before msg.c (sort/collate order)
      # (2) c-format tags are produced
      # (3) msgid with many duplicates wraps the source markers at width=79
      # (4) when a template is bumped & '-wrapped, the ' is bumped to the next line as well (#90)
      expect_all_match(
        paste(src_pot_lines, collapse='\n'),
        c('MSGs\\.c.*msg\\.c', '#, c-format', '#: msg\\.c:.*#: msg\\.c', '"\'%s\': %s"')
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
})

test_that("use_base_rules is auto-detected", {
  restore_package(
    pkg <- test_package("grDevices"),
    {
      translate_package(pkg)

      r_pot_lines <- readLines(file.path(pkg, 'po', 'R-grDevices.pot'))
      src_pot_lines <- readLines(file.path(pkg, 'po', 'grDevices.pot'))

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

      # message is in src/cairo subdirectory which is excluded because use_base_rules is detected as TRUE
      expect_all_match(
        src_pot_lines,
        'unimplemented cairo-based device',
        fixed = TRUE, invert = TRUE
      )
    }
  )
})

test_that("translation of 'base' works correctly", {
  restore_package(
    pkg <- test_package("r-devel/src/library/base"),
    {
      translate_package(pkg, diagnostics = NULL)
      # TODO: why not R.pot?
      # TODO: add some R messages to the mock base
      browser()
    }
  )
})
