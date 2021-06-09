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
        translate_package(pkg, "zh_CN"),
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
  # expect_match(prompts, "a similar message was previously translated as", all=FALSE)
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
      expect_error(translate_package(pkg, "zh_CN"), "File terminated before char array completed", fixed = TRUE)
    }
  )

  restore_package(
    pkg <- test_package("r_src_err_2"),
    {
      expect_error(translate_package(pkg, "zh_CN"), "File terminated before translation array completed", fixed = TRUE)
    }
  )

  restore_package(
    pkg <- test_package("r_src_err_3"),
    {
      expect_error(translate_package(pkg, "zh_CN"), "File terminated before message call completed", fixed = TRUE)
    }
  )

  restore_package(
    pkg <- test_package("r_src_err_4"),
    {
      expect_error(translate_package(pkg, "zh_CN"), "Unexpected sequence", fixed = TRUE)
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
        translate_package(pkg, "zh_CN"),
        "Found 3 src messaging calls that were not properly marked for translation",
        fixed = TRUE
      )
    }
  )
  expect_outputs(
    prompts,
    c(
      'an untranslated string',
      'an untranslated error',
      "msg"
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
    tmp_conn = mock_translation("test-translate-package-unusual_msg-1.input"),
    {
      translate_package(pkg)

      pot_lines <- readLines(file.path(pkg, "po", "R-rMsgUnusual.pot"))

      # raw strings edge cases
      expect_all_match(
        pot_lines,
        c('msgid "\'abc\'"', 'msgid "\\"def\\""', 'msgid "R(\'abc\')"', 'msgid "r(\\"def\\")"', 'msgid "ghi"'),
        fixed = TRUE
      )

      # skip empty strings
      expect_all_match(paste(pot_lines, collapse = "\n"), 'msgid ""\nmsgstr ""\n\n', fixed = TRUE, invert = TRUE)

      # don't collapse strings in similar node positions across files
      expect_all_match(pot_lines, c('msgid "copy one"', 'msgid "copy two"'))

      # ordering within the file
      expect_true(which(pot_lines == 'msgid "first"') < which(pot_lines == 'msgid "second"'))
      expect_true(which(pot_lines == 'msgid "second"') < which(pot_lines == 'msgid "third"'))
      expect_true(which(pot_lines == 'msgid "third"') < which(pot_lines == 'msgid "fourth"'))

      # escaping/unescaping
      expect_all_match(
        pot_lines,
        c('msgid "\\\\n vs \\n is OK"', 'msgid "\\\\t vs \\t is OK"',
          'msgid "strings with \\"quotes\\" are OK"', 'msgid "strings with escaped \\"quotes\\" are OK"'),
        fixed = TRUE
      )
    }
  )
})

test_that("use_base_rules produces the correct differences", {
  restore_package(
    pkg <- test_package("unusual_msg"),
    {
      translate_package(pkg, diagnostics = NULL)
      r_pot_lines <- readLines(file.path(pkg, "po", "R-rMsgUnusual.pot"))
      src_pot_lines <- readLines(file.path(pkg, "po", "rMsgUnusual.pot"))

      expect_all_match(
        r_pot_lines,
        # third is testing plural string padding
        c("SOME DESCRIPTIVE TITLE", "Language: \\n", "nplurals=INTEGER", 'msgid "singular"'),
        fixed = TRUE
      )
      expect_all_match(
        src_pot_lines,
        # testing no strwrap for many duplicate locations
        "(msg\\.c:[0-9]+ ){10}"
      )

      translate_package(pkg, use_base_rules = TRUE, diagnostics = NULL)
      r_pot_lines <- readLines(file.path(pkg, "po", "R-rMsgUnusual.pot"))
      src_pot_lines <- readLines(file.path(pkg, "po", "rMsgUnusual.pot"))

      expect_all_match(
        r_pot_lines,
        # third is testing plural string padding
        c("SOME DESCRIPTIVE TITLE", "Language: [\\]n", "nplurals=INTEGER"),
        fixed = TRUE, invert = TRUE
      )
      expect_all_match(r_pot_lines, 'msgid        "small fail"', fixed = TRUE)
      # TODO(#89): activate this test
      # expect_all_match(
      #   src_pot_lines,
      #   # testing no strwrap for many duplicate locations
      #   "(bar\\.c:[0-9]+ ){10}"
      # )

      # MSG.c comes before msg.c (sort/collate order)
      expect_match(paste(src_pot_lines, collapse='\n'), 'MSG\\.c.*msg\\.c')
    }
  )
})
