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
})

test_that("translate_package works on a simple package", {
  # simple run-through without doing translations
  restore_package(
    pkg <- test_package("r_msg"),
    {
      expect_messages(
        translate_package(pkg, verbose=TRUE),
        c("Running tools::update_pkg_po", "No languages provided"),
        fixed = TRUE
      )

      pkg_files <- list.files(pkg, recursive=TRUE)

      expect_true("po/R-rMsg.pot" %in% pkg_files)
      # Windows doesn't produce the en@quot translations at all
      if (.Platform$OS.type != "windows") {
        expect_true(any(grepl("inst/po/en@quot/LC_MESSAGES/R-rMsg.mo", pkg_files)))
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
        c("Beginning new translations", "BEGINNING TRANSLATION", "Re-running tools::update_pkg_po()"),
        fixed = TRUE
      )

      pkg_files <- list.files(pkg, recursive = TRUE)

      expect_true("po/R-zh_CN.po" %in% pkg_files)
      # . here is LC_MESSAGES; not sure how platform-robust that is
      expect_true(any(grepl("inst/po/zh_CN/.*/R-rMsg.mo", pkg_files)))

      zh_translations <- readLines(file.path(pkg, "po/R-zh_CN.po"), encoding='UTF-8')

      expect_true(any(grepl("Last-Translator.*test-user.*test-user@github.com", zh_translations)))
      expect_true(any(grepl("早上好", zh_translations)))
      # plural message
      expect_true(any(grepl("该起床了", zh_translations)))
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
        "Found 1 R messaging calls that might be better suited for gettextf",
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
      'cat(gettext("Oh no you don\'t!"))',
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
