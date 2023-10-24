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
        c("No languages provided"),
        fixed = TRUE
      )

      pkg_files <- list.files(pkg, recursive=TRUE)

      pot_file <- "po/R-rMsg.pot"
      expect_true(pot_file %in% pkg_files)
      # testing gettextf's ... arguments are skipped
      expect_all_match(readLines(file.path(pkg, pot_file)), "don't translate me", invert=TRUE, fixed=TRUE)

      # Non-UTF-8 machines don't run en@quot translations by default.
      #   Mostly applies to Windows, but can also apply to Unix
      #   (e.g. r-devel-linux-x86_64-debian-clang on CRAN), #191
      if (l10n_info()[["UTF-8"]]) {
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
        c("Beginning new translations", "BEGINNING TRANSLATION", "Recompiling 'zh_CN' R translation"),
        fixed = TRUE
      )

      pkg_files <- list.files(pkg, recursive = TRUE)

      expect_true("po/R-zh_CN.po" %in% pkg_files)
      expect_match(pkg_files, "inst/po/zh_CN/LC_MESSAGES/R-rMsg.mo", all = FALSE, fixed = TRUE)

      zh_translations <- readLines(file.path(pkg, "po/R-zh_CN.po"), encoding='UTF-8')

      expect_match(zh_translations, "Last-Translator.*test-user.*test-user@github.com", all = FALSE)
      expect_match(zh_translations, "早上好", all = FALSE)
      # plural message
      expect_match(zh_translations, "该起床了", all = FALSE)
    }
  )
  expect_all_match(prompts, c("^---^", "^^"), fixed=TRUE)

  # all translations already done
  restore_package(
    pkg,
    {
      expect_messages(
        translate_package(pkg, "fa", verbose=TRUE),
        "Translations for fa are up to date! Skipping",
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

# NB: keep this test here (not in test-diagnostics) to keep coverage of the diagnostic flow in translate_package()
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
  expect_all_match(
    prompts,
    c(
      'cat(gettext("I warned you!"), fill=TRUE)',
      'cat(gettext("Oh no you\\ndon\'t!"))',
      "Hixxboss"
    ),
    fixed=TRUE
  )
  expect_all_match(
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
        # earlier, did Arabic, but now that's a chosen language. switched two Welsh on the
        #   (perhaps naive) judgment that it's unlikely to enter our scope anytime soon
        #   and because there are still several (4) plural forms
        translate_package(pkg, 'cy'),
        c(
          'not a known language', 'Please file an issue',
          # NB: this test will fail if test_that is re-run on the same R session since potools'
          #   internal state is altered for the remainder of the session... not sure it's worth changing...
          "Did not match any known 'plural's"
        ),
        fixed=TRUE
      )
    }
  )
  # also include coverage tests of incorrect templating in supplied translations
  expect_all_match(
    prompts,
    c(
      'How would you refer to this language in English?',
      'received the same set of templates',
      'received 2 unique templated arguments',
      'received 4 unique templated arguments',
      'received 5 unique templated arguments'
    ),
    fixed=TRUE
  )

  # whitespace matching for plural is lenient, #183
  prompts = restore_package(
    pkg <- test_package('r_msg'),
    tmp_conn = mock_translation('test-translate-package-r_msg-5.input'),
    {
      expect_messages(
        # Catalan -- romance language with >1 plural
        translate_package(pkg, 'ca', diagnostics=NULL),
        c("Did not match any known 'plural's"),
        fixed=TRUE, invert=TRUE
      )
    }
  )
  expect_all_match(
    prompts,
    c("when n = 1", "when n is not 1"),
    fixed = TRUE
  )
})

test_that('Erroneous messages stop get_specials_metadata', {
  restore_package(
    pkg <- test_package('r_msg'),
    tmp_conn = mock_translation('test-translate-package-r_msg-3.input'),
    {
      expect_error(
        translate_package(pkg, 'zh_CN', diagnostics = NULL),
        'Invalid templated message. If any %N$', fixed = TRUE
      )
    }
  )

  restore_package(
    pkg <- test_package('r_msg'),
    tmp_conn = mock_translation('test-translate-package-r_msg-4.input'),
    {
      expect_error(
        translate_package(pkg, 'zh_CN', diagnostics = NULL),
        'all messages pointing to the same input', fixed = TRUE
      )
    }
  )
})

test_that("Packages with src code work correctly", {
  prompts = restore_package(
    pkg <- test_package('r_src_c'),
    tmp_conn = mock_translation('test-translate-package-r_src_c-1.input'),
    {
      translate_package(pkg, "zh_CN", diagnostics = check_untranslated_src)

      pkg_files <- list.files(pkg, recursive = TRUE)
      expect_true("po/R-zh_CN.po" %in% pkg_files)
      expect_true("po/zh_CN.po" %in% pkg_files)
      expect_true("po/rSrcMsg.pot" %in% pkg_files)
      expect(
        any(grepl("inst/po/zh_CN/LC_MESSAGES/rSrcMsg.mo", pkg_files, fixed = TRUE)),
        sprintf(
          paste(
            "Didn't find rSrcMsg.mo; found %s.",
            "**Sysreq paths: %s.",
            "**po/zh_CN contents:",
            "%s",
            "**Direct msgfmt output:",
            "%s**Session info:",
            "%s",
            sep = "\n"
          ),
          toString(pkg_files), toString(Sys.which(potools:::SYSTEM_REQUIREMENTS)),
          paste(readLines(file.path(pkg, 'po/zh_CN.po')), collapse='\n'),
          {
            out <- tempfile()
            system2(
              "msgfmt",
              c("-o", tempfile(fileext = '.mo'), file.path(pkg, "po/zh_CN.po")),
              stdout = out, stdin = out, stderr = out
            )
            paste(readLines(out), collapse='\n')
          },
          paste(capture.output(print(sessionInfo())), collapse = '\n')
        )
      )

      # NB: paste(readLines(), collapse="\n") instead of readChar() for platform robustness
      pot_lines <- paste(readLines(file.path(pkg, 'po', 'rSrcMsg.pot')), collapse = "\n")
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

  expect_all_match(
    prompts,
    c("Rprintf(_(", "warning(_("),
    fixed = TRUE
  )

  # error(ngettext(...)) doesn't show error() in check_untranslated_src
  expect_all_match(
    prompts,
    "Problematic call",
    invert = TRUE, fixed = TRUE
  )
})

test_that("Packages with src code & fuzzy messages work", {
  prompts = restore_package(
    pkg <- test_package("r_src_fuzzy"),
    tmp_conn = mock_translation('test-translate-package-r_src_fuzzy-1.input'),
    {
      expect_messages(
        translate_package(pkg, "zh_CN", verbose = TRUE),
        "Found existing src translations",
        fixed = TRUE
      )
    }
  )
  expect_all_match(
    prompts,
    "Note: a similar message was previously translated as",
    fixed = TRUE
  )
})

# TODO: separate get_message_data() tests from write_po_files() tests here
test_that("Various edge cases in retrieving/outputting messages in R files are handled", {
  restore_package(
    pkg <- test_package("unusual_msg"),
    {
      translate_package(pkg, diagnostics = NULL)

      r_pot_file <- readLines(file.path(pkg, "po", "R-rMsgUnusual.pot"))
      src_pot_file <- readLines(file.path(pkg, "po", "rMsgUnusual.pot"))

      # (1)-(4) raw strings edge cases
      # (5)-(6) whitespace trimming behavior (trim for R singular, don't for R plural)
      # (7) repeated escapables (#130)
      # (8)-(9) gettextf(paste()) gets nested strings (#163)
      expect_all_match(
        r_pot_file,
        c(
          'msgid "\'abc\'"', 'msgid "\\"def\\""', 'msgid "R(\'abc\')"', 'msgid "r(\\"def\\")"',
          'msgid "ghi"', 'good %s', 'msgid "singular "', '"I warned you!"',
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
        c('"\\\\n vs \\n', 'msgid "\\\\t vs \\t is OK"',
          'msgid "strings with \\"quotes\\" are OK"', 'msgid "strings with escaped \\"quotes\\" are OK"'),
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
          'looks like [*]/ "', 'looks like %s "', '"This message[\\]n"',
          '#, c-format\nmsgid "Exotic formatters', '#: msg[.]c.*#: cairo/bedfellows[.]c:13',
          '"any old message"', '#: msg[.]c:[0-9]+\n#, c-format\nmsgid "a message in a macro %s"',
          '#: msg[.]c:[0-9]+ msg[.]c:[0-9]+\nmsgid "abc"',
          '#:( msg[.]c:[0-9]+){3}\nmsgid "abcdef"',
          '#: msg[.]c:[0-9]+ msg[.]c:[0-9]+\n#, c-format\nmsgid "This one does not[\\]n"'
        ),
      )
    }
  )
})

test_that("use_base_rules=FALSE produces our preferred behavior", {
  restore_package(
    pkg <- test_package("unusual_msg"),
    tmp_conn = mock_translation("test-translate-package-unusual_msg-1.input"),
    {
      translate_package(pkg, "es", copyright = "Mata Hari", diagnostics = NULL)
      r_pot_lines <- readLines(file.path(pkg, "po", "R-rMsgUnusual.pot"))
      src_pot_lines <- readLines(file.path(pkg, "po", "rMsgUnusual.pot"))

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
})

test_that("use_base_rules=TRUE produces base-aligned behavior", {
  restore_package(
    pkg <- test_package("unusual_msg"),
    tmp_conn = mock_translation("test-translate-package-unusual_msg-1.input"),
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
})

test_that("use_base_rules is auto-detected", {
  restore_package(
    pkg <- test_package("r-devel/src/library/grDevices"),
    {
      translate_package(pkg, diagnostics = NULL)

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

      # first argument to dgettext() should be ignored, #184
      expect_all_match(
        src_pot_lines,
        'msgid "grDevices"',
        fixed = TRUE, invert = TRUE
      )
    }
  )
})

# NB: this is _mostly_ about get_message_data(), but we also test the correct R.pot file is created
test_that("translation of 'base' works correctly", {
  restore_package(
    pkg <- test_package("r-devel/src/library/base"),
    {
      # NB: it seems file.rename doesn't work for directories on Windows, so we have the
      #   more cumbersome file.copy() approach here
      correct_share <- file.path(pkg, '../../../share')
      tmp_share <- file.path(tempdir(), 'share')
      dir.create(tmp_share)
      on.exit(unlink(tmp_share, recursive=TRUE))
      file.copy(dirname(correct_share), tmp_share, recursive = TRUE)
      unlink(correct_share, recursive = TRUE)
      expect_error(translate_package(pkg, diagnostics = NULL), "Translation of the 'base' package", fixed = TRUE)
      dir.create(correct_share)
      file.copy(tmp_share, dirname(correct_share), recursive = TRUE)

      correct_potfiles <- normalizePath(file.path(pkg, '../../../po/POTFILES'))
      # tried file.rename, but it fails on some systems (e.g. Debian) as an "Invalid cross-device link"
      expect_true(file.copy(correct_potfiles, tmp_potfiles <- tempfile()))
      unlink(correct_potfiles)
      expect_error(translate_package(pkg, diagnostics = NULL), "Translation of the 'base' package", fixed = TRUE)
      file.copy(tmp_potfiles, correct_potfiles)
      on.exit(unlink(tmp_potfiles), add = TRUE)

      translate_package(pkg, diagnostics = NULL)

      expect_true(file.exists(file.path(pkg, 'po', 'R-base.pot')))
      expect_true(file.exists(file.path(pkg, 'po', 'R.pot')))

      r_pot_lines <- readLines(file.path(pkg, 'po', 'R-base.pot'))
      src_pot_lines <- readLines(file.path(pkg, 'po', 'R.pot'))

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
})

test_that("max_translations works as expected", {
  prompts <- restore_package(
    pkg <- test_package("r_msg"),
    tmp_conn = mock_translation('test-translate-package-r_msg-1.input'),
    {
      translate_package(pkg, 'es', max_translations = 1L, diagnostics = NULL)
    }
  )
  expect_all_match(
    prompts,
    "Oh no you don't!",
    fixed = TRUE, invert = TRUE
  )
})
