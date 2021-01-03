test_that("translate_package arg checking errors work", {
  expect_error(translate_package(c("dplyr", "data.table")), "Only one package at a time", fixed=TRUE)
  expect_error(translate_package(1), "'dir' must be a character", fixed=TRUE)
  expect_error(translate_package(languages = 1L), "'languages' must be a character vector", fixed=TRUE)
})

test_that("translate_package handles empty packages", {
  pkg <- test_path("test_packages/no_msg")
  restore_package(pkg, {
    expect_invisible(translate_package(pkg))

    expect_message(translate_package(pkg, verbose=TRUE), "No messages to translate", fixed=TRUE)
  })
})

test_that("translate_package works on a simple package", {
  pkg <- test_path("test_packages/r_msg")
  # simple run-through without doing translations
  restore_package(
    pkg,
    {
      msg <- capture_messages(translate_package(pkg, verbose=TRUE))
      expect_true(
        Reduce(`&&`, lapply(
          c("Running tools::update_pkg_po", "No languages provided"),
          function(ptn) any(grepl(ptn, msg, fixed=TRUE))
        ))
      )

      pkg_files <- list.files(pkg, recursive=TRUE)

      expect_true("po/R-rMsg.pot" %in% pkg_files)
      # . here is en@quot/LC_MESSAGES; not sure how platform-robust that is
      expect_true(any(grepl("inst/po/.*/R-rMsg.mo", pkg_files)))
    }
  )
  # do translations with mocked input
  restore_package(
    pkg,
    tmp_conn = test_path("mock_translations/test-translate-package-r_msg-1.input"),
    {
      translate_package(pkg, "zh_CN")

      pkg_files <- list.files(pkg, recursive = TRUE)
      writeLines(pkg_files)

      expect_true("po/R-zh_CN.po" %in% pkg_files)
      # . here is LC_MESSAGES; not sure how platform-robust that is
      expect_true(any(grepl("inst/po/zh_CN/.*/R-rMsg.mo", pkg_files)))

      zh_translations <- readLines(file.path(pkg, "po/R-zh_CN.po"))

      expect_true(any(grepl("Last-Translator.*test-user.*test-user@github.com", zh_translations)))
      expect_true(any(grepl("早上好", zh_translations)))
    }
  )
})
