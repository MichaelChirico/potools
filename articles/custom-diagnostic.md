# Writing a custom diagnostic

## Motivation

`potools` provides several “diagnostic” functions used to check the
“health” of the messaging corpus available in a given package. These are
`check_cracked_messages`, which looks for messages split into chunks
which are hard to translate; `check_untranslated_cat`, which looks for
messages displayed via [`cat()`](https://rdrr.io/r/base/cat.html) which
are not marked for translation; and `check_untranslated_src`, which
looks for messages in the `src` directory which are not marked for
translation.

These just crack the surface of the types of diagnostics that are
possible for improving the quality of messaging to users – not only in
the process of translation, but also for bettering the experience in
English!

In this vignette we’ll demonstrate just such a use case by writing a
custom diagnostic function that checks for typos in your messages by
applying the function
[`utils::aspell()`](https://rdrr.io/r/utils/aspell.html).

## Writing the diagnostic

We’ll call our function `check_spelling`; it will take as input a
`data.table` like that produced by
[`get_message_data()`](https://michaelchirico.github.io/potools/reference/get_message_data.md),
and give as output a `data.table` indexing any issues found.
Specifically, it should have three or four columns: `call`, `file`,
`line_number`, and `replacement`. The first three come directly from the
input; the last one is optional and suggests to the user a way to repair
any “unhealthy” messages.

``` r
check_spelling = function(message_data) {
  # if aspell isn't installed, this won't work; be sure to return an object with the right schema anyway
  if (!nzchar(Sys.which("aspell"))) {
    warning("'aspell' is not installed; returning nothing")
    return(message_data[0, .(call, file, line_number)])
  }

  # aspell() works on files, so we'll write the msgid to files
  aspell_dir <- file.path(tempdir(), 'aspell')
  dir.create(aspell_dir)
  original_dir <- setwd(aspell_dir)
  on.exit({
    unlink(aspell_dir, recursive = TRUE)
    setwd(original_dir)
  })

  # (!is_repeat) makes sure we only check duplicate messages once
  # plural messages are in a list, so handle them separately
  message_data[(!is_repeat), by = .(file, type), {
    if (.BY$type == "singular") {
      cat(msgid, file = .BY$file, sep = "\n")
      # aspell() results has 5 columns: Original, File, Line, Column, Suggestions; we only need 1 & 5
      results = utils::aspell(.BY$file)
      unlink(.BY$file)

      typo_idx <- sapply(results$Original, grep, msgid)
      # take the first suggestion
      replacement = sapply(
        seq_along(results$Suggestions),
        function(typo_i) {
          # take the identified typo & replace it with aspell's 1st suggestion in the original `call`
          gsub(
            results$Original[typo_i], results$Suggestions[[typo_i]][1L],
            call[typo_idx[typo_i]], fixed = TRUE
          )
        }
      )

      .(
        call = call[typo_idx],
        file = file[typo_idx],
        line_number = line_number[typo_idx],
        replacement = replacement
      )
    } else {
      # unlist() to write both the n=1 and n!=1 messages to the file side-by-side
      all_msgid <- unlist(msgid_plural)
      cat(all_msgid, file = .BY$file, sep = "\n")
      results = utils::aspell(.BY$file)
      unlink(.BY$file)

      # odd numbers in grep output --> first entry for each plural_msgid; even numbers --> second entry.
      # do this arithmetic trick to re-map that to the original entry number in msgid_plural
      typo_idx <- ((sapply(results$Original, grep, all_msgid) - 1L) %/% 2L) + 1L
      # potentially overwrite each call >1 time if both messages have a typo
      replacement = call
      for (typo_i in seq_along(results$Suggestions)) {
        replacement[typo_idx[typo_i]] <- gsub(
          results$Original[typo_i], results$Suggestions[[typo_i]][1L],
          replacement[typo_idx[typo_i]], fixed = TRUE
        )
      }
      typo_idx <- unique(typo_idx)

      .(
        call = call[typo_idx],
        file = file[typo_idx],
        line_number = line_number[typo_idx],
        replacement = replacement[typo_idx]
      )
    }
  }]
}
```

In a package, we would probably use a few more helper functions to clean
up & simplify the body of this diagnostic; we’re piling everything in
sequence for illustration to have everything in one place.

## Running the diagnostic

We can check how the diagnostic works on a simple test package
`GreatSpelling` created for this vignette.

``` r
library(potools)
great_spelling_messages = get_message_data("GreatSpelling")
```

    ## Getting R-level messages...

``` r
# showing the structure of the messagedata for this package
great_spelling_messages
```

    ##    message_source     type       file                           msgid
    ##            <char>   <char>     <char>                          <char>
    ## 1:              R singular    hazel.R These dark arts are forbiddden!
    ## 2:              R singular spellman.R     This is byond my abilities!
    ## 3:              R   plural   merlin.R                            <NA>
    ##                  msgid_plural                                         call
    ##                        <list>                                       <char>
    ## 1:                     [NULL]      stop("These dark arts are forbiddden!")
    ## 2:                     [NULL]       warning("This is byond my abilities!")
    ## 3: %d lyfe left,%d lyves left ngettext(n, "%d lyfe left", "%d lyves left")
    ##    line_number is_repeat is_marked_for_translation is_templated
    ##          <int>    <lgcl>                    <lgcl>       <lgcl>
    ## 1:           2     FALSE                      TRUE        FALSE
    ## 2:           2     FALSE                      TRUE        FALSE
    ## 3:           2     FALSE                      TRUE        FALSE

``` r
# running our diagnostic
check_spelling(great_spelling_messages)
```

    ## Warning in check_spelling(great_spelling_messages): 'aspell' is not installed;
    ## returning nothing

    ## Empty data.table (0 rows and 3 cols): call,file,line_number

That should covers the basics – I look forward to seeing all the great
uses you more creative developers can devise. Thanks for reading!
