# translate_package arg checking errors work

    Code
      translate_package()

---

    Code
      translate_package(verbose = TRUE)
    Message <simpleMessage>
      Starting translations for package 'noMsg'
      Getting R-level messages.
      No messages to translate; finishing

---

    Code
      translate_package(verbose = TRUE)
    Message <simpleMessage>
      Starting translations for package 'rDataPkg'
      No messages to translate; finishing

---

    Code
      translate_package(verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rMsg' (last updated 0000-01-01 00:00:00)
      Getting R-level messages.
      Running message diagnostics.
      Writing R-rMsg.pot
      No languages provided; finishing

---

    Code
      translate_package(languages = "zh_CN", verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rMsg' (last updated 0000-01-01 00:00:00)
      Getting R-level messages.
      Running message diagnostics.
      Writing R-rMsg.pot
      Beginning new translations for zh_CN (Mainland Chinese/普通话); found 6 untranslated messages
      (To quit translating, press 'Esc'; progress will be saved)
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
    Message <simpleMessage>
      ***************************
      ** BEGINNING TRANSLATION **
      ***************************
      
      Some helpful reminders:
       * You can skip a translation by entering nothing (just press RETURN)
       * Special characters (like newlines, \n, or tabs, \t) should be written just like that (with an escape)
       * Be sure to match message templates. The count of templates (%s, %d, etc.) must match in all languages, as must initial and terminal newlines (\n)
       * While the count of templates must match, the _order_ can be changed by using e.g. %2$s to mean 'use the second input as a string here'
       * Whenever templates or escaping is happening in a string, these will be 'highlighted' by carets (^) in the line below
    Output
      
      File: foo.R
      Call: base::warning("I warned you!")
      Message: I warned you!
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: stop("Oh no you don't!")
      Message: Oh no you don't!
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: gettext("Hi there")
      Message: Hi there
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Mainland Chinese?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail", msg2 = "big fail")
      Plural message: small fail
                      
      How would you translate this message into Mainland Chinese independently of n?
    Message <simpleMessage>
      Writing R-zh_CN.po
      Recompiling 'zh_CN' R translation
      running msgfmt on R-zh_CN.po succeeded; output:
        5 translated messages.

---

    Code
      translate_package(languages = "fa", verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rMsg' (last updated 0000-01-01 00:00:00)
      Getting R-level messages.
      Running message diagnostics.
      Writing R-rMsg.pot
      Found existing R translations for fa (Farsi/فارسی) in ./po/R-fa.po. Running msgmerge.
      . done.
      Translations for fa are up to date! Skipping.
      Recompiling 'fa' R translation
      running msgfmt on R-fa.po succeeded; output:
        5 translated messages.

---

    Code
      translate_package(languages = "zh_CN", verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rFuzzyMsg' (last updated 0000-01-01 00:00:00)
      Getting R-level messages.
      Running message diagnostics.
      Writing R-rFuzzyMsg.pot
      Found existing R translations for zh_CN (Mainland Chinese/普通话) in ./po/R-zh_CN.po. Running msgmerge.
      . done.
      Found 2 translations marked as deprecated in ./po/R-zh_CN.po.
      Typically, this means the corresponding error messages have been refactored.
      Reproducing these messages here for your reference since they might still provide some utility.
       ** SINGULAR MESSAGES **
    Output
      ------------------------------------------------------------------------
      Oh no you don't!
      当然不会！
    Message <simpleMessage>
       ** PLURAL MESSAGES **
    Output
      ------------------------------------------------------------------------
      small fail\n
      失败了\n
      ------------------------------------------------------------------------
      big fail\n
      失败了\n
    Message <simpleMessage>
      Beginning new translations for zh_CN (Mainland Chinese/普通话); found 2 untranslated messages
      (To quit translating, press 'Esc'; progress will be saved)
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
    Message <simpleMessage>
      ***************************
      ** BEGINNING TRANSLATION **
      ***************************
      
      Some helpful reminders:
       * You can skip a translation by entering nothing (just press RETURN)
       * Special characters (like newlines, \n, or tabs, \t) should be written just like that (with an escape)
       * Be sure to match message templates. The count of templates (%s, %d, etc.) must match in all languages, as must initial and terminal newlines (\n)
       * While the count of templates must match, the _order_ can be changed by using e.g. %2$s to mean 'use the second input as a string here'
       * Whenever templates or escaping is happening in a string, these will be 'highlighted' by carets (^) in the line below
    Output
      
      File: foo.R
      Call: stop("I really wish you'd reconsider")
      Message: I really wish you'd reconsider
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: ngettext(length(x), "SOMEWHAT EPIC FAIL", "MAJORLY EPIC FAIL")
      Plural message: SOMEWHAT EPIC FAIL
                      
      How would you translate this message into Mainland Chinese independently of n?
    Message <simpleMessage>
      Writing R-zh_CN.po
      Recompiling 'zh_CN' R translation
      running msgfmt on R-zh_CN.po succeeded; output:
        3 translated messages.

---

    Code
      translate_package(languages = "zh_CN")
    Message <simpleMessage>
      Found 4 untranslated messaging calls passed through cat():
    Output
      
      Problematic call:
      base::cat("I warned you!", fill=TRUE, append=TRUE)
      < File:foo.R, Line:2 >
      Potential replacement:
      cat(gettext("I warned you!"), fill=TRUE)
      
      Problematic call:
      cat("Oh no", "you\ndon't!")
      < File:foo.R, Line:8 >
      Potential replacement:
      cat(gettext("Oh no you\ndon't!"))
      
      Problematic call:
      cat("Hi", "boss", sep="xx")
      < File:foo.R, Line:15 >
      Potential replacement:
      cat(gettext("Hixxboss"))
      
      Problematic call:
      cat("This costs", x, "dollars")
      < File:foo.R, Line:22 >
      Potential replacement:
      cat(gettextf("This costs %s dollars", x))
      Exit now to repair any of these? [y/N]

---

    Code
      translate_package(languages = "cy")
    Message <simpleMessage>
      Writing R-rMsg.pot
      'cy' is not a known language. 
      Please help supply some metadata about it. You can check https://l10n.gnome.org/teams/<language>
    Output
      How would you refer to this language in English?
      How would you refer to this language in the language itself?
      How many pluralizations are there for this language [nplurals]?
    Message <simpleMessage>
      Input must be of type 'integer', but received 'character'. Trying again.
    Output
      How many pluralizations are there for this language [nplurals]?
      What is the rule for deciding which plural applies as a function of n [plural]?
    Message <simpleMessage>
      Supplied 'plural':
      (n==1) ? 0 : (n==2) ? 1 : (n != 8 && n != 11) ? 2 : 3
      Did not match any known 'plural's:
      (n!=1)
      (n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)
      (n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)
      (n>1)
      0
      n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 && n%100<=99 ? 4 : 5
      Using generic description of cases instead.
      Thanks! Please file an issue on GitHub to get this language recognized permanently
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: foo.R
      Call: base::warning("I warned you!")
      Message: I warned you!
               
      How would you translate this message into Welsh?
      
      File: foo.R
      Call: stop("Oh no you don't!")
      Message: Oh no you don't!
               
      How would you translate this message into Welsh?
      
      File: foo.R
      Call: gettext("Hi there")
      Message: Hi there
               
      How would you translate this message into Welsh?
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Welsh?
      
      
      ** Oops! Invalid translation -- received the same set of templates + bordering newlines, but in incorrect order ([%.02f, %d, %s] vs [%.02f, %s, %d]). Recall that you can use %$N to do redirect, e.g. to swap the order of '%d %s' to be translated more naturally, your translation can use '%1$s %2$d'. Retrying. **
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Welsh?
      
      
      ** Oops! Invalid translation -- received templates + bordering newlines not present in the original: %s. Retrying. **
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Welsh?
      
      
      ** Oops! Invalid translation -- received the same set of templates + bordering newlines, but in incorrect order ([%.02f, %d, %s] vs [%1$.02f, %2$s, %3$d]). Recall that you can use %$N to do redirect, e.g. to swap the order of '%d %s' to be translated more naturally, your translation can use '%1$s %2$d'. Retrying. **
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Welsh?
      
      
      ** Oops! Invalid translation -- received 2 unique templated arguments + bordering newlines but there are 3 in the original. Retrying. **
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Welsh?
      
      
      ** Oops! Invalid translation -- received 4 unique templated arguments + bordering newlines but there are 3 in the original. Retrying. **
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Welsh?
      
      
      ** Oops! Invalid translation -- received 5 unique templated arguments + bordering newlines but there are 3 in the original. Retrying. **
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Welsh?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail", msg2 = "big fail")
      Plural message: small fail
                      
      How would you translate this message into Welsh for n where 'plural' resolves to 0?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail", msg2 = "big fail")
      Plural message: small fail
                      
      How would you translate this message into Welsh for n where 'plural' resolves to 1?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail", msg2 = "big fail")
      Plural message: small fail
                      
      How would you translate this message into Welsh for n where 'plural' resolves to 2?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail", msg2 = "big fail")
      Plural message: small fail
                      
      How would you translate this message into Welsh for n where 'plural' resolves to 3?
    Message <simpleMessage>
      Writing R-cy.po

---

    Code
      translate_package(languages = "ca", diagnostics = NULL)
    Message <simpleMessage>
      Writing R-rMsg.pot
      'ca' is not a known language. 
      Please help supply some metadata about it. You can check https://l10n.gnome.org/teams/<language>
    Output
      How would you refer to this language in English?
      How would you refer to this language in the language itself?
      How many pluralizations are there for this language [nplurals]?
      What is the rule for deciding which plural applies as a function of n [plural]?
    Message <simpleMessage>
      Thanks! Please file an issue on GitHub to get this language recognized permanently
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: foo.R
      Call: base::warning("I warned you!")
      Message: I warned you!
               
      How would you translate this message into Catalan?
      
      File: foo.R
      Call: stop("Oh no you don't!")
      Message: Oh no you don't!
               
      How would you translate this message into Catalan?
      
      File: foo.R
      Call: gettext("Hi there")
      Message: Hi there
               
      How would you translate this message into Catalan?
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Catalan?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail", msg2 = "big fail")
      Plural message: small fail
                      
      How would you translate this message into Catalan when n = 1?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail", msg2 = "big fail")
      Plural message: small fail
                      
      How would you translate this message into Catalan when n is not 1?
    Message <simpleMessage>
      Writing R-ca.po

---

    Code
      translate_package(languages = "zh_CN", diagnostics = NULL)
    Message <simpleMessage>
      Writing R-rMsg.pot
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: foo.R
      Call: base::warning("I warned you!")
      Message: I warned you!
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: stop("Oh no you don't!")
      Message: Oh no you don't!
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: gettext("Hi there")
      Message: Hi there
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Mainland Chinese?
    Error <simpleError>
      Invalid templated message. If any %N$ redirects are used, all templates must be redirected.
      	Redirected tempates: %1$d
      	 Un-redirected templates: %d
    Message <simpleMessage>
      Writing R-zh_CN.po

---

    Code
      translate_package(languages = "zh_CN", diagnostics = NULL)
    Message <simpleMessage>
      Writing R-rMsg.pot
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: foo.R
      Call: base::warning("I warned you!")
      Message: I warned you!
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: stop("Oh no you don't!")
      Message: Oh no you don't!
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: gettext("Hi there")
      Message: Hi there
               
      How would you translate this message into Mainland Chinese?
      
      File: foo.R
      Call: gettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )
      Message: Avg cat() failures: %.02f; N failures: %d; failure: %s
                                   ^---^              ^^           ^^
      How would you translate this message into Mainland Chinese?
    Error <simpleError>
      Invalid templated message string with redirects -- all messages pointing to the same input must have identical formats, but received [%1$s, %1$d]
    Message <simpleMessage>
      Writing R-zh_CN.po

---

    Code
      translate_package(languages = "zh_CN", diagnostics = check_untranslated_src)
    Message <simpleMessage>
      Writing R-rSrcMsg.pot
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: foo.R
      Call: message("a string")
      Message: a string
               
      How would you translate this message into Mainland Chinese?
      
      File: bar.c
      Call: N_("Don't translate me now.")
      Message: Don't translate me now.
               
      How would you translate this message into Mainland Chinese?
      
      File: bar.c
      Call: Rprintf(_("an translated templated string: %"  "<PRId64>"  "\n"), 10000LL)
      Message: an translated templated string: %<PRId64>\n
                                               ^-------^^^
      How would you translate this message into Mainland Chinese?
      
      File: bar.c
      Call: warning(_("a translated "\
      "warning: %s\n"), stardust(z))
      Message: a translated warning: %s\n
                                     ^^^^
      How would you translate this message into Mainland Chinese?
      
      File: bar.c
      Call: snprintf(BUF, 100, _("a simple message"))
      Message: a simple message
               
      How would you translate this message into Mainland Chinese?
      
      File: bar.c
      Call: ngettext("singular", "plural", z)
      Plural message: singular
                      
      How would you translate this message into Mainland Chinese independently of n?
      
      File: bar.c
      Call: ngettext("singular %d", "plural %d", z)
      Plural message: singular %d
                               ^^
      How would you translate this message into Mainland Chinese independently of n?
    Message <simpleMessage>
      Writing R-zh_CN.po

---

    Code
      translate_package(languages = "zh_CN", verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rSrcFuzzyMsg' (last updated 0000-01-01 00:00:00)
      Getting R-level messages.
      Getting src-level messages.
      Running message diagnostics.
      Writing R-rSrcFuzzyMsg.pot
      Found existing R translations for zh_CN (Mainland Chinese/普通话) in ./po/R-zh_CN.po. Running msgmerge.
      . done.
      Found existing src translations for zh_CN (Mainland Chinese/普通话) in ./po/zh_CN.po. Running msgmerge.
      . done.
      Translations for zh_CN are up to date! Skipping.
      Recompiling 'zh_CN' R translation
      running msgfmt on R-zh_CN.po succeeded; output:
        1 translated message.
      Recompiling 'zh_CN' src translation
      running msgfmt on zh_CN.po succeeded; output:
        0 translated messages, 2 fuzzy translations.

---

    Code
      translate_package(languages = "es", copyright = "Mata Hari", diagnostics = NULL)
    Message <simpleMessage>
      Writing R-rMsgUnusual.pot
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: copy1.R
      Call: stop("copy one")
      Message: copy one
               
      How would you translate this message into Spanish?
      
      File: copy2.R
      Call: stop("copy two")
      Message: copy two
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: base::warning("  I warned you!\n\n")
      Message: I warned you!
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message(r"('abc')")
      Message: 'abc'
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message(R'("def")')
      Message: "def"
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message("R('abc')")
      Message: R('abc')
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message('r("def")')
      Message: r("def")
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message(R'---[ghi]---')
      Message: ghi
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: gettext("Hi there")
      Message: Hi there
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: gettextf(fmt = "good %s ", "grief")
      Message: good %s
                    ^^
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: "first"
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: second
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: third
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: fourth
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: fifth
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: sixth
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message("\\n vs \n is OK")
      Message: \\n vs \n is OK
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message("\\t vs \t is OK")
      Message: \\t vs \t is OK
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message('strings with "quotes" are OK')
      Message: strings with "quotes" are OK
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message("strings with escaped \"quotes\" are OK")
      Message: strings with escaped "quotes" are OK
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: gettextf( paste("part 1 %s", "part 2"), "input" )
      Message: part 1 %s
                      ^^
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: gettextf( paste("part 1 %s", "part 2"), "input" )
      Message: part 2
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: ngettext( 10, "singular ", "plural " )
      Plural message: singular 
                      
      How would you translate this message into Spanish when n = 1?
      
      File: foo.R
      Call: ngettext( 10, "singular ", "plural " )
      Plural message: singular 
                      
      How would you translate this message into Spanish when n is not 1?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail ", msg2 = "big fail ")
      Plural message: small fail 
                      
      How would you translate this message into Spanish when n = 1?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail ", msg2 = "big fail ")
      Plural message: small fail 
                      
      How would you translate this message into Spanish when n is not 1?
      
      File: ABCDEFGHIJKLMNOPQRSTUVWXYZ.c
      Call: _("an translated templated string: %"  "<PRId64>"  "\n")
      Message: an translated templated string: %<PRId64>\n
                                               ^-------^^^
      How would you translate this message into Spanish?
      
      File: MSGs.c
      Call: _("any old \
      message")
      Message: any old message
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: _("a message in a macro %s")
      Message: a message in a macro %s
                                    ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: Rprintf(_("that's a mighty big %"  "<PRId64>""-sized wall over %""<PRIu64>"), 100LL, 10L)
      Message: that's a mighty big %<PRId64>-sized wall over %<PRIu64>
                                   ^-------^                 ^-------^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: Rprintf(_("/* this is what a C comment looks like */ "))
      Message: /* this is what a C comment looks like */ 
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: Rprintf(_("// this is what a C comment looks like %s "), "abc")
      Message: // this is what a C comment looks like %s 
                                                      ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: Rprintf(_(
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
        ))
      Message: 01234567890123456789.0123456789012345678901234567890123456789.0123456789012345678901234567890123456789.0123456789012345678901234567890123456789.01234567890123456789
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("This message\nSpans two lines"))
      Message: This message\nSpans two lines
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("This one does not\n"))
      Message: This one does not\n
                                ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("Exotic formatters like %I32u, %llx, %li, %ls, %lc"))
      Message: Exotic formatters like %I32u, %llx, %li, %ls, %lc
                                      ^---^  ^--^  ^-^  ^-^  ^-^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456\"890"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345(\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345("890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345'\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345'"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345a\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345a"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345A\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345A"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345#\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345#"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345@\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345@"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s."))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s.
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s]"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s]
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_(test ? "abc" : "def"))
      Message: abc
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_(xxx "abc" "def"))
      Message: abcdef
               
      How would you translate this message into Spanish?
      
      File: z.c
      Call: error(_("You found me!"))
      Message: You found me!
               
      How would you translate this message into Spanish?
      
      File: cairo/bedfellows.c
      Call: _(
            "any new message")
      Message: any new message
               
      How would you translate this message into Spanish?
    Message <simpleMessage>
      Writing R-es.po

---

    Code
      translate_package(languages = "es", use_base_rules = TRUE, diagnostics = NULL)
    Message <simpleMessage>
      Writing R-rMsgUnusual.pot
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: copy1.R
      Call: stop("copy one")
      Message: copy one
               
      How would you translate this message into Spanish?
      
      File: copy2.R
      Call: stop("copy two")
      Message: copy two
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: base::warning("  I warned you!\n\n")
      Message: I warned you!
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message(r"('abc')")
      Message: 'abc'
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message(R'("def")')
      Message: "def"
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message("R('abc')")
      Message: R('abc')
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message('r("def")')
      Message: r("def")
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message(R'---[ghi]---')
      Message: ghi
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: gettext("Hi there")
      Message: Hi there
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: gettextf(fmt = "good %s ", "grief")
      Message: good %s
                    ^^
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: "first"
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: second
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: third
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: fourth
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: fifth
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: warning( '"first"', "second", "third", "fourth", "fifth", "sixth" )
      Message: sixth
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message("\\n vs \n is OK")
      Message: \\n vs \n is OK
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message("\\t vs \t is OK")
      Message: \\t vs \t is OK
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message('strings with "quotes" are OK')
      Message: strings with "quotes" are OK
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: message("strings with escaped \"quotes\" are OK")
      Message: strings with escaped "quotes" are OK
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: gettextf( paste("part 1 %s", "part 2"), "input" )
      Message: part 1 %s
                      ^^
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: gettextf( paste("part 1 %s", "part 2"), "input" )
      Message: part 2
               
      How would you translate this message into Spanish?
      
      File: foo.R
      Call: ngettext( 10, "singular ", "plural " )
      Plural message: singular 
                      
      How would you translate this message into Spanish when n = 1?
      
      File: foo.R
      Call: ngettext( 10, "singular ", "plural " )
      Plural message: singular 
                      
      How would you translate this message into Spanish when n is not 1?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail ", msg2 = "big fail ")
      Plural message: small fail 
                      
      How would you translate this message into Spanish when n = 1?
      
      File: windows/bar.R
      Call: ngettext(length(x), msg1 = "small fail ", msg2 = "big fail ")
      Plural message: small fail 
                      
      How would you translate this message into Spanish when n is not 1?
      
      File: ABCDEFGHIJKLMNOPQRSTUVWXYZ.c
      Call: _("an translated templated string: %"  "<PRId64>"  "\n")
      Message: an translated templated string: %<PRId64>\n
                                               ^-------^^^
      How would you translate this message into Spanish?
      
      File: MSGs.c
      Call: _("any old \
      message")
      Message: any old message
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: _("a message in a macro %s")
      Message: a message in a macro %s
                                    ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: Rprintf(_("that's a mighty big %"  "<PRId64>""-sized wall over %""<PRIu64>"), 100LL, 10L)
      Message: that's a mighty big %<PRId64>-sized wall over %<PRIu64>
                                   ^-------^                 ^-------^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: Rprintf(_("/* this is what a C comment looks like */ "))
      Message: /* this is what a C comment looks like */ 
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: Rprintf(_("// this is what a C comment looks like %s "), "abc")
      Message: // this is what a C comment looks like %s 
                                                      ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: Rprintf(_(
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
        ))
      Message: 01234567890123456789.0123456789012345678901234567890123456789.0123456789012345678901234567890123456789.0123456789012345678901234567890123456789.01234567890123456789
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("This message\nSpans two lines"))
      Message: This message\nSpans two lines
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("This one does not\n"))
      Message: This one does not\n
                                ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("Exotic formatters like %I32u, %llx, %li, %ls, %lc"))
      Message: Exotic formatters like %I32u, %llx, %li, %ls, %lc
                                      ^---^  ^--^  ^-^  ^-^  ^-^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456\"890"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345(\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345("890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345'\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345'"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345a\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345a"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345A\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345A"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345#\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345#"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345@\"890"))
      Message: 0123456789012345678901234567890123456789012345678901234567890123456789012345@"890
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s."))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s.
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s]"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s]
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-"))
      Message: 01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-
                                                                                              ^^
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_(test ? "abc" : "def"))
      Message: abc
               
      How would you translate this message into Spanish?
      
      File: msg.c
      Call: error(_(xxx "abc" "def"))
      Message: abcdef
               
      How would you translate this message into Spanish?
      
      File: z.c
      Call: error(_("You found me!"))
      Message: You found me!
               
      How would you translate this message into Spanish?
      
      File: cairo/bedfellows.c
      Call: _(
            "any new message")
      Message: any new message
               
      How would you translate this message into Spanish?
    Message <simpleMessage>
      Writing R-es.po

---

    Code
      translate_package(languages = "es", max_translations = 1L, diagnostics = NULL)
    Message <simpleMessage>
      Writing R-rMsg.pot
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: foo.R
      Call: base::warning("I warned you!")
      Message: I warned you!
               
      How would you translate this message into Spanish?
    Message <simpleMessage>
      Writing R-es.po

