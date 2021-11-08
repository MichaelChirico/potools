# translate_package arg checking errors work

    Code
      translate_package()

---

    Code
      translate_package(verbose = TRUE)
    Message <simpleMessage>
      Starting translations for package 'noMsg'
    Message <simpleMessage>
      Getting R-level messages...
    Message <simpleMessage>
      Getting src-level messages...
    Message <simpleMessage>
      No messages to translate; finishing

---

    Code
      translate_package(verbose = TRUE)
    Message <simpleMessage>
      Starting translations for package 'rDataPkg'
    Message <simpleMessage>
      Getting R-level messages...
    Message <simpleMessage>
      Getting src-level messages...
    Message <simpleMessage>
      No messages to translate; finishing

---

    Code
      translate_package(verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rMsg' (last updated 2021-11-07 23:42:57)
    Message <simpleMessage>
      Getting R-level messages...
    Message <simpleMessage>
      Getting src-level messages...
    Message <simpleMessage>
      Running message diagnostics...
    Message <simpleMessage>
      Generating .pot files...
    Message <simpleMessage>
      Generating en@quot translations
    Message <simpleMessage>
      No languages provided; finishing

---

    Code
      translate_package(languages = "zh_CN", verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rMsg' (last updated 2021-11-07 23:42:58)
    Message <simpleMessage>
      Getting R-level messages...
    Message <simpleMessage>
      Getting src-level messages...
    Message <simpleMessage>
      Running message diagnostics...
    Message <simpleMessage>
      Generating .pot files...
    Message <simpleMessage>
      Generating en@quot translations
    Message <simpleMessage>
      Beginning new translations for zh_CN (Mainland Chinese/普通话); found 6 untranslated messages
    Message <simpleMessage>
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
      
      File: [37mfoo.R[39m
      Call: [32mbase::warning("I warned you!")[39m
      Message: [31mI warned you![39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mstop("Oh no you don't!")[39m
      Message: [31mOh no you don't![39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettext("Hi there")[39m
      Message: [31mHi there[39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail", msg2 = "big fail")[39m
      Plural message: [31msmall fail[39m
                      
      How would you translate this message into [36mMainland Chinese[39m [33mindependently of n[39m?
    Message <simpleMessage>
      "Installing" translations with msgfmt

---

    Code
      translate_package(languages = "fa", verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rMsg' (last updated 2021-11-07 23:42:58)
    Message <simpleMessage>
      Getting R-level messages...
    Message <simpleMessage>
      Getting src-level messages...
    Message <simpleMessage>
      Running message diagnostics...
    Message <simpleMessage>
      Generating .pot files...
    Message <simpleMessage>
      Generating en@quot translations
    Message <simpleMessage>
      Found existing R translations for fa (Farsi/فارسی) in /tmp/RtmpoWfRHo/file775e716e91bb/r_msg/po/R-fa.po. Running msgmerge...
    Message <simpleMessage>
      Translations for fa are up to date! Skipping.
    Message <simpleMessage>
      "Installing" translations with msgfmt

---

    Code
      translate_package(languages = "zh_CN", verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rFuzzyMsg' (last updated 2021-11-07 23:42:59)
    Message <simpleMessage>
      Getting R-level messages...
    Message <simpleMessage>
      Getting src-level messages...
    Message <simpleMessage>
      Running message diagnostics...
    Message <simpleMessage>
      Generating .pot files...
    Message <simpleMessage>
      Generating en@quot translations
    Message <simpleMessage>
      Found existing R translations for zh_CN (Mainland Chinese/普通话) in /tmp/RtmpoWfRHo/file775e707d4657/r_fuzzy/po/R-zh_CN.po. Running msgmerge...
    Message <simpleMessage>
      Found 2 translations marked as deprecated in /tmp/RtmpoWfRHo/file775e707d4657/r_fuzzy/po/R-zh_CN.po.
    Message <simpleMessage>
      Typically, this means the corresponding error messages have been refactored.
    Message <simpleMessage>
      Reproducing these messages here for your reference since they might still provide some utility.
    Message <simpleMessage>
       ** SINGULAR MESSAGES **
    Output
      ----------------------------------------------------------------------------------
      Oh no you don't!
      当然不会！
    Message <simpleMessage>
       ** PLURAL MESSAGES **
    Output
      ----------------------------------------------------------------------------------
      small fail\n
      失败了\n
      ----------------------------------------------------------------------------------
      big fail\n
      失败了\n
    Message <simpleMessage>
      Beginning new translations for zh_CN (Mainland Chinese/普通话); found 3 untranslated messages
    Message <simpleMessage>
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
      
      File: [37mfoo.R[39m
      Call: [32mwarning("I warned you!!")[39m
      Message: [31mI warned you!![39m
               
      How would you translate this message into [36mMainland Chinese[39m?
       **Note: a similar message was previously translated as: **
      已经告诉你！
      
      File: [37mfoo.R[39m
      Call: [32mstop("I really wish you'd reconsider")[39m
      Message: [31mI really wish you'd reconsider[39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mngettext(length(x), "SOMEWHAT EPIC FAIL", "MAJORLY EPIC FAIL")[39m
      Plural message: [31mSOMEWHAT EPIC FAIL[39m
                      
      How would you translate this message into [36mMainland Chinese[39m [33mindependently of n[39m?
    Message <simpleMessage>
      "Installing" translations with msgfmt

---

    Code
      translate_package(languages = "zh_CN")
    Message <simpleMessage>
      Found 4 untranslated messaging calls passed through cat():
    Output
      
      Problematic call:
      [32mbase::cat("I warned you!", fill=TRUE, append=TRUE)[39m
      < File:[37mfoo.R[39m, Line:[37m2[39m >
      Potential replacement:
      [34mcat(gettext("I warned you!"), fill=TRUE)[39m
      
      Problematic call:
      [32mcat("Oh no", "you\ndon't!")[39m
      < File:[37mfoo.R[39m, Line:[37m8[39m >
      Potential replacement:
      [34mcat(gettext("Oh no you\ndon't!"))[39m
      
      Problematic call:
      [32mcat("Hi", "boss", sep="xx")[39m
      < File:[37mfoo.R[39m, Line:[37m15[39m >
      Potential replacement:
      [34mcat(gettext("Hixxboss"))[39m
      
      Problematic call:
      [32mcat("This costs", x, "dollars")[39m
      < File:[37mfoo.R[39m, Line:[37m22[39m >
      Potential replacement:
      [34mcat(gettextf("This costs %s dollars", x))[39m
      Exit now to repair any of these? [y/N]

---

    Code
      translate_package(languages = "cy")
    Message <simpleMessage>
      Generating en@quot translations
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: [37mfoo.R[39m
      Call: [32mbase::warning("I warned you!")[39m
      Message: [31mI warned you![39m
               
      How would you translate this message into [36mWelsh[39m?
      
      File: [37mfoo.R[39m
      Call: [32mstop("Oh no you don't!")[39m
      Message: [31mOh no you don't![39m
               
      How would you translate this message into [36mWelsh[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettext("Hi there")[39m
      Message: [31mHi there[39m
               
      How would you translate this message into [36mWelsh[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received the same set of templates + bordering newlines, but in incorrect order ([%.02f, %d, %s] vs [%.02f, %s, %d]). Recall that you can use %$N to do redirect, e.g. to swap the order of '%d %s' to be translated more naturally, your translation can use '%1$s %2$d'. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received templates + bordering newlines not present in the original: %s. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received the same set of templates + bordering newlines, but in incorrect order ([%.02f, %d, %s] vs [%1$.02f, %2$s, %3$d]). Recall that you can use %$N to do redirect, e.g. to swap the order of '%d %s' to be translated more naturally, your translation can use '%1$s %2$d'. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received 2 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received 4 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      
      ** Oops! Invalid translation -- received 5 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mWelsh[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail", msg2 = "big fail")[39m
      Plural message: [31msmall fail[39m
                      
      How would you translate this message into [36mWelsh[39m [33mfor n where 'plural' resolves to 0[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail", msg2 = "big fail")[39m
      Plural message: [31msmall fail[39m
                      
      How would you translate this message into [36mWelsh[39m [33mfor n where 'plural' resolves to 1[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail", msg2 = "big fail")[39m
      Plural message: [31msmall fail[39m
                      
      How would you translate this message into [36mWelsh[39m [33mfor n where 'plural' resolves to 2[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail", msg2 = "big fail")[39m
      Plural message: [31msmall fail[39m
                      
      How would you translate this message into [36mWelsh[39m [33mfor n where 'plural' resolves to 3[39m?

---

    Code
      translate_package(languages = "ca", diagnostics = NULL)
    Message <simpleMessage>
      Generating en@quot translations
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: [37mfoo.R[39m
      Call: [32mbase::warning("I warned you!")[39m
      Message: [31mI warned you![39m
               
      How would you translate this message into [36mCatalan[39m?
      
      File: [37mfoo.R[39m
      Call: [32mstop("Oh no you don't!")[39m
      Message: [31mOh no you don't![39m
               
      How would you translate this message into [36mCatalan[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettext("Hi there")[39m
      Message: [31mHi there[39m
               
      How would you translate this message into [36mCatalan[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mCatalan[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mCatalan[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mCatalan[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mCatalan[39m?
      
      
      ** Oops! Invalid translation -- received 0 unique templated arguments + bordering newlines but there are 3 in the original. Retrying... **
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mCatalan[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail", msg2 = "big fail")[39m
      Plural message: [31msmall fail[39m
                      
      How would you translate this message into [36mCatalan[39m [33mwhen n = 1[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail", msg2 = "big fail")[39m
      Plural message: [31msmall fail[39m
                      
      How would you translate this message into [36mCatalan[39m [33mwhen n is not 1[39m?

---

    Code
      translate_package(languages = "zh_CN", diagnostics = NULL)
    Message <simpleMessage>
      Generating en@quot translations
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: [37mfoo.R[39m
      Call: [32mbase::warning("I warned you!")[39m
      Message: [31mI warned you![39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mstop("Oh no you don't!")[39m
      Message: [31mOh no you don't![39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettext("Hi there")[39m
      Message: [31mHi there[39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mMainland Chinese[39m?
    Error <simpleError>
      Invalid templated message. If any %N$ redirects are used, all templates must be redirected.
      	Redirected tempates: %1$d
      	 Un-redirected templates: %d

---

    Code
      translate_package(languages = "zh_CN", diagnostics = NULL)
    Message <simpleMessage>
      Generating en@quot translations
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: [37mfoo.R[39m
      Call: [32mbase::warning("I warned you!")[39m
      Message: [31mI warned you![39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mstop("Oh no you don't!")[39m
      Message: [31mOh no you don't![39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettext("Hi there")[39m
      Message: [31mHi there[39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s", mean(x), length(x), "don't translate me" )[39m
      Message: [31mAvg cat() failures: %.02f; N failures: %d; failure: %s[39m
                                   ^---^              ^^           ^^
      How would you translate this message into [36mMainland Chinese[39m?
    Error <simpleError>
      Invalid templated message string with redirects -- all messages pointing to the same input must have identical formats, but received [%1$s, %1$d]

---

    Code
      translate_package(languages = "zh_CN", diagnostics = check_untranslated_src)
    Message <simpleMessage>
      Generating en@quot translations
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("a string")[39m
      Message: [31ma string[39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mbar.c[39m
      Call: [32mN_("Don't translate me now.")[39m
      Message: [31mDon't translate me now.[39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mbar.c[39m
      Call: [32mRprintf(_("an translated templated string: %"  "<PRId64>"  "\n"), 10000LL)[39m
      Message: [31man translated templated string: %<PRId64>\n[39m
                                               ^-------^^^
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mbar.c[39m
      Call: [32mwarning(_("a translated "\
      "warning: %s\n"), stardust(z))[39m
      Message: [31ma translated warning: %s\n[39m
                                     ^^^^
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mbar.c[39m
      Call: [32msnprintf(BUF, 100, _("a simple message"))[39m
      Message: [31ma simple message[39m
               
      How would you translate this message into [36mMainland Chinese[39m?
      
      File: [37mbar.c[39m
      Call: [32mngettext("singular", "plural", z)[39m
      Plural message: [31msingular[39m
                      
      How would you translate this message into [36mMainland Chinese[39m [33mindependently of n[39m?
      
      File: [37mbar.c[39m
      Call: [32mngettext("singular %d", "plural %d", z)[39m
      Plural message: [31msingular %d[39m
                               ^^
      How would you translate this message into [36mMainland Chinese[39m [33mindependently of n[39m?

---

    Code
      translate_package(languages = "zh_CN", verbose = TRUE)
    Message <simpleMessage>
      Updating translation template for package 'rSrcFuzzyMsg' (last updated 2021-11-07 23:43:01)
    Message <simpleMessage>
      Getting R-level messages...
    Message <simpleMessage>
      Getting src-level messages...
    Message <simpleMessage>
      Running message diagnostics...
    Message <simpleMessage>
      Generating .pot files...
    Message <simpleMessage>
      Generating en@quot translations
    Message <simpleMessage>
      Found existing R translations for zh_CN (Mainland Chinese/普通话) in /tmp/RtmpoWfRHo/file775e4758b448/r_src_fuzzy/po/R-zh_CN.po. Running msgmerge...
    Message <simpleMessage>
      Found existing src translations for zh_CN (Mainland Chinese/普通话) in /tmp/RtmpoWfRHo/file775e4758b448/r_src_fuzzy/po/zh_CN.po. Running msgmerge...
    Message <simpleMessage>
      Beginning new translations for zh_CN (Mainland Chinese/普通话); found 2 untranslated messages
    Message <simpleMessage>
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
      
      File: [37msrc.c[39m
      Call: [32merror(_("I really wish you'd do something else"))[39m
      Message: [31mI really wish you'd do something else[39m
               
      How would you translate this message into [36mMainland Chinese[39m?
       **Note: a similar message was previously translated as: **
      失败了
      
      File: [37msrc.c[39m
      Call: [32mRprintf(_("Here's what is wrong: %s %s\n"), "a", "b")[39m
      Message: [31mHere's what is wrong: %s %s\n[39m
                                     ^^ ^^^^
      How would you translate this message into [36mMainland Chinese[39m?
       **Note: a similar message was previously translated as: **
      当然%s %s\n
    Message <simpleMessage>
      "Installing" translations with msgfmt

---

    Code
      translate_package(languages = "es", copyright = "Mata Hari", diagnostics = NULL)
    Message <simpleMessage>
      Generating en@quot translations
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: [37mcopy1.R[39m
      Call: [32mstop("copy one")[39m
      Message: [31mcopy one[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mcopy2.R[39m
      Call: [32mstop("copy two")[39m
      Message: [31mcopy two[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mbase::warning("  I warned you!\n\n")[39m
      Message: [31mI warned you![39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage(r"('abc')")[39m
      Message: [31m'abc'[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage(R'("def")')[39m
      Message: [31m"def"[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("R('abc')")[39m
      Message: [31mR('abc')[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage('r("def")')[39m
      Message: [31mr("def")[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage(R'---[ghi]---')[39m
      Message: [31mghi[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettext("Hi there")[39m
      Message: [31mHi there[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf(fmt = "good %s ", "grief")[39m
      Message: [31mgood %s[39m
                    ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31m"first"[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31msecond[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31mthird[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31mfourth[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31mfifth[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31msixth[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("\\n vs \n is OK")[39m
      Message: [31m\\n vs \n is OK[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("\\t vs \t is OK")[39m
      Message: [31m\\t vs \t is OK[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage('strings with "quotes" are OK')[39m
      Message: [31mstrings with "quotes" are OK[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("strings with escaped \"quotes\" are OK")[39m
      Message: [31mstrings with escaped "quotes" are OK[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( paste("part 1 %s", "part 2"), "input" )[39m
      Message: [31mpart 1 %s[39m
                      ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( paste("part 1 %s", "part 2"), "input" )[39m
      Message: [31mpart 2[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mngettext( 10, "singular ", "plural " )[39m
      Plural message: [31msingular [39m
                      
      How would you translate this message into [36mSpanish[39m [33mwhen n = 1[39m?
      
      File: [37mfoo.R[39m
      Call: [32mngettext( 10, "singular ", "plural " )[39m
      Plural message: [31msingular [39m
                      
      How would you translate this message into [36mSpanish[39m [33mwhen n is not 1[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail ", msg2 = "big fail ")[39m
      Plural message: [31msmall fail [39m
                      
      How would you translate this message into [36mSpanish[39m [33mwhen n = 1[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail ", msg2 = "big fail ")[39m
      Plural message: [31msmall fail [39m
                      
      How would you translate this message into [36mSpanish[39m [33mwhen n is not 1[39m?
      
      File: [37mABCDEFGHIJKLMNOPQRSTUVWXYZ.c[39m
      Call: [32m_("an translated templated string: %"  "<PRId64>"  "\n")[39m
      Message: [31man translated templated string: %<PRId64>\n[39m
                                               ^-------^^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mMSGs.c[39m
      Call: [32m_("any old \
      message")[39m
      Message: [31many old message[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32m_("a message in a macro %s")[39m
      Message: [31ma message in a macro %s[39m
                                    ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32mRprintf(_("that's a mighty big %"  "<PRId64>""-sized wall over %""<PRIu64>"), 100LL, 10L)[39m
      Message: [31mthat's a mighty big %<PRId64>-sized wall over %<PRIu64>[39m
                                   ^-------^                 ^-------^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32mRprintf(_("/* this is what a C comment looks like */ "))[39m
      Message: [31m/* this is what a C comment looks like */ [39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32mRprintf(_("// this is what a C comment looks like %s "), "abc")[39m
      Message: [31m// this is what a C comment looks like %s [39m
                                                      ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32mRprintf(_(
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
        ))[39m
      Message: [31m01234567890123456789.0123456789012345678901234567890123456789.0123456789012345678901234567890123456789.0123456789012345678901234567890123456789.01234567890123456789[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("This message\nSpans two lines"))[39m
      Message: [31mThis message\nSpans two lines[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("This one does not\n"))[39m
      Message: [31mThis one does not\n[39m
                                ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("Exotic formatters like %I32u, %llx, %li, %ls, %lc"))[39m
      Message: [31mExotic formatters like %I32u, %llx, %li, %ls, %lc[39m
                                      ^---^  ^--^  ^-^  ^-^  ^-^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456\"890"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345(\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345("890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345'\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345'"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345a\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345a"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345A\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345A"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345#\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345#"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345@\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345@"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s."))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s.[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s]"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s][39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_(test ? "abc" : "def"))[39m
      Message: [31mabc[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_(xxx "abc" "def"))[39m
      Message: [31mabcdef[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mz.c[39m
      Call: [32merror(_("You found me!"))[39m
      Message: [31mYou found me![39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mcairo/bedfellows.c[39m
      Call: [32m_(
            "any new message")[39m
      Message: [31many new message[39m
               
      How would you translate this message into [36mSpanish[39m?

---

    Code
      translate_package(languages = "es", use_base_rules = TRUE, diagnostics = NULL)
    Message <simpleMessage>
      Generating en@quot translations
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: [37mcopy1.R[39m
      Call: [32mstop("copy one")[39m
      Message: [31mcopy one[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mcopy2.R[39m
      Call: [32mstop("copy two")[39m
      Message: [31mcopy two[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mbase::warning("  I warned you!\n\n")[39m
      Message: [31mI warned you![39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage(r"('abc')")[39m
      Message: [31m'abc'[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage(R'("def")')[39m
      Message: [31m"def"[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("R('abc')")[39m
      Message: [31mR('abc')[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage('r("def")')[39m
      Message: [31mr("def")[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage(R'---[ghi]---')[39m
      Message: [31mghi[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettext("Hi there")[39m
      Message: [31mHi there[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf(fmt = "good %s ", "grief")[39m
      Message: [31mgood %s[39m
                    ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31m"first"[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31msecond[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31mthird[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31mfourth[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31mfifth[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mwarning( '"first"', "second", "third", "fourth", "fifth", "sixth" )[39m
      Message: [31msixth[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("\\n vs \n is OK")[39m
      Message: [31m\\n vs \n is OK[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("\\t vs \t is OK")[39m
      Message: [31m\\t vs \t is OK[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage('strings with "quotes" are OK')[39m
      Message: [31mstrings with "quotes" are OK[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mmessage("strings with escaped \"quotes\" are OK")[39m
      Message: [31mstrings with escaped "quotes" are OK[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( paste("part 1 %s", "part 2"), "input" )[39m
      Message: [31mpart 1 %s[39m
                      ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mgettextf( paste("part 1 %s", "part 2"), "input" )[39m
      Message: [31mpart 2[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mfoo.R[39m
      Call: [32mngettext( 10, "singular ", "plural " )[39m
      Plural message: [31msingular [39m
                      
      How would you translate this message into [36mSpanish[39m [33mwhen n = 1[39m?
      
      File: [37mfoo.R[39m
      Call: [32mngettext( 10, "singular ", "plural " )[39m
      Plural message: [31msingular [39m
                      
      How would you translate this message into [36mSpanish[39m [33mwhen n is not 1[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail ", msg2 = "big fail ")[39m
      Plural message: [31msmall fail [39m
                      
      How would you translate this message into [36mSpanish[39m [33mwhen n = 1[39m?
      
      File: [37mwindows/bar.R[39m
      Call: [32mngettext(length(x), msg1 = "small fail ", msg2 = "big fail ")[39m
      Plural message: [31msmall fail [39m
                      
      How would you translate this message into [36mSpanish[39m [33mwhen n is not 1[39m?
      
      File: [37mABCDEFGHIJKLMNOPQRSTUVWXYZ.c[39m
      Call: [32m_("an translated templated string: %"  "<PRId64>"  "\n")[39m
      Message: [31man translated templated string: %<PRId64>\n[39m
                                               ^-------^^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mMSGs.c[39m
      Call: [32m_("any old \
      message")[39m
      Message: [31many old message[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32m_("a message in a macro %s")[39m
      Message: [31ma message in a macro %s[39m
                                    ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32mRprintf(_("that's a mighty big %"  "<PRId64>""-sized wall over %""<PRIu64>"), 100LL, 10L)[39m
      Message: [31mthat's a mighty big %<PRId64>-sized wall over %<PRIu64>[39m
                                   ^-------^                 ^-------^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32mRprintf(_("/* this is what a C comment looks like */ "))[39m
      Message: [31m/* this is what a C comment looks like */ [39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32mRprintf(_("// this is what a C comment looks like %s "), "abc")[39m
      Message: [31m// this is what a C comment looks like %s [39m
                                                      ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32mRprintf(_(
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
          "01234567890123456789.01234567890123456789"
        ))[39m
      Message: [31m01234567890123456789.0123456789012345678901234567890123456789.0123456789012345678901234567890123456789.0123456789012345678901234567890123456789.01234567890123456789[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("This message\nSpans two lines"))[39m
      Message: [31mThis message\nSpans two lines[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("This one does not\n"))[39m
      Message: [31mThis one does not\n[39m
                                ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("Exotic formatters like %I32u, %llx, %li, %ls, %lc"))[39m
      Message: [31mExotic formatters like %I32u, %llx, %li, %ls, %lc[39m
                                      ^---^  ^--^  ^-^  ^-^  ^-^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456\"890"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345(\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345("890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345'\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345'"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345a\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345a"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345A\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345A"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345#\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345#"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("0123456789012345678901234567890123456789012345678901234567890123456789012345@\"890"))[39m
      Message: [31m0123456789012345678901234567890123456789012345678901234567890123456789012345@"890[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s."))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s.[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s]"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s][39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-"))[39m
      Message: [31m01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-[39m
                                                                                              ^^
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_(test ? "abc" : "def"))[39m
      Message: [31mabc[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mmsg.c[39m
      Call: [32merror(_(xxx "abc" "def"))[39m
      Message: [31mabcdef[39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mz.c[39m
      Call: [32merror(_("You found me!"))[39m
      Message: [31mYou found me![39m
               
      How would you translate this message into [36mSpanish[39m?
      
      File: [37mcairo/bedfellows.c[39m
      Call: [32m_(
            "any new message")[39m
      Message: [31many new message[39m
               
      How would you translate this message into [36mSpanish[39m?

---

    Code
      translate_package(languages = "es", max_translations = 1L, diagnostics = NULL)
    Message <simpleMessage>
      Generating en@quot translations
    Output
      Thanks! Who should be credited with these translations?
      And what is their email?
      
      File: [37mfoo.R[39m
      Call: [32mbase::warning("I warned you!")[39m
      Message: [31mI warned you![39m
               
      How would you translate this message into [36mSpanish[39m?

