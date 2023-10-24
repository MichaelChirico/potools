# the user is told what's happening

    Code
      po_create("jp", verbose = TRUE)
    Message
      Creating 'jp' R translation
      Running system command msginit -i "./po/R-test.pot" -o "./po/R-jp.po" -l "jp" -w 80 --no-translator...
      Created ./po/R-jp.po.

---

    Code
      po_create("jp", verbose = TRUE)
    Message
      Updating 'jp' R translation
      Running system command msgmerge --update "./po/R-jp.po" --backup=off --previous "./po/R-test.pot"...
      . done.

