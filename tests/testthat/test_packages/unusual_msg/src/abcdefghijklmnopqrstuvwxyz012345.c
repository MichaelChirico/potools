// a duplicate of a string in msg.c; the file name is constructed so that
// `#: $FILE.c:3 msg.c:43` is exactly 79 characters wide
Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
