void hello_world(SEXP x) {
  Rprintf(_("that's a mighty big %"  PRId64"-sized wall over %"PRIu64), 100LL, 10L);
  return;
}
